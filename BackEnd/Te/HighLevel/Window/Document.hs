module Te.HighLevel.Window.Document
  (DocumentWindow,
   DocumentWindowID,
   documentWindowID,
   getDocumentWindowMinimumSize,
   getDocumentWindowDesiredSize,
   getDocumentWindowTitle,
   getDocumentWindowTitleIcon,
   documentWindowAdjustPanes,
   getDocumentWindowPanes,
   getDocumentWindowHorizontalDividers,
   getDocumentWindowVerticalDividers,
   getDocumentWindowCursorRectangles,
   documentWindowMouseDown,
   documentWindowMouseDragged,
   documentWindowMouseUp)
  where

import Control.Concurrent.MVar
import Control.Monad
import Data.Array.Unboxed
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Data.Geometry
import Te.HighLevel.ApplicationPrivate
import Te.HighLevel.Window
import Te.HighLevel.Window.Document.Pane
import Te.HighLevel.Window.DocumentPrivate
import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.LowLevel.FrontEndCallbacks
import Te.LowLevel.Identifiers
import Te.Types


instance Window DocumentWindow where
  windowID = toWindowID . documentWindowID
  windowProject = documentWindowProject
  getWindowTitle = getDocumentWindowTitle
  getWindowTitleIcon = getDocumentWindowTitleIcon
  browserWindowDo _ default' _ = return default'
  documentWindowDo window _ action = action window


getDocumentWindowMinimumSize :: DocumentWindow -> IO Size
getDocumentWindowMinimumSize documentWindow = do
  getWindowSizeFromPaneSizes documentWindow getDocumentPaneMinimumSize


getDocumentWindowDesiredSize :: DocumentWindow -> IO Size
getDocumentWindowDesiredSize documentWindow = do
  getWindowSizeFromPaneSizes documentWindow getDocumentPaneDesiredSize


getDocumentWindowTitle :: DocumentWindow -> IO String
getDocumentWindowTitle documentWindow = do
  let project = documentWindowProject documentWindow
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    documentInode <- lookupDocumentWindowInode documentWindow
    documentInodeInformation <- lookupInodeInformation documentInode
    let documentName = inodeInformationName documentInodeInformation
    return documentName


getDocumentWindowTitleIcon :: DocumentWindow -> IO String
getDocumentWindowTitleIcon documentWindow = do
  let project = documentWindowProject documentWindow
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "File" $ do
    documentInode <- lookupDocumentWindowInode documentWindow
    documentInodeInformation <- lookupInodeInformation documentInode
    let documentType = inodeInformationType documentInodeInformation
    case documentType of
      DirectoryInodeType -> return "Folder"
      HaskellInodeType -> return "File"


documentWindowAdjustPanes :: DocumentWindow -> IO ()
documentWindowAdjustPanes documentWindow = do
  return ()


getDocumentWindowPanes
  :: DocumentWindow -> IO [DocumentPane]
getDocumentWindowPanes documentWindow = do
  paneMap <- readMVar $ documentWindowPanes documentWindow
  return $ Map.elems paneMap


getDocumentWindowVerticalDividers
  :: DocumentWindow -> IO [DocumentVerticalDivider]
getDocumentWindowVerticalDividers documentWindow = do
  verticalDividerMap <-
    readMVar $ documentWindowVerticalDividers documentWindow
  return $ Map.elems verticalDividerMap


getDocumentWindowHorizontalDividers
  :: DocumentWindow -> IO [DocumentHorizontalDivider]
getDocumentWindowHorizontalDividers documentWindow = do
  horizontalDividerMap <-
    readMVar $ documentWindowHorizontalDividers documentWindow
  return $ Map.elems horizontalDividerMap


getDocumentWindowCursorRectangles
  :: DocumentWindow -> IO [(CursorType, Rectangle)]
getDocumentWindowCursorRectangles window = do
  allDividers <- getAllDividers window
  mapM (\anyDivider -> do
          frame <- getAnyDocumentDividerClickFrame window anyDivider
          case anyDivider of
            AnyDocumentVerticalDivider divider -> do
              panesLeft <-
                readMVar $ documentVerticalDividerPanesLeft divider
              panesRight <-
                readMVar $ documentVerticalDividerPanesRight divider
              case (panesLeft, panesRight) of
                ([], []) -> return Nothing
                ([], _) -> return $ Just (ResizeRightCursorType, frame)
                (_, []) -> return $ Just (ResizeLeftCursorType, frame)
                _ -> return $ Just (ResizeLeftRightCursorType, frame)
            AnyDocumentHorizontalDivider divider -> do
              panesUp <-
                readMVar $ documentHorizontalDividerPanesUp divider
              panesDown <-
                readMVar $ documentHorizontalDividerPanesDown divider
              case (panesUp, panesDown) of
                ([], []) -> return Nothing
                ([], _) -> return $ Just (ResizeDownCursorType, frame)
                (_, []) -> return $ Just (ResizeUpCursorType, frame)
                _ -> return $ Just (ResizeUpDownCursorType, frame)
            FarTopVirtualDocumentHorizontalDivider -> do
              return $ Just (ResizeDownCursorType, frame)
            FarLeftVirtualDocumentVerticalDivider -> do
              return $ Just (ResizeRightCursorType, frame)
            FarRightVirtualDocumentVerticalDivider -> do
              return $ Just (ResizeLeftCursorType, frame))
       allDividers
  >>= return . catMaybes


documentWindowMouseDown
  :: DocumentWindow -> Point -> Bool -> IO ()
documentWindowMouseDown window location optionDown = do
  allDividers <- getAllDividers window
  maybeFoundDivider <-
    foldM (\maybeResult found ->
             case maybeResult of
               Just _ -> return maybeResult
               Nothing -> do
                 frame <- getAnyDocumentDividerClickFrame window found
                 if pointInRectangle location frame
                   then return $ Just found
                   else return Nothing)
          Nothing
          allDividers
  case maybeFoundDivider of
    Nothing -> return ()
    Just foundDivider -> do
      isTerminalDivider <- getIsTerminalDivider window foundDivider
      let creatingNewDivider = isTerminalDivider || optionDown
      hasGhostWindow <- if creatingNewDivider
        then do
          ghostFrame <- getGhostStartingFrame window foundDivider
          case dividerOrientation foundDivider of
            VerticalOrientation -> do
              newGhostWindowWithVerticalDivider window ghostFrame location
            HorizontalOrientation -> do
              newGhostWindowWithHorizontalDivider window ghostFrame location
          return True
        else return False
      let project = documentWindowProject window
          applicationState = projectApplicationState project
      putDragState applicationState
                   $ Just $ DividerDragState {
                                dragStatePreviousDragPoint = location,
                                dragStateHasGhostWindow = hasGhostWindow,
                                dividerDragStateDivider = foundDivider,
                                dividerDragStateCreatedBefore = False,
                                dividerDragStateCreatedAfter = False,
                                dividerDragStateCreatingNewDivider =
                                  creatingNewDivider
                              }


documentWindowMouseDragged
  :: DocumentWindow -> Point -> IO ()
documentWindowMouseDragged window location = do
  let project = documentWindowProject window
      applicationState = projectApplicationState project
  maybeDragState <- getDragState applicationState
  case maybeDragState of
    Nothing -> return ()
    Just dragState@(DividerDragState { }) -> do
      let dragState' = dragState {
                           dragStatePreviousDragPoint = location
                         }
      putDragState applicationState $ Just dragState'
      ghostWindowUpdateMouse applicationState location


documentWindowMouseUp
  :: DocumentWindow -> Point -> IO ()
documentWindowMouseUp window location = do
  let project = documentWindowProject window
      applicationState = projectApplicationState project
  maybeDragState <- getDragState applicationState
  case maybeDragState of
    Nothing -> return ()
    Just dragState@(DividerDragState { }) -> do
      putDragState applicationState Nothing
      cleanupGhostWindow applicationState
