module Te.HighLevel.Window.Document
  (DocumentWindow,
   DocumentWindowID,
   documentWindowID,
   getDocumentWindowDefaultSize,
   getDocumentWindowMinimumSize,
   getDocumentWindowDesiredSize,
   getDocumentWindowTitle,
   getDocumentWindowTitleIcon,
   documentWindowAdjustPanes)
  where

import Control.Concurrent.MVar
import Data.Array.Unboxed

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


getDocumentWindowDefaultSize :: MVar ApplicationState -> IO (Int, Int)
getDocumentWindowDefaultSize applicationStateMVar = do
  emWidth <- getEmWidth applicationStateMVar
  lineHeight <- getLineHeight applicationStateMVar
  leftMarginWidth <- getDefaultLeftMarginWidth applicationStateMVar
  leftPaddingWidth <- getDefaultLeftPaddingWidth applicationStateMVar
  rightPaddingWidth <- getDefaultRightPaddingWidth applicationStateMVar
  rightMarginWidth <- getDefaultRightMarginWidth applicationStateMVar
  bottomMarginWidth <- getDefaultBottomMarginWidth applicationStateMVar
  (visibleWidth, visibleHeight) <- getVisibleSize applicationStateMVar
  let dividerHeight = dividerThicknessForOrientation HorizontalOrientation
      dividerWidth = dividerThicknessForOrientation VerticalOrientation
      width = dividerWidth
              + leftMarginWidth
              + leftPaddingWidth
              + (ceiling $ emWidth * 81.0)
              + rightPaddingWidth
              + rightMarginWidth
      height = (ceiling $ 50.0 * lineHeight)
               + dividerHeight
               + bottomMarginWidth
      linesToRemove =
        if realToFrac height > visibleHeight
          then ceiling $ (realToFrac height - visibleHeight) / lineHeight
          else 0
      heightAfterRemoving =
        height - (floor $ realToFrac linesToRemove * lineHeight)
      columnsToRemove =
        if realToFrac width > visibleWidth
          then ceiling $ (realToFrac width - visibleWidth) / emWidth
          else 0
      widthAfterRemoving =
        width - (floor $ realToFrac columnsToRemove * emWidth)
  return (widthAfterRemoving, heightAfterRemoving)


getDocumentWindowMinimumSize :: DocumentWindow -> IO (Int, Int)
getDocumentWindowMinimumSize documentWindow = do
  getWindowSizeFromPaneSizes documentWindow getDocumentPaneMinimumSize


getDocumentWindowDesiredSize :: DocumentWindow -> IO (Int, Int)
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
