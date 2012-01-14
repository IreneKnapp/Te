module Te.HighLevel.Window.DocumentPrivate
  (newDocumentWindow,
   getDocumentWindowDefaultSize,
   getMinimumDividerThicknessForOrientation)
  where

import Control.Concurrent.MVar
import Data.Array.Unboxed
import Data.Map (Map)
import qualified Data.Map as Map

import Te.HighLevel.Window.Document
import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.LowLevel.FrontEndCallbacks
import Te.LowLevel.Identifiers
import Te.Types


newDocumentWindow :: Project -> Inode -> IO ()
newDocumentWindow project documentInode = do
  let applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    newDocumentWindowID' <- newDocumentWindowID
    newDocumentPaneID' <- newDocumentPaneID
    bottomDividerID <- newDocumentHorizontalDividerID
    leftDividerID <- newDocumentVerticalDividerID
    (paneWidth, paneHeight) <-
      getDocumentWindowDefaultSize applicationStateMVar
    let bottomDivider =
          DocumentHorizontalDivider {
              documentHorizontalDividerID = bottomDividerID,
              documentHorizontalDividerWindow = newDocumentWindow',
              documentHorizontalDividerColumnLeft = 0,
              documentHorizontalDividerColumnSpan = 1,
              documentHorizontalDividerRowDown = 1,
              documentHorizontalDividerPanesUp = [newDocumentPane'],
              documentHorizontalDividerPanesDown = []
            }
        leftDivider =
          DocumentVerticalDivider {
              documentVerticalDividerID = leftDividerID,
              documentVerticalDividerWindow = newDocumentWindow',
              documentVerticalDividerRowTop = 0,
              documentVerticalDividerRowSpan = 1,
              documentVerticalDividerColumnRight = 0,
              documentVerticalDividerPanesLeft = [],
              documentVerticalDividerPanesRight = [newDocumentPane']
            }
        newDocumentPane' =
          DocumentPane {
              documentPaneID = newDocumentPaneID',
              documentPaneWindow = newDocumentWindow',
              documentPaneRowTop = 0,
              documentPaneColumnLeft = 0,
              documentPaneRowSpan = 1,
              documentPaneColumnSpan = 1,
              documentPaneDividerUp = Nothing,
              documentPaneDividerDown = Just bottomDivider,
              documentPaneDividerLeft = Just leftDivider,
              documentPaneDividerRight = Nothing
            }
        paneMap =
          Map.singleton newDocumentPaneID' newDocumentPane'
        horizontalDividersMap =
          Map.singleton bottomDividerID bottomDivider
        verticalDividersMap =
          Map.singleton leftDividerID leftDivider
        grid = array ((0, 0), (0, 0))
                     [((0, 0), newDocumentPane')]
        columnWidths = array (0, 0) [(0, paneWidth)]
        rowHeights = array (0, 0) [(0, paneHeight)]
        newDocumentWindow' =
          DocumentWindow {
              documentWindowID = newDocumentWindowID',
              documentWindowProject = project,
              documentWindowPanes = paneMap,
              documentWindowVerticalDividers = verticalDividersMap,
              documentWindowHorizontalDividers = horizontalDividersMap,
              documentWindowGrid = grid,
              documentWindowColumnWidths = columnWidths,
              documentWindowRowHeights = rowHeights
            }
        newWindow = AnyWindow newDocumentWindow'
        newWindowID = windowID newWindow
    windows <- takeMVar $ projectWindows project
    let windows' = Map.insert newWindowID newWindow windows
    putMVar (projectWindows project) windows'
    recordNewDocumentWindow newDocumentWindow' documentInode
    noteNewDocumentWindow newDocumentWindow'


getDocumentWindowDefaultSize :: MVar ApplicationState -> IO (Int, Int)
getDocumentWindowDefaultSize applicationStateMVar = do
  emWidth <- getEmWidth applicationStateMVar
  lineHeight <- getLineHeight applicationStateMVar
  dividerHeight <-
    getMinimumDividerThicknessForOrientation applicationStateMVar
                                             HorizontalOrientation
  dividerWidth <-
    getMinimumDividerThicknessForOrientation applicationStateMVar
                                             VerticalOrientation
  leftMarginWidth <- getLeftMarginWidth applicationStateMVar
  leftPaddingWidth <- getLeftPaddingWidth applicationStateMVar
  rightPaddingWidth <- getRightPaddingWidth applicationStateMVar
  rightMarginWidth <- getRightMarginWidth applicationStateMVar
  bottomMarginWidth <- getBottomMarginWidth applicationStateMVar
  visibleWidth <- getVisibleWidth applicationStateMVar
  visibleHeight <- getVisibleHeight applicationStateMVar
  let width = dividerWidth
              + leftMarginWidth
              + leftPaddingWidth
              + (emWidth * 81.0)
              + rightPaddingWidth
              + rightMarginWidth
      height = 50.8 * lineHeight
               + dividerHeight
               + bottomMarginWidth
      linesToRemove =
        if height > visibleHeight
          then ceiling $ (height - visibleHeight) / lineHeight
          else 0
      heightAfterRemoving = height - realToFrac linesToRemove * lineHeight
      columnsToRemove =
        if width > visibleWidth
          then ceiling $ (width - visibleWidth) / emWidth
          else 0
      widthAfterRemoving = width - realToFrac columnsToRemove * emWidth
  return (ceiling widthAfterRemoving, ceiling heightAfterRemoving)


getMinimumDividerThicknessForOrientation
  :: MVar ApplicationState -> DividerOrientation -> IO Double
getMinimumDividerThicknessForOrientation _ HorizontalOrientation = do
  return 22.0
getMinimumDividerThicknessForOrientation _ VerticalOrientation = do
  return 1.0


getLeftMarginWidth :: MVar ApplicationState -> IO Double
getLeftMarginWidth applicationStateMVar = do
  lineNumberAreaWidth <- getLineNumberAreaWidth applicationStateMVar
  return $ lineNumberAreaWidth + 16.0


getLineNumberPaddingWidth :: MVar ApplicationState -> IO Double
getLineNumberPaddingWidth applicationStateMVar = do
  return 2.0


getLineNumberAreaWidth :: MVar ApplicationState -> IO Double
getLineNumberAreaWidth applicationStateMVar = do
  lineNumberPaddingWidth <- getLineNumberPaddingWidth applicationStateMVar
  lineNumberEmWidth <- getLineNumberEmWidth applicationStateMVar
  return $ lineNumberPaddingWidth
           + (realToFrac $ ceiling $ 4.0 * lineNumberEmWidth)


getLeftPaddingWidth :: MVar ApplicationState -> IO Double
getLeftPaddingWidth applicationStateMVar = do
  return 1.0


getRightPaddingWidth :: MVar ApplicationState -> IO Double
getRightPaddingWidth applicationStateMVar = do
  emWidth <- getEmWidth applicationStateMVar
  return $ emWidth / 2.0


getRightMarginWidth :: MVar ApplicationState -> IO Double
getRightMarginWidth applicationStateMVar = do
  scrollerWidth <- getScrollerWidth applicationStateMVar
  return scrollerWidth


getBottomMarginWidth :: MVar ApplicationState -> IO Double
getBottomMarginWidth applicationStateMVar = do
  scrollerWidth <- getScrollerWidth applicationStateMVar
  return scrollerWidth
