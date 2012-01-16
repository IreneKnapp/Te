module Te.HighLevel.Window.DocumentPrivate
  (newDocumentWindow,
   getDocumentWindowDefaultFrame,
   getWindowSizeFromPaneSizes,
   getDefaultLeftMarginWidth,
   getDefaultRightMarginWidth,
   getDefaultBottomMarginWidth,
   getDefaultLeftPaddingWidth,
   getDefaultRightPaddingWidth,
   getDefaultLineNumberPaddingWidth,
   getDefaultLineNumberAreaWidth,
   dividerThicknessForOrientation,
   getDocumentWindowWidthToLeft,
   getDocumentWindowHeightAbove,
   getDocumentWindowWidthInRange,
   getDocumentWindowHeightInRange)
  where

import Control.Concurrent.MVar
import Control.Monad
import Data.Array.Unboxed
import Data.Function
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map

import {-# SOURCE #-} Te.HighLevel.Window.Document
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
    contentRectangle@(_, (windowWidth, windowHeight)) <-
      getDocumentWindowDefaultFrame applicationStateMVar
    paneMapMVar <- newEmptyMVar
    verticalDividersMapMVar <- newEmptyMVar
    horizontalDividersMapMVar <- newEmptyMVar
    gridMVar <- newEmptyMVar
    columnWidthsMVar <- newEmptyMVar
    rowHeightsMVar <- newEmptyMVar
    let newDocumentWindow' =
          DocumentWindow {
              documentWindowID = newDocumentWindowID',
              documentWindowProject = project,
              documentWindowPanes = paneMapMVar,
              documentWindowVerticalDividers = verticalDividersMapMVar,
              documentWindowHorizontalDividers = horizontalDividersMapMVar,
              documentWindowGrid = gridMVar,
              documentWindowColumnWidths = columnWidthsMVar,
              documentWindowRowHeights = rowHeightsMVar
            }
        bottomDivider =
          DocumentHorizontalDivider {
              documentHorizontalDividerID = bottomDividerID,
              documentHorizontalDividerWindow = newDocumentWindow',
              documentHorizontalDividerColumnLeft = 0,
              documentHorizontalDividerColumnSpan = 1,
              documentHorizontalDividerRowDown = 1,
              documentHorizontalDividerPanesUp = [newDocumentPane'],
              documentHorizontalDividerPanesDown = []
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
              documentPaneDividerLeft = Nothing,
              documentPaneDividerRight = Nothing
            }
        paneMap = Map.singleton newDocumentPaneID' newDocumentPane'
        horizontalDividersMap = Map.singleton bottomDividerID bottomDivider
        verticalDividersMap = Map.empty
        grid = array ((0, 0), (0, 0))
                     [((0, 0), newDocumentPane')]
        paneWidth = windowWidth
        paneHeight = windowHeight
                     - dividerThicknessForOrientation HorizontalOrientation
        columnWidths = array (0, 0) [(0, paneWidth)]
        rowHeights = array (0, 0) [(0, paneHeight)]
        newWindow = AnyWindow newDocumentWindow'
        newWindowID = windowID newWindow
    putMVar paneMapMVar paneMap
    putMVar horizontalDividersMapMVar horizontalDividersMap
    putMVar verticalDividersMapMVar verticalDividersMap
    putMVar gridMVar grid
    putMVar columnWidthsMVar columnWidths
    putMVar rowHeightsMVar rowHeights
    windows <- takeMVar $ projectWindows project
    let windows' = Map.insert newWindowID newWindow windows
    putMVar (projectWindows project) windows'
    recordNewDocumentWindow newDocumentWindow' documentInode
    noteNewDocumentWindow newDocumentWindow'
                          contentRectangle
    noteNewDocumentPane newDocumentWindow'
                        newDocumentPane'
                        ((0, 0), (paneWidth, paneHeight))


getDocumentWindowDefaultFrame
  :: MVar ApplicationState -> IO ((Int64, Int64), (Int64, Int64))
getDocumentWindowDefaultFrame applicationStateMVar = do
  emWidth <- getEmWidth applicationStateMVar
  lineHeight <- getLineHeight applicationStateMVar
  leftMarginWidth <- getDefaultLeftMarginWidth applicationStateMVar
  leftPaddingWidth <- getDefaultLeftPaddingWidth applicationStateMVar
  rightPaddingWidth <- getDefaultRightPaddingWidth applicationStateMVar
  rightMarginWidth <- getDefaultRightMarginWidth applicationStateMVar
  bottomMarginWidth <- getDefaultBottomMarginWidth applicationStateMVar
  ((visibleLeft, visibleTop), (visibleWidth, visibleHeight)) <-
    getVisibleFrame applicationStateMVar
  let dividerHeight = dividerThicknessForOrientation HorizontalOrientation
      idealWidth = leftMarginWidth
                   + leftPaddingWidth
                   + (ceiling $ emWidth * 81.0)
                   + rightPaddingWidth
                   + rightMarginWidth
      idealHeight = (ceiling $ 50.0 * lineHeight)
                    + dividerHeight
                    + bottomMarginWidth
      provisionalWidth = min visibleWidth idealWidth
      provisionalHeight = min visibleHeight idealHeight
      provisionalLeft = visibleLeft + div (visibleWidth - provisionalWidth) 2
      provisionalTop = visibleTop
  getDocumentContentFromFrame
   applicationStateMVar
   ((provisionalLeft, provisionalTop), (provisionalWidth, provisionalHeight))


getWindowSizeFromPaneSizes
  :: DocumentWindow -> (DocumentPane -> IO (Int64, Int64)) -> IO (Int64, Int64)
getWindowSizeFromPaneSizes documentWindow getDocumentPaneRelevantSize = do
  grid <- readMVar $ documentWindowGrid documentWindow
  let getWidth = do
        let (startIndex, endIndex) = computeBoundsForAxis snd
        foldM (\soFar rowIndex -> do
                 here <- getWidthForRow rowIndex
                 return $ max soFar here)
              0
              [startIndex .. endIndex]
      getHeight = do
        let (startIndex, endIndex) = computeBoundsForAxis snd
        foldM (\soFar columnIndex -> do
                 here <- getHeightForColumn columnIndex
                 return $ max soFar here)
              0
              [startIndex .. endIndex]
      getWidthForRow rowIndex = do
        getDimensionForAxis VerticalOrientation
                            (computeBoundsForAxis fst)
                            (\columnIndex -> getPane rowIndex columnIndex)
                            fst
                            False
      getHeightForColumn columnIndex = do
        getDimensionForAxis HorizontalOrientation
                            (computeBoundsForAxis snd)
                            (\rowIndex -> getPane rowIndex columnIndex)
                            snd
                            True
      computeBoundsForAxis computeBoundForAxis =
        let (start, end) = bounds grid
        in (computeBoundForAxis start, computeBoundForAxis end)
      getDimensionForAxis dividerOrientation
                          (startIndex, endIndex)
                          getPaneForIndex
                          computeDimension
                          finalItemHasDivider = do
        (dimension, _) <-
          foldM (\(soFar, maybePreviousPaneID) index -> do
                    maybePane <- getPaneForIndex index
                    if on (==) (fmap documentPaneID)
                          maybePane maybePreviousPaneID
                      then do
                        here <-
                          case maybePane of
                            Just pane -> do
                              dimensions <- getDocumentPaneRelevantSize pane
                              let dividerThickness =
                                   case maybePreviousPaneID of
                                     Nothing -> 0
                                     Just _ ->
                                       if (index /= endIndex)
                                          || finalItemHasDivider
                                         then dividerThicknessForOrientation
                                               dividerOrientation
                                         else 0
                              return $ computeDimension dimensions
                                       + dividerThickness
                            Nothing -> return 0
                        return (soFar + here, maybePane)
                      else return (soFar, maybePane))
                (0, Nothing)
                [startIndex .. endIndex]
        return dimension
      getPane rowIndex columnIndex = do
        let index = (columnIndex, rowIndex)
        if inRange (bounds grid) index
          then return $ Just $ grid ! index
          else return Nothing
  width <- getWidth
  height <- getHeight
  return (width, height)


getDefaultLeftMarginWidth :: MVar ApplicationState -> IO Int64
getDefaultLeftMarginWidth applicationStateMVar = do
  lineNumberAreaWidth <- getDefaultLineNumberAreaWidth applicationStateMVar
  return $ lineNumberAreaWidth + 16


getDefaultRightMarginWidth :: MVar ApplicationState -> IO Int64
getDefaultRightMarginWidth applicationStateMVar = do
  scrollerWidth <- getScrollerWidth applicationStateMVar
  return $ ceiling scrollerWidth


getDefaultBottomMarginWidth :: MVar ApplicationState -> IO Int64
getDefaultBottomMarginWidth applicationStateMVar = do
  scrollerWidth <- getScrollerWidth applicationStateMVar
  return $ ceiling scrollerWidth


getDefaultLeftPaddingWidth :: MVar ApplicationState -> IO Int64
getDefaultLeftPaddingWidth applicationStateMVar = do
  return 1


getDefaultRightPaddingWidth :: MVar ApplicationState -> IO Int64
getDefaultRightPaddingWidth applicationStateMVar = do
  emWidth <- getEmWidth applicationStateMVar
  return $ floor $ emWidth / 2.0


getDefaultLineNumberPaddingWidth :: MVar ApplicationState -> IO Int64
getDefaultLineNumberPaddingWidth applicationStateMVar = do
  return 2


getDefaultLineNumberAreaWidth :: MVar ApplicationState -> IO Int64
getDefaultLineNumberAreaWidth applicationStateMVar = do
  lineNumberPaddingWidth <-
    getDefaultLineNumberPaddingWidth applicationStateMVar
  lineNumberEmWidth <- getLineNumberEmWidth applicationStateMVar
  return $ lineNumberPaddingWidth
           + (ceiling $ 4.0 * lineNumberEmWidth)


dividerThicknessForOrientation :: DividerOrientation -> Int64
dividerThicknessForOrientation HorizontalOrientation = 22
dividerThicknessForOrientation VerticalOrientation = 1


getDocumentWindowWidthToLeft :: DocumentWindow -> Int -> IO Int64
getDocumentWindowWidthToLeft window columnIndex = do
  columnWidths <- readMVar $ documentWindowColumnWidths window
  let contentWidth =
        foldl (\soFar indexHere ->
                 let widthHere = columnWidths ! indexHere
                 in soFar + widthHere)
              0
              [0 .. columnIndex - 1]
      individualDividerWidth =
        dividerThicknessForOrientation VerticalOrientation
      dividerWidth =
        case columnIndex of
          0 -> 0
          _ -> individualDividerWidth * (fromIntegral $ columnIndex - 1)
      totalWidth = contentWidth + dividerWidth
  return totalWidth


getDocumentWindowHeightAbove :: DocumentWindow -> Int -> IO Int64
getDocumentWindowHeightAbove window rowIndex = do
  rowHeights <- readMVar $ documentWindowRowHeights window
  let contentHeight =
        foldl (\soFar indexHere ->
                 let heightHere = rowHeights ! indexHere
                 in soFar + heightHere)
              0
              [0 .. rowIndex - 1]
      individualDividerHeight =
        dividerThicknessForOrientation HorizontalOrientation
      dividerHeight =
        case rowIndex of
          0 -> 0
          _ -> individualDividerHeight * (fromIntegral $ rowIndex - 1)
      totalHeight = contentHeight + dividerHeight
  return totalHeight


getDocumentWindowWidthInRange :: DocumentWindow -> Int -> Int -> IO Int64
getDocumentWindowWidthInRange window columnLeft columnSpan = do
  columnWidths <- readMVar $ documentWindowColumnWidths window
  let contentWidth =
        foldl (\soFar columnIndex -> do
                 if inRange (bounds columnWidths) columnIndex
                   then soFar + (columnWidths ! columnIndex)
                   else soFar)
              0
              [columnLeft .. columnLeft + (columnSpan - 1)]
      individualDividerWidth =
        dividerThicknessForOrientation VerticalOrientation
      dividerWidth =
        case columnSpan of
          0 -> 0
          _ -> individualDividerWidth * (fromIntegral $ columnSpan - 1)
      totalWidth = contentWidth + dividerWidth
  return totalWidth


getDocumentWindowHeightInRange :: DocumentWindow -> Int -> Int -> IO Int64
getDocumentWindowHeightInRange window rowTop rowSpan = do
  rowHeights <- readMVar $ documentWindowRowHeights window
  let contentHeight =
        foldl (\soFar rowIndex -> do
                 if inRange (bounds rowHeights) rowIndex
                   then soFar + (rowHeights ! rowIndex)
                   else soFar)
              0
              [rowTop .. rowTop + (rowSpan - 1)]
      individualDividerHeight =
        dividerThicknessForOrientation HorizontalOrientation
      dividerHeight =
        case rowSpan of
          0 -> 0
          _ -> individualDividerHeight * (fromIntegral $ rowSpan - 1)
      totalHeight = contentHeight + dividerHeight
  return totalHeight
