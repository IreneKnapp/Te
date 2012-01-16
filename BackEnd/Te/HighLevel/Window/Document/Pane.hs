module Te.HighLevel.Window.Document.Pane
  (getDocumentPaneLeftMarginWidth,
   getDocumentPaneRightMarginWidth,
   getDocumentPaneBottomMarginWidth,
   getDocumentPaneLeftPaddingWidth,
   getDocumentPaneRightPaddingWidth,
   getDocumentPaneLineNumberPaddingWidth,
   getDocumentPaneLineNumberAreaWidth,
   getDocumentPaneMinimumSize,
   getDocumentPaneDesiredSize,
   getDocumentPaneCaption,
   getDocumentPaneSizeReport)
  where

import Control.Concurrent.MVar
import Data.Array.Unboxed
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map

import Te.HighLevel.Window.DocumentPrivate
import Te.HighLevel.Window.Document.PanePrivate
import Te.LowLevel.FrontEndCallbacks
import Te.Types


getDocumentPaneLeftMarginWidth :: DocumentPane -> IO Int64
getDocumentPaneLeftMarginWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultLeftMarginWidth applicationState


getDocumentPaneRightMarginWidth :: DocumentPane -> IO Int64
getDocumentPaneRightMarginWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultRightMarginWidth applicationState


getDocumentPaneBottomMarginWidth :: DocumentPane -> IO Int64
getDocumentPaneBottomMarginWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultBottomMarginWidth applicationState


getDocumentPaneLeftPaddingWidth :: DocumentPane -> IO Int64
getDocumentPaneLeftPaddingWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultLeftPaddingWidth applicationState


getDocumentPaneRightPaddingWidth :: DocumentPane -> IO Int64
getDocumentPaneRightPaddingWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultRightPaddingWidth applicationState


getDocumentPaneLineNumberPaddingWidth :: DocumentPane -> IO Int64
getDocumentPaneLineNumberPaddingWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultLineNumberPaddingWidth applicationState


getDocumentPaneLineNumberAreaWidth :: DocumentPane -> IO Int64
getDocumentPaneLineNumberAreaWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultLineNumberAreaWidth applicationState


getDocumentPaneMinimumSize :: DocumentPane -> IO (Int64, Int64)
getDocumentPaneMinimumSize documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  emWidth <- getEmWidth applicationState
  lineHeight <- getLineHeight applicationState
  leftMarginWidth <- getDocumentPaneLeftMarginWidth documentPane
  leftPaddingWidth <- getDocumentPaneLeftPaddingWidth documentPane
  rightPaddingWidth <- getDocumentPaneRightPaddingWidth documentPane
  rightMarginWidth <- getDocumentPaneRightMarginWidth documentPane
  bottomMarginWidth <- getDocumentPaneBottomMarginWidth documentPane
  let width = leftMarginWidth
              + leftPaddingWidth
              + (ceiling $ emWidth * realToFrac minimumColumns)
              + rightPaddingWidth
              + rightMarginWidth
      height = (ceiling $ lineHeight * realToFrac minimumLines)
               + bottomMarginWidth
  return (width, height)


getDocumentPaneDesiredSize :: DocumentPane -> IO (Int64, Int64)
getDocumentPaneDesiredSize documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  emWidth <- getEmWidth applicationState
  lineHeight <- getLineHeight applicationState
  (currentWidth, currentHeight) <- getDocumentPaneCurrentSize documentPane
  leftMarginWidth <- getDocumentPaneLeftMarginWidth documentPane
  leftPaddingWidth <- getDocumentPaneLeftPaddingWidth documentPane
  rightPaddingWidth <- getDocumentPaneRightPaddingWidth documentPane
  rightMarginWidth <- getDocumentPaneRightMarginWidth documentPane
  bottomMarginWidth <- getDocumentPaneBottomMarginWidth documentPane
  let contentWidth = currentWidth
                     - (leftMarginWidth
                        + leftPaddingWidth
                        + rightPaddingWidth
                        + rightMarginWidth)
      contentHeight = currentHeight - bottomMarginWidth
      nLines = if currentHeight > 0
                 then floor $ realToFrac contentHeight / lineHeight
                 else 0
      nColumns = if currentWidth > 0
                   then floor $ realToFrac contentWidth / emWidth
                   else 0
      desiredLines = max nLines minimumLines
      desiredColumns = max nColumns minimumColumns
      desiredWidth = leftMarginWidth
                     + leftPaddingWidth
                     + (ceiling $ realToFrac desiredColumns * emWidth)
                     + rightPaddingWidth
                     + rightMarginWidth
      desiredHeight = (ceiling $ realToFrac desiredLines * lineHeight)
                      + bottomMarginWidth
  return (desiredWidth, desiredHeight)


getDocumentPaneCaption :: DocumentPane -> IO String
getDocumentPaneCaption _ = do
  return "(12, 13) in 1980"


getDocumentPaneSizeReport :: DocumentPane -> IO String
getDocumentPaneSizeReport documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  emWidth <- getEmWidth applicationState
  lineHeight <- getLineHeight applicationState
  (currentWidth, currentHeight) <- getDocumentPaneCurrentSize documentPane
  leftMarginWidth <- getDocumentPaneLeftMarginWidth documentPane
  leftPaddingWidth <- getDocumentPaneLeftPaddingWidth documentPane
  rightPaddingWidth <- getDocumentPaneRightPaddingWidth documentPane
  rightMarginWidth <- getDocumentPaneRightMarginWidth documentPane
  bottomMarginWidth <- getDocumentPaneBottomMarginWidth documentPane
  let contentWidth = currentWidth
                     - (leftMarginWidth
                        + leftPaddingWidth
                        + rightPaddingWidth
                        + rightMarginWidth)
      contentHeight = currentHeight - bottomMarginWidth
      nLines = if currentHeight > 0
                 then floor $ realToFrac contentHeight / lineHeight
                 else 0
      nColumns = if currentWidth > 0
                   then floor $ realToFrac contentWidth / emWidth
                   else 0
      sizeReport = (show nColumns) ++ " x " ++ (show nLines)
  return sizeReport


getDocumentPaneCurrentSize :: DocumentPane -> IO (Int64, Int64)
getDocumentPaneCurrentSize documentPane = do
  let documentWindow = documentPaneWindow documentPane
  rowHeights <- readMVar $ documentWindowRowHeights documentWindow
  columnWidths <- readMVar $ documentWindowColumnWidths documentWindow
  let individualDividerHeight =
        dividerThicknessForOrientation HorizontalOrientation
      individualDividerWidth =
        dividerThicknessForOrientation VerticalOrientation
      rowTop = documentPaneRowTop documentPane
      rowSpan = documentPaneRowSpan documentPane
      columnLeft = documentPaneColumnLeft documentPane
      columnSpan = documentPaneColumnSpan documentPane
      contentHeight =
        foldl (\soFar rowIndex -> do
                 if inRange (bounds rowHeights) rowIndex
                   then soFar + (rowHeights ! rowIndex)
                   else soFar)
              0
              [rowTop .. rowTop + (rowSpan - 1)]
      contentWidth =
        foldl (\soFar columnIndex -> do
                 if inRange (bounds columnWidths) columnIndex
                   then soFar + (columnWidths ! columnIndex)
                   else soFar)
              0
              [columnLeft .. columnLeft + (columnSpan - 1)]
      totalHeight =
        contentHeight + (fromIntegral rowSpan - 1) * individualDividerHeight
      totalWidth =
        contentWidth + (fromIntegral columnSpan - 1) * individualDividerWidth
  return (totalWidth, totalHeight)
