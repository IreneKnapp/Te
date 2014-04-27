{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Geometry
import Te.HighLevel.Window.DocumentPrivate
import Te.HighLevel.Window.Document.PanePrivate
import Te.LowLevel.FrontEndCallbacks
import Te.Types


getDocumentPaneLeftMarginWidth :: DocumentPane -> IO Coordinate
getDocumentPaneLeftMarginWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultLeftMarginWidth applicationState


getDocumentPaneRightMarginWidth :: DocumentPane -> IO Coordinate
getDocumentPaneRightMarginWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultRightMarginWidth applicationState


getDocumentPaneBottomMarginWidth :: DocumentPane -> IO Coordinate
getDocumentPaneBottomMarginWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultBottomMarginWidth applicationState


getDocumentPaneLeftPaddingWidth :: DocumentPane -> IO Coordinate
getDocumentPaneLeftPaddingWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultLeftPaddingWidth applicationState


getDocumentPaneRightPaddingWidth :: DocumentPane -> IO Coordinate
getDocumentPaneRightPaddingWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultRightPaddingWidth applicationState


getDocumentPaneLineNumberPaddingWidth :: DocumentPane -> IO Coordinate
getDocumentPaneLineNumberPaddingWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultLineNumberPaddingWidth applicationState


getDocumentPaneLineNumberAreaWidth :: DocumentPane -> IO Coordinate
getDocumentPaneLineNumberAreaWidth documentPane = do
  let documentWindow = documentPaneWindow documentPane
      project = documentWindowProject documentWindow
      applicationState = projectApplicationState project
  getDefaultLineNumberAreaWidth applicationState


getDocumentPaneMinimumSize :: DocumentPane -> IO Size
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


getDocumentPaneDesiredSize :: DocumentPane -> IO Size
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


getDocumentPaneCaption :: DocumentPane -> IO Text
getDocumentPaneCaption _ = do
  return "(12, 13) in 1980"


getDocumentPaneSizeReport :: DocumentPane -> IO Text
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
      sizeReport = Text.concat
        [Text.pack $ show nColumns, " x ", Text.pack $ show nLines]
  return sizeReport
