module Te.HighLevel.Window.Document.HorizontalDividerPrivate
  (getDocumentHorizontalDividerOrigin,
   getDocumentHorizontalDividerLeft,
   getDocumentHorizontalDividerTop,
   getDocumentHorizontalDividerSize,
   getDocumentHorizontalDividerWidth,
   getDocumentHorizontalDividerHeight)
  where

import Control.Concurrent.MVar
import Data.Int

import Data.Geometry
import Te.HighLevel.Window.Document.Pane
import Te.HighLevel.Window.DocumentPrivate
import Te.Types


getDocumentHorizontalDividerOrigin
  :: DocumentHorizontalDivider
  -> IO Point
getDocumentHorizontalDividerOrigin divider = do
  left <- getDocumentHorizontalDividerLeft divider
  top <- getDocumentHorizontalDividerTop divider
  return (left, top)


getDocumentHorizontalDividerLeft
  :: DocumentHorizontalDivider
  -> IO Coordinate
getDocumentHorizontalDividerLeft divider = do
  let window = documentHorizontalDividerWindow divider
  columnLeft <- readMVar $ documentHorizontalDividerColumnLeft divider
  getDocumentWindowWidthToLeft window columnLeft


getDocumentHorizontalDividerTop
  :: DocumentHorizontalDivider
  -> IO Coordinate
getDocumentHorizontalDividerTop divider = do
  let window = documentHorizontalDividerWindow divider
  rowDown <- readMVar $ documentHorizontalDividerRowDown divider
  getDocumentWindowHeightInRange window 0 rowDown


getDocumentHorizontalDividerSize
  :: DocumentHorizontalDivider
  -> IO Size
getDocumentHorizontalDividerSize divider = do
  width <- getDocumentHorizontalDividerWidth divider
  height <- getDocumentHorizontalDividerHeight divider
  return (width, height)


getDocumentHorizontalDividerWidth
  :: DocumentHorizontalDivider
  -> IO Coordinate
getDocumentHorizontalDividerWidth divider = do
  let window = documentHorizontalDividerWindow divider
  columnLeft <- readMVar $ documentHorizontalDividerColumnLeft divider
  columnSpan <- readMVar $ documentHorizontalDividerColumnSpan divider
  getDocumentWindowWidthInRange window columnLeft columnSpan


getDocumentHorizontalDividerHeight
  :: DocumentHorizontalDivider
  -> IO Coordinate
getDocumentHorizontalDividerHeight _ = do
  return $ dividerThicknessForOrientation HorizontalOrientation
