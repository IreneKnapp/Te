module Te.HighLevel.Window.Document.VerticalDividerPrivate
  (getDocumentVerticalDividerOrigin,
   getDocumentVerticalDividerLeft,
   getDocumentVerticalDividerTop,
   getDocumentVerticalDividerSize,
   getDocumentVerticalDividerWidth,
   getDocumentVerticalDividerHeight)
  where

import Control.Concurrent.MVar
import Data.Int

import Data.Geometry
import Te.HighLevel.Window.Document.Pane
import Te.HighLevel.Window.DocumentPrivate
import Te.Types


getDocumentVerticalDividerOrigin
  :: DocumentVerticalDivider
  -> IO Point
getDocumentVerticalDividerOrigin divider = do
  left <- getDocumentVerticalDividerLeft divider
  top <- getDocumentVerticalDividerTop divider
  return (left, top)


getDocumentVerticalDividerLeft
  :: DocumentVerticalDivider
  -> IO Coordinate
getDocumentVerticalDividerLeft divider = do
  let window = documentVerticalDividerWindow divider
  columnRight <- readMVar $ documentVerticalDividerColumnRight divider
  getDocumentWindowWidthInRange window 0 columnRight


getDocumentVerticalDividerTop
  :: DocumentVerticalDivider
  -> IO Coordinate
getDocumentVerticalDividerTop divider = do
  let window = documentVerticalDividerWindow divider
  rowTop <- readMVar $ documentVerticalDividerRowTop divider
  getDocumentWindowHeightAbove window rowTop


getDocumentVerticalDividerSize
  :: DocumentVerticalDivider
  -> IO Size
getDocumentVerticalDividerSize divider = do
  width <- getDocumentVerticalDividerWidth divider
  height <- getDocumentVerticalDividerHeight divider
  return (width, height)


getDocumentVerticalDividerWidth
  :: DocumentVerticalDivider
  -> IO Coordinate
getDocumentVerticalDividerWidth _ = do
  return $ dividerThicknessForOrientation VerticalOrientation


getDocumentVerticalDividerHeight
  :: DocumentVerticalDivider
  -> IO Coordinate
getDocumentVerticalDividerHeight divider = do
  let window = documentVerticalDividerWindow divider
  rowTop <- readMVar $ documentVerticalDividerRowTop divider
  rowSpan <- readMVar $ documentVerticalDividerRowSpan divider
  getDocumentWindowHeightInRange window rowTop rowSpan
