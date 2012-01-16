module Te.HighLevel.Window.Document.HorizontalDivider
  (getDocumentHorizontalDividerFrame)
  where

import Data.Int

import Te.HighLevel.Window.Document.Pane
import Te.HighLevel.Window.DocumentPrivate
import Te.Types


getDocumentHorizontalDividerFrame
  :: DocumentHorizontalDivider
  -> IO ((Int64, Int64), (Int64, Int64))
getDocumentHorizontalDividerFrame divider = do
  origin <- getDocumentHorizontalDividerOrigin divider
  size <- getDocumentHorizontalDividerSize divider
  return (origin, size)


getDocumentHorizontalDividerOrigin
  :: DocumentHorizontalDivider
  -> IO (Int64, Int64)
getDocumentHorizontalDividerOrigin divider = do
  left <- getDocumentHorizontalDividerLeft divider
  top <- getDocumentHorizontalDividerTop divider
  return (left, top)


getDocumentHorizontalDividerLeft
  :: DocumentHorizontalDivider
  -> IO Int64
getDocumentHorizontalDividerLeft divider = do
  let window = documentHorizontalDividerWindow divider
      columnLeft = documentHorizontalDividerColumnLeft divider
  getDocumentWindowWidthToLeft window columnLeft


getDocumentHorizontalDividerTop
  :: DocumentHorizontalDivider
  -> IO Int64
getDocumentHorizontalDividerTop divider = do
  let window = documentHorizontalDividerWindow divider
      rowDown = documentHorizontalDividerRowDown divider
  getDocumentWindowHeightInRange window 0 rowDown


getDocumentHorizontalDividerSize
  :: DocumentHorizontalDivider
  -> IO (Int64, Int64)
getDocumentHorizontalDividerSize divider = do
  width <- getDocumentHorizontalDividerWidth divider
  height <- getDocumentHorizontalDividerHeight divider
  return (width, height)


getDocumentHorizontalDividerWidth
  :: DocumentHorizontalDivider
  -> IO Int64
getDocumentHorizontalDividerWidth divider = do
  let window = documentHorizontalDividerWindow divider
      columnLeft = documentHorizontalDividerColumnLeft divider
      columnSpan = documentHorizontalDividerColumnSpan divider
  getDocumentWindowWidthInRange window columnLeft columnSpan


getDocumentHorizontalDividerHeight
  :: DocumentHorizontalDivider
  -> IO Int64
getDocumentHorizontalDividerHeight _ = do
  return $ dividerThicknessForOrientation HorizontalOrientation
