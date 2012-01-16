module Te.HighLevel.Window.Document.PanePrivate
  (minimumLines,
   minimumColumns,
   collapseLines,
   collapseColumns,
   getDocumentPaneCurrentFrame,
   getDocumentPaneCurrentOrigin,
   getDocumentPaneCurrentLeft,
   getDocumentPaneCurrentTop,
   getDocumentPaneCurrentSize,
   getDocumentPaneCurrentWidth,
   getDocumentPaneCurrentHeight)
  where

import Control.Concurrent.MVar
import Data.Array.Unboxed
import Data.Int

import Te.HighLevel.Window.DocumentPrivate
import Te.Types


minimumLines :: Int
minimumLines = 5


minimumColumns :: Int
minimumColumns = 35


collapseLines :: Double
collapseLines = 1.5


collapseColumns :: Double
collapseColumns = 17.5


getDocumentPaneCurrentFrame
  :: DocumentPane -> IO ((Int64, Int64), (Int64, Int64))
getDocumentPaneCurrentFrame documentPane = do
  origin <- getDocumentPaneCurrentOrigin documentPane
  size <- getDocumentPaneCurrentSize documentPane
  return (origin, size)


getDocumentPaneCurrentOrigin
  :: DocumentPane -> IO (Int64, Int64)
getDocumentPaneCurrentOrigin documentPane = do
  left <- getDocumentPaneCurrentLeft documentPane
  top <- getDocumentPaneCurrentTop documentPane
  return (left, top)


getDocumentPaneCurrentLeft
  :: DocumentPane -> IO Int64
getDocumentPaneCurrentLeft documentPane = do
  let window = documentPaneWindow documentPane
      columnLeft = documentPaneColumnLeft documentPane
  getDocumentWindowWidthToLeft window columnLeft


getDocumentPaneCurrentTop
  :: DocumentPane -> IO Int64
getDocumentPaneCurrentTop documentPane = do
  let window = documentPaneWindow documentPane
      rowTop = documentPaneRowTop documentPane
  getDocumentWindowHeightAbove window rowTop


getDocumentPaneCurrentSize :: DocumentPane -> IO (Int64, Int64)
getDocumentPaneCurrentSize documentPane = do
  width <- getDocumentPaneCurrentWidth documentPane
  height <- getDocumentPaneCurrentHeight documentPane
  return (width, height)


getDocumentPaneCurrentWidth :: DocumentPane -> IO Int64
getDocumentPaneCurrentWidth documentPane = do
  let window = documentPaneWindow documentPane
      columnLeft = documentPaneColumnLeft documentPane
      columnSpan = documentPaneColumnSpan documentPane
  getDocumentWindowWidthInRange window columnLeft columnSpan


getDocumentPaneCurrentHeight :: DocumentPane -> IO Int64
getDocumentPaneCurrentHeight documentPane = do
  let window = documentPaneWindow documentPane
      rowTop = documentPaneRowTop documentPane
      rowSpan = documentPaneRowSpan documentPane
  getDocumentWindowHeightInRange window rowTop rowSpan
