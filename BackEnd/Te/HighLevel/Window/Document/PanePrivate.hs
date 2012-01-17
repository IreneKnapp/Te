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

import Data.Geometry
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
  :: DocumentPane -> IO Rectangle
getDocumentPaneCurrentFrame documentPane = do
  origin <- getDocumentPaneCurrentOrigin documentPane
  size <- getDocumentPaneCurrentSize documentPane
  return (origin, size)


getDocumentPaneCurrentOrigin
  :: DocumentPane -> IO Point
getDocumentPaneCurrentOrigin documentPane = do
  left <- getDocumentPaneCurrentLeft documentPane
  top <- getDocumentPaneCurrentTop documentPane
  return (left, top)


getDocumentPaneCurrentLeft
  :: DocumentPane -> IO Coordinate
getDocumentPaneCurrentLeft documentPane = do
  let window = documentPaneWindow documentPane
  columnLeft <- readMVar $ documentPaneColumnLeft documentPane
  getDocumentWindowWidthToLeft window columnLeft


getDocumentPaneCurrentTop
  :: DocumentPane -> IO Coordinate
getDocumentPaneCurrentTop documentPane = do
  let window = documentPaneWindow documentPane
  rowTop <- readMVar $ documentPaneRowTop documentPane
  getDocumentWindowHeightAbove window rowTop


getDocumentPaneCurrentSize :: DocumentPane -> IO Size
getDocumentPaneCurrentSize documentPane = do
  width <- getDocumentPaneCurrentWidth documentPane
  height <- getDocumentPaneCurrentHeight documentPane
  return (width, height)


getDocumentPaneCurrentWidth :: DocumentPane -> IO Coordinate
getDocumentPaneCurrentWidth documentPane = do
  let window = documentPaneWindow documentPane
  columnLeft <- readMVar $ documentPaneColumnLeft documentPane
  columnSpan <- readMVar $ documentPaneColumnSpan documentPane
  getDocumentWindowWidthInRange window columnLeft columnSpan


getDocumentPaneCurrentHeight :: DocumentPane -> IO Coordinate
getDocumentPaneCurrentHeight documentPane = do
  let window = documentPaneWindow documentPane
  rowTop <- readMVar $ documentPaneRowTop documentPane
  rowSpan <- readMVar $ documentPaneRowSpan documentPane
  getDocumentWindowHeightInRange window rowTop rowSpan
