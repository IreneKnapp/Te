module Te.HighLevel.Window.Document.HorizontalDivider
  (getDocumentHorizontalDividerFrame)
  where

import Data.Int

import Data.Geometry
import Te.HighLevel.Window.Document.Pane
import Te.HighLevel.Window.Document.HorizontalDividerPrivate
import Te.HighLevel.Window.DocumentPrivate
import Te.Types


getDocumentHorizontalDividerFrame
  :: DocumentHorizontalDivider
  -> IO Rectangle
getDocumentHorizontalDividerFrame divider = do
  origin <- getDocumentHorizontalDividerOrigin divider
  size <- getDocumentHorizontalDividerSize divider
  return (origin, size)
