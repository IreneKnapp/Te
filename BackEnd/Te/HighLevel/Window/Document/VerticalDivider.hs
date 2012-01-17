module Te.HighLevel.Window.Document.VerticalDivider
  (getDocumentVerticalDividerFrame)
  where

import Data.Int

import Data.Geometry
import Te.HighLevel.Window.Document.Pane
import Te.HighLevel.Window.Document.VerticalDividerPrivate
import Te.HighLevel.Window.DocumentPrivate
import Te.Types


getDocumentVerticalDividerFrame
  :: DocumentVerticalDivider
  -> IO Rectangle
getDocumentVerticalDividerFrame divider = do
  origin <- getDocumentVerticalDividerOrigin divider
  size <- getDocumentVerticalDividerSize divider
  return (origin, size)

