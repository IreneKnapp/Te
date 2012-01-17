module Te.HighLevel.Window.Document.VerticalDivider
  (getDocumentVerticalDividerFrame)
  where

import Data.Int

import Te.Types


getDocumentVerticalDividerFrame
  :: DocumentVerticalDivider
  -> IO ((Int64, Int64), (Int64, Int64))
