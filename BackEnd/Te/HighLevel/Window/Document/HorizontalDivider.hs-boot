module Te.HighLevel.Window.Document.HorizontalDivider
  (getDocumentHorizontalDividerFrame)
  where

import Data.Int

import Te.Types


getDocumentHorizontalDividerFrame
  :: DocumentHorizontalDivider
  -> IO ((Int64, Int64), (Int64, Int64))
