module Te.HighLevel.Window.Browser
  (BrowserWindow,
   BrowserWindowID,
   browserWindowID,
   browserWindowRoot)
  where

import {-# SOURCE #-} Te.HighLevel.Window
import Te.LowLevel.Identifiers
import Te.Types


instance WindowType BrowserWindow


browserWindowRoot :: BrowserWindow -> IO BrowserItem
