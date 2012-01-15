module Te.HighLevel.Window.Document
  (getDocumentWindowDefaultSize,
   getDocumentWindowTitle,
   getDocumentWindowTitleIcon)
  where

import Control.Concurrent.MVar

import Te.Types


instance Window DocumentWindow


getDocumentWindowDefaultSize :: MVar ApplicationState -> IO (Int, Int)
getDocumentWindowTitle :: DocumentWindow -> IO String
getDocumentWindowTitleIcon :: DocumentWindow -> IO String
