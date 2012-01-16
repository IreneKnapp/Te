module Te.HighLevel.Window.Document
  (getDocumentWindowTitle,
   getDocumentWindowTitleIcon)
  where

import Control.Concurrent.MVar

import Te.Types


instance Window DocumentWindow


getDocumentWindowTitle :: DocumentWindow -> IO String
getDocumentWindowTitleIcon :: DocumentWindow -> IO String
