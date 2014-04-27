module Te.HighLevel.Window.Document
  (getDocumentWindowTitle,
   getDocumentWindowTitleIcon)
  where

import Control.Concurrent.MVar
import Data.Text (Text)

import Te.Types


instance Window DocumentWindow


getDocumentWindowTitle :: DocumentWindow -> IO Text
getDocumentWindowTitleIcon :: DocumentWindow -> IO Text
