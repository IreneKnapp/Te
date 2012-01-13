module Te.HighLevel.Window
  (Window,
   windowID,
   WindowType(..),
   windowClose,
   windowTitle,
   windowTitleIcon)
  where

import Te.Types


class WindowType window where
  toWindow :: window -> Window
  getFromWindow :: Window -> IO (Maybe window)


windowClose :: Window -> IO ()
windowTitle :: Window -> IO String
windowTitleIcon :: Window -> IO String
