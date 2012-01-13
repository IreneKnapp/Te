module Te.HighLevel.Window.Browser
  (BrowserWindow,
   BrowserWindowID,
   browserWindowID,
   browserWindowRoot)
  where

import Te.HighLevel.Window
import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.LowLevel.Identifiers
import Te.Types


instance WindowType BrowserWindow where
  toWindow browserWindow =
             Window {
                 windowID = toWindowID $ browserWindowID browserWindow,
                 windowProject = browserWindowProject browserWindow
               }
  getFromWindow window = do
    kind <- lookupWindowKind window
    case kind of
      WindowKindBrowser ->
        return $ Just $ BrowserWindow {
                            browserWindowID = fromWindowID $ windowID window,
                            browserWindowProject = windowProject window
                          }
      _ -> return Nothing


browserWindowRoot :: BrowserWindow -> IO BrowserItem
browserWindowRoot browserWindow = do
  let project = browserWindowProject browserWindow
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar
          (BrowserItem {
               browserItemInode = Inode {
                                      inodeID = nullInodeID,
                                      inodeProject = project
                                    },
               browserItemBrowserWindow = browserWindow
             })
          $ do
    inode <- lookupBrowserWindowRoot browserWindow
    return $ BrowserItem {
                 browserItemInode = inode,
                 browserItemBrowserWindow = browserWindow
               }
