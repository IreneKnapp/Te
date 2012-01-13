module Te.HighLevel.Window.Browser.Item
  (BrowserItem(..),
   browserItemNewFolderInside,
   browserItemNewFileInside,
   browserItemExpanded,
   browserItemSetExpanded)
  where

import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.LowLevel.FrontEndCallbacks
import Te.LowLevel.Identifiers
import Te.HighLevel.ApplicationPrivate
import Te.HighLevel.InodePrivate
import Te.Types


browserItemNewFolderInside :: BrowserItem -> IO ()
browserItemNewFolderInside parentBrowserItem = do
  let parentInode = browserItemInode parentBrowserItem
      project = inodeProject parentInode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    siblingNames <- getChildInodeNames parentInode
    let newName = computeNextNumberedName "New Folder" "" siblingNames False
    inodeID' <- newInodeID
    let newInode = Inode {
                       inodeID = inodeID',
                       inodeProject = inodeProject parentInode
                     }
    recordNewInode newInode parentInode newName InodeKindDirectory Nothing
    let browserWindow = browserItemBrowserWindow parentBrowserItem
    noteBrowserItemsChanged browserWindow
    let newBrowserItem = BrowserItem {
                             browserItemInode = newInode,
                             browserItemBrowserWindow = browserWindow
                           }
    editBrowserItemName newBrowserItem


browserItemNewFileInside :: BrowserItem -> IO ()
browserItemNewFileInside parentBrowserItem = do
  let parentInode = browserItemInode parentBrowserItem
      project = inodeProject parentInode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    siblingNames <- getChildInodeNames parentInode
    let newName = computeNextNumberedName "New Module" ".hs" siblingNames False
    inodeID' <- newInodeID
    let newInode = Inode {
                       inodeID = inodeID',
                       inodeProject = inodeProject parentInode
                     }
    recordNewInode newInode parentInode newName InodeKindHaskell (Just 0)
    let browserWindow = browserItemBrowserWindow parentBrowserItem
    noteBrowserItemsChanged browserWindow
    let newBrowserItem = BrowserItem {
                             browserItemInode = newInode,
                             browserItemBrowserWindow = browserWindow
                           }
    editBrowserItemName newBrowserItem


browserItemExpanded :: BrowserItem -> IO Bool
browserItemExpanded browserItem = do
  let inode = browserItemInode browserItem
      project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar False $ do
    lookupBrowserItemExpanded browserItem


browserItemSetExpanded :: BrowserItem -> Bool -> IO ()
browserItemSetExpanded browserItem expanded = do
  let inode = browserItemInode browserItem
      project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    recordBrowserItemExpanded browserItem expanded
