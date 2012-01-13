module Te.HighLevel.InodePrivate
  (getChildInodeNames,
   getInodesRecursiveStatistics,
   getInodesKernel,
   noteBrowserItemsChangedInAllBrowserWindows)
  where

import Control.Concurrent.MVar
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

import Data.ByteSize
import Te.LowLevel.Database
import Te.LowLevel.FrontEndCallbacks
import Te.HighLevel.Window
import {-# SOURCE #-} Te.HighLevel.Inode
import Te.Types


getChildInodeNames :: Inode -> IO [String]
getChildInodeNames inode = do
  childCount <- inodeChildCount inode
  let getChildInodes resultsSoFar index = do
        if index == childCount
          then return resultsSoFar
          else do
            childInode <- lookupInodeChild inode index
            getChildInodes (resultsSoFar ++ [childInode]) (index + 1)
  childInodes <- getChildInodes [] 0
  mapM (\inode -> do
          inodeInformation <- lookupInodeInformation inode
          return $ inodeInformationName inodeInformation)
       childInodes


getInodesRecursiveStatistics :: [Inode] -> IO (Int, Int, ByteSize)
getInodesRecursiveStatistics inodes = do
  inodesKernel <- getInodesKernel inodes
  let visitInode inode = do
        inodeInformation <- lookupInodeInformation inode
        let isFolder = inodeInformationKind inodeInformation
                       == InodeKindDirectory
            size = case inodeInformationSize inodeInformation of
                     Just size -> size
                     Nothing -> 0
        inodeChildren <- lookupInodeChildren inode
        visitInodeList (if isFolder
                          then 1
                          else 0,
                        if not isFolder
                          then 1
                          else 0,
                        size)
                       inodeChildren
      visitInodeList information inodes = do
        foldM (\(nFoldersSoFar, nFilesSoFar, totalSizeSoFar) inode -> do
                 (nFoldersHere, nFilesHere, totalSizeHere) <- visitInode inode
                 return (nFoldersSoFar + nFoldersHere,
                         nFilesSoFar + nFilesHere,
                         totalSizeSoFar + totalSizeHere))
              information
              inodes
  visitInodeList (0, 0, 0) inodesKernel


getInodesKernel :: [Inode] -> IO [Inode]
getInodesKernel inodes = do
  let getIsAncestorOf :: Inode -> Inode -> IO Bool
      getIsAncestorOf potentialAncestorInode scrutineeInode = do
        if inodeID potentialAncestorInode == inodeID scrutineeInode
          then return True
          else do
            maybeScrutineeParentInode <- lookupInodeParent scrutineeInode
            case maybeScrutineeParentInode of
              Nothing -> return False
              Just scrutineeParentInode ->
                getIsAncestorOf potentialAncestorInode scrutineeParentInode
      
      visit :: [Inode] -> IO [Inode]
      visit [] = return []
      visit (firstInode:rest) = do
        (firstInode, rest)
          <- foldM (\(firstInode, okayInodesSoFar) foundInode -> do
                      isAncestor <- getIsAncestorOf firstInode foundInode
                      if isAncestor
                        then return (firstInode, okayInodesSoFar)
                        else do
                          isDescendant <- getIsAncestorOf foundInode firstInode
                          if isDescendant
                            then return (foundInode, okayInodesSoFar)
                            else return (firstInode,
                                         foundInode : okayInodesSoFar))
                   (firstInode, [])
                   rest
        rest <- visit rest
        return $ firstInode : rest
  
  visit inodes


noteBrowserItemsChangedInAllBrowserWindows :: Project -> IO ()
noteBrowserItemsChangedInAllBrowserWindows project = do
  windows <- readMVar $ projectWindows project
  mapM_ (\window -> do
           maybeBrowserWindow <- getFromWindow window
           case maybeBrowserWindow of
             Just browserWindow -> noteBrowserItemsChanged browserWindow
             Nothing -> return ())
        $ Map.elems windows
