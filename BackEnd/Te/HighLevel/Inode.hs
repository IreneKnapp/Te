{-# LANGUAGE TemplateHaskell #-}
module Te.HighLevel.Inode
  (Inode,
   InodeID,
   inodeID,
   inodeParent,
   inodeExpandable,
   inodeChildCount,
   inodeChild,
   inodeName,
   inodeKind,
   inodeSize,
   inodeCreationTimestamp,
   inodeModificationTimestamp,
   inodeIcon,
   inodeRename,
   inodeListDelete,
   inodeOpen,
   inodeValidateDrop,
   inodeAcceptDrop,
   inodesKernel)
  where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Word

import Data.ByteSize
import Data.Timestamp
import Te.HighLevel.ApplicationPrivate
import Te.HighLevel.DragAndDropPrivate
import Te.HighLevel.InodePrivate
import Te.HighLevel.Window
import Te.HighLevel.Window.DocumentPrivate
import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.LowLevel.FrontEndCallbacks
import Te.LowLevel.Identifiers
import Te.Types


inodeParent :: Inode -> IO (Maybe Inode)
inodeParent inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar Nothing $ do
    lookupInodeParent inode


inodeExpandable :: Inode -> IO Bool
inodeExpandable inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar False $ do
    inodeInformation <- lookupInodeInformation inode
    case inodeInformationType inodeInformation of
      DirectoryInodeType -> return True
      _ -> return False


inodeChildCount :: Inode -> IO Word64
inodeChildCount inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar 0 $ do
    lookupInodeChildCount inode


inodeChild :: Inode -> Word64 -> IO Inode
inodeChild inode index = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar
          (Inode {
               inodeID = nullInodeID,
               inodeProject = project
             })
          $ do
    lookupInodeChild inode index


inodeName :: Inode -> IO String
inodeName inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    inodeInformation <- lookupInodeInformation inode
    return $ inodeInformationName inodeInformation


inodeKind :: Inode -> IO String
inodeKind inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    inodeInformation <- lookupInodeInformation inode
    case inodeInformationType inodeInformation of
      DirectoryInodeType -> return "Folder"
      HaskellInodeType -> return "Haskell"


inodeSize :: Inode -> IO (Maybe ByteSize)
inodeSize inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar Nothing $ do
    inodeInformation <- lookupInodeInformation inode
    return $ inodeInformationSize inodeInformation


inodeCreationTimestamp :: Inode -> IO Timestamp
inodeCreationTimestamp inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar minBound $ do
    inodeInformation <- lookupInodeInformation inode
    return $ inodeInformationCreationTimestamp inodeInformation


inodeModificationTimestamp :: Inode -> IO Timestamp
inodeModificationTimestamp inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar minBound $ do
    inodeInformation <- lookupInodeInformation inode
    return $ inodeInformationModificationTimestamp inodeInformation


inodeIcon :: Inode -> IO String
inodeIcon inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "File" $ do
    inodeInformation <- lookupInodeInformation inode
    case inodeInformationType inodeInformation of
      DirectoryInodeType -> return "Folder"
      HaskellInodeType -> return "File"


inodeRename :: Inode -> String -> IO ()
inodeRename inode newName = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    maybeParent <- lookupInodeParent inode
    case maybeParent of
      Just parent -> do
        recordMovedInode inode newName parent
        noteBrowserItemsChangedInAllBrowserWindows project
      Nothing -> throwIO $(internalFailure)


inodeListDelete :: Maybe AnyWindow -> [Inode] -> IO ()
inodeListDelete maybeWindow inodes = do
  inodes <- getInodesKernel inodes
  case inodes of
    [] -> return ()
    (firstInode:_) -> do
      let project = inodeProject firstInode
          applicationStateMVar = projectApplicationState project
      catchTe applicationStateMVar () $ do
        (nFolders, nFiles, totalSize) <- getInodesRecursiveStatistics inodes
        if (nFolders > 0) || (nFiles > 0)
          then do
            let pluralize :: Int -> String -> String
                pluralize n word =
                  if n == 1
                    then (show n) ++ " " ++ word
                    else (show n) ++ " " ++ word ++ "s"
                omitIfZero :: Int -> String -> Maybe String
                omitIfZero n word =
                  if n == 0
                    then Nothing
                    else Just $ pluralize n word
                message = "Do you want to delete "
                          ++ (if length inodes == 1
                                then "this item"
                                else "these items")
                          ++ "?"
                details = intercalate " and "
                                      (catMaybes $ map (uncurry omitIfZero)
                                                       [(nFolders, "folder"),
                                                        (nFiles, "file")])
                          ++ ", totalling "
                          ++ (show totalSize)
                          ++ ", will be deleted."
                          ++ "  "
                          ++ "This action is irreversible."
            confirm applicationStateMVar
                    (ConfirmationDialog {
                         confirmationDialogWindow = maybeWindow,
                         confirmationDialogMessage = message,
                         confirmationDialogDetails = details,
                         confirmationDialogDefaultButtonIndex = Just 0,
                         confirmationDialogCancelButtonIndex = Just 1,
                         confirmationDialogButtons = ["Yes", "No"]
                       })
                    (\result -> do
                       if result == 0
                         then do
                           mapM_ recordDeletedInode inodes
                           noteBrowserItemsChangedInAllBrowserWindows project
                         else return ())
          else return ()


inodeOpen :: Inode -> IO ()
inodeOpen inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    inodeInformation <- lookupInodeInformation inode
    case inodeInformationType inodeInformation of
      DirectoryInodeType -> do
        newBrowserWindow project $ Just inode
      HaskellInodeType -> do
        windowMap <- readMVar $ projectWindows project
        maybeFoundWindow <-
          foldM (\maybeFoundWindow window -> do
                   case maybeFoundWindow of
                     Just _ -> return maybeFoundWindow
                     Nothing -> do
                       documentWindowDo window Nothing $ \documentWindow -> do
                         documentWindowInode <-
                           lookupDocumentWindowInode documentWindow
                         if inodeID documentWindowInode == inodeID inode
                           then return $ Just window
                           else return Nothing)
                Nothing
                $ Map.elems windowMap
        case maybeFoundWindow of
          Just foundWindow -> activateWindow foundWindow
          Nothing -> newDocumentWindow project inode


inodeValidateDrop
    :: Inode
    -> DragInformation
    -> IO (Maybe (Inode, Maybe Word64, DragOperation))
inodeValidateDrop prospectiveTargetInode dragInformation = do
  let project = inodeProject prospectiveTargetInode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar Nothing $ do
    getDropTargetAndOperation prospectiveTargetInode dragInformation


inodeAcceptDrop :: Inode -> DragInformation -> IO Bool
inodeAcceptDrop prospectiveTargetInode dragInformation = do
  let project = inodeProject prospectiveTargetInode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar False $ do
    maybeDropTargetAndOperation <-
      getDropTargetAndOperation prospectiveTargetInode dragInformation
    case maybeDropTargetAndOperation of
      Just (targetInode, _, dragOperation) -> do
        case dragInformation of
          InodeDragInformation { } -> do
            mapM (\draggedInode -> do
                    draggedInodeInformation <-
                      lookupInodeInformation draggedInode
                    let name = inodeInformationName draggedInodeInformation
                    recordMovedInode draggedInode name targetInode)
                 $ inodeDragInformationInodes dragInformation
            noteBrowserItemsChangedInAllBrowserWindows project
            return True
          _ -> return False
      _ -> throwIO $(internalFailure)


inodesKernel :: [Inode] -> IO [Inode]
inodesKernel inodes = do
  case inodes of
    [] -> return []
    (firstInode:_) -> do
      let project = inodeProject firstInode
          applicationStateMVar = projectApplicationState project
      catchTe applicationStateMVar [] $ do
        getInodesKernel inodes
