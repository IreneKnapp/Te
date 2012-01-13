{-# LANGUAGE TemplateHaskell #-}
module Te.HighLevel.Window
  (Window,
   WindowID,
   windowID,
   WindowType(..),
   toWindowID,
   fromWindowID,
   windowClose,
   windowTitle,
   windowTitleIcon)
  where

import Control.Concurrent.MVar
import Control.Exception
import Data.Map (Map)
import qualified Data.Map as Map

import Te.HighLevel.ApplicationPrivate
import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.LowLevel.FrontEndCallbacks
import Te.LowLevel.Identifiers
import Te.Types


class WindowType window where
  toWindow :: window -> Window
  getFromWindow :: Window -> IO (Maybe window)


windowClose :: Window -> IO ()
windowClose window = do
  let project = windowProject window
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    windows <- takeMVar $ projectWindows project
    let deletedWindowID = windowID window
        windows' = Map.delete deletedWindowID windows
    noteDeletedWindow window
    if Map.null windows'
      then do
        projectClose project
        putMVar (projectWindows project) windows'
      else do
        putMVar (projectWindows project) windows'


windowTitle :: Window -> IO String
windowTitle window = do
  let project = windowProject window
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    kind <- lookupWindowKind window
    case kind of
      WindowKindBrowser -> do
        maybeBrowserWindow <- getFromWindow window
        case maybeBrowserWindow of
          Just browserWindow -> do
            projectName <- readMVar $ projectName project
            folderInode <- lookupBrowserWindowRoot browserWindow
            rootInode <- lookupProjectRoot project
            if inodeID rootInode == inodeID folderInode
              then return projectName
              else do
                folderInodeInformation <- lookupInodeInformation folderInode
                let folderName = inodeInformationName folderInodeInformation
                return $ projectName ++ " - " ++ folderName
          Nothing -> throwIO $(internalFailure)
      WindowKindDocument -> do
        maybeDocumentWindow <- getFromWindow window
        case maybeDocumentWindow of
          Just documentWindow -> do
            documentInode <- lookupDocumentWindowInode documentWindow
            documentInodeInformation <- lookupInodeInformation documentInode
            let documentName = inodeInformationName documentInodeInformation
            return documentName
          Nothing -> throwIO $(internalFailure)


windowTitleIcon :: Window -> IO String
windowTitleIcon window = do
  let project = windowProject window
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Project" $ do
    kind <- lookupWindowKind window
    case kind of
      WindowKindBrowser -> do
        maybeBrowserWindow <- getFromWindow window
        case maybeBrowserWindow of
          Just browserWindow -> do
            inode <- lookupBrowserWindowRoot browserWindow
            rootInode <- lookupProjectRoot project
            if inodeID rootInode == inodeID inode
              then return "Project"
              else return "Folder"
          Nothing -> throwIO $(internalFailure)
      WindowKindDocument -> do
        maybeDocumentWindow <- getFromWindow window
        case maybeDocumentWindow of
          Just documentWindow -> do
            documentInode <- lookupDocumentWindowInode documentWindow
            documentInodeInformation <- lookupInodeInformation documentInode
            let documentKind = inodeInformationKind documentInodeInformation
            case documentKind of
              InodeKindDirectory -> return "Folder"
              InodeKindHaskell -> return "File"
          Nothing -> throwIO $(internalFailure)
