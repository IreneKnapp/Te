{-# LANGUAGE OverloadedStrings #-}
module Te.HighLevel.Window.Browser
  (BrowserWindow,
   BrowserWindowID,
   browserWindowID,
   getBrowserWindowTitle,
   getBrowserWindowTitleIcon,
   getBrowserWindowRoot)
  where

import Control.Concurrent.MVar
import Data.Text (Text)
import qualified Data.Text as Text

import Te.HighLevel.Window
import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.LowLevel.Identifiers
import Te.Types


instance Window BrowserWindow where
  windowID = toWindowID . browserWindowID
  windowProject = browserWindowProject
  getWindowTitle = getBrowserWindowTitle
  getWindowTitleIcon = getBrowserWindowTitleIcon
  browserWindowDo window _ action = action window
  documentWindowDo _ default' _ = return default'


getBrowserWindowTitle :: BrowserWindow -> IO Text
getBrowserWindowTitle browserWindow = do
  let project = browserWindowProject browserWindow
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    projectName <- readMVar $ projectName project
    folderInode <- lookupBrowserWindowRoot browserWindow
    rootInode <- lookupProjectRoot project
    if inodeID rootInode == inodeID folderInode
      then return projectName
      else do
        folderInodeInformation <- lookupInodeInformation folderInode
        let folderName = inodeInformationName folderInodeInformation
        return $ Text.concat [projectName, " - ", folderName]


getBrowserWindowTitleIcon :: BrowserWindow -> IO Text
getBrowserWindowTitleIcon browserWindow = do
  let project = browserWindowProject browserWindow
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Project" $ do
    inode <- lookupBrowserWindowRoot browserWindow
    rootInode <- lookupProjectRoot project
    if inodeID rootInode == inodeID inode
      then return "Project"
      else return "Folder"


getBrowserWindowRoot :: BrowserWindow -> IO BrowserItem
getBrowserWindowRoot browserWindow = do
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
