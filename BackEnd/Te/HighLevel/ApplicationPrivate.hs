{-# LANGUAGE OverloadedStrings #-}
module Te.HighLevel.ApplicationPrivate
  (addProjectToApplicationState,
   removeProjectFromApplicationState,
   updateProjectInRecentProjects,
   restoreProjectWindows,
   projectClose,
   computeNextNumberedName,
   getNextUntitledProjectName,
   computeNameForFilePath,
   newBrowserWindow,
   getDragState,
   putDragState)
  where

import Control.Concurrent.MVar
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath

import Data.Timestamp
import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.LowLevel.FrontEndCallbacks
import Te.LowLevel.Identifiers
import {-# SOURCE #-} Te.HighLevel.Window.Browser ()
import {-# SOURCE #-} Te.HighLevel.Window.Document ()
import Te.Types


addProjectToApplicationState :: MVar ApplicationState -> Project -> IO ()
addProjectToApplicationState applicationStateMVar project = do
  applicationState <- takeMVar applicationStateMVar
  let projects = applicationStateProjects applicationState
      projects' = Map.insert (projectID project) project projects
      applicationState' = applicationState {
                              applicationStateProjects = projects'
                            }
  putMVar applicationStateMVar applicationState'


removeProjectFromApplicationState :: MVar ApplicationState -> Project -> IO ()
removeProjectFromApplicationState applicationStateMVar project = do
  applicationState <- takeMVar applicationStateMVar
  let projects = applicationStateProjects applicationState
      projects' = Map.delete (projectID project) projects
      applicationState' = applicationState {
                              applicationStateProjects = projects'
                            }
  putMVar applicationStateMVar applicationState'


updateProjectInRecentProjects :: MVar ApplicationState -> Project -> IO ()
updateProjectInRecentProjects applicationStateMVar project = do
  maybeFilePath <- readMVar $ projectFilePath project
  case maybeFilePath of
    Nothing -> return ()
    Just filePath -> do
      timestamp <- getTimestamp
      applicationState <- takeMVar applicationStateMVar
      let newRecentProject = RecentProject {
                                 recentProjectID = projectID project,
                                 recentProjectFilePath = filePath,
                                 recentProjectTimestamp = timestamp
                               }
          recentProjects = applicationStateRecentProjects applicationState
          recentProjects' = take 8
                                 $ newRecentProject
                                   : filter (\recentProject ->
                                               recentProjectID recentProject
                                               /= projectID project)
                                            recentProjects
          applicationState' = applicationState {
                                  applicationStateRecentProjects = recentProjects'
                                }
      putMVar applicationStateMVar applicationState'


restoreProjectWindows :: Project -> IO ()
restoreProjectWindows project = do
  windows <- takeMVar $ projectWindows project
  if Map.null windows
    then do
      putMVar (projectWindows project) windows
      newBrowserWindow project Nothing
    else do
      putMVar (projectWindows project) windows
      return ()


projectClose :: Project -> IO ()
projectClose project = do
  closeProjectDatabase $ projectDatabase project
  removeProjectFromApplicationState (projectApplicationState project) project


computeNextNumberedName :: Text -> Text -> [Text] -> Bool -> Text
computeNextNumberedName prefix suffix siblingNames caseSensitive =
  let prefix' :: Text
      prefix' = caseFoldIfAppropriate prefix
      
      suffix' :: Text
      suffix' = caseFoldIfAppropriate suffix
      
      prefixWithSpace' :: Text
      prefixWithSpace' = Text.concat [prefix', " "]
      
      caseFoldIfAppropriate :: Text -> Text
      caseFoldIfAppropriate string =
        if caseSensitive
          then string
          else Text.map toUpper string
      
      stripPrefixAndSuffix :: Text -> Text -> Text -> Maybe Text
      stripPrefixAndSuffix prefix suffix name =
        if Text.isPrefixOf prefix name
          then let nameWithoutPrefix = Text.drop (Text.length prefix) name
               in if Text.isSuffixOf suffix nameWithoutPrefix
                    then Just $ Text.reverse
                              $ Text.drop (Text.length suffix)
                              $ Text.reverse nameWithoutPrefix
                    else Nothing
          else Nothing
      
      nameNumber :: Text -> Maybe Int
      nameNumber name =
        let name' = caseFoldIfAppropriate name
        in if name' == Text.concat [prefix', suffix']
             then Just 1
             else case stripPrefixAndSuffix prefixWithSpace' suffix' name' of
                    Nothing -> Nothing
                    Just numberPart ->
                      if (Text.length numberPart > 0)
                         && (Text.all isDigit numberPart)
                        then Just $ read $ Text.unpack numberPart
                        else Nothing
      
      number :: Int
      number = 1 + (foldl1 max
                           $ catMaybes $ [Just 0]
                                         ++ map nameNumber
                                                siblingNames)
      
      name :: Text
      name = if number == 1
               then Text.concat [prefix, suffix]
               else Text.concat [prefix, " ", Text.pack $ show number, suffix]
  in name


getNextUntitledProjectName :: MVar ApplicationState -> IO Text
getNextUntitledProjectName applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  projectNames <- mapM (\project -> readMVar $ projectName project)
                       $ Map.elems $ applicationStateProjects applicationState
  return $ computeNextNumberedName "Untitled" "" projectNames True


computeNameForFilePath :: Text -> Text
computeNameForFilePath filePath =
  let fileName = Text.pack $ takeFileName $ Text.unpack filePath
      fileNameWithoutExtension = if Text.isSuffixOf ".te" fileName
                                   then Text.pack $ dropExtension
                                        $ Text.unpack fileName
                                   else fileName
  in fileNameWithoutExtension


newBrowserWindow :: Project -> Maybe Inode -> IO ()
newBrowserWindow project maybeRootInode = do
  let applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    newBrowserWindowID <- newBrowserWindowID
    let newBrowserWindow' = BrowserWindow {
                                browserWindowID = newBrowserWindowID,
                                browserWindowProject = project
                              }
        newWindow = AnyWindow newBrowserWindow'
        newWindowID = windowID newWindow
    windows <- takeMVar $ projectWindows project
    let windows' = Map.insert newWindowID newWindow windows
    putMVar (projectWindows project) windows'
    rootInode <- case maybeRootInode of
                   Just rootInode -> return rootInode
                   Nothing -> lookupProjectRoot project
    recordNewBrowserWindow newBrowserWindow' rootInode
    noteNewBrowserWindow newBrowserWindow'


getDragState :: MVar ApplicationState -> IO (Maybe DragState)
getDragState applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  return $ applicationStateDragState applicationState


putDragState :: MVar ApplicationState -> Maybe DragState -> IO ()
putDragState applicationStateMVar maybeDragState = do
  applicationState <- takeMVar applicationStateMVar
  let applicationState' =
        applicationState {
            applicationStateDragState = maybeDragState
          }
  putMVar applicationStateMVar applicationState'
