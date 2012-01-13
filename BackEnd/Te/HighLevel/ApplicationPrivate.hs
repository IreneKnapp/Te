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
   newDocumentWindow)
  where

import Control.Concurrent.MVar
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import System.FilePath

import Data.Timestamp
import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.LowLevel.FrontEndCallbacks
import Te.LowLevel.Identifiers
import {-# SOURCE #-} Te.HighLevel.Window
import {-# SOURCE #-} Te.HighLevel.Window.Browser
import {-# SOURCE #-} Te.HighLevel.Window.Document
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


computeNextNumberedName :: String -> String -> [String] -> Bool -> String
computeNextNumberedName prefix suffix siblingNames caseSensitive =
  let prefix' :: String
      prefix' = caseFoldIfAppropriate prefix
      
      suffix' :: String
      suffix' = caseFoldIfAppropriate suffix
      
      prefixWithSpace' :: String
      prefixWithSpace' = prefix' ++ " "
      
      caseFoldIfAppropriate :: String -> String
      caseFoldIfAppropriate string =
        if caseSensitive
          then string
          else map toUpper string
      
      stripPrefixAndSuffix :: String -> String -> String -> Maybe String
      stripPrefixAndSuffix prefix suffix name =
        if isPrefixOf prefix name
          then let nameWithoutPrefix = drop (length prefix) name
               in if isSuffixOf suffix nameWithoutPrefix
                    then Just $ reverse $ drop (length suffix)
                                               $ reverse nameWithoutPrefix
                    else Nothing
          else Nothing
      
      nameNumber :: String -> Maybe Int
      nameNumber name =
        let name' = caseFoldIfAppropriate name
        in if name' == prefix' ++ suffix'
             then Just 1
             else case stripPrefixAndSuffix prefixWithSpace' suffix' name' of
                    Nothing -> Nothing
                    Just numberPart ->
                      if (length numberPart > 0) && (all isDigit numberPart)
                        then Just $ read numberPart
                        else Nothing
      
      number :: Int
      number = 1 + (foldl1 max
                           $ catMaybes $ [Just 0]
                                         ++ map nameNumber
                                                siblingNames)
      
      name :: String
      name = if number == 1
               then prefix ++ suffix
               else prefix ++ " " ++ show number ++ suffix
  in name


getNextUntitledProjectName :: MVar ApplicationState -> IO String
getNextUntitledProjectName applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  projectNames <- mapM (\project -> readMVar $ projectName project)
                       $ Map.elems $ applicationStateProjects applicationState
  return $ computeNextNumberedName "Untitled" "" projectNames True


computeNameForFilePath :: String -> String
computeNameForFilePath filePath =
  let fileName = takeFileName filePath
      fileNameWithoutExtension = if isSuffixOf ".te" fileName
                                   then dropExtension fileName
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
        newWindow = toWindow newBrowserWindow'
        newWindowID = windowID newWindow
    windows <- takeMVar $ projectWindows project
    let windows' = Map.insert newWindowID newWindow windows
    putMVar (projectWindows project) windows'
    rootInode <- case maybeRootInode of
                   Just rootInode -> return rootInode
                   Nothing -> lookupProjectRoot project
    recordNewBrowserWindow newBrowserWindow' rootInode
    noteNewBrowserWindow newBrowserWindow'


newDocumentWindow :: Project -> Inode -> IO ()
newDocumentWindow project documentInode = do
  let applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    newDocumentWindowID <- newDocumentWindowID
    let newDocumentWindow' = DocumentWindow {
                                documentWindowID = newDocumentWindowID,
                                documentWindowProject = project
                              }
        newWindow = toWindow newDocumentWindow'
        newWindowID = windowID newWindow
    windows <- takeMVar $ projectWindows project
    let windows' = Map.insert newWindowID newWindow windows
    putMVar (projectWindows project) windows'
    recordNewDocumentWindow newDocumentWindow' documentInode
    noteNewDocumentWindow newDocumentWindow'
