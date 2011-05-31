module Te
  (ApplicationState(..),
   Project(..),
   BrowserWindow(..),
   catchTe,
   versionString,
   timestampToString,
   uuidHash,
   uuidEqual,
   uuidShow,
   applicationInit,
   applicationExit,
   applicationRecentProjectCount,
   applicationRecentProjectName,
   applicationRecentProjectTimestamp,
   applicationNewProject,
   applicationOpenProject,
   applicationOpenRecentProject,
   projectClose)
  where

import Control.Concurrent.MVar
import Control.Exception
import Data.Digest.Murmur64
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.UUID
import Data.Version
import Data.Word
import System.Directory
import System.FilePath
import Prelude hiding (catch)

import Data.Timestamp
import Te.Database
import Te.Exceptions
import Te.Identifiers
import Te.Types
import Paths_te


catchTe :: MVar ApplicationState -> a -> IO a -> IO a
catchTe applicationStateMVar default' action = do
  catch (action)
        (\e -> do
           applicationState <- readMVar applicationStateMVar
           let messageString = show (e :: TeException)
               detailsString = exceptionDetails e
               callbacks = applicationStateFrontEndCallbacks applicationState
               callback = frontEndCallbacksException callbacks
           callback messageString detailsString
           return default')


versionString :: String
versionString = showVersion version


timestampToString :: Word64 -> IO String
timestampToString timestamp = do
  describeTimestamp timestamp


uuidHash :: UUID -> IO Word64
uuidHash uuid = do
  return $ asWord64 $ hash64 uuid


uuidEqual :: UUID -> UUID -> IO Bool
uuidEqual uuidA uuidB = do
  return $ uuidA == uuidB


uuidShow :: UUID -> IO String
uuidShow uuid = do
  return $ show uuid


applicationInit
    :: (String -> String -> IO ())
    -> (IO ())
    -> (Project -> IO ())
    -> IO (MVar ApplicationState)
applicationInit
    exception'
    noteRecentProjectsChanged'
    noteNewProject' = do
  let callbacks = FrontEndCallbacks {
                      frontEndCallbacksException =
                        exception',
                      frontEndCallbacksNoteRecentProjectsChanged =
                        noteRecentProjectsChanged',
                      frontEndCallbacksNoteNewProject =
                        noteNewProject'
                    }
      applicationState = ApplicationState {
                             applicationStateRecentProjects = [],
                             applicationStateProjects = Map.empty,
                             applicationStateFrontEndCallbacks = callbacks
                           }
  newMVar applicationState


applicationExit :: MVar ApplicationState -> IO ()
applicationExit applicationStateMVar = do
  return ()


noteRecentProjectsChanged :: MVar ApplicationState -> IO ()
noteRecentProjectsChanged applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteRecentProjectsChanged callbacks
  callback


noteNewProject :: Project -> IO ()
noteNewProject project = do
  applicationState <- readMVar $ projectApplicationState project
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteNewProject callbacks
  callback project


applicationRecentProjectCount :: MVar ApplicationState -> IO Word64
applicationRecentProjectCount applicationStateMVar =
  catchTe applicationStateMVar 0 $ do
    applicationState <- readMVar applicationStateMVar
    let result = length $ applicationStateRecentProjects applicationState
    return $ fromIntegral result


applicationRecentProjectName
    :: MVar ApplicationState -> Word64 -> IO (Maybe String)
applicationRecentProjectName applicationStateMVar index =
  catchTe applicationStateMVar Nothing $ do
    applicationState <- readMVar applicationStateMVar
    let recentProjects = applicationStateRecentProjects applicationState
        index' = fromIntegral index
    if index' < length recentProjects
      then do
        let filePath = recentProjectFilePath $ recentProjects !! index'
            nameWithExtension = takeFileName filePath
            name = if isSuffixOf ".te" nameWithExtension
                     then dropExtension nameWithExtension
                     else nameWithExtension
        return $ Just name
      else return Nothing


applicationRecentProjectTimestamp
    :: MVar ApplicationState -> Word64 -> IO (Maybe Word64)
applicationRecentProjectTimestamp applicationStateMVar index =
  catchTe applicationStateMVar Nothing $ do
    applicationState <- readMVar applicationStateMVar
    let recentProjects = applicationStateRecentProjects applicationState
        index' = fromIntegral index
    if index' < length recentProjects
      then return $ Just $ recentProjectTimestamp $ recentProjects !! index'
      else return Nothing


applicationNewProject :: MVar ApplicationState -> IO ()
applicationNewProject applicationStateMVar =
  catchTe applicationStateMVar () $ do
    projectID <- newProjectID
    database <- newProjectDatabase
    initProjectDatabaseSchema database
    filePathMVar <- newMVar Nothing
    browserWindowsMVar <- newMVar Map.empty
    let newProject = Project {
                         projectID = projectID,
                         projectApplicationState = applicationStateMVar,
                         projectDatabase = database,
                         projectFilePath = filePathMVar,
                         projectBrowserWindows = browserWindowsMVar
                       }
    addProjectToApplicationState applicationStateMVar newProject
    noteNewProject newProject
    restoreProjectWindows newProject


applicationOpenProject
    :: MVar ApplicationState -> FilePath -> IO ()
applicationOpenProject applicationStateMVar filePath =
  catchTe applicationStateMVar () $ do
    applicationOpenProject' applicationStateMVar filePath


applicationOpenProject'
    :: MVar ApplicationState -> FilePath -> IO ()
applicationOpenProject' applicationStateMVar filePath = do
  exists <- doesFileExist filePath
  if not exists
    then throwIO $ TeExceptionFileDoesNotExist filePath
    else do
      database <- newProjectDatabase
      attachSucceeded <- attachFileToProjectDatabase database filePath
      if attachSucceeded
        then do
          maybeSchemaVersion <- getProjectDatabaseSchemaVersion database
          case maybeSchemaVersion of
            Just 1 -> do
              projectID <- newProjectID
              filePathMVar <- newMVar $ Just filePath
              browserWindowsMVar <- newMVar Map.empty
              let newProject = Project {
                                   projectID = projectID,
                                   projectApplicationState =
                                     applicationStateMVar,
                                   projectDatabase = database,
                                   projectFilePath = filePathMVar,
                                   projectBrowserWindows =
                                     browserWindowsMVar
                                 }
              addProjectToApplicationState applicationStateMVar newProject
              updateProjectInRecentProjects applicationStateMVar newProject
              noteNewProject newProject
              restoreProjectWindows newProject
            Just _ -> do
              closeProjectDatabase database
              throwIO $ TeExceptionFileCreatedByNewerVersion filePath
            Nothing -> do
              closeProjectDatabase database
              throwIO $ TeExceptionFileNotInRecognizedFormat filePath
        else do
          closeProjectDatabase database
          throwIO $ TeExceptionFileNotInRecognizedFormat filePath


applicationOpenRecentProject
    :: MVar ApplicationState -> Word64 -> IO ()
applicationOpenRecentProject applicationStateMVar index =
  catchTe applicationStateMVar () $ do
    applicationState <- readMVar applicationStateMVar
    let recentProjects = applicationStateRecentProjects applicationState
        index' = fromIntegral index
    if index' < length recentProjects
      then do
        applicationOpenProject' applicationStateMVar
                                $ recentProjectFilePath
                                   $ recentProjects !! index'
      else return ()


projectClose :: Project -> IO ()
projectClose project =
  catchTe (projectApplicationState project) () $ do
    closeProjectDatabase $ projectDatabase project
    removeProjectFromApplicationState (projectApplicationState project) project


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
  return () -- TODO
