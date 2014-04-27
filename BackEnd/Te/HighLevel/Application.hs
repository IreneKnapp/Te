{-# LANGUAGE OverloadedStrings #-}
module Te.HighLevel.Application
  (ByteSize(..),
   Timestamp(..),
   FrontEndCallbacks(..),
   frontEndInternalFailure,
   versionText,
   timestampShow,
   byteSizePlaceholderText,
   byteSizeShow,
   ApplicationState,
   applicationInit,
   applicationExit,
   applicationRecentProjectCount,
   applicationRecentProjectName,
   applicationRecentProjectTimestamp,
   applicationNewProject,
   applicationOpenProject,
   applicationOpenRecentProject)
  where

import Control.Concurrent.MVar
import Control.Exception
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Version
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import System.Directory
import System.FilePath

import Data.ByteSize
import Data.Timestamp
import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.LowLevel.Identifiers
import Te.HighLevel.ApplicationPrivate
import Te.Types
import Paths_te


frontEndInternalFailure :: MVar ApplicationState -> Text -> Word64 -> IO ()
frontEndInternalFailure applicationStateMVar filename lineNumber =
  catchTe applicationStateMVar () $ do
    let lineNumber' = fromIntegral lineNumber
    throwIO $ TeExceptionInternal filename lineNumber'


versionText :: Text
versionText = Text.pack $ showVersion version


timestampShow :: Timestamp -> IO Text
timestampShow timestamp = do
  describeTimestamp timestamp


byteSizePlaceholderText :: IO Text
byteSizePlaceholderText = do
  return "—"


byteSizeShow :: ByteSize -> IO Text
byteSizeShow byteSize = do
  return $ Text.pack $ show byteSize


applicationInit :: FrontEndCallbacks -> IO (MVar ApplicationState)
applicationInit callbacks = do
  let applicationState = ApplicationState {
                             applicationStateRecentProjects = [],
                             applicationStateProjects = Map.empty,
                             applicationStateFrontEndCallbacks = callbacks,
                             applicationStateDragState = Nothing
                           }
  newMVar applicationState


applicationExit :: MVar ApplicationState -> IO ()
applicationExit applicationStateMVar = do
  return ()


applicationRecentProjectCount :: MVar ApplicationState -> IO Word64
applicationRecentProjectCount applicationStateMVar =
  catchTe applicationStateMVar 0 $ do
    applicationState <- readMVar applicationStateMVar
    let result = length $ applicationStateRecentProjects applicationState
    return $ fromIntegral result


applicationRecentProjectName
    :: MVar ApplicationState -> Word64 -> IO (Maybe Text)
applicationRecentProjectName applicationStateMVar index =
  catchTe applicationStateMVar Nothing $ do
    applicationState <- readMVar applicationStateMVar
    let recentProjects = applicationStateRecentProjects applicationState
        index' = fromIntegral index
    if index' < length recentProjects
      then do
        let filePath = recentProjectFilePath $ recentProjects !! index'
            nameWithExtension = Text.pack $ takeFileName $ Text.unpack filePath
            name = if Text.isSuffixOf ".te" nameWithExtension
                     then Text.pack $ dropExtension
                            $ Text.unpack nameWithExtension
                     else nameWithExtension
        return $ Just name
      else return Nothing


applicationRecentProjectTimestamp
    :: MVar ApplicationState -> Word64 -> IO (Maybe Timestamp)
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
    name <- getNextUntitledProjectName applicationStateMVar
    nameMVar <- newMVar name
    filePathMVar <- newMVar Nothing
    windowsMVar <- newMVar Map.empty
    let newProject = Project {
                         projectID = projectID,
                         projectApplicationState = applicationStateMVar,
                         projectDatabase = database,
                         projectName = nameMVar,
                         projectFilePath = filePathMVar,
                         projectWindows = windowsMVar
                       }
    addProjectToApplicationState applicationStateMVar newProject
    restoreProjectWindows newProject


applicationOpenProject
    :: MVar ApplicationState -> Text -> IO ()
applicationOpenProject applicationStateMVar filePath =
  catchTe applicationStateMVar () $ do
    applicationOpenProject' applicationStateMVar filePath


applicationOpenProject'
    :: MVar ApplicationState -> Text -> IO ()
applicationOpenProject' applicationStateMVar filePath = do
  exists <- doesFileExist $ Text.unpack filePath
  if not exists
    then throwIO $ TeExceptionFileDoesNotExist filePath
    else do
      database <- newProjectDatabase
      attachSucceeded <- attachFileToProjectDatabase database filePath
      if attachSucceeded
        then do
          maybeSchemaVersion <-
            getProjectDatabaseSchemaVersion database
          case maybeSchemaVersion of
            Just 1 -> do
              projectID <- newProjectID
              nameMVar <- newMVar $ computeNameForFilePath filePath
              filePathMVar <- newMVar $ Just filePath
              windowsMVar <- newMVar Map.empty
              let newProject = Project {
                                   projectID = projectID,
                                   projectApplicationState =
                                     applicationStateMVar,
                                   projectDatabase = database,
                                   projectName = nameMVar,
                                   projectFilePath = filePathMVar,
                                   projectWindows = windowsMVar
                                 }
              addProjectToApplicationState applicationStateMVar newProject
              updateProjectInRecentProjects applicationStateMVar newProject
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
