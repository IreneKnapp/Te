{-# LANGUAGE DeriveDataTypeable #-}
module Te.Types
  (ApplicationState(..),
   FrontEndCallbacks(..),
   RecentProject(..),
   Project(..),
   TeException(..))
  where

import Control.Concurrent.MVar
import Control.Exception
import Data.Map (Map)
import Data.Typeable
import Data.Word
import Database.SQLite3 (Database)

import Te.Identifiers


data ApplicationState =
  ApplicationState {
      applicationStateRecentProjects :: [RecentProject],
      applicationStateProjects :: Map ProjectID Project,
      applicationStateFrontEndCallbacks :: FrontEndCallbacks
    }


data FrontEndCallbacks =
  FrontEndCallbacks {
      frontEndCallbacksException :: String -> IO (),
      frontEndCallbacksNoteRecentProjectsChanged :: IO ()
    }


data RecentProject =
  RecentProject {
      recentProjectID :: ProjectID,
      recentProjectFilePath :: String,
      recentProjectTimestamp :: Word64
    }


data Project =
  Project {
      projectID :: ProjectID,
      projectApplicationState :: MVar ApplicationState,
      projectDatabase :: Database,
      projectFilePath :: MVar (Maybe FilePath)
    }


data TeException
  = TeExceptionFileCreatedByNewerVersion FilePath
  | TeExceptionFileNotInRecognizedFormat FilePath
  | TeExceptionFileDoesNotExist FilePath
  deriving (Typeable)


instance Show TeException where
  show (TeExceptionFileCreatedByNewerVersion _) =
    "File created by a newer version."
  show (TeExceptionFileNotInRecognizedFormat _) =
    "File not in recognized format."
  show (TeExceptionFileDoesNotExist _) =
    "File does not exist."


instance Exception TeException
