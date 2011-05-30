module Te.Types
  (ApplicationState(..),
   FrontEndCallbacks(..),
   RecentProject(..),
   Project(..))
  where

import Control.Concurrent.MVar
import Data.Map (Map)
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
      frontEndCallbacksException :: String -> String -> IO (),
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
