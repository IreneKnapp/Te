module Te
  (ApplicationState(..),
   RecentProject(..),
   versionString,
   applicationInit,
   applicationExit,
   applicationRecentProjectCount,
   applicationRecentProjectName,
   applicationRecentProjectTimestamp,
   timestampToString)
  where

import Control.Concurrent.MVar
import Data.Version
import Data.Word

import Data.Timestamp
import Paths_te


data ApplicationState =
  ApplicationState {
      applicationStateRecentProjects :: [RecentProject]
    }


data RecentProject =
  RecentProject {
      recentProjectName :: String,
      recentProjectTimestamp :: Word64
    }


versionString :: String
versionString = showVersion version


applicationInit :: IO (MVar ApplicationState)
applicationInit = do
  let applicationState = ApplicationState {
                             applicationStateRecentProjects = []
                           }
  newMVar applicationState


applicationExit :: MVar ApplicationState -> IO ()
applicationExit applicationStateMVar = do
  return ()


applicationRecentProjectCount :: MVar ApplicationState -> IO Word64
applicationRecentProjectCount applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  let result = length $ applicationStateRecentProjects applicationState
  return $ fromIntegral result


applicationRecentProjectName
    :: MVar ApplicationState -> Word64 -> IO (Maybe String)
applicationRecentProjectName applicationStateMVar index = do
  applicationState <- readMVar applicationStateMVar
  let recentProjects = applicationStateRecentProjects applicationState
      index' = fromIntegral index
  if index' < length recentProjects
    then return $ Just $ recentProjectName $ recentProjects !! index'
    else return Nothing


applicationRecentProjectTimestamp
    :: MVar ApplicationState -> Word64 -> IO (Maybe Word64)
applicationRecentProjectTimestamp applicationStateMVar index = do
  applicationState <- readMVar applicationStateMVar
  let recentProjects = applicationStateRecentProjects applicationState
      index' = fromIntegral index
  if index' < length recentProjects
    then return $ Just $ recentProjectTimestamp $ recentProjects !! index'
    else return Nothing


timestampToString :: Word64 -> IO String
timestampToString timestamp = do
  describeTimestamp timestamp
