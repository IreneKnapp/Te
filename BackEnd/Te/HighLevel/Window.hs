{-# LANGUAGE TemplateHaskell #-}
module Te.HighLevel.Window
  (Window,
   WindowID,
   windowID,
   toWindowID,
   fromWindowID,
   windowClose)
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


windowClose :: AnyWindow -> IO ()
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
