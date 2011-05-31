module Te.FrontEndCallbacks
  (noteRecentProjectsChanged,
   noteNewBrowserWindow,
   noteDeletedBrowserWindow)
  where

import Control.Concurrent.MVar

import Te.Types


noteRecentProjectsChanged :: MVar ApplicationState -> IO ()
noteRecentProjectsChanged applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteRecentProjectsChanged callbacks
  callback


noteNewBrowserWindow :: BrowserWindow -> IO ()
noteNewBrowserWindow browserWindow = do
  applicationState <- readMVar $ projectApplicationState
                                  $ browserWindowProject browserWindow
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteNewBrowserWindow callbacks
  callback browserWindow


noteDeletedBrowserWindow :: BrowserWindow -> IO ()
noteDeletedBrowserWindow browserWindow = do
  applicationState <- readMVar $ projectApplicationState
                                  $ browserWindowProject browserWindow
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteDeletedBrowserWindow callbacks
  callback browserWindow
