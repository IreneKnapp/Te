module Te.FrontEndCallbacks
  (exception,
   confirm,
   noteRecentProjectsChanged,
   noteNewBrowserWindow,
   noteDeletedWindow,
   activateWindow,
   noteBrowserItemsChanged,
   editBrowserItemName,
   noteNewDocumentWindow)
  where

import Control.Concurrent.MVar
import Data.Word

import Te.Exceptions
import Te.Types


exception :: MVar ApplicationState -> TeException -> IO ()
exception applicationStateMVar exception = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksException callbacks
      messageString = show exception
      detailsString = exceptionDetails exception
  callback messageString detailsString


confirm
    :: MVar ApplicationState
    -> ConfirmationDialog
    -> (Word64 -> IO ())
    -> IO ()
confirm applicationStateMVar confirmationDialog completionHandler = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksConfirm callbacks
  callback confirmationDialog completionHandler


noteRecentProjectsChanged :: MVar ApplicationState -> IO ()
noteRecentProjectsChanged applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteRecentProjectsChanged callbacks
  callback


noteNewBrowserWindow :: BrowserWindow -> IO ()
noteNewBrowserWindow browserWindow = do
  let project = browserWindowProject browserWindow
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteNewBrowserWindow callbacks
  callback browserWindow


noteDeletedWindow :: Window -> IO ()
noteDeletedWindow window = do
  let project = windowProject window
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteDeletedWindow callbacks
  callback window


activateWindow :: Window -> IO ()
activateWindow window = do
  let project = windowProject window
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksActivateWindow callbacks
  callback window


noteBrowserItemsChanged :: BrowserWindow -> IO ()
noteBrowserItemsChanged browserWindow = do
  let project = browserWindowProject browserWindow
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteBrowserItemsChanged callbacks
  callback browserWindow


editBrowserItemName :: BrowserItem -> IO ()
editBrowserItemName browserItem = do
  let browserWindow = browserItemBrowserWindow browserItem
      project = browserWindowProject browserWindow
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksEditBrowserItemName callbacks
  callback browserItem


noteNewDocumentWindow :: DocumentWindow -> IO ()
noteNewDocumentWindow documentWindow = do
  let project = documentWindowProject documentWindow
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteNewDocumentWindow callbacks
  callback documentWindow
