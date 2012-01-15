module Te.LowLevel.FrontEndCallbacks
  (exception,
   confirm,
   noteRecentProjectsChanged,
   getEmWidth,
   getLineHeight,
   getLineNumberEmWidth,
   getScrollerWidth,
   getVisibleSize,
   noteNewBrowserWindow,
   noteDeletedWindow,
   activateWindow,
   noteBrowserItemsChanged,
   editBrowserItemName,
   noteNewDocumentWindow)
  where

import Control.Concurrent.MVar
import Data.Word

import {-# SOURCE #-} Te.LowLevel.Exceptions
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


getEmWidth :: MVar ApplicationState -> IO Double
getEmWidth applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksGetEmWidth callbacks
  callback


getLineHeight :: MVar ApplicationState -> IO Double
getLineHeight applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksGetLineHeight callbacks
  callback


getLineNumberEmWidth :: MVar ApplicationState -> IO Double
getLineNumberEmWidth applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksGetLineNumberEmWidth callbacks
  callback


getScrollerWidth :: MVar ApplicationState -> IO Double
getScrollerWidth applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksGetScrollerWidth callbacks
  callback


getVisibleSize :: MVar ApplicationState -> IO (Double, Double)
getVisibleSize applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksGetVisibleSize callbacks
  callback


noteNewBrowserWindow :: BrowserWindow -> IO ()
noteNewBrowserWindow browserWindow = do
  let project = browserWindowProject browserWindow
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteNewBrowserWindow callbacks
  callback browserWindow


noteDeletedWindow :: AnyWindow -> IO ()
noteDeletedWindow window = do
  let project = windowProject window
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteDeletedWindow callbacks
  callback window


activateWindow :: AnyWindow -> IO ()
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
