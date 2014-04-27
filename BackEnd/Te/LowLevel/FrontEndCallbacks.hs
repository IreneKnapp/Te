module Te.LowLevel.FrontEndCallbacks
  (exception,
   confirm,
   noteRecentProjectsChanged,
   getEmWidth,
   getLineHeight,
   getLineNumberEmWidth,
   getScrollerWidth,
   getVisibleFrame,
   getDocumentContentFromFrame,
   noteNewBrowserWindow,
   noteDeletedWindow,
   activateWindow,
   noteBrowserItemsChanged,
   editBrowserItemName,
   noteNewDocumentWindow,
   noteNewDocumentPane,
   newGhostWindowWithHorizontalDivider,
   newGhostWindowWithVerticalDivider,
   cleanupGhostWindow,
   ghostWindowUpdateMouse)
  where

import Control.Concurrent.MVar
import Data.Int
import qualified Data.Text as Text
import Data.Word

import Data.Geometry
import {-# SOURCE #-} Te.LowLevel.Exceptions
import Te.Types


exception :: MVar ApplicationState -> TeException -> IO ()
exception applicationStateMVar exception = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksException callbacks
      messageText = Text.pack $ show exception
      detailsText = exceptionDetails exception
  callback messageText detailsText


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


getVisibleFrame
  :: MVar ApplicationState -> IO Rectangle
getVisibleFrame applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksGetVisibleFrame callbacks
  callback


getDocumentContentFromFrame
  :: MVar ApplicationState
  -> Rectangle
  -> IO Rectangle
getDocumentContentFromFrame applicationStateMVar frame = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksGetDocumentContentFromFrame callbacks
  callback frame


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


noteNewDocumentWindow
  :: DocumentWindow
  -> Rectangle
  -> IO ()
noteNewDocumentWindow documentWindow frame = do
  let project = documentWindowProject documentWindow
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteNewDocumentWindow callbacks
  callback documentWindow frame


noteNewDocumentPane
  :: DocumentWindow
  -> DocumentPane
  -> Rectangle
  -> IO ()
noteNewDocumentPane documentWindow documentPane frame = do
  let project = documentWindowProject documentWindow
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNoteNewDocumentPane callbacks
  callback documentWindow documentPane frame


newGhostWindowWithHorizontalDivider
  :: DocumentWindow
  -> Rectangle
  -> Point
  -> IO ()
newGhostWindowWithHorizontalDivider documentWindow frame location = do
  let project = documentWindowProject documentWindow
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNewGhostWindowWithHorizontalDivider callbacks
  callback documentWindow frame location


newGhostWindowWithVerticalDivider
  :: DocumentWindow
  -> Rectangle
  -> Point
  -> IO ()
newGhostWindowWithVerticalDivider documentWindow frame location = do
  let project = documentWindowProject documentWindow
      applicationStateMVar = projectApplicationState project
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksNewGhostWindowWithVerticalDivider callbacks
  callback documentWindow frame location


cleanupGhostWindow :: MVar ApplicationState -> IO ()
cleanupGhostWindow applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksCleanupGhostWindow callbacks
  callback


ghostWindowUpdateMouse :: MVar ApplicationState -> Point -> IO ()
ghostWindowUpdateMouse applicationStateMVar point = do
  applicationState <- readMVar applicationStateMVar
  let callbacks = applicationStateFrontEndCallbacks applicationState
      callback = frontEndCallbacksGhostWindowUpdateMouse callbacks
  callback point
