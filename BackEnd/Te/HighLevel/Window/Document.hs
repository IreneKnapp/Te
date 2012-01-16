module Te.HighLevel.Window.Document
  (DocumentWindow,
   DocumentWindowID,
   documentWindowID,
   getDocumentWindowMinimumSize,
   getDocumentWindowDesiredSize,
   getDocumentWindowTitle,
   getDocumentWindowTitleIcon,
   documentWindowAdjustPanes)
  where

import Control.Concurrent.MVar
import Data.Array.Unboxed
import Data.Int

import Te.HighLevel.Window
import Te.HighLevel.Window.Document.Pane
import Te.HighLevel.Window.DocumentPrivate
import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.LowLevel.FrontEndCallbacks
import Te.LowLevel.Identifiers
import Te.Types


instance Window DocumentWindow where
  windowID = toWindowID . documentWindowID
  windowProject = documentWindowProject
  getWindowTitle = getDocumentWindowTitle
  getWindowTitleIcon = getDocumentWindowTitleIcon
  browserWindowDo _ default' _ = return default'
  documentWindowDo window _ action = action window


getDocumentWindowMinimumSize :: DocumentWindow -> IO (Int64, Int64)
getDocumentWindowMinimumSize documentWindow = do
  getWindowSizeFromPaneSizes documentWindow getDocumentPaneMinimumSize


getDocumentWindowDesiredSize :: DocumentWindow -> IO (Int64, Int64)
getDocumentWindowDesiredSize documentWindow = do
  getWindowSizeFromPaneSizes documentWindow getDocumentPaneDesiredSize


getDocumentWindowTitle :: DocumentWindow -> IO String
getDocumentWindowTitle documentWindow = do
  let project = documentWindowProject documentWindow
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    documentInode <- lookupDocumentWindowInode documentWindow
    documentInodeInformation <- lookupInodeInformation documentInode
    let documentName = inodeInformationName documentInodeInformation
    return documentName


getDocumentWindowTitleIcon :: DocumentWindow -> IO String
getDocumentWindowTitleIcon documentWindow = do
  let project = documentWindowProject documentWindow
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "File" $ do
    documentInode <- lookupDocumentWindowInode documentWindow
    documentInodeInformation <- lookupInodeInformation documentInode
    let documentType = inodeInformationType documentInodeInformation
    case documentType of
      DirectoryInodeType -> return "Folder"
      HaskellInodeType -> return "File"


documentWindowAdjustPanes :: DocumentWindow -> IO ()
documentWindowAdjustPanes documentWindow = do
  return ()
