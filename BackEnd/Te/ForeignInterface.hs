{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module Te.ForeignInterface () where

import Control.Concurrent.MVar
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.UUID
import Data.Word
import Foreign
import Foreign.C

import Te (ApplicationState, Project)
import qualified Te as Te
import Te.Exceptions
import Te.Identifiers (ProjectID, BrowserWindowID)
import qualified Te.Identifiers as Te


foreign import ccall "dynamic" mkVoidCallback
    :: FunPtr (IO ()) -> IO ()
foreign import ccall "dynamic" mkVoidStringStringCallback
    :: FunPtr (CString -> CString -> IO ()) -> (CString -> CString -> IO ())
foreign import ccall "dynamic" mkVoidProjectIDPtrCallback
    :: FunPtr (Ptr ProjectID -> IO ()) -> (Ptr ProjectID -> IO ())
foreign import ccall "dynamic" mkVoidProjectIDPtrBrowserWindowIDPtrCallback
    :: FunPtr (Ptr ProjectID -> Ptr BrowserWindowID -> IO ())
    -> (Ptr ProjectID -> Ptr BrowserWindowID -> IO ())
foreign import ccall "dynamic" mkVoidBrowserWindowIDPtrCallback
    :: FunPtr (Ptr BrowserWindowID -> IO ()) -> (Ptr BrowserWindowID -> IO ())


foreign export ccall "teStringFree" stringFree
    :: CString -> IO ()
foreign export ccall "teVersionString" versionString
    :: IO CString
foreign export ccall "teTimestampToString" timestampToString
    :: Word64 -> IO CString
foreign export ccall "teUUIDHash" uuidHash
    :: Ptr UUID -> IO Word64
foreign export ccall "teUUIDEqual" uuidEqual
    :: Ptr UUID -> Ptr UUID -> IO CInt
foreign export ccall "teUUIDShow" uuidShow
    :: Ptr UUID -> IO CString
foreign export ccall "teApplicationInit" applicationInit
    :: FunPtr (CString -> CString -> IO ())
    -> FunPtr (IO ())
    -> FunPtr (Ptr ProjectID -> IO ())
    -> FunPtr (Ptr ProjectID -> IO ())
    -> FunPtr (Ptr ProjectID -> Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> IO (StablePtr (MVar ApplicationState))
foreign export ccall "teApplicationExit" applicationExit
    :: StablePtr (MVar ApplicationState) -> IO ()
foreign export ccall "teApplicationRecentProjectCount"
                     applicationRecentProjectCount
    :: StablePtr (MVar ApplicationState) -> IO Word64
foreign export ccall "teApplicationRecentProjectName"
                     applicationRecentProjectName
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO CString
foreign export ccall "teApplicationRecentProjectTimestamp"
                     applicationRecentProjectTimestamp
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO Word64
foreign export ccall "teApplicationNewProject" applicationNewProject
    :: StablePtr (MVar ApplicationState) -> IO ()
foreign export ccall "teApplicationOpenProject" applicationOpenProject
    :: StablePtr (MVar ApplicationState) -> CString -> IO ()
foreign export ccall "teApplicationOpenRecentProject"
                     applicationOpenRecentProject
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO ()
foreign export ccall "teProjectClose" projectClose
    :: StablePtr (MVar ApplicationState) -> Ptr ProjectID -> IO ()


stringNew :: String -> IO CString
stringNew string = do
  let byteString = UTF8.fromString string
      bufferLength = 1 + BS.length byteString
  cString <- mallocArray bufferLength
  mapM_ (\index -> do
           let byte = BS.index byteString index
               element = advancePtr cString index
           poke element byte)
        [0 .. bufferLength - 2]
  poke (advancePtr cString $ bufferLength - 1) 0x00
  return $ castPtr cString


stringFree :: CString -> IO ()
stringFree cString = free cString


versionString :: IO CString
versionString = do
  stringNew Te.versionString


timestampToString :: Word64 -> IO CString
timestampToString timestamp = do
  result <- Te.timestampToString timestamp
  stringNew result


uuidHash :: Ptr UUID -> IO Word64
uuidHash uuidPtr = do
  uuid <- peek uuidPtr
  Te.uuidHash uuid


uuidEqual :: Ptr UUID -> Ptr UUID -> IO CInt
uuidEqual uuidPtrA uuidPtrB = do
  uuidA <- peek uuidPtrA
  uuidB <- peek uuidPtrB
  result <- Te.uuidEqual uuidA uuidB
  if result
    then return 1
    else return 0


uuidShow :: Ptr UUID -> IO CString
uuidShow uuidPtr = do
  uuid <- peek uuidPtr
  string <- Te.uuidShow uuid
  stringNew string


applicationInit
    :: FunPtr (CString -> CString -> IO ())
    -> FunPtr (IO ())
    -> FunPtr (Ptr ProjectID -> IO ())
    -> FunPtr (Ptr ProjectID -> IO ())
    -> FunPtr (Ptr ProjectID -> Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> IO (StablePtr (MVar ApplicationState))
applicationInit foreignException
                foreignNoteRecentProjectsChanged
                foreignNoteNewProject
                foreignNoteDeletedProject
                foreignNoteNewBrowserWindow
                foreignNoteDeletedBrowserWindow = do
  let callbackException messageString detailsString = do
        withCString messageString
                    (\messageCString -> do
                       withCString detailsString
                                   (\detailsCString -> do
                                      callbackException' messageCString
                                                         detailsCString))
      callbackException' =
        mkVoidStringStringCallback foreignException
      callbackNoteRecentProjectsChanged =
        mkVoidCallback foreignNoteRecentProjectsChanged
      callbackNoteNewProject project = do
        alloca (\projectIDPtr -> do
                  poke projectIDPtr $ Te.projectID project
                  callbackNoteNewProject' projectIDPtr)
      callbackNoteNewProject' =
        mkVoidProjectIDPtrCallback foreignNoteNewProject
      callbackNoteDeletedProject project = do
        alloca (\projectIDPtr -> do
                  poke projectIDPtr $ Te.projectID project
                  callbackNoteDeletedProject' projectIDPtr)
      callbackNoteDeletedProject' =
        mkVoidProjectIDPtrCallback foreignNoteNewProject
      callbackNoteNewBrowserWindow project browserWindow = do
        alloca (\projectIDPtr -> do
                  poke projectIDPtr $ Te.projectID project
                  alloca (\browserWindowIDPtr -> do
                            poke browserWindowIDPtr
                                 $ Te.browserWindowID browserWindow
                            callbackNoteNewBrowserWindow' projectIDPtr
                                                          browserWindowIDPtr))
      callbackNoteNewBrowserWindow' =
        mkVoidProjectIDPtrBrowserWindowIDPtrCallback
         foreignNoteNewBrowserWindow
      callbackNoteDeletedBrowserWindow browserWindow = do
        alloca (\browserWindowIDPtr -> do
                  poke browserWindowIDPtr $ Te.browserWindowID browserWindow
                  callbackNoteDeletedBrowserWindow' browserWindowIDPtr)
      callbackNoteDeletedBrowserWindow' =
        mkVoidBrowserWindowIDPtrCallback foreignNoteDeletedBrowserWindow
      callbacks = Te.FrontEndCallbacks {
                      Te.frontEndCallbacksException =
                        callbackException,
                      Te.frontEndCallbacksNoteRecentProjectsChanged =
                        callbackNoteRecentProjectsChanged,
                      Te.frontEndCallbacksNoteNewProject =
                        callbackNoteNewProject,
                      Te.frontEndCallbacksNoteDeletedProject =
                        callbackNoteDeletedProject,
                      Te.frontEndCallbacksNoteNewBrowserWindow =
                        callbackNoteNewBrowserWindow,
                      Te.frontEndCallbacksNoteDeletedBrowserWindow =
                        callbackNoteDeletedBrowserWindow
                    }
  applicationStateMVar <- Te.applicationInit callbacks
  newStablePtr applicationStateMVar


applicationExit :: StablePtr (MVar ApplicationState) -> IO ()
applicationExit applicationStateMVarStablePtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  Te.applicationExit applicationStateMVar
  freeStablePtr applicationStateMVarStablePtr


applicationRecentProjectCount
    :: StablePtr (MVar ApplicationState) -> IO Word64
applicationRecentProjectCount applicationStateMVarStablePtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  Te.applicationRecentProjectCount applicationStateMVar


applicationRecentProjectName
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO CString
applicationRecentProjectName applicationStateMVarStablePtr index = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  maybeResult <- Te.applicationRecentProjectName applicationStateMVar index
  case maybeResult of
    Just result -> stringNew result
    Nothing -> return nullPtr


applicationRecentProjectTimestamp
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO Word64
applicationRecentProjectTimestamp applicationStateMVarStablePtr index = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  maybeResult <-
    Te.applicationRecentProjectTimestamp applicationStateMVar index
  case maybeResult of
    Just result -> return result
    Nothing -> return 0


applicationNewProject
    :: StablePtr (MVar ApplicationState) -> IO ()
applicationNewProject applicationStateMVarStablePtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  Te.applicationNewProject applicationStateMVar


applicationOpenProject
    :: StablePtr (MVar ApplicationState) -> CString -> IO ()
applicationOpenProject applicationStateMVarStablePtr
                       filePathCString = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  filePath <- peekCString filePathCString
  Te.applicationOpenProject applicationStateMVar filePath


applicationOpenRecentProject
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO ()
applicationOpenRecentProject applicationStateMVarStablePtr
                             index = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  Te.applicationOpenRecentProject applicationStateMVar index


projectClose :: StablePtr (MVar ApplicationState) -> Ptr ProjectID -> IO ()
projectClose applicationStateMVarStablePtr projectIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  Te.catchTe applicationStateMVar () $ do
    applicationState <- readMVar applicationStateMVar
    projectID <- peek projectIDPtr
    case Map.lookup projectID $ Te.applicationStateProjects applicationState of
      Just project -> Te.projectClose project
      Nothing -> throwIO $(internalFailure)
