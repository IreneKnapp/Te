{-# LANGUAGE ForeignFunctionInterface #-}
module Te.ForeignInterface () where

import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Word
import Foreign
import Foreign.C

import Te (ApplicationState, Project)
import qualified Te as Te


foreign import ccall "dynamic" mkVoidCallback
    :: FunPtr (IO ()) -> IO ()
foreign import ccall "dynamic" mkVoidStringStringCallback
    :: FunPtr (CString -> CString -> IO ()) -> (CString -> CString -> IO ())


foreign export ccall "string_free" stringFree
    :: CString -> IO ()
foreign export ccall "version_string" versionString
    :: IO CString
foreign export ccall "timestamp_to_string" timestampToString
    :: Word64 -> IO CString
foreign export ccall "application_init" applicationInit
    :: FunPtr (CString -> CString -> IO ())
    -> FunPtr (IO ())
    -> IO (StablePtr (MVar ApplicationState))
foreign export ccall "application_exit" applicationExit
    :: StablePtr (MVar ApplicationState) -> IO ()
foreign export ccall "application_recent_project_count"
                     applicationRecentProjectCount
    :: StablePtr (MVar ApplicationState) -> IO Word64
foreign export ccall "application_recent_project_name"
                     applicationRecentProjectName
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO CString
foreign export ccall "application_recent_project_timestamp"
                     applicationRecentProjectTimestamp
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO Word64
foreign export ccall "application_new_project" applicationNewProject
    :: StablePtr (MVar ApplicationState) -> IO (StablePtr Project)
foreign export ccall "application_open_project" applicationOpenProject
    :: StablePtr (MVar ApplicationState) -> CString -> IO (StablePtr Project)
foreign export ccall "application_open_recent_project"
                     applicationOpenRecentProject
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO (StablePtr Project)
foreign export ccall "project_close" projectClose
    :: StablePtr Project -> IO ()


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


applicationInit
    :: FunPtr (CString -> CString -> IO ())
    -> FunPtr (IO ())
    -> IO (StablePtr (MVar ApplicationState))
applicationInit foreignException foreignNoteRecentProjectsChanged = do
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
  applicationStateMVar <- Te.applicationInit callbackException
                                             callbackNoteRecentProjectsChanged
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
    :: StablePtr (MVar ApplicationState) -> IO (StablePtr Project)
applicationNewProject applicationStateMVarStablePtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  maybeResult <- Te.applicationNewProject applicationStateMVar
  case maybeResult of
    Just result -> newStablePtr result
    Nothing -> return $ castPtrToStablePtr nullPtr


applicationOpenProject
    :: StablePtr (MVar ApplicationState) -> CString -> IO (StablePtr Project)
applicationOpenProject applicationStateMVarStablePtr filePathCString = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  filePath <- peekCString filePathCString
  maybeResult <- Te.applicationOpenProject applicationStateMVar filePath
  case maybeResult of
    Just result -> newStablePtr result
    Nothing -> return $ castPtrToStablePtr nullPtr


applicationOpenRecentProject
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO (StablePtr Project)
applicationOpenRecentProject applicationStateMVarStablePtr index = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  maybeResult <- Te.applicationOpenRecentProject applicationStateMVar index
  case maybeResult of
    Just result -> newStablePtr result
    Nothing -> return $ castPtrToStablePtr nullPtr


projectClose :: StablePtr Project -> IO ()
projectClose projectStablePtr = do
  project <- deRefStablePtr projectStablePtr
  Te.projectClose project
