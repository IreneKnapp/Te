{-# LANGUAGE ForeignFunctionInterface #-}
module Te.ForeignInterface () where

import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Word
import Foreign
import Foreign.C

import Te (ApplicationState(..), RecentProject(..))
import qualified Te as Te


foreign export ccall "string_free" stringFree
    :: CString -> IO ()
foreign export ccall "version_string" versionString
    :: IO CString
foreign export ccall "application_init" applicationInit
    :: IO (StablePtr (MVar ApplicationState))
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
foreign export ccall "timestamp_to_string" timestampToString
    :: Word64 -> IO CString


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


applicationInit :: IO (StablePtr (MVar ApplicationState))
applicationInit = do
  applicationStateMVar <- Te.applicationInit
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


timestampToString :: Word64 -> IO CString
timestampToString timestamp = do
  result <- Te.timestampToString timestamp
  stringNew result
