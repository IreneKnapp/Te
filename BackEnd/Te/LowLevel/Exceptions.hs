{-# LANGUAGE DeriveDataTypeable #-}
module Te.LowLevel.Exceptions
  (TeException(..),
   exceptionDetails,
   internalFailure,
   catchTe)
  where

import Control.Concurrent.MVar
import Control.Exception
import Data.Typeable
import Data.Word
import Language.Haskell.TH
import Prelude hiding (catch)

import Te.LowLevel.FrontEndCallbacks
import Te.Types


data TeException
  = TeExceptionInternal String Int
  | TeExceptionFileCreatedByNewerVersion FilePath
  | TeExceptionFileNotInRecognizedFormat FilePath
  | TeExceptionFileDoesNotExist FilePath
  deriving (Typeable)


instance Show TeException where
  show (TeExceptionInternal _ _) =
    "Internal failure."
  show (TeExceptionFileCreatedByNewerVersion _) =
    "File created by a newer version."
  show (TeExceptionFileNotInRecognizedFormat _) =
    "File not in recognized format."
  show (TeExceptionFileDoesNotExist _) =
    "File does not exist."


instance Exception TeException


exceptionDetails :: TeException -> String
exceptionDetails (TeExceptionInternal moduleName lineNumber) =
  "Te has experienced an internal failure.  This should never happen, but "
  ++ "apparently has.  You might wish to write down the error code \""
  ++ moduleName
  ++ " "
  ++ show lineNumber
  ++ "\" in case it helps."
exceptionDetails (TeExceptionFileCreatedByNewerVersion filePath) =
  "The file "
  ++ show filePath
  ++ " appears to be a Te file, but it cannot be read by this version of Te.  "
  ++ "This is most likely because it was created on a newer version."
exceptionDetails (TeExceptionFileNotInRecognizedFormat filePath) =
  "The file "
  ++ show filePath
  ++ " does not appear to be a Te file.  This may be because it was created "
  ++ "by a different program, such as a compression tool, or because it has "
  ++ "been corrupted in transmission."
exceptionDetails (TeExceptionFileDoesNotExist filePath) =
  "The file "
  ++ show filePath
  ++ " does not appear to exist.  This may be because it is on a removable or "
  ++ "network drive which is no longer present."


internalFailure :: Q Exp
internalFailure = do
  location' <- location
  let module' = loc_module location'
      (lineNumber, _) = loc_start location'
  return $ AppE (AppE (ConE $ mkName "TeExceptionInternal")
                      (LitE $ StringL module'))
                (LitE $ IntegerL $ fromIntegral lineNumber)


catchTe :: MVar ApplicationState -> a -> IO a -> IO a
catchTe applicationStateMVar defaultValue action = do
  catch (action)
        (\e -> do
           exception applicationStateMVar e
           return defaultValue)
