{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Te.LowLevel.Exceptions
  (TeException(..),
   exceptionDetails,
   internalFailure,
   catchTe)
  where

import Control.Concurrent.MVar
import Control.Exception
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import Data.Word
import Language.Haskell.TH
import Prelude hiding (catch)

import Te.LowLevel.FrontEndCallbacks
import Te.Types


data TeException
  = TeExceptionInternal Text Int
  | TeExceptionFileCreatedByNewerVersion Text
  | TeExceptionFileNotInRecognizedFormat Text
  | TeExceptionFileDoesNotExist Text
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


exceptionDetails :: TeException -> Text
exceptionDetails (TeExceptionInternal moduleName lineNumber) = Text.concat
  ["Te has experienced an internal failure.  This should never happen, but ",
   "apparently has.  You might wish to write down the error code \"",
   moduleName,
   " ",
   Text.pack $ show lineNumber,
   "\" in case it helps."]
exceptionDetails (TeExceptionFileCreatedByNewerVersion filePath) = Text.concat
  ["The file ",
   Text.pack $ show filePath,
   " appears to be a Te file, but it cannot be read by this version of Te.  ",
   "This is most likely because it was created on a newer version."]
exceptionDetails (TeExceptionFileNotInRecognizedFormat filePath) = Text.concat
  ["The file ",
   Text.pack $ show filePath,
   " does not appear to be a Te file.  This may be because it was created ",
   "by a different program, such as a compression tool, or because it has ",
   "been corrupted in transmission."]
exceptionDetails (TeExceptionFileDoesNotExist filePath) = Text.concat
  ["The file ",
   Text.pack $ show filePath,
   " does not appear to exist.  This may be because it is on a removable or ",
   "network drive which is no longer present."]


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
