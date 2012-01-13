module Te.LowLevel.Exceptions
  (TeException(..),
   exceptionDetails,
   internalFailure,
   catchTe)
  where

import Control.Concurrent.MVar
import Control.Exception
import Language.Haskell.TH

import Te.Types


data TeException
  = TeExceptionInternal String Int
  | TeExceptionFileCreatedByNewerVersion FilePath
  | TeExceptionFileNotInRecognizedFormat FilePath
  | TeExceptionFileDoesNotExist FilePath
instance Show TeException
instance Exception TeException


exceptionDetails :: TeException -> String
internalFailure :: Q Exp
catchTe :: MVar ApplicationState -> a -> IO a -> IO a
