module Te.LowLevel.Exceptions
  (TeException(..),
   exceptionDetails,
   internalFailure,
   catchTe)
  where

import Control.Concurrent.MVar
import Control.Exception
import Data.Text (Text)
import Language.Haskell.TH

import Te.Types


data TeException
  = TeExceptionInternal Text Int
  | TeExceptionFileCreatedByNewerVersion Text
  | TeExceptionFileNotInRecognizedFormat Text
  | TeExceptionFileDoesNotExist Text
instance Show TeException
instance Exception TeException


exceptionDetails :: TeException -> Text
internalFailure :: Q Exp
catchTe :: MVar ApplicationState -> a -> IO a -> IO a
