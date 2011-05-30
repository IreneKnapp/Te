{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Te.Identifiers
  (ProjectID,
   newProjectID)
  where

import Data.Binary
import Data.UUID
import qualified System.UUID.V4 as V4


newtype ProjectID = ProjectID UUID deriving (Eq, Ord, Binary)


newProjectID :: IO ProjectID
newProjectID = do
  uuid <- V4.uuid
  return $ ProjectID uuid
