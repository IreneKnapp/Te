{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Te.Identifiers
  (ProjectID,
   BrowserWindowID,
   newProjectID,
   nullProjectID,
   newBrowserWindowID,
   nullBrowserWindowID)
  where

import Data.Binary
import Data.UUID
import Foreign.Storable
import qualified System.UUID.V4 as V4


newtype ProjectID =
  ProjectID UUID deriving (Eq, Ord, Binary, Storable)


newtype BrowserWindowID =
  BrowserWindowID UUID deriving (Eq, Ord, Binary, Storable)


newProjectID :: IO ProjectID
newProjectID = do
  uuid <- V4.uuid
  return $ ProjectID uuid


nullProjectID :: ProjectID
nullProjectID = ProjectID minBound


newBrowserWindowID :: IO BrowserWindowID
newBrowserWindowID = do
  uuid <- V4.uuid
  return $ BrowserWindowID uuid


nullBrowserWindowID :: BrowserWindowID
nullBrowserWindowID = BrowserWindowID minBound
