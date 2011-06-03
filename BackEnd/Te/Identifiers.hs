{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Te.Identifiers
  (ProjectID,
   BrowserWindowID,
   InodeID,
   newProjectID,
   nullProjectID,
   newBrowserWindowID,
   nullBrowserWindowID,
   newInodeID,
   nullInodeID)
  where

import Data.Binary
import Data.UUID
import Foreign.Storable
import qualified System.UUID.V4 as V4

import Data.SQLable


newtype ProjectID =
  ProjectID UUID deriving (Eq, Ord, Binary, Storable, SQLable)


newtype BrowserWindowID =
  BrowserWindowID UUID deriving (Eq, Ord, Binary, Storable, SQLable)


newtype InodeID =
  InodeID UUID deriving (Eq, Ord, Binary, Storable, SQLable)


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


newInodeID :: IO InodeID
newInodeID = do
  uuid <- V4.uuid
  return $ InodeID uuid


nullInodeID :: InodeID
nullInodeID = InodeID minBound
