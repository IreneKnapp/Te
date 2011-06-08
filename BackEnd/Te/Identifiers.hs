{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Te.Identifiers
  (ProjectID,
   WindowID,
   WindowTypeID(..),
   BrowserWindowID,
   DocumentWindowID,
   InodeID,
   newProjectID,
   nullProjectID,
   nullWindowID,
   newBrowserWindowID,
   nullBrowserWindowID,
   newDocumentWindowID,
   nullDocumentWindowID,
   newInodeID,
   nullInodeID)
  where

import Data.Binary
import Data.UUID
import Foreign.Storable
import qualified System.UUID.V4 as V4

import Data.SQLable


newtype ProjectID =
  ProjectID' UUID deriving (Eq, Ord, Binary, Storable, SQLable)


newtype WindowID =
  WindowID' UUID deriving (Eq, Ord, Binary, Storable, SQLable)


class WindowTypeID windowTypeID where
  toWindowID :: windowTypeID -> WindowID
  fromWindowID :: WindowID -> windowTypeID


newtype BrowserWindowID =
  BrowserWindowID' UUID deriving (Eq, Ord, Binary, Storable, SQLable)


instance WindowTypeID BrowserWindowID where
  toWindowID (BrowserWindowID' uuid) = WindowID' uuid
  fromWindowID (WindowID' uuid) = BrowserWindowID' uuid


newtype DocumentWindowID =
  DocumentWindowID' UUID deriving (Eq, Ord, Binary, Storable, SQLable)


instance WindowTypeID DocumentWindowID where
  toWindowID (DocumentWindowID' uuid) = WindowID' uuid
  fromWindowID (WindowID' uuid) = DocumentWindowID' uuid


newtype InodeID =
  InodeID' UUID deriving (Eq, Ord, Binary, Storable, SQLable)


newProjectID :: IO ProjectID
newProjectID = do
  uuid <- V4.uuid
  return $ ProjectID' uuid


nullProjectID :: ProjectID
nullProjectID = ProjectID' minBound


nullWindowID :: WindowID
nullWindowID = WindowID' minBound


newBrowserWindowID :: IO BrowserWindowID
newBrowserWindowID = do
  uuid <- V4.uuid
  return $ BrowserWindowID' uuid


nullBrowserWindowID :: BrowserWindowID
nullBrowserWindowID = BrowserWindowID' minBound


newDocumentWindowID :: IO DocumentWindowID
newDocumentWindowID = do
  uuid <- V4.uuid
  return $ DocumentWindowID' uuid


nullDocumentWindowID :: DocumentWindowID
nullDocumentWindowID = DocumentWindowID' minBound


newInodeID :: IO InodeID
newInodeID = do
  uuid <- V4.uuid
  return $ InodeID' uuid


nullInodeID :: InodeID
nullInodeID = InodeID' minBound
