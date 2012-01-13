{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Te.LowLevel.Identifiers
  (ProjectID,
   WindowID,
   SubtypeWindowID(..),
   BrowserWindowID,
   DocumentWindowID,
   DocumentPaneID,
   DocumentVerticalDividerID,
   DocumentHorizontalDividerID,
   InodeID,
   newProjectID,
   nullProjectID,
   nullWindowID,
   newBrowserWindowID,
   nullBrowserWindowID,
   newDocumentWindowID,
   nullDocumentWindowID,
   newDocumentPaneID,
   nullDocumentPaneID,
   newDocumentVerticalDividerID,
   nullDocumentVerticalDividerID,
   newDocumentHorizontalDividerID,
   nullDocumentHorizontalDividerID,
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


class SubtypeWindowID subtypeWindowID where
  toWindowID :: subtypeWindowID -> WindowID
  fromWindowID :: WindowID -> subtypeWindowID


newtype BrowserWindowID =
  BrowserWindowID' UUID deriving (Eq, Ord, Binary, Storable, SQLable)


instance SubtypeWindowID BrowserWindowID where
  toWindowID (BrowserWindowID' uuid) = WindowID' uuid
  fromWindowID (WindowID' uuid) = BrowserWindowID' uuid


newtype DocumentWindowID =
  DocumentWindowID' UUID deriving (Eq, Ord, Binary, Storable, SQLable)


instance SubtypeWindowID DocumentWindowID where
  toWindowID (DocumentWindowID' uuid) = WindowID' uuid
  fromWindowID (WindowID' uuid) = DocumentWindowID' uuid


newtype DocumentPaneID =
  DocumentPaneID' UUID deriving (Eq, Ord, Binary, Storable, SQLable)


newtype DocumentVerticalDividerID =
  DocumentVerticalDividerID' UUID deriving (Eq, Ord, Binary, Storable, SQLable)


newtype DocumentHorizontalDividerID =
  DocumentHorizontalDividerID' UUID
  deriving (Eq, Ord, Binary, Storable, SQLable)


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


newDocumentPaneID :: IO DocumentPaneID
newDocumentPaneID = do
  uuid <- V4.uuid
  return $ DocumentPaneID' uuid


nullDocumentPaneID :: DocumentPaneID
nullDocumentPaneID = DocumentPaneID' minBound


newDocumentVerticalDividerID :: IO DocumentVerticalDividerID
newDocumentVerticalDividerID = do
  uuid <- V4.uuid
  return $ DocumentVerticalDividerID' uuid


nullDocumentVerticalDividerID :: DocumentVerticalDividerID
nullDocumentVerticalDividerID = DocumentVerticalDividerID' minBound


newDocumentHorizontalDividerID :: IO DocumentHorizontalDividerID
newDocumentHorizontalDividerID = do
  uuid <- V4.uuid
  return $ DocumentHorizontalDividerID' uuid


nullDocumentHorizontalDividerID :: DocumentHorizontalDividerID
nullDocumentHorizontalDividerID = DocumentHorizontalDividerID' minBound


newInodeID :: IO InodeID
newInodeID = do
  uuid <- V4.uuid
  return $ InodeID' uuid


nullInodeID :: InodeID
nullInodeID = InodeID' minBound
