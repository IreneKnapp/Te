module Te.Types
  (ApplicationState(..),
   FrontEndCallbacks(..),
   RecentProject(..),
   Project(..),
   Inode(..),
   InodeInformation(..),
   InodeKind(..),
   BrowserWindow(..),
   BrowserItem(..),
   DragInformation(..),
   DragOperation(..))
  where

import Control.Concurrent.MVar
import Data.Map (Map)
import Data.Set (Set)
import Data.Word
import Database.SQLite3 (Database)

import Data.Bitfield
import Data.ByteSize
import Data.Timestamp
import Te.Identifiers


data ApplicationState =
  ApplicationState {
      applicationStateRecentProjects :: [RecentProject],
      applicationStateProjects :: Map ProjectID Project,
      applicationStateFrontEndCallbacks :: FrontEndCallbacks
    }


data FrontEndCallbacks =
  FrontEndCallbacks {
      frontEndCallbacksException :: String -> String -> IO (),
      frontEndCallbacksNoteRecentProjectsChanged :: IO (),
      frontEndCallbacksNoteNewBrowserWindow :: BrowserWindow -> IO (),
      frontEndCallbacksNoteDeletedBrowserWindow :: BrowserWindow -> IO (),
      frontEndCallbacksNoteBrowserItemsChanged :: BrowserWindow -> IO (),
      frontEndCallbacksEditBrowserItemName :: BrowserItem -> IO ()
    }


data RecentProject =
  RecentProject {
      recentProjectID :: ProjectID,
      recentProjectFilePath :: String,
      recentProjectTimestamp :: Timestamp
    }


data Project =
  Project {
      projectID :: ProjectID,
      projectApplicationState :: MVar ApplicationState,
      projectDatabase :: Database,
      projectName :: MVar String,
      projectFilePath :: MVar (Maybe FilePath),
      projectBrowserWindows :: MVar (Map BrowserWindowID BrowserWindow)
    }


data Inode =
  Inode {
      inodeID :: InodeID,
      inodeProject :: Project
    }


data InodeInformation =
  InodeInformation {
      inodeInformationName :: String,
      inodeInformationKind :: InodeKind,
      inodeInformationSize :: Maybe ByteSize,
      inodeInformationCreationTimestamp :: Timestamp,
      inodeInformationModificationTimestamp :: Timestamp
    }


data InodeKind
  = InodeKindDirectory
  | InodeKindHaskell


data BrowserWindow =
  BrowserWindow {
      browserWindowID :: BrowserWindowID,
      browserWindowProject :: Project
    }


data BrowserItem =
  BrowserItem {
      browserItemInode :: Inode,
      browserItemBrowserWindow :: BrowserWindow
    }


data DragInformation
  = BrowserItemDragInformation {
        dragInformationAllowedDragOperations :: Set DragOperation,
        browserItemDragInformationBrowserItems :: [BrowserItem]
      }
  | ExternalFileDragInformation {
        dragInformationAllowedDragOperations :: Set DragOperation,
        externalFileDragInformationFilePaths :: [FilePath]
      }


data DragOperation
  = DragOperationCopy
  | DragOperationLink
  | DragOperationGeneric
  | DragOperationMove
  | DragOperationDelete
  deriving (Eq, Ord)


instance Bitfieldable DragOperation where
  bitfieldInformation = [(0, DragOperationCopy),
                         (1, DragOperationLink),
                         (2, DragOperationGeneric),
                         (3, DragOperationMove),
                         (4, DragOperationDelete)]
