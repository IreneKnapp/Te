module Te.Types
  (ApplicationState(..),
   FrontEndCallbacks(..),
   ConfirmationDialog(..),
   RecentProject(..),
   Project(..),
   Inode(..),
   InodeInformation(..),
   InodeKind(..),
   Window(..),
   WindowKind(..),
   BrowserWindow(..),
   BrowserItem(..),
   DocumentWindow(..),
   DocumentPane(..),
   DocumentVerticalDivider(..),
   DocumentHorizontalDivider(..),
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
import Te.LowLevel.Identifiers


data ApplicationState =
  ApplicationState {
      applicationStateRecentProjects :: [RecentProject],
      applicationStateProjects :: Map ProjectID Project,
      applicationStateFrontEndCallbacks :: FrontEndCallbacks
    }


data FrontEndCallbacks =
  FrontEndCallbacks {
      frontEndCallbacksException :: String -> String -> IO (),
      frontEndCallbacksConfirm
          :: ConfirmationDialog -> (Word64 -> IO ()) -> IO (),
      frontEndCallbacksNoteRecentProjectsChanged :: IO (),
      frontEndCallbacksNoteDeletedWindow :: Window -> IO (),
      frontEndCallbacksActivateWindow :: Window -> IO (),
      frontEndCallbacksNoteNewBrowserWindow :: BrowserWindow -> IO (),
      frontEndCallbacksNoteBrowserItemsChanged :: BrowserWindow -> IO (),
      frontEndCallbacksEditBrowserItemName :: BrowserItem -> IO (),
      frontEndCallbacksNoteNewDocumentWindow :: DocumentWindow -> IO ()
    }


data ConfirmationDialog =
  ConfirmationDialog {
      confirmationDialogWindow :: Maybe Window,
      confirmationDialogMessage :: String,
      confirmationDialogDetails :: String,
      confirmationDialogDefaultButtonIndex :: Maybe Word64,
      confirmationDialogCancelButtonIndex :: Maybe Word64,
      confirmationDialogButtons :: [String]
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
      projectWindows :: MVar (Map WindowID Window)
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
  deriving (Eq)


data Window =
  Window {
      windowID :: WindowID,
      windowProject :: Project
    }


data WindowKind
  = WindowKindBrowser
  | WindowKindDocument


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


data DocumentWindow =
  DocumentWindow {
      documentWindowID :: DocumentWindowID,
      documentWindowProject :: Project
    }


data DocumentPane =
  DocumentPane {
      documentPaneID :: DocumentPaneID,
      documentPaneDocumentWindow :: DocumentWindow
    }


data DocumentVerticalDivider =
  DocumentVerticalDivider {
      documentVerticalDividerID :: DocumentVerticalDividerID,
      documentVerticalDividerDocumentWindow :: DocumentWindow
    }


data DocumentHorizontalDivider =
  DocumentHorizontalDivider {
      documentHorizontalDividerID :: DocumentHorizontalDividerID,
      documentHorizontalDividerDocumentWindow :: DocumentWindow
    }


data DragInformation
  = InodeDragInformation {
        dragInformationAllowedDragOperations :: Set DragOperation,
        inodeDragInformationBrowserWindow :: BrowserWindow,
        inodeDragInformationInodes :: [Inode]
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
