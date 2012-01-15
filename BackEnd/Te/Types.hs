{-# LANGUAGE ExistentialQuantification #-}
module Te.Types
  (ApplicationState(..),
   FrontEndCallbacks(..),
   ConfirmationDialog(..),
   RecentProject(..),
   Project(..),
   Inode(..),
   InodeInformation(..),
   InodeType(..),
   Window(..),
   AnyWindow(..),
   BrowserWindow(..),
   BrowserItem(..),
   DocumentWindow(..),
   DocumentPane(..),
   DocumentVerticalDivider(..),
   DocumentHorizontalDivider(..),
   DividerOrientation(..),
   DragInformation(..),
   DragOperation(..))
  where

import Control.Concurrent.MVar
import Data.Array.Unboxed
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
      frontEndCallbacksGetEmWidth :: IO Double,
      frontEndCallbacksGetLineHeight :: IO Double,
      frontEndCallbacksGetLineNumberEmWidth :: IO Double,
      frontEndCallbacksGetScrollerWidth :: IO Double,
      frontEndCallbacksGetVisibleSize :: IO (Double, Double),
      frontEndCallbacksNoteDeletedWindow :: AnyWindow -> IO (),
      frontEndCallbacksActivateWindow :: AnyWindow -> IO (),
      frontEndCallbacksNoteNewBrowserWindow :: BrowserWindow -> IO (),
      frontEndCallbacksNoteBrowserItemsChanged :: BrowserWindow -> IO (),
      frontEndCallbacksEditBrowserItemName :: BrowserItem -> IO (),
      frontEndCallbacksNoteNewDocumentWindow :: DocumentWindow -> IO (),
      frontEndCallbacksNoteNewDocumentPane
        :: DocumentWindow
        -> DocumentPane
        -> ((Double, Double), (Double, Double))
        -> IO ()
    }


data ConfirmationDialog =
  ConfirmationDialog {
      confirmationDialogWindow :: Maybe AnyWindow,
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
      projectWindows :: MVar (Map WindowID AnyWindow)
    }


data Inode =
  Inode {
      inodeID :: InodeID,
      inodeProject :: Project
    }


data InodeInformation =
  InodeInformation {
      inodeInformationName :: String,
      inodeInformationType :: InodeType,
      inodeInformationSize :: Maybe ByteSize,
      inodeInformationCreationTimestamp :: Timestamp,
      inodeInformationModificationTimestamp :: Timestamp
    }


data InodeType
  = DirectoryInodeType
  | HaskellInodeType
  deriving (Eq)


class Window window where
  windowID :: window -> WindowID
  windowProject :: window -> Project
  getWindowTitle :: window -> IO String
  getWindowTitleIcon :: window -> IO String
  browserWindowDo :: window -> a -> (BrowserWindow -> IO a) -> IO a
  documentWindowDo :: window -> a -> (DocumentWindow -> IO a) -> IO a
data AnyWindow = forall window . Window window => AnyWindow window
instance Window AnyWindow where
  windowID (AnyWindow window) = windowID window
  windowProject (AnyWindow window) = windowProject window
  getWindowTitle (AnyWindow window) = getWindowTitle window
  getWindowTitleIcon (AnyWindow window) = getWindowTitleIcon window
  browserWindowDo (AnyWindow window) = browserWindowDo window
  documentWindowDo (AnyWindow window) = documentWindowDo window


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
      documentWindowProject :: Project,
      documentWindowPanes :: MVar (Map DocumentPaneID DocumentPane),
      documentWindowVerticalDividers
        :: MVar (Map DocumentVerticalDividerID DocumentVerticalDivider),
      documentWindowHorizontalDividers
        :: MVar (Map DocumentHorizontalDividerID DocumentHorizontalDivider),
      documentWindowGrid :: MVar (Array (Int, Int) DocumentPane),
      documentWindowColumnWidths :: MVar (UArray Int Int),
      documentWindowRowHeights :: MVar (UArray Int Int)
    }


data DocumentPane =
  DocumentPane {
      documentPaneID :: DocumentPaneID,
      documentPaneWindow :: DocumentWindow,
      documentPaneRowTop :: Int,
      documentPaneColumnLeft :: Int,
      documentPaneRowSpan :: Int,
      documentPaneColumnSpan :: Int,
      documentPaneDividerUp :: Maybe DocumentHorizontalDivider,
      documentPaneDividerDown :: Maybe DocumentHorizontalDivider,
      documentPaneDividerLeft :: Maybe DocumentVerticalDivider,
      documentPaneDividerRight :: Maybe DocumentVerticalDivider
    }


data DocumentVerticalDivider =
  DocumentVerticalDivider {
      documentVerticalDividerID :: DocumentVerticalDividerID,
      documentVerticalDividerWindow :: DocumentWindow,
      documentVerticalDividerRowTop :: Int,
      documentVerticalDividerRowSpan :: Int,
      documentVerticalDividerColumnRight :: Int,
      documentVerticalDividerPanesLeft :: [DocumentPane],
      documentVerticalDividerPanesRight :: [DocumentPane]
    }


data DocumentHorizontalDivider =
  DocumentHorizontalDivider {
      documentHorizontalDividerID :: DocumentHorizontalDividerID,
      documentHorizontalDividerWindow :: DocumentWindow,
      documentHorizontalDividerColumnLeft :: Int,
      documentHorizontalDividerColumnSpan :: Int,
      documentHorizontalDividerRowDown :: Int,
      documentHorizontalDividerPanesUp :: [DocumentPane],
      documentHorizontalDividerPanesDown :: [DocumentPane]
    }


data DividerOrientation
  = HorizontalOrientation
  | VerticalOrientation
  deriving (Eq)


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
