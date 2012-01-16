{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module Te.LowLevel.ForeignInterface () where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.UUID
import Data.Word
import Foreign
import Foreign.C

import Data.Bitfield
import Data.ByteSize (ByteSize(..))
import Data.Timestamp (Timestamp(..))
import Te
import Te.LowLevel.Exceptions
import Te.LowLevel.FrontEndCallbacks
import Te.LowLevel.Identifiers
import Te.Types


foreign import ccall "dynamic" mkVoidCallback
    :: FunPtr (IO ()) -> IO ()
foreign import ccall "dynamic" mkDoubleCallback
    :: FunPtr (IO Double) -> IO Double
foreign import ccall "dynamic" mkIntPtrQuadCallback
    :: FunPtr (Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> IO ())
    -> (Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> IO ())
foreign import ccall "dynamic" mkVoidStringStringCallback
    :: FunPtr (CString -> CString -> IO ()) -> (CString -> CString -> IO ())
foreign import ccall "dynamic" mkVoidWindowIDPtrCallback
    :: FunPtr (Ptr WindowID -> IO ()) -> (Ptr WindowID -> IO ())
foreign import ccall "dynamic" mkVoidBrowserWindowIDPtrCallback
    :: FunPtr (Ptr BrowserWindowID -> IO ()) -> (Ptr BrowserWindowID -> IO ())
foreign import ccall "dynamic" mkVoidBrowserWindowIDPtrInodeIDPtrCallback
    :: FunPtr (Ptr BrowserWindowID -> Ptr InodeID -> IO ())
    -> (Ptr BrowserWindowID -> Ptr InodeID -> IO ())
foreign import ccall "dynamic" mkVoidDocumentWindowIDPtrRectangleCallback
    :: FunPtr (Ptr DocumentWindowID
               -> Int64 -> Int64 -> Int64 -> Int64
               -> IO ())
    -> (Ptr DocumentWindowID
        -> Int64 -> Int64 -> Int64 -> Int64
        -> IO ())
foreign import ccall "dynamic" mkIntConfirmationDialogCompletionHandlerCallback
    :: FunPtr (StablePtr ConfirmationDialog
               -> (FunPtr (Word64 -> IO ()))
               -> IO ())
    -> (StablePtr ConfirmationDialog
        -> FunPtr (Word64 -> IO ())
        -> IO ())
foreign import ccall "dynamic"
    mkVoidDocumentWindowIDPtrDocumentPaneIDPtrRectangleCallback
    :: FunPtr (Ptr DocumentWindowID
               -> Ptr DocumentPaneID
               -> Int64 -> Int64 -> Int64 -> Int64
               -> IO ())
    -> (Ptr DocumentWindowID
        -> Ptr DocumentPaneID
        -> Int64 -> Int64 -> Int64 -> Int64
        -> IO ())


foreign import ccall "wrapper" wrapCompletionHandler
    :: (Word64 -> IO ())
    -> IO (FunPtr (Word64 -> IO ()))


foreign export ccall "teFrontEndInternalFailure"
                      foreignFrontEndInternalFailure
    :: StablePtr (MVar ApplicationState) -> CString -> Word64 -> IO ()
foreign export ccall "teStringFree"
                     foreignStringFree
    :: CString -> IO ()
foreign export ccall "teVersionString"
                     foreignVersionString
    :: IO CString
foreign export ccall "teTimestampShow"
                     foreignTimestampShow
    :: Word64 -> IO CString
foreign export ccall "teByteSizePlaceholderString"
                     foreignByteSizePlaceholderString
    :: IO CString
foreign export ccall "teByteSizeShow"
                     foreignByteSizeShow
    :: Word64 -> IO CString
foreign export ccall "teUUIDHash"
                     foreignUUIDHash
    :: Ptr UUID -> IO Word64
foreign export ccall "teUUIDEqual"
                     foreignUUIDEqual
    :: Ptr UUID -> Ptr UUID -> IO CInt
foreign export ccall "teUUIDShow"
                     foreignUUIDShow
    :: Ptr UUID -> IO CString
foreign export ccall "teCompletionHandlerFree"
                     foreignCompletionHandlerFree
    :: FunPtr (Word64 -> IO ()) -> IO ()
foreign export ccall "teApplicationInit"
                     foreignApplicationInit
    :: FunPtr (CString -> CString -> IO ())
    -> FunPtr (StablePtr ConfirmationDialog
               -> FunPtr (Word64 -> IO ())
               -> IO ())
    -> FunPtr (IO ())
    -> FunPtr (IO Double)
    -> FunPtr (IO Double)
    -> FunPtr (IO Double)
    -> FunPtr (IO Double)
    -> FunPtr (Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> IO ())
    -> FunPtr (Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> IO ())
    -> FunPtr (Ptr WindowID -> IO ())
    -> FunPtr (Ptr WindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> Ptr InodeID -> IO ())
    -> FunPtr (Ptr DocumentWindowID
               -> Int64 -> Int64 -> Int64 -> Int64
               -> IO ())
    -> FunPtr (Ptr DocumentWindowID -> Ptr DocumentPaneID
               -> Int64 -> Int64 -> Int64 -> Int64
               -> IO ())
    -> IO (StablePtr (MVar ApplicationState))
foreign export ccall "teApplicationExit"
                     foreignApplicationExit
    :: StablePtr (MVar ApplicationState) -> IO ()
foreign export ccall "teApplicationRecentProjectCount"
                     foreignApplicationRecentProjectCount
    :: StablePtr (MVar ApplicationState) -> IO Word64
foreign export ccall "teApplicationRecentProjectName"
                     foreignApplicationRecentProjectName
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO CString
foreign export ccall "teApplicationRecentProjectTimestamp"
                     foreignApplicationRecentProjectTimestamp
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO Word64
foreign export ccall "teApplicationNewProject"
                     foreignApplicationNewProject
    :: StablePtr (MVar ApplicationState) -> IO ()
foreign export ccall "teApplicationOpenProject"
                     foreignApplicationOpenProject
    :: StablePtr (MVar ApplicationState) -> CString -> IO ()
foreign export ccall "teApplicationOpenRecentProject"
                     foreignApplicationOpenRecentProject
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO ()
foreign export ccall "teWindowClose"
                     foreignWindowClose
    :: StablePtr (MVar ApplicationState) -> Ptr WindowID -> IO ()
foreign export ccall "teWindowTitle"
                     foreignWindowTitle
    :: StablePtr (MVar ApplicationState) -> Ptr WindowID -> IO CString
foreign export ccall "teWindowTitleIcon"
                     foreignWindowTitleIcon
    :: StablePtr (MVar ApplicationState) -> Ptr WindowID -> IO CString
foreign export ccall "teBrowserWindowRoot"
                     foreignBrowserWindowRoot
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreign export ccall "teBrowserItemNewFolderInside"
                     foreignBrowserItemNewFolderInside
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreign export ccall "teBrowserItemNewFileInside"
                     foreignBrowserItemNewFileInside
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreign export ccall "teBrowserItemExpanded"
                     foreignBrowserItemExpanded
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreign export ccall "teBrowserItemSetExpanded"
                     foreignBrowserItemSetExpanded
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> Word64
    -> IO ()
foreign export ccall "teDocumentWindowMinimumSize"
                     foreignDocumentWindowMinimumSize
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> Ptr Int64
    -> Ptr Int64
    -> IO ()
foreign export ccall "teDocumentWindowDesiredSize"
                     foreignDocumentWindowDesiredSize
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> Ptr Int64
    -> Ptr Int64
    -> IO ()
foreign export ccall "teDocumentWindowAdjustPanes"
                     foreignDocumentWindowAdjustPanes
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> IO ()
foreign export ccall "teDocumentWindowPanes"
                     foreignDocumentWindowPanes
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> IO (StablePtr [DocumentPane])
foreign export ccall "teDocumentPanesCount"
                     foreignDocumentPanesCount
    :: StablePtr [DocumentPane]
    -> IO Word64
foreign export ccall "teDocumentPanesItem"
                     foreignDocumentPanesItem
    :: StablePtr [DocumentPane]
    -> Word64
    -> Ptr DocumentPaneID
    -> IO ()
foreign export ccall "teDocumentWindowHorizontalDividers"
                     foreignDocumentWindowHorizontalDividers
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> IO (StablePtr [DocumentHorizontalDivider])
foreign export ccall "teDocumentHorizontalDividersCount"
                     foreignDocumentHorizontalDividersCount
    :: StablePtr [DocumentHorizontalDivider]
    -> IO Word64
foreign export ccall "teDocumentHorizontalDividersItem"
                     foreignDocumentHorizontalDividersItem
    :: StablePtr [DocumentHorizontalDivider]
    -> Word64
    -> Ptr DocumentHorizontalDividerID
    -> IO ()
foreign export ccall "teDocumentWindowVerticalDividers"
                     foreignDocumentWindowVerticalDividers
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> IO (StablePtr [DocumentVerticalDivider])
foreign export ccall "teDocumentVerticalDividersCount"
                     foreignDocumentVerticalDividersCount
    :: StablePtr [DocumentVerticalDivider]
    -> IO Word64
foreign export ccall "teDocumentVerticalDividersItem"
                     foreignDocumentVerticalDividersItem
    :: StablePtr [DocumentVerticalDivider]
    -> Word64
    -> Ptr DocumentVerticalDividerID
    -> IO ()
foreign export ccall "teDocumentPaneLeftMarginWidth"
                     foreignDocumentPaneLeftMarginWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreign export ccall "teDocumentPaneRightMarginWidth"
                     foreignDocumentPaneRightMarginWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreign export ccall "teDocumentPaneBottomMarginWidth"
                     foreignDocumentPaneBottomMarginWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreign export ccall "teDocumentPaneLeftPaddingWidth"
                     foreignDocumentPaneLeftPaddingWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreign export ccall "teDocumentPaneRightPaddingWidth"
                     foreignDocumentPaneRightPaddingWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreign export ccall "teDocumentPaneLineNumberPaddingWidth"
                     foreignDocumentPaneLineNumberPaddingWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreign export ccall "teDocumentPaneLineNumberAreaWidth"
                     foreignDocumentPaneLineNumberAreaWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreign export ccall "teDocumentPaneMinimumSize"
                     foreignDocumentPaneMinimumSize
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> Ptr Int64
    -> Ptr Int64
    -> IO ()
foreign export ccall "teDocumentPaneDesiredSize"
                     foreignDocumentPaneDesiredSize
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> Ptr Int64
    -> Ptr Int64
    -> IO ()
foreign export ccall "teDocumentPaneCaption"
                     foreignDocumentPaneCaption
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO CString
foreign export ccall "teDocumentPaneSizeReport"
                     foreignDocumentPaneSizeReport
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO CString
foreign export ccall "teDocumentHorizontalDividerFrame"
                     foreignDocumentHorizontalDividerFrame
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentHorizontalDividerID
    -> Ptr Int64
    -> Ptr Int64
    -> Ptr Int64
    -> Ptr Int64
    -> IO ()
foreign export ccall "teInodeParent"
                     foreignInodeParent
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> Ptr InodeID
    -> IO Word64
foreign export ccall "teInodeExpandable"
                     foreignInodeExpandable
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreign export ccall "teInodeChildCount"
                     foreignInodeChildCount
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreign export ccall "teInodeChild"
                     foreignInodeChild
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> Word64
    -> Ptr InodeID
    -> IO ()
foreign export ccall "teInodeName"
                     foreignInodeName
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO CString
foreign export ccall "teInodeKind"
                     foreignInodeKind
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO CString
foreign export ccall "teInodeSize"
                     foreignInodeSize
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> Ptr Word64
    -> IO Word64
foreign export ccall "teInodeCreationTimestamp"
                     foreignInodeCreationTimestamp
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreign export ccall "teInodeModificationTimestamp"
                     foreignInodeModificationTimestamp
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreign export ccall "teInodeIcon"
                     foreignInodeIcon
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO CString
foreign export ccall "teInodeRename"
                     foreignInodeRename
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> CString
    -> IO ()
foreign export ccall "teInodeListDelete"
                     foreignInodeListDelete
    :: StablePtr (MVar ApplicationState)
    -> Ptr WindowID
    -> StablePtr [Inode]
    -> IO ()
foreign export ccall "teInodeOpen"
                     foreignInodeOpen
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreign export ccall "teInodeValidateDrop"
                     foreignInodeValidateDrop
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> StablePtr DragInformation
    -> Ptr InodeID
    -> Ptr Word64
    -> Ptr Word64
    -> IO Word64
foreign export ccall "teInodeAcceptDrop"
                     foreignInodeAcceptDrop
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> StablePtr DragInformation
    -> IO Word64
foreign export ccall "teInodeListNew"
                     foreignInodeListNew
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Word64
    -> Ptr InodeID
    -> IO (StablePtr [Inode])
foreign export ccall "teInodeListFree"
                     foreignInodeListFree
    :: StablePtr [Inode]
    -> IO ()
foreign export ccall "teInodeListKernel"
                     foreignInodeListKernel
    :: StablePtr [Inode]
    -> IO (StablePtr [Inode])
foreign export ccall
    "teBrowserWindowDraggingSourceIntraApplicationOperations"
    foreignBrowserWindowDraggingSourceIntraApplicationOperations
    :: Bitfield DragOperation
foreign export ccall
    "teBrowserWindowDraggingSourceInterApplicationOperations"
    foreignBrowserWindowDraggingSourceInterApplicationOperations
    :: Bitfield DragOperation
foreign export ccall "teDragOperationCopy"
                     foreignDragOperationCopy
    :: Word64
foreign export ccall "teDragOperationLink"
                     foreignDragOperationLink
    :: Word64
foreign export ccall "teDragOperationGeneric"
                     foreignDragOperationGeneric
    :: Word64
foreign export ccall "teDragOperationMove"
                     foreignDragOperationMove
    :: Word64
foreign export ccall "teDragOperationDelete"
                     foreignDragOperationDelete
    :: Word64
foreign export ccall "teInodeDragInformationNew"
                     foreignInodeDragInformationNew
    :: StablePtr (MVar ApplicationState)
    -> Bitfield DragOperation
    -> Ptr BrowserWindowID
    -> StablePtr [Inode]
    -> IO (StablePtr DragInformation)
foreign export ccall "teExternalFileDragInformationNew"
                     foreignExternalFileDragInformationNew
    :: StablePtr (MVar ApplicationState)
    -> Bitfield DragOperation
    -> Word64
    -> Ptr CString
    -> IO (StablePtr DragInformation)
foreign export ccall "teDragInformationFree"
                     foreignDragInformationFree
    :: StablePtr DragInformation
    -> IO ()
foreign export ccall "teDragInformationAllowedDragOperations"
                     foreignDragInformationAllowedDragOperations
    :: StablePtr DragInformation
    -> IO (Bitfield DragOperation)
foreign export ccall "teDragInformationBrowserWindow"
                     foreignDragInformationBrowserWindow
    :: StablePtr DragInformation
    -> Ptr BrowserWindowID
    -> IO ()
foreign export ccall "teDragInformationInodeCount"
                     foreignDragInformationInodeCount
    :: StablePtr DragInformation
    -> IO Word64
foreign export ccall "teDragInformationInode"
                     foreignDragInformationInode
    :: StablePtr DragInformation
    -> Word64
    -> Ptr InodeID
    -> IO ()
foreign export ccall "teDragInformationFilePathCount"
                     foreignDragInformationFilePathCount
    :: StablePtr DragInformation
    -> IO Word64
foreign export ccall "teDragInformationFilePath"
                     foreignDragInformationFilePath
    :: StablePtr DragInformation
    -> Word64
    -> IO CString
foreign export ccall "teConfirmationDialogNew"
                     foreignConfirmationDialogNew
    :: StablePtr (MVar ApplicationState)
    -> Ptr WindowID
    -> CString
    -> CString
    -> Word64
    -> Word64
    -> Word64
    -> Ptr CString
    -> IO (StablePtr ConfirmationDialog)
foreign export ccall "teConfirmationDialogFree"
                     foreignConfirmationDialogFree
    :: StablePtr ConfirmationDialog
    -> IO ()
foreign export ccall "teConfirmationDialogWindow"
                     foreignConfirmationDialogWindow
    :: StablePtr ConfirmationDialog
    -> Ptr WindowID
    -> IO ()
foreign export ccall "teConfirmationDialogMessage"
                     foreignConfirmationDialogMessage
    :: StablePtr ConfirmationDialog
    -> IO CString
foreign export ccall "teConfirmationDialogDetails"
                     foreignConfirmationDialogDetails
    :: StablePtr ConfirmationDialog
    -> IO CString
foreign export ccall "teConfirmationDialogDefaultButtonIndex"
                     foreignConfirmationDialogDefaultButtonIndex
    :: StablePtr ConfirmationDialog
    -> IO Word64
foreign export ccall "teConfirmationDialogCancelButtonIndex"
                     foreignConfirmationDialogCancelButtonIndex
    :: StablePtr ConfirmationDialog
    -> IO Word64
foreign export ccall "teConfirmationDialogButtonCount"
                     foreignConfirmationDialogButtonCount
    :: StablePtr ConfirmationDialog
    -> IO Word64
foreign export ccall "teConfirmationDialogButton"
                     foreignConfirmationDialogButton
    :: StablePtr ConfirmationDialog
    -> Word64
    -> IO CString


outOfBandWord64 :: Word64
outOfBandWord64 = 0xFFFFFFFFFFFFFFFF


foreignFrontEndInternalFailure
    :: StablePtr (MVar ApplicationState) -> CString -> Word64 -> IO ()
foreignFrontEndInternalFailure applicationStateMVarStablePtr
                               filenameCString
                               lineNumber = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  filename <- peekCString filenameCString
  frontEndInternalFailure applicationStateMVar filename lineNumber


stringNew :: String -> IO CString
stringNew string = do
  let byteString = UTF8.fromString string
      bufferLength = 1 + BS.length byteString
  cString <- mallocArray bufferLength
  mapM_ (\index -> do
           let byte = BS.index byteString index
               element = advancePtr cString index
           poke element byte)
        [0 .. bufferLength - 2]
  poke (advancePtr cString $ bufferLength - 1) 0x00
  return $ castPtr cString


foreignStringFree :: CString -> IO ()
foreignStringFree cString = free cString


foreignVersionString :: IO CString
foreignVersionString = do
  stringNew versionString


foreignTimestampShow :: Word64 -> IO CString
foreignTimestampShow timestamp = do
  result <- timestampShow $ Timestamp timestamp
  stringNew result


foreignByteSizePlaceholderString :: IO CString
foreignByteSizePlaceholderString = do
  result <- byteSizePlaceholderString
  stringNew result


foreignByteSizeShow :: Word64 -> IO CString
foreignByteSizeShow byteSize = do
  result <- byteSizeShow $ ByteSize byteSize
  stringNew result


foreignUUIDHash :: Ptr UUID -> IO Word64
foreignUUIDHash uuidPtr = do
  uuid <- peek uuidPtr
  uuidHash uuid


foreignUUIDEqual :: Ptr UUID -> Ptr UUID -> IO CInt
foreignUUIDEqual uuidPtrA uuidPtrB = do
  uuidA <- peek uuidPtrA
  uuidB <- peek uuidPtrB
  result <- uuidEqual uuidA uuidB
  if result
    then return 1
    else return 0


foreignUUIDShow :: Ptr UUID -> IO CString
foreignUUIDShow uuidPtr = do
  uuid <- peek uuidPtr
  string <- uuidShow uuid
  stringNew string


foreignCompletionHandlerFree :: FunPtr (Word64 -> IO ()) -> IO ()
foreignCompletionHandlerFree completionHandler = do
  freeHaskellFunPtr completionHandler


wrapVoidCallback
    :: FunPtr (IO ())
    -> (IO ())
wrapVoidCallback foreignCallback =
  let lowLevelCallback = mkVoidCallback foreignCallback
      highLevelCallback = do
        lowLevelCallback
  in highLevelCallback


wrapDoubleCallback
    :: FunPtr (IO Double)
    -> (IO Double)
wrapDoubleCallback foreignCallback =
  let lowLevelCallback = mkDoubleCallback foreignCallback
      highLevelCallback = do
        lowLevelCallback
  in highLevelCallback


wrapRectangleCallback
    :: FunPtr (Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> IO ())
    -> (IO ((Int64, Int64), (Int64, Int64)))
wrapRectangleCallback foreignCallback =
  let lowLevelCallback = mkIntPtrQuadCallback foreignCallback
      highLevelCallback = do
        alloca $ \leftPtr -> do
          alloca $ \topPtr -> do
            alloca $ \widthPtr -> do
              alloca $ \heightPtr -> do
                poke leftPtr 0
                poke topPtr 0
                poke widthPtr 0
                poke heightPtr 0
                lowLevelCallback leftPtr topPtr widthPtr heightPtr
                left <- peek leftPtr
                top <- peek topPtr
                width <- peek widthPtr
                height <- peek heightPtr
                return ((left, top), (width, height))
  in highLevelCallback


wrapRectangleRectangleCallback
    :: FunPtr (Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> IO ())
    -> (((Int64, Int64), (Int64, Int64))
        -> IO ((Int64, Int64), (Int64, Int64)))
wrapRectangleRectangleCallback foreignCallback =
  let lowLevelCallback = mkIntPtrQuadCallback foreignCallback
      highLevelCallback ((left, top), (width, height)) = do
        alloca $ \leftPtr -> do
          alloca $ \topPtr -> do
            alloca $ \widthPtr -> do
              alloca $ \heightPtr -> do
                poke leftPtr left
                poke topPtr top
                poke widthPtr width
                poke heightPtr height
                lowLevelCallback leftPtr topPtr widthPtr heightPtr
                left' <- peek leftPtr
                top' <- peek topPtr
                width' <- peek widthPtr
                height' <- peek heightPtr
                return ((left', top'), (width', height'))
  in highLevelCallback


wrapVoidStringStringCallback
    :: FunPtr (CString -> CString -> IO ())
    -> (String -> String -> IO ())
wrapVoidStringStringCallback foreignCallback =
  let lowLevelCallback = mkVoidStringStringCallback foreignCallback
      highLevelCallback stringA stringB = do
        withCString stringA
                    (\cStringA -> do
                       withCString stringB
                                   (\cStringB -> do
                                      lowLevelCallback cStringA cStringB))
  in highLevelCallback


wrapVoidWindowCallback
    :: FunPtr (Ptr WindowID -> IO ())
    -> (AnyWindow -> IO ())
wrapVoidWindowCallback foreignCallback =
  let lowLevelCallback = mkVoidWindowIDPtrCallback foreignCallback
      highLevelCallback window = do
        alloca (\windowIDPtr -> do
                  poke windowIDPtr $ windowID window
                  lowLevelCallback windowIDPtr)
  in highLevelCallback


wrapVoidBrowserWindowCallback
    :: FunPtr (Ptr BrowserWindowID -> IO ())
    -> (BrowserWindow -> IO ())
wrapVoidBrowserWindowCallback foreignCallback =
  let lowLevelCallback = mkVoidBrowserWindowIDPtrCallback foreignCallback
      highLevelCallback browserWindow = do
        alloca (\browserWindowIDPtr -> do
                  poke browserWindowIDPtr $ browserWindowID browserWindow
                  lowLevelCallback browserWindowIDPtr)
  in highLevelCallback


wrapVoidBrowserItemCallback
    :: FunPtr (Ptr BrowserWindowID -> Ptr InodeID -> IO ())
    -> (BrowserItem -> IO ())
wrapVoidBrowserItemCallback foreignCallback =
  let lowLevelCallback =
        mkVoidBrowserWindowIDPtrInodeIDPtrCallback foreignCallback
      highLevelCallback browserItem = do
        alloca (\browserWindowIDPtr -> do
                  alloca (\inodeIDPtr -> do
                            let inode = browserItemInode browserItem
                                browserWindow =
                                  browserItemBrowserWindow browserItem
                            poke browserWindowIDPtr
                                 $ browserWindowID browserWindow
                            poke inodeIDPtr $ inodeID inode
                            lowLevelCallback browserWindowIDPtr inodeIDPtr))
  in highLevelCallback


wrapVoidConfirmationDialogCompletionHandlerCallback
    :: FunPtr (StablePtr ConfirmationDialog
               -> FunPtr (Word64 -> IO ())
               -> IO ())
    -> (ConfirmationDialog -> (Word64 -> IO ()) -> IO ())
wrapVoidConfirmationDialogCompletionHandlerCallback foreignCallback =
  let lowLevelCallback =
        mkIntConfirmationDialogCompletionHandlerCallback foreignCallback
      highLevelCallback confirmationDialog completionHandler = do
        confirmationDialogStablePtr <- newStablePtr confirmationDialog
        foreignCompletionHandler <- wrapCompletionHandler completionHandler
        lowLevelCallback confirmationDialogStablePtr foreignCompletionHandler
        freeStablePtr confirmationDialogStablePtr
  in highLevelCallback


wrapVoidDocumentWindowRectangleCallback
    :: FunPtr (Ptr DocumentWindowID
               -> Int64 -> Int64 -> Int64 -> Int64
               -> IO ())
    -> (DocumentWindow
        -> ((Int64, Int64), (Int64, Int64))
        -> IO ())
wrapVoidDocumentWindowRectangleCallback foreignCallback =
  let lowLevelCallback =
        mkVoidDocumentWindowIDPtrRectangleCallback foreignCallback
      highLevelCallback documentWindow ((left, top), (width, height)) = do
        alloca (\documentWindowIDPtr -> do
                  poke documentWindowIDPtr $ documentWindowID documentWindow
                  lowLevelCallback documentWindowIDPtr left top width height)
  in highLevelCallback


wrapVoidDocumentWindowDocumentPaneRectangleCallback
    :: FunPtr (Ptr DocumentWindowID -> Ptr DocumentPaneID
               -> Int64 -> Int64 -> Int64 -> Int64
               -> IO ())
    -> (DocumentWindow
        -> DocumentPane
        -> ((Int64, Int64), (Int64, Int64))
        -> IO ())
wrapVoidDocumentWindowDocumentPaneRectangleCallback foreignCallback =
  let lowLevelCallback =
        mkVoidDocumentWindowIDPtrDocumentPaneIDPtrRectangleCallback
         foreignCallback
      highLevelCallback documentWindow
                        documentPane
                        ((x, y), (width, height)) = do
        alloca $ \documentWindowIDPtr -> do
          poke documentWindowIDPtr $ documentWindowID documentWindow
          alloca $ \documentPaneIDPtr -> do
            poke documentPaneIDPtr $ documentPaneID documentPane
            lowLevelCallback documentWindowIDPtr
                             documentPaneIDPtr
                             x y width height
  in highLevelCallback


foreignApplicationInit
    :: FunPtr (CString -> CString -> IO ())
    -> FunPtr (StablePtr ConfirmationDialog
               -> FunPtr (Word64 -> IO ())
               -> IO ())
    -> FunPtr (IO ())
    -> FunPtr (IO Double)
    -> FunPtr (IO Double)
    -> FunPtr (IO Double)
    -> FunPtr (IO Double)
    -> FunPtr (Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> IO ())
    -> FunPtr (Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> Ptr Int64 -> IO ())
    -> FunPtr (Ptr WindowID -> IO ())
    -> FunPtr (Ptr WindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> Ptr InodeID -> IO ())
    -> FunPtr (Ptr DocumentWindowID
               -> Int64 -> Int64 -> Int64 -> Int64
               -> IO ())
    -> FunPtr (Ptr DocumentWindowID -> Ptr DocumentPaneID
               -> Int64 -> Int64 -> Int64 -> Int64
               -> IO ())
    -> IO (StablePtr (MVar ApplicationState))
foreignApplicationInit foreignException
                       foreignConfirm
                       foreignNoteRecentProjectsChanged
                       foreignGetEmWidth
                       foreignGetLineHeight
                       foreignGetLineNumberEmWidth
                       foreignGetScrollerWidth
                       foreignGetVisibleFrame
                       foreignGetDocumentContentFromFrame
                       foreignNoteDeletedWindow
                       foreignActivateWindow
                       foreignNoteNewBrowserWindow
                       foreignNoteBrowserItemsChanged
                       foreignEditBrowserItemName
                       foreignNoteNewDocumentWindow
                       foreignNoteNewDocumentPane = do
  let callbackException =
        wrapVoidStringStringCallback foreignException
      callbackConfirm =
        wrapVoidConfirmationDialogCompletionHandlerCallback foreignConfirm
      callbackNoteRecentProjectsChanged =
        wrapVoidCallback foreignNoteRecentProjectsChanged
      callbackGetEmWidth =
        wrapDoubleCallback foreignGetEmWidth
      callbackGetLineHeight =
        wrapDoubleCallback foreignGetLineHeight
      callbackGetLineNumberEmWidth =
        wrapDoubleCallback foreignGetLineNumberEmWidth
      callbackGetScrollerWidth =
        wrapDoubleCallback foreignGetScrollerWidth
      callbackGetVisibleFrame =
        wrapRectangleCallback foreignGetVisibleFrame
      callbackGetDocumentContentFromFrame =
        wrapRectangleRectangleCallback foreignGetDocumentContentFromFrame
      callbackNoteDeletedWindow =
        wrapVoidWindowCallback foreignNoteDeletedWindow
      callbackActivateWindow =
        wrapVoidWindowCallback foreignActivateWindow
      callbackNoteNewBrowserWindow =
        wrapVoidBrowserWindowCallback foreignNoteNewBrowserWindow
      callbackNoteBrowserItemsChanged =
        wrapVoidBrowserWindowCallback foreignNoteBrowserItemsChanged
      callbackEditBrowserItemName =
        wrapVoidBrowserItemCallback foreignEditBrowserItemName
      callbackNoteNewDocumentWindow =
        wrapVoidDocumentWindowRectangleCallback foreignNoteNewDocumentWindow
      callbackNoteNewDocumentPane =
        wrapVoidDocumentWindowDocumentPaneRectangleCallback
         foreignNoteNewDocumentPane
      callbacks = FrontEndCallbacks {
                      frontEndCallbacksException =
                        callbackException,
                      frontEndCallbacksConfirm =
                        callbackConfirm,
                      frontEndCallbacksNoteRecentProjectsChanged =
                        callbackNoteRecentProjectsChanged,
                      frontEndCallbacksGetEmWidth =
                        callbackGetEmWidth,
                      frontEndCallbacksGetLineHeight =
                        callbackGetLineHeight,
                      frontEndCallbacksGetLineNumberEmWidth =
                        callbackGetLineNumberEmWidth,
                      frontEndCallbacksGetScrollerWidth =
                        callbackGetScrollerWidth,
                      frontEndCallbacksGetVisibleFrame =
                        callbackGetVisibleFrame,
                      frontEndCallbacksGetDocumentContentFromFrame =
                        callbackGetDocumentContentFromFrame,
                      frontEndCallbacksNoteDeletedWindow =
                        callbackNoteDeletedWindow,
                      frontEndCallbacksActivateWindow =
                        callbackActivateWindow,
                      frontEndCallbacksNoteNewBrowserWindow =
                        callbackNoteNewBrowserWindow,
                      frontEndCallbacksNoteBrowserItemsChanged =
                        callbackNoteBrowserItemsChanged,
                      frontEndCallbacksEditBrowserItemName =
                        callbackEditBrowserItemName,
                      frontEndCallbacksNoteNewDocumentWindow =
                        callbackNoteNewDocumentWindow,
                      frontEndCallbacksNoteNewDocumentPane =
                        callbackNoteNewDocumentPane
                    }
  applicationStateMVar <- applicationInit callbacks
  newStablePtr applicationStateMVar


foreignApplicationExit :: StablePtr (MVar ApplicationState) -> IO ()
foreignApplicationExit applicationStateMVarStablePtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  applicationExit applicationStateMVar
  freeStablePtr applicationStateMVarStablePtr


foreignApplicationRecentProjectCount
    :: StablePtr (MVar ApplicationState) -> IO Word64
foreignApplicationRecentProjectCount applicationStateMVarStablePtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  applicationRecentProjectCount applicationStateMVar


foreignApplicationRecentProjectName
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO CString
foreignApplicationRecentProjectName applicationStateMVarStablePtr index = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  maybeResult <- applicationRecentProjectName applicationStateMVar index
  case maybeResult of
    Just result -> stringNew result
    Nothing -> return nullPtr


foreignApplicationRecentProjectTimestamp
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO Word64
foreignApplicationRecentProjectTimestamp applicationStateMVarStablePtr
                                         index = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  maybeResult <-
    applicationRecentProjectTimestamp applicationStateMVar index
  case maybeResult of
    Just (Timestamp result) -> return result
    Nothing -> return 0


foreignApplicationNewProject
    :: StablePtr (MVar ApplicationState) -> IO ()
foreignApplicationNewProject applicationStateMVarStablePtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  applicationNewProject applicationStateMVar


foreignApplicationOpenProject
    :: StablePtr (MVar ApplicationState) -> CString -> IO ()
foreignApplicationOpenProject applicationStateMVarStablePtr
                              filePathCString = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  filePath <- peekCString filePathCString
  applicationOpenProject applicationStateMVar filePath


foreignApplicationOpenRecentProject
    :: StablePtr (MVar ApplicationState) -> Word64 -> IO ()
foreignApplicationOpenRecentProject applicationStateMVarStablePtr
                                    index = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  applicationOpenRecentProject applicationStateMVar index


foreignWindowClose
    :: StablePtr (MVar ApplicationState) -> Ptr WindowID -> IO ()
foreignWindowClose applicationStateMVarStablePtr
                   windowIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  windowID <- peek windowIDPtr
  maybeWindow <- findWindowByID applicationStateMVar windowID
  case maybeWindow of
    Just window -> do
      windowClose window
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return ()


foreignWindowTitle
    :: StablePtr (MVar ApplicationState) -> Ptr WindowID -> IO CString
foreignWindowTitle applicationStateMVarStablePtr
                   windowIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  windowID <- peek windowIDPtr
  maybeWindow <- findWindowByID applicationStateMVar windowID
  case maybeWindow of
    Just window -> do
      string <- getWindowTitle window
      stringNew string
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return nullPtr


foreignWindowTitleIcon
    :: StablePtr (MVar ApplicationState) -> Ptr WindowID -> IO CString
foreignWindowTitleIcon applicationStateMVarStablePtr
                       windowIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  windowID <- peek windowIDPtr
  maybeWindow <- findWindowByID applicationStateMVar windowID
  case maybeWindow of
    Just window -> do
      string <- getWindowTitleIcon window
      stringNew string
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return nullPtr


findWindowByID
    :: MVar ApplicationState
    -> WindowID
    -> IO (Maybe AnyWindow)
findWindowByID applicationStateMVar windowID' = do
  applicationState <- readMVar applicationStateMVar
  foldM (\maybeWindow project -> do
           case maybeWindow of
             Just _ -> return maybeWindow
             Nothing -> do
               windows <- readMVar $ projectWindows project
               return $ Map.lookup windowID' windows)
        Nothing
        $ Map.elems $ applicationStateProjects applicationState


findBrowserWindowByID
    :: MVar ApplicationState
    -> BrowserWindowID
    -> IO (Maybe BrowserWindow)
findBrowserWindowByID applicationStateMVar
                      browserWindowID' = do
  maybeWindow <- findWindowByID applicationStateMVar
                                $ toWindowID browserWindowID'
  case maybeWindow of
    Just window -> browserWindowDo window Nothing (return . Just)
    Nothing -> return Nothing


findDocumentWindowByID
    :: MVar ApplicationState
    -> DocumentWindowID
    -> IO (Maybe DocumentWindow)
findDocumentWindowByID applicationStateMVar
                       documentWindowID' = do
  maybeWindow <- findWindowByID applicationStateMVar
                                $ toWindowID documentWindowID'
  case maybeWindow of
    Just window -> documentWindowDo window Nothing (return . Just)
    Nothing -> return Nothing


findMaybeWindowFromIDPtr
    :: MVar ApplicationState
    -> Ptr WindowID
    -> IO (Maybe AnyWindow)
findMaybeWindowFromIDPtr applicationStateMVar
                         windowIDPtr = do
  if windowIDPtr == nullPtr
    then return Nothing
    else do
      windowID <- peek windowIDPtr
      findWindowByID applicationStateMVar windowID


findDocumentPaneByID
    :: MVar ApplicationState
    -> DocumentPaneID
    -> IO (Maybe DocumentPane)
findDocumentPaneByID applicationStateMVar
                     documentPaneID' = do
  applicationState <- readMVar applicationStateMVar
  let visitAllProjects = do
        foldM (\maybeDocumentPane project -> do
                 case maybeDocumentPane of
                   Just _ -> return maybeDocumentPane
                   Nothing -> visitProject project)
              Nothing
              $ Map.elems $ applicationStateProjects applicationState
      visitProject project = do
        windowMap <- readMVar $ projectWindows project
        foldM (\maybeDocumentPane window -> do
                 case maybeDocumentPane of
                   Just _ -> return maybeDocumentPane
                   Nothing -> visitWindow window)
              Nothing
              $ Map.elems windowMap
      visitWindow window = do
        documentWindowDo window Nothing visitDocumentWindow
      visitDocumentWindow documentWindow = do
        panes <- readMVar $ documentWindowPanes documentWindow
        foldM (\maybeDocumentPane foundPane -> do
                 case maybeDocumentPane of
                   Just _ -> return maybeDocumentPane
                   Nothing -> visitPane foundPane)
              Nothing
              $ Map.elems panes
      visitPane foundPane = do
        if documentPaneID foundPane == documentPaneID'
          then return $ Just foundPane
          else return Nothing
  visitAllProjects


findDocumentHorizontalDividerByID
    :: MVar ApplicationState
    -> DocumentHorizontalDividerID
    -> IO (Maybe DocumentHorizontalDivider)
findDocumentHorizontalDividerByID applicationStateMVar
                                  horizontalDividerID' = do
  applicationState <- readMVar applicationStateMVar
  let visitAllProjects = do
        foldM (\maybeDocumentPane project -> do
                 case maybeDocumentPane of
                   Just _ -> return maybeDocumentPane
                   Nothing -> visitProject project)
              Nothing
              $ Map.elems $ applicationStateProjects applicationState
      visitProject project = do
        windowMap <- readMVar $ projectWindows project
        foldM (\maybeDocumentPane window -> do
                 case maybeDocumentPane of
                   Just _ -> return maybeDocumentPane
                   Nothing -> visitWindow window)
              Nothing
              $ Map.elems windowMap
      visitWindow window = do
        documentWindowDo window Nothing visitDocumentWindow
      visitDocumentWindow documentWindow = do
        dividers <- readMVar $ documentWindowHorizontalDividers documentWindow
        foldM (\maybeDivider foundDivider -> do
                 case maybeDivider of
                   Just _ -> return maybeDivider
                   Nothing -> visitDivider foundDivider)
              Nothing
              $ Map.elems dividers
      visitDivider foundDivider = do
        if documentHorizontalDividerID foundDivider == horizontalDividerID'
          then return $ Just foundDivider
          else return Nothing
  visitAllProjects
    

foreignBrowserWindowRoot
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreignBrowserWindowRoot applicationStateMVarStablePtr
                         browserWindowIDPtr
                         inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  inodeID' <- case maybeBrowserWindow of
                Just browserWindow -> do
                  browserItem <- getBrowserWindowRoot browserWindow
                  return $ inodeID $ browserItemInode browserItem
                Nothing -> do
                  exception applicationStateMVar $(internalFailure)
                  return nullInodeID
  poke inodeIDPtr inodeID'


foreignBrowserItemNewFolderInside
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreignBrowserItemNewFolderInside applicationStateMVarStablePtr
                                  browserWindowIDPtr
                                  inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
          browserItem = BrowserItem {
                            browserItemInode = inode,
                            browserItemBrowserWindow = browserWindow
                          }
      browserItemNewFolderInside browserItem
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return ()


foreignBrowserItemNewFileInside
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreignBrowserItemNewFileInside applicationStateMVarStablePtr
                                browserWindowIDPtr
                                inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
          browserItem = BrowserItem {
                            browserItemInode = inode,
                            browserItemBrowserWindow = browserWindow
                          }
      browserItemNewFileInside browserItem
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return ()


foreignBrowserItemExpanded
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreignBrowserItemExpanded applicationStateMVarStablePtr
                           browserWindowIDPtr
                           inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
          browserItem = BrowserItem {
                            browserItemInode = inode,
                            browserItemBrowserWindow = browserWindow
                          }
      result <- browserItemExpanded browserItem
      if result
        then return 1
        else return 0
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignBrowserItemSetExpanded
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> Word64
    -> IO ()
foreignBrowserItemSetExpanded applicationStateMVarStablePtr
                              browserWindowIDPtr
                              inodeIDPtr
                              expanded = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID,
                      inodeProject = project
                    }
          browserItem = BrowserItem {
                            browserItemInode = inode,
                            browserItemBrowserWindow = browserWindow
                          }
          expanded' = case expanded of
                        0 -> False
                        _ -> True
      browserItemSetExpanded browserItem expanded'
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return ()


foreignDocumentWindowMinimumSize
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> Ptr Int64
    -> Ptr Int64
    -> IO ()
foreignDocumentWindowMinimumSize applicationStateMVarStablePtr
                                 documentWindowIDPtr
                                 widthPtr
                                 heightPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentWindowID <- peek documentWindowIDPtr
  maybeDocumentWindow <- findDocumentWindowByID applicationStateMVar
                                                documentWindowID
  case maybeDocumentWindow of
    Just documentWindow -> do
      (width, height) <- getDocumentWindowMinimumSize documentWindow
      poke widthPtr width
      poke heightPtr height
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      poke widthPtr 0
      poke heightPtr 0


foreignDocumentWindowDesiredSize
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> Ptr Int64
    -> Ptr Int64
    -> IO ()
foreignDocumentWindowDesiredSize applicationStateMVarStablePtr
                                 documentWindowIDPtr
                                 widthPtr
                                 heightPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentWindowID <- peek documentWindowIDPtr
  maybeDocumentWindow <- findDocumentWindowByID applicationStateMVar
                                                documentWindowID
  case maybeDocumentWindow of
    Just documentWindow -> do
      (width, height) <- getDocumentWindowDesiredSize documentWindow
      poke widthPtr width
      poke heightPtr height
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      poke widthPtr 0
      poke heightPtr 0


foreignDocumentWindowAdjustPanes
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> IO ()
foreignDocumentWindowAdjustPanes applicationStateMVarStablePtr
                                 documentWindowIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentWindowID <- peek documentWindowIDPtr
  maybeDocumentWindow <- findDocumentWindowByID applicationStateMVar
                                                documentWindowID
  case maybeDocumentWindow of
    Just documentWindow -> do
      documentWindowAdjustPanes documentWindow
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return ()


foreignDocumentWindowPanes
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> IO (StablePtr [DocumentPane])
foreignDocumentWindowPanes applicationStateMVarStablePtr
                           documentWindowIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentWindowID <- peek documentWindowIDPtr
  maybeDocumentWindow <- findDocumentWindowByID applicationStateMVar
                                                documentWindowID
  case maybeDocumentWindow of
    Just documentWindow -> do
      panes <- getDocumentWindowPanes documentWindow
      newStablePtr panes
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return $ castPtrToStablePtr nullPtr


foreignDocumentWindowHorizontalDividers
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> IO (StablePtr [DocumentHorizontalDivider])
foreignDocumentWindowHorizontalDividers applicationStateMVarStablePtr
                                        documentWindowIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentWindowID <- peek documentWindowIDPtr
  maybeDocumentWindow <- findDocumentWindowByID applicationStateMVar
                                                documentWindowID
  case maybeDocumentWindow of
    Just documentWindow -> do
      horizontalDividers <- getDocumentWindowHorizontalDividers documentWindow
      newStablePtr horizontalDividers
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return $ castPtrToStablePtr nullPtr


foreignDocumentWindowVerticalDividers
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentWindowID
    -> IO (StablePtr [DocumentVerticalDivider])
foreignDocumentWindowVerticalDividers applicationStateMVarStablePtr
                                      documentWindowIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentWindowID <- peek documentWindowIDPtr
  maybeDocumentWindow <- findDocumentWindowByID applicationStateMVar
                                                documentWindowID
  case maybeDocumentWindow of
    Just documentWindow -> do
      verticalDividers <- getDocumentWindowVerticalDividers documentWindow
      newStablePtr verticalDividers
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return $ castPtrToStablePtr nullPtr


foreignDocumentPanesCount
    :: StablePtr [DocumentPane]
    -> IO Word64
foreignDocumentPanesCount documentPanesStablePtr = do
  documentPanes <- deRefStablePtr documentPanesStablePtr
  return $ fromIntegral $ length documentPanes


foreignDocumentPanesItem
    :: StablePtr [DocumentPane]
    -> Word64
    -> Ptr DocumentPaneID
    -> IO ()
foreignDocumentPanesItem itemsStablePtr index identifierPtr = do
  items <- deRefStablePtr itemsStablePtr
  let identifier =
        if fromIntegral index < length items
          then let item = items !! fromIntegral index
               in documentPaneID item
          else nullDocumentPaneID
  poke identifierPtr identifier


foreignDocumentHorizontalDividersCount
    :: StablePtr [DocumentHorizontalDivider]
    -> IO Word64
foreignDocumentHorizontalDividersCount
    documentHorizontalDividersStablePtr = do
  documentHorizontalDividers <-
    deRefStablePtr documentHorizontalDividersStablePtr
  return $ fromIntegral $ length documentHorizontalDividers


foreignDocumentHorizontalDividersItem
    :: StablePtr [DocumentHorizontalDivider]
    -> Word64
    -> Ptr DocumentHorizontalDividerID
    -> IO ()
foreignDocumentHorizontalDividersItem itemsStablePtr index identifierPtr = do
  items <- deRefStablePtr itemsStablePtr
  let identifier =
        if fromIntegral index < length items
          then let item = items !! fromIntegral index
               in documentHorizontalDividerID item
          else nullDocumentHorizontalDividerID
  poke identifierPtr identifier


foreignDocumentVerticalDividersCount
    :: StablePtr [DocumentVerticalDivider]
    -> IO Word64
foreignDocumentVerticalDividersCount
    documentVerticalDividersStablePtr = do
  documentVerticalDividers <-
    deRefStablePtr documentVerticalDividersStablePtr
  return $ fromIntegral $ length documentVerticalDividers


foreignDocumentVerticalDividersItem
    :: StablePtr [DocumentVerticalDivider]
    -> Word64
    -> Ptr DocumentVerticalDividerID
    -> IO ()
foreignDocumentVerticalDividersItem itemsStablePtr index identifierPtr = do
  items <- deRefStablePtr itemsStablePtr
  let identifier =
        if fromIntegral index < length items
          then let item = items !! fromIntegral index
               in documentVerticalDividerID item
          else nullDocumentVerticalDividerID
  poke identifierPtr identifier


foreignDocumentPaneLeftMarginWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreignDocumentPaneLeftMarginWidth applicationStateMVarStablePtr
                                   documentPaneIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentPaneID <- peek documentPaneIDPtr
  maybeDocumentPane <- findDocumentPaneByID applicationStateMVar
                                            documentPaneID
  case maybeDocumentPane of
    Just documentPane -> do
      getDocumentPaneLeftMarginWidth documentPane
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignDocumentPaneRightMarginWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreignDocumentPaneRightMarginWidth applicationStateMVarStablePtr
                                    documentPaneIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentPaneID <- peek documentPaneIDPtr
  maybeDocumentPane <- findDocumentPaneByID applicationStateMVar
                                            documentPaneID
  case maybeDocumentPane of
    Just documentPane -> do
      getDocumentPaneRightMarginWidth documentPane
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignDocumentPaneBottomMarginWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreignDocumentPaneBottomMarginWidth applicationStateMVarStablePtr
                                     documentPaneIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentPaneID <- peek documentPaneIDPtr
  maybeDocumentPane <- findDocumentPaneByID applicationStateMVar
                                            documentPaneID
  case maybeDocumentPane of
    Just documentPane -> do
      getDocumentPaneBottomMarginWidth documentPane
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignDocumentPaneLeftPaddingWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreignDocumentPaneLeftPaddingWidth applicationStateMVarStablePtr
                                    documentPaneIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentPaneID <- peek documentPaneIDPtr
  maybeDocumentPane <- findDocumentPaneByID applicationStateMVar
                                            documentPaneID
  case maybeDocumentPane of
    Just documentPane -> do
      getDocumentPaneLeftPaddingWidth documentPane
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignDocumentPaneRightPaddingWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreignDocumentPaneRightPaddingWidth applicationStateMVarStablePtr
                                     documentPaneIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentPaneID <- peek documentPaneIDPtr
  maybeDocumentPane <- findDocumentPaneByID applicationStateMVar
                                            documentPaneID
  case maybeDocumentPane of
    Just documentPane -> do
      getDocumentPaneRightPaddingWidth documentPane
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignDocumentPaneLineNumberPaddingWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreignDocumentPaneLineNumberPaddingWidth applicationStateMVarStablePtr
                                          documentPaneIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentPaneID <- peek documentPaneIDPtr
  maybeDocumentPane <- findDocumentPaneByID applicationStateMVar
                                            documentPaneID
  case maybeDocumentPane of
    Just documentPane -> do
      getDocumentPaneLineNumberPaddingWidth documentPane
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignDocumentPaneLineNumberAreaWidth
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO Int64
foreignDocumentPaneLineNumberAreaWidth applicationStateMVarStablePtr
                                       documentPaneIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentPaneID <- peek documentPaneIDPtr
  maybeDocumentPane <- findDocumentPaneByID applicationStateMVar
                                            documentPaneID
  case maybeDocumentPane of
    Just documentPane -> do
      getDocumentPaneLineNumberAreaWidth documentPane
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignDocumentPaneMinimumSize
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> Ptr Int64
    -> Ptr Int64
    -> IO ()
foreignDocumentPaneMinimumSize applicationStateMVarStablePtr
                               documentPaneIDPtr
                               minimumWidthPtr
                               minimumHeightPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentPaneID <- peek documentPaneIDPtr
  maybeDocumentPane <- findDocumentPaneByID applicationStateMVar
                                            documentPaneID
  case maybeDocumentPane of
    Just documentPane -> do
      (minimumWidth, minimumHeight) <-
        getDocumentPaneMinimumSize documentPane
      poke minimumWidthPtr minimumWidth
      poke minimumHeightPtr minimumHeight
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return ()


foreignDocumentPaneDesiredSize
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> Ptr Int64
    -> Ptr Int64
    -> IO ()
foreignDocumentPaneDesiredSize applicationStateMVarStablePtr
                               documentPaneIDPtr
                               desiredWidthPtr
                               desiredHeightPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentPaneID <- peek documentPaneIDPtr
  maybeDocumentPane <- findDocumentPaneByID applicationStateMVar
                                            documentPaneID
  case maybeDocumentPane of
    Just documentPane -> do
      (desiredWidth, desiredHeight) <-
        getDocumentPaneDesiredSize documentPane
      poke desiredWidthPtr desiredWidth
      poke desiredHeightPtr desiredHeight
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return ()


foreignDocumentPaneCaption
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO CString
foreignDocumentPaneCaption applicationStateMVarStablePtr
                           documentPaneIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentPaneID <- peek documentPaneIDPtr
  maybeDocumentPane <- findDocumentPaneByID applicationStateMVar
                                            documentPaneID
  case maybeDocumentPane of
    Just documentPane -> do
      caption <- getDocumentPaneCaption documentPane
      newCString caption
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return nullPtr


foreignDocumentPaneSizeReport
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentPaneID
    -> IO CString
foreignDocumentPaneSizeReport applicationStateMVarStablePtr
                              documentPaneIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentPaneID <- peek documentPaneIDPtr
  maybeDocumentPane <- findDocumentPaneByID applicationStateMVar
                                            documentPaneID
  case maybeDocumentPane of
    Just documentPane -> do
      sizeReport <- getDocumentPaneCaption documentPane
      newCString sizeReport
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return nullPtr


foreignDocumentHorizontalDividerFrame
    :: StablePtr (MVar ApplicationState)
    -> Ptr DocumentHorizontalDividerID
    -> Ptr Int64
    -> Ptr Int64
    -> Ptr Int64
    -> Ptr Int64
    -> IO ()
foreignDocumentHorizontalDividerFrame applicationStateMVarStablePtr
                                      documentHorizontalDividerIDPtr
                                      leftPtr
                                      topPtr
                                      widthPtr
                                      heightPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  documentHorizontalDividerID <- peek documentHorizontalDividerIDPtr
  maybeDocumentHorizontalDivider <-
    findDocumentHorizontalDividerByID applicationStateMVar
                                      documentHorizontalDividerID
  case maybeDocumentHorizontalDivider of
    Just documentHorizontalDivider -> do
      ((left, top), (width, height)) <-
        getDocumentHorizontalDividerFrame documentHorizontalDivider
      poke leftPtr left
      poke topPtr top
      poke widthPtr width
      poke heightPtr height
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return ()


foreignInodeParent
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> Ptr InodeID
    -> IO Word64
foreignInodeParent applicationStateMVarStablePtr
                   browserWindowIDPtr
                   inodeIDPtr
                   resultInodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      maybeParentInode <- inodeParent inode
      case maybeParentInode of
        Just parentInode -> do
          poke resultInodeIDPtr $ inodeID parentInode
          return 1
        Nothing -> do
          return 0
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignInodeExpandable
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreignInodeExpandable applicationStateMVarStablePtr
                       browserWindowIDPtr
                       inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      result <- inodeExpandable inode
      if result
        then return 1
        else return 0
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignInodeChildCount
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreignInodeChildCount applicationStateMVarStablePtr
                       browserWindowIDPtr
                       inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      inodeChildCount inode
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignInodeChild
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> Word64
    -> Ptr InodeID
    -> IO ()
foreignInodeChild applicationStateMVarStablePtr
                  browserWindowIDPtr
                  inodeIDPtr
                  index
                  inodeChildIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  inodeChildID <-
    case maybeBrowserWindow of
      Just browserWindow -> do
        inodeID' <- peek inodeIDPtr
        let project = browserWindowProject browserWindow
            inode = Inode {
                        inodeID = inodeID',
                        inodeProject = project
                      }
        inodeChild' <- inodeChild inode index
        return $ inodeID inodeChild'
      Nothing -> do
        exception applicationStateMVar $(internalFailure)
        return nullInodeID
  poke inodeChildIDPtr inodeChildID


foreignInodeName
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO CString
foreignInodeName applicationStateMVarStablePtr
                 browserWindowIDPtr
                 inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      string <- inodeName inode
      newCString string
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return nullPtr


foreignInodeKind
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO CString
foreignInodeKind applicationStateMVarStablePtr
                 browserWindowIDPtr
                 inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      string <- inodeKind inode
      newCString string
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return nullPtr


foreignInodeSize
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> Ptr Word64
    -> IO Word64
foreignInodeSize applicationStateMVarStablePtr
                 browserWindowIDPtr
                 inodeIDPtr
                 sizePtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      maybeSize <- inodeSize inode
      case maybeSize of
        Just size -> do
          poke sizePtr $ fromIntegral size
          return 1
        Nothing -> do
          poke sizePtr 0
          return 0
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      poke sizePtr 0
      return 0


foreignInodeCreationTimestamp
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreignInodeCreationTimestamp applicationStateMVarStablePtr
                              browserWindowIDPtr
                              inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      result <- inodeCreationTimestamp inode
      return $ fromIntegral result
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignInodeModificationTimestamp
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreignInodeModificationTimestamp applicationStateMVarStablePtr
                                  browserWindowIDPtr
                                  inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      result <- inodeModificationTimestamp inode
      return $ fromIntegral result
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignInodeIcon
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO CString
foreignInodeIcon applicationStateMVarStablePtr
                 browserWindowIDPtr
                 inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      icon <- inodeIcon inode
      stringNew icon
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return nullPtr


foreignInodeRename
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> CString
    -> IO ()
foreignInodeRename applicationStateMVarStablePtr
                   browserWindowIDPtr
                   inodeIDPtr
                   newNameCString = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      inodeID' <- peek inodeIDPtr
      let project = browserWindowProject browserWindow
          inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      newName <- peekCString newNameCString
      inodeRename inode newName
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return ()


foreignInodeListDelete
    :: StablePtr (MVar ApplicationState)
    -> Ptr WindowID
    -> StablePtr [Inode]
    -> IO ()
foreignInodeListDelete applicationStateMVarStablePtr
                       windowIDPtr
                       inodeListStablePtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  maybeWindow <- findMaybeWindowFromIDPtr applicationStateMVar windowIDPtr
  inodeList <- deRefStablePtr inodeListStablePtr
  inodeListDelete maybeWindow inodeList


foreignInodeOpen
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreignInodeOpen applicationStateMVarStablePtr
                 browserWindowIDPtr
                 inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      let project = browserWindowProject browserWindow
      inodeID' <- peek inodeIDPtr
      let inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      inodeOpen inode
    Nothing -> exception applicationStateMVar $(internalFailure)


foreignInodeValidateDrop
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> StablePtr DragInformation
    -> Ptr InodeID
    -> Ptr Word64
    -> Ptr Word64
    -> IO Word64
foreignInodeValidateDrop applicationStateMVarStablePtr
                         browserWindowIDPtr
                         inodeIDPtr
                         dragInformationStablePtr
                         resultInodeIDPtr
                         resultChildIndexPtr
                         resultDragOperationPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      let project = browserWindowProject browserWindow
      inodeID' <- peek inodeIDPtr
      let inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      dragInformation <- deRefStablePtr dragInformationStablePtr
      result <- inodeValidateDrop inode dragInformation
      case result of
        Nothing -> return 0
        Just (resultInode, Nothing, resultDragOperation) -> do
          poke resultInodeIDPtr $ inodeID resultInode
          poke resultChildIndexPtr 0
          let resultDragOperationBit = valueToBit resultDragOperation
          poke resultDragOperationPtr resultDragOperationBit
          return 1
        Just (resultInode, Just resultChildIndex, resultDragOperation) -> do
          poke resultInodeIDPtr $ inodeID resultInode
          poke resultChildIndexPtr resultChildIndex
          let resultDragOperationBit = valueToBit resultDragOperation
          poke resultDragOperationPtr resultDragOperationBit
          return 2
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignInodeAcceptDrop
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> StablePtr DragInformation
    -> IO Word64
foreignInodeAcceptDrop applicationStateMVarStablePtr
                       browserWindowIDPtr
                       inodeIDPtr
                       dragInformationStablePtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      let project = browserWindowProject browserWindow
      inodeID' <- peek inodeIDPtr
      let inode = Inode {
                      inodeID = inodeID',
                      inodeProject = project
                    }
      dragInformation <- deRefStablePtr dragInformationStablePtr
      result <- inodeAcceptDrop inode dragInformation
      if result
        then return 1
        else return 0
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return 0


foreignInodeListNew
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Word64
    -> Ptr InodeID
    -> IO (StablePtr [Inode])
foreignInodeListNew applicationStateMVarStablePtr
                    browserWindowIDPtr
                    inodeIDCount
                    inodeIDPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      let inodeIDCount' = fromIntegral inodeIDCount
          project = browserWindowProject browserWindow
      inodeIDs <- peekArray inodeIDCount' inodeIDPtr
      let inodes = map (\inodeID' ->
                           Inode {
                               inodeID = inodeID',
                               inodeProject = project
                             })
                       inodeIDs
      newStablePtr inodes
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return $ castPtrToStablePtr nullPtr


foreignInodeListFree
    :: StablePtr [Inode]
    -> IO ()
foreignInodeListFree inodesStablePtr = do
  freeStablePtr inodesStablePtr


foreignInodeListKernel
    :: StablePtr [Inode]
    -> IO (StablePtr [Inode])
foreignInodeListKernel inodesStablePtr = do
  inodes <- deRefStablePtr inodesStablePtr
  inodesKernel' <- inodesKernel inodes
  newStablePtr inodesKernel'


foreignBrowserWindowDraggingSourceIntraApplicationOperations
    :: Bitfield DragOperation
foreignBrowserWindowDraggingSourceIntraApplicationOperations =
  setToBitfield browserWindowDraggingSourceIntraApplicationOperations


foreignBrowserWindowDraggingSourceInterApplicationOperations
    :: Bitfield DragOperation
foreignBrowserWindowDraggingSourceInterApplicationOperations =
  setToBitfield browserWindowDraggingSourceInterApplicationOperations


foreignDragOperationCopy :: Word64
foreignDragOperationCopy = valueToBit DragOperationCopy


foreignDragOperationLink :: Word64
foreignDragOperationLink = valueToBit DragOperationLink


foreignDragOperationGeneric :: Word64
foreignDragOperationGeneric = valueToBit DragOperationGeneric


foreignDragOperationMove :: Word64
foreignDragOperationMove = valueToBit DragOperationMove


foreignDragOperationDelete :: Word64
foreignDragOperationDelete = valueToBit DragOperationDelete


foreignInodeDragInformationNew
    :: StablePtr (MVar ApplicationState)
    -> Bitfield DragOperation
    -> Ptr BrowserWindowID
    -> StablePtr [Inode]
    -> IO (StablePtr DragInformation)
foreignInodeDragInformationNew applicationStateMVarStablePtr
                               allowedDragOperationsBitfield
                               browserWindowIDPtr
                               inodesStablePtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  browserWindowID <- peek browserWindowIDPtr
  maybeBrowserWindow <- findBrowserWindowByID applicationStateMVar
                                              browserWindowID
  case maybeBrowserWindow of
    Just browserWindow -> do
      let allowedDragOperations = bitfieldToSet allowedDragOperationsBitfield
      inodes <- deRefStablePtr inodesStablePtr
      let dragInformation =
            InodeDragInformation {
                dragInformationAllowedDragOperations = allowedDragOperations,
                inodeDragInformationBrowserWindow = browserWindow,
                inodeDragInformationInodes = inodes
              }
      newStablePtr dragInformation
    Nothing -> do
      exception applicationStateMVar $(internalFailure)
      return $ castPtrToStablePtr nullPtr


foreignExternalFileDragInformationNew
    :: StablePtr (MVar ApplicationState)
    -> Bitfield DragOperation
    -> Word64
    -> Ptr CString
    -> IO (StablePtr DragInformation)
foreignExternalFileDragInformationNew applicationStateMVarStablePtr
                                      allowedDragOperationsBitfield
                                      filePathCount
                                      filePathCStringPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  let allowedDragOperations = bitfieldToSet allowedDragOperationsBitfield
      filePathCount' = fromIntegral filePathCount
  filePathCStrings <- peekArray filePathCount' filePathCStringPtr
  filePaths <- mapM peekCString filePathCStrings
  let dragInformation =
        ExternalFileDragInformation {
            dragInformationAllowedDragOperations = allowedDragOperations,
            externalFileDragInformationFilePaths = filePaths
          }
  newStablePtr dragInformation


foreignDragInformationFree
    :: StablePtr DragInformation
    -> IO ()
foreignDragInformationFree dragInformationStablePtr = do
  freeStablePtr dragInformationStablePtr


foreignDragInformationAllowedDragOperations
    :: StablePtr DragInformation
    -> IO (Bitfield DragOperation)
foreignDragInformationAllowedDragOperations dragInformationStablePtr = do
  dragInformation <- deRefStablePtr dragInformationStablePtr
  let allowedDragOperationsSet =
        dragInformationAllowedDragOperations dragInformation
      allowedDragOperationsBitfield = setToBitfield allowedDragOperationsSet
  return allowedDragOperationsBitfield


foreignDragInformationBrowserWindow
    :: StablePtr DragInformation
    -> Ptr BrowserWindowID
    -> IO ()
foreignDragInformationBrowserWindow dragInformationStablePtr
                                    browserWindowIDPtr = do
  dragInformation <- deRefStablePtr dragInformationStablePtr
  case dragInformation of
    InodeDragInformation { } -> do
      let browserWindow = inodeDragInformationBrowserWindow dragInformation
      poke browserWindowIDPtr $ browserWindowID browserWindow
    _ -> return ()


foreignDragInformationInodeCount
    :: StablePtr DragInformation
    -> IO Word64
foreignDragInformationInodeCount dragInformationStablePtr = do
  dragInformation <- deRefStablePtr dragInformationStablePtr
  case dragInformation of
    InodeDragInformation { } -> do
      let inodes = inodeDragInformationInodes dragInformation
      return $ fromIntegral $ length inodes
    _ -> return 0


foreignDragInformationInode
    :: StablePtr DragInformation
    -> Word64
    -> Ptr InodeID
    -> IO ()
foreignDragInformationInode dragInformationStablePtr
                            index
                            inodeIDPtr = do
  dragInformation <- deRefStablePtr dragInformationStablePtr
  case dragInformation of
    InodeDragInformation { } -> do
      let inodes = inodeDragInformationInodes dragInformation
          index' = fromIntegral index
      if index' < length inodes
        then do
          let inode = inodes !! index'
          poke inodeIDPtr $ inodeID inode
        else return ()
    _ -> return ()


foreignDragInformationFilePathCount
    :: StablePtr DragInformation
    -> IO Word64
foreignDragInformationFilePathCount dragInformationStablePtr = do
  dragInformation <- deRefStablePtr dragInformationStablePtr
  case dragInformation of
    ExternalFileDragInformation { } -> do
      let filePaths = externalFileDragInformationFilePaths dragInformation
      return $ fromIntegral $ length $ filePaths
    _ -> return 0


foreignDragInformationFilePath
    :: StablePtr DragInformation
    -> Word64
    -> IO CString
foreignDragInformationFilePath dragInformationStablePtr index = do
  dragInformation <- deRefStablePtr dragInformationStablePtr
  case dragInformation of
    ExternalFileDragInformation { } -> do
      let filePaths = externalFileDragInformationFilePaths dragInformation
          index' = fromIntegral index
      if index' < length filePaths
        then do
          let filePath = filePaths !! index'
          stringNew filePath
        else return nullPtr
    _ -> return nullPtr


foreignConfirmationDialogNew
    :: StablePtr (MVar ApplicationState)
    -> Ptr WindowID
    -> CString
    -> CString
    -> Word64
    -> Word64
    -> Word64
    -> Ptr CString
    -> IO (StablePtr ConfirmationDialog)
foreignConfirmationDialogNew applicationStateMVarStablePtr
                             windowIDPtr
                             messageCString
                             detailsCString
                             defaultButtonIndex
                             cancelButtonIndex
                             buttonCount
                             buttonCStringsPtr = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  maybeWindow <- findMaybeWindowFromIDPtr applicationStateMVar windowIDPtr
  message <- peekCString messageCString
  details <- peekCString detailsCString
  let maybeDefaultButtonIndex =
        if defaultButtonIndex == outOfBandWord64
          then Nothing
          else Just defaultButtonIndex
      maybeCancelButtonIndex =
        if cancelButtonIndex == outOfBandWord64
          then Nothing
          else Just cancelButtonIndex
  buttonCStrings <- peekArray (fromIntegral buttonCount) buttonCStringsPtr
  buttons <- mapM peekCString buttonCStrings
  newStablePtr $ ConfirmationDialog {
                     confirmationDialogWindow = maybeWindow,
                     confirmationDialogMessage = message,
                     confirmationDialogDetails = details,
                     confirmationDialogDefaultButtonIndex =
                       maybeDefaultButtonIndex,
                     confirmationDialogCancelButtonIndex =
                       maybeCancelButtonIndex,
                     confirmationDialogButtons = buttons
                   }


foreignConfirmationDialogFree
    :: StablePtr ConfirmationDialog
    -> IO ()
foreignConfirmationDialogFree confirmationDialogStablePtr = do
  freeStablePtr confirmationDialogStablePtr


foreignConfirmationDialogWindow
    :: StablePtr ConfirmationDialog
    -> Ptr WindowID
    -> IO ()
foreignConfirmationDialogWindow confirmationDialogStablePtr
                                resultWindowIDPtr = do
  confirmationDialog <- deRefStablePtr confirmationDialogStablePtr
  let windowID' =
        case confirmationDialogWindow confirmationDialog of
          Just window -> windowID window
          Nothing -> nullWindowID
  poke resultWindowIDPtr windowID'


foreignConfirmationDialogMessage
    :: StablePtr ConfirmationDialog
    -> IO CString
foreignConfirmationDialogMessage confirmationDialogStablePtr = do
  confirmationDialog <- deRefStablePtr confirmationDialogStablePtr
  newCString $ confirmationDialogMessage confirmationDialog


foreignConfirmationDialogDetails
    :: StablePtr ConfirmationDialog
    -> IO CString
foreignConfirmationDialogDetails confirmationDialogStablePtr = do
  confirmationDialog <- deRefStablePtr confirmationDialogStablePtr
  newCString $ confirmationDialogDetails confirmationDialog


foreignConfirmationDialogDefaultButtonIndex
    :: StablePtr ConfirmationDialog
    -> IO Word64
foreignConfirmationDialogDefaultButtonIndex confirmationDialogStablePtr = do
  confirmationDialog <- deRefStablePtr confirmationDialogStablePtr
  let maybeDefaultButtonIndex =
        confirmationDialogDefaultButtonIndex confirmationDialog
      defaultButtonIndex = case maybeDefaultButtonIndex of
                             Just defaultButtonIndex -> defaultButtonIndex
                             Nothing -> outOfBandWord64
  return defaultButtonIndex


foreignConfirmationDialogCancelButtonIndex
    :: StablePtr ConfirmationDialog
    -> IO Word64
foreignConfirmationDialogCancelButtonIndex confirmationDialogStablePtr = do
  confirmationDialog <- deRefStablePtr confirmationDialogStablePtr
  let maybeCancelButtonIndex =
        confirmationDialogCancelButtonIndex confirmationDialog
      defaultButtonIndex = case maybeCancelButtonIndex of
                             Just defaultButtonIndex -> defaultButtonIndex
                             Nothing -> outOfBandWord64
  return defaultButtonIndex


foreignConfirmationDialogButtonCount
    :: StablePtr ConfirmationDialog
    -> IO Word64
foreignConfirmationDialogButtonCount confirmationDialogStablePtr = do
  confirmationDialog <- deRefStablePtr confirmationDialogStablePtr
  let buttons = confirmationDialogButtons confirmationDialog
  return $ fromIntegral $ length buttons


foreignConfirmationDialogButton
    :: StablePtr ConfirmationDialog
    -> Word64
    -> IO CString
foreignConfirmationDialogButton confirmationDialogStablePtr
                                index = do
  confirmationDialog <- deRefStablePtr confirmationDialogStablePtr
  let index' = fromIntegral index
      buttons = confirmationDialogButtons confirmationDialog
  if index' < length buttons
    then newCString $ buttons !! index'
    else return nullPtr
