{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module Te.ForeignInterface () where

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
import Data.Timestamp (Timestamp(..))
import Te
import Te.Exceptions
import Te.Identifiers
import Te.Identifiers
import Te.Types


foreign import ccall "dynamic" mkVoidCallback
    :: FunPtr (IO ()) -> IO ()
foreign import ccall "dynamic" mkVoidStringStringCallback
    :: FunPtr (CString -> CString -> IO ()) -> (CString -> CString -> IO ())
foreign import ccall "dynamic" mkVoidBrowserWindowIDPtrCallback
    :: FunPtr (Ptr BrowserWindowID -> IO ()) -> (Ptr BrowserWindowID -> IO ())
foreign import ccall "dynamic" mkVoidBrowserWindowIDPtrInodeIDPtrCallback
    :: FunPtr (Ptr BrowserWindowID -> Ptr InodeID -> IO ())
    -> (Ptr BrowserWindowID -> Ptr InodeID -> IO ())


foreign export ccall "teFrontEndInternalFailure"
                      foreignFrontEndInternalFailure
    :: StablePtr (MVar ApplicationState) -> CString -> Word64 -> IO ()
foreign export ccall "teStringFree"
                     foreignStringFree
    :: CString -> IO ()
foreign export ccall "teVersionString"
                     foreignVersionString
    :: IO CString
foreign export ccall "teTimestampToString"
                     foreignTimestampToString
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
foreign export ccall "teApplicationInit"
                     foreignApplicationInit
    :: FunPtr (CString -> CString -> IO ())
    -> FunPtr (IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> Ptr InodeID -> IO ())
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
foreign export ccall "teBrowserWindowClose"
                     foreignBrowserWindowClose
    :: StablePtr (MVar ApplicationState) -> Ptr BrowserWindowID -> IO ()
foreign export ccall "teBrowserWindowTitle"
                     foreignBrowserWindowTitle
    :: StablePtr (MVar ApplicationState) -> Ptr BrowserWindowID -> IO CString
foreign export ccall "teBrowserWindowTitleIcon"
                     foreignBrowserWindowTitleIcon
    :: StablePtr (MVar ApplicationState) -> Ptr BrowserWindowID -> IO CString
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
foreign export ccall "teInodeRename"
                     foreignInodeRename
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> CString
    -> IO ()
foreign export ccall "teInodesDelete"
                     foreignInodesDelete
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Word64
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
    -> IO Word64
foreign export ccall "teInodeAcceptDrop"
                     foreignInodeAcceptDrop
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> StablePtr DragInformation
    -> IO ()
foreign export ccall "teBrowserItemDragInformationNew"
                     foreignBrowserItemDragInformationNew
    :: StablePtr (MVar ApplicationState)
    -> Bitfield DragOperation
    -> Ptr BrowserWindowID
    -> Word64
    -> Ptr InodeID
    -> IO (StablePtr DragInformation)
foreign export ccall "teExternalFileDragInformationNew"
                     foreignExternalFileDragInformationNew
    :: StablePtr (MVar ApplicationState)
    -> Bitfield DragOperation
    -> Ptr BrowserWindowID
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
foreign export ccall "teDragInformationBrowserItemCount"
                     foreignDragInformationBrowserItemCount
    :: StablePtr DragInformation
    -> IO Word64
foreign export ccall "teDragInformationBrowserItem"
                     foreignDragInformationBrowserItem
    :: StablePtr DragInformation
    -> Word64
    -> Ptr BrowserWindowID
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


foreignTimestampToString :: Word64 -> IO CString
foreignTimestampToString timestamp = do
  result <- timestampToString $ Timestamp timestamp
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


wrapVoidCallback
    :: FunPtr (IO ())
    -> (IO ())
wrapVoidCallback foreignCallback =
  let lowLevelCallback = mkVoidCallback foreignCallback
      highLevelCallback = do
        lowLevelCallback
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


foreignApplicationInit
    :: FunPtr (CString -> CString -> IO ())
    -> FunPtr (IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> IO ())
    -> FunPtr (Ptr BrowserWindowID -> Ptr InodeID -> IO ())
    -> IO (StablePtr (MVar ApplicationState))
foreignApplicationInit foreignException
                       foreignNoteRecentProjectsChanged
                       foreignNoteNewBrowserWindow
                       foreignNoteDeletedBrowserWindow
                       foreignNoteBrowserItemsChanged
                       foreignEditBrowserItemName = do
  let callbackException =
        wrapVoidStringStringCallback foreignException
      callbackNoteRecentProjectsChanged =
        wrapVoidCallback foreignNoteRecentProjectsChanged
      callbackNoteNewBrowserWindow =
        wrapVoidBrowserWindowCallback foreignNoteNewBrowserWindow
      callbackNoteDeletedBrowserWindow =
        wrapVoidBrowserWindowCallback foreignNoteDeletedBrowserWindow
      callbackNoteBrowserItemsChanged =
        wrapVoidBrowserWindowCallback foreignNoteBrowserItemsChanged
      callbackEditBrowserItemName =
        wrapVoidBrowserItemCallback foreignEditBrowserItemName
      callbacks = FrontEndCallbacks {
                      frontEndCallbacksException =
                        callbackException,
                      frontEndCallbacksNoteRecentProjectsChanged =
                        callbackNoteRecentProjectsChanged,
                      frontEndCallbacksNoteNewBrowserWindow =
                        callbackNoteNewBrowserWindow,
                      frontEndCallbacksNoteDeletedBrowserWindow =
                        callbackNoteDeletedBrowserWindow,
                      frontEndCallbacksNoteBrowserItemsChanged =
                        callbackNoteBrowserItemsChanged,
                      frontEndCallbacksEditBrowserItemName =
                        callbackEditBrowserItemName
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


foreignBrowserWindowClose
    :: StablePtr (MVar ApplicationState) -> Ptr BrowserWindowID -> IO ()
foreignBrowserWindowClose applicationStateMVarStablePtr
                          browserWindowIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        ()
                        browserWindowClose


foreignBrowserWindowTitle
    :: StablePtr (MVar ApplicationState) -> Ptr BrowserWindowID -> IO CString
foreignBrowserWindowTitle applicationStateMVarStablePtr
                          browserWindowIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        nullPtr
                        (\browserWindow -> do
                           string <- browserWindowTitle browserWindow
                           stringNew string)


foreignBrowserWindowTitleIcon
    :: StablePtr (MVar ApplicationState) -> Ptr BrowserWindowID -> IO CString
foreignBrowserWindowTitleIcon applicationStateMVarStablePtr
                              browserWindowIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        nullPtr
                        (\browserWindow -> do
                           string <- browserWindowTitleIcon browserWindow
                           stringNew string)


findBrowserWindowByID
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> a
    -> (BrowserWindow -> IO a)
    -> IO a
findBrowserWindowByID applicationStateMVarStablePtr
                      browserWindowIDPtr
                      defaultValue
                      action = do
  applicationStateMVar <- deRefStablePtr applicationStateMVarStablePtr
  catchTe applicationStateMVar defaultValue $ do
    applicationState <- readMVar applicationStateMVar
    browserWindowID <- peek browserWindowIDPtr
    maybeBrowserWindow <-
      foldM (\maybeBrowserWindow project -> do
               case maybeBrowserWindow of
                 Just browserWindow -> return maybeBrowserWindow
                 Nothing -> do
                   browserWindows <-
                     readMVar $ projectBrowserWindows project
                   return $ Map.lookup browserWindowID browserWindows)
            Nothing
            $ Map.elems $ applicationStateProjects applicationState
    case maybeBrowserWindow of
      Just browserWindow -> action browserWindow
      Nothing -> throwIO $(internalFailure)


foreignBrowserWindowRoot
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreignBrowserWindowRoot applicationStateMVarStablePtr
                         browserWindowIDPtr
                         inodeIDPtr = do
  inodeID' <-
    findBrowserWindowByID applicationStateMVarStablePtr
                          browserWindowIDPtr
                          nullInodeID
                          (\browserWindow -> do
                             browserItem <- browserWindowRoot browserWindow
                             return $ inodeID $ browserItemInode browserItem)
  poke inodeIDPtr inodeID'


foreignBrowserItemNewFolderInside
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreignBrowserItemNewFolderInside applicationStateMVarStablePtr
                                  browserWindowIDPtr
                                  inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        ()
                        (\browserWindow -> do
                           inodeID' <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID',
                                           inodeProject = project
                                         }
                               browserItem = BrowserItem {
                                                 browserItemInode = inode,
                                                 browserItemBrowserWindow =
                                                   browserWindow
                                               }
                           browserItemNewFolderInside browserItem)


foreignBrowserItemNewFileInside
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreignBrowserItemNewFileInside applicationStateMVarStablePtr
                                browserWindowIDPtr
                                inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        ()
                        (\browserWindow -> do
                           inodeID' <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID',
                                           inodeProject = project
                                         }
                               browserItem = BrowserItem {
                                                 browserItemInode = inode,
                                                 browserItemBrowserWindow =
                                                   browserWindow
                                               }
                           browserItemNewFolderInside browserItem)


foreignBrowserItemExpanded
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreignBrowserItemExpanded applicationStateMVarStablePtr
                           browserWindowIDPtr
                           inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        0
                        (\browserWindow -> do
                           inodeID' <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID',
                                           inodeProject = project
                                         }
                               browserItem = BrowserItem {
                                                 browserItemInode = inode,
                                                 browserItemBrowserWindow =
                                                   browserWindow
                                               }
                           result <- browserItemExpanded browserItem
                           if result
                             then return 1
                             else return 0)


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
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        ()
                        (\browserWindow -> do
                           inodeID <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID,
                                           inodeProject = project
                                         }
                               browserItem = BrowserItem {
                                                 browserItemInode = inode,
                                                 browserItemBrowserWindow =
                                                   browserWindow
                                               }
                               expanded' = case expanded of
                                             0 -> False
                                             _ -> True
                           browserItemSetExpanded browserItem expanded')


foreignInodeExpandable
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreignInodeExpandable applicationStateMVarStablePtr
                       browserWindowIDPtr
                       inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        0
                        (\browserWindow -> do
                           inodeID <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID,
                                           inodeProject = project
                                         }
                           result <- inodeExpandable inode
                           if result
                             then return 1
                             else return 0)


foreignInodeChildCount
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreignInodeChildCount applicationStateMVarStablePtr
                       browserWindowIDPtr
                       inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        0
                        (\browserWindow -> do
                           inodeID <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID,
                                           inodeProject = project
                                         }
                           inodeChildCount inode)


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
  inodeChildID <-
    findBrowserWindowByID applicationStateMVarStablePtr
                          browserWindowIDPtr
                          nullInodeID
                          (\browserWindow -> do
                             inodeID' <- peek inodeIDPtr
                             let project = browserWindowProject browserWindow
                                 inode = Inode {
                                             inodeID = inodeID',
                                             inodeProject = project
                                           }
                             maybeInodeChild <-
                               inodeChild inode index
                             case maybeInodeChild of
                               Just inodeChild -> return $ inodeID inodeChild
                               Nothing -> return nullInodeID)
  poke inodeChildIDPtr inodeChildID


foreignInodeName
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO CString
foreignInodeName applicationStateMVarStablePtr
                 browserWindowIDPtr
                 inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        nullPtr
                        (\browserWindow -> do
                           inodeID' <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID',
                                           inodeProject = project
                                         }
                           string <- inodeName inode
                           newCString string)


foreignInodeKind
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO CString
foreignInodeKind applicationStateMVarStablePtr
                 browserWindowIDPtr
                 inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        nullPtr
                        (\browserWindow -> do
                           inodeID' <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID',
                                           inodeProject = project
                                         }
                           string <- inodeKind inode
                           newCString string)


foreignInodeSize
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreignInodeSize applicationStateMVarStablePtr
                 browserWindowIDPtr
                 inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        0
                        (\browserWindow -> do
                           inodeID' <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID',
                                           inodeProject = project
                                         }
                           maybeSize <- inodeSize inode
                           case maybeSize of
                             Just size -> return size
                             Nothing -> return 0)


foreignInodeCreationTimestamp
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreignInodeCreationTimestamp applicationStateMVarStablePtr
                              browserWindowIDPtr
                              inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        0
                        (\browserWindow -> do
                           inodeID' <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID',
                                           inodeProject = project
                                         }
                           result <- inodeCreationTimestamp inode
                           return $ fromIntegral result)


foreignInodeModificationTimestamp
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO Word64
foreignInodeModificationTimestamp applicationStateMVarStablePtr
                                  browserWindowIDPtr
                                  inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        0
                        (\browserWindow -> do
                           inodeID' <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID',
                                           inodeProject = project
                                         }
                           result <- inodeModificationTimestamp inode
                           return $ fromIntegral result)


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
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        ()
                        (\browserWindow -> do
                           inodeID' <- peek inodeIDPtr
                           let project = browserWindowProject browserWindow
                               inode = Inode {
                                           inodeID = inodeID',
                                           inodeProject = project
                                         }
                           newName <- peekCString newNameCString
                           inodeRename inode newName)


foreignInodesDelete
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Word64
    -> Ptr InodeID
    -> IO ()
foreignInodesDelete applicationStateMVarStablePtr
                    browserWindowIDPtr
                    inodeIDCount
                    inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        ()
                        (\browserWindow -> do
                           let project = browserWindowProject browserWindow
                               inodeIDCount' = fromIntegral inodeIDCount
                           inodeIDs <- peekArray inodeIDCount' inodeIDPtr
                           let inodes = map (\inodeID' ->
                                               Inode {
                                                   inodeID = inodeID',
                                                   inodeProject = project
                                                 })
                                            inodeIDs
                           inodesDelete inodes)


foreignInodeValidateDrop
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> StablePtr DragInformation
    -> Ptr InodeID
    -> Ptr Word64
    -> IO Word64
foreignInodeValidateDrop applicationStateMVarStablePtr
                         browserWindowIDPtr
                         inodeIDPtr
                         dragInformationStablePtr
                         resultInodeIDPtr
                         resultDragOperationPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        0
                        (\browserWindow -> do
                           let project = browserWindowProject browserWindow
                           inodeID' <- peek inodeIDPtr
                           let inode = Inode {
                                           inodeID = inodeID',
                                           inodeProject = project
                                         }
                           dragInformation <-
                             deRefStablePtr dragInformationStablePtr
                           result <- inodeValidateDrop inode dragInformation
                           case result of
                             Nothing -> return 0
                             Just (resultInode, resultDragOperation) -> do
                               poke resultInodeIDPtr $ inodeID resultInode
                               let resultDragOperationBit =
                                     valueToBit resultDragOperation
                               poke resultDragOperationPtr
                                    resultDragOperationBit
                               return 1)


foreignInodeAcceptDrop
    :: StablePtr (MVar ApplicationState)
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> StablePtr DragInformation
    -> IO ()
foreignInodeAcceptDrop applicationStateMVarStablePtr
                       browserWindowIDPtr
                       inodeIDPtr
                       dragInformationStablePtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        ()
                        (\browserWindow -> do
                           let project = browserWindowProject browserWindow
                           inodeID' <- peek inodeIDPtr
                           let inode = Inode {
                                           inodeID = inodeID',
                                           inodeProject = project
                                         }
                           dragInformation <-
                             deRefStablePtr dragInformationStablePtr
                           inodeAcceptDrop inode dragInformation)


foreignBrowserItemDragInformationNew
    :: StablePtr (MVar ApplicationState)
    -> Bitfield DragOperation
    -> Ptr BrowserWindowID
    -> Word64
    -> Ptr InodeID
    -> IO (StablePtr DragInformation)
foreignBrowserItemDragInformationNew applicationStateMVarStablePtr
                                     allowedDragOperationsBitfield
                                     browserWindowIDPtr
                                     inodeIDCount
                                     inodeIDPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        (castPtrToStablePtr nullPtr)
                        (\browserWindow -> do
                           let allowedDragOperations =
                                 bitfieldToSet allowedDragOperationsBitfield
                               inodeIDCount' = fromIntegral inodeIDCount
                               project = browserWindowProject browserWindow
                           inodeIDs <- peekArray inodeIDCount' inodeIDPtr
                           let browserItems =
                                 map (\inodeID' ->
                                        let inode =
                                              Inode {
                                                  inodeID = inodeID',
                                                  inodeProject = project
                                                }
                                            browserItem =
                                              BrowserItem {
                                                  browserItemInode = inode,
                                                  browserItemBrowserWindow =
                                                    browserWindow
                                                }
                                        in browserItem)
                                     inodeIDs
                           let dragInformation =
                                 BrowserItemDragInformation {
                                     dragInformationAllowedDragOperations =
                                       allowedDragOperations,
                                     browserItemDragInformationBrowserItems =
                                       browserItems
                                   }
                           newStablePtr dragInformation)


foreignExternalFileDragInformationNew
    :: StablePtr (MVar ApplicationState)
    -> Bitfield DragOperation
    -> Ptr BrowserWindowID
    -> Word64
    -> Ptr CString
    -> IO (StablePtr DragInformation)
foreignExternalFileDragInformationNew applicationStateMVarStablePtr
                                      allowedDragOperationsBitfield
                                      browserWindowIDPtr
                                      filePathCount
                                      filePathCStringPtr = do
  findBrowserWindowByID applicationStateMVarStablePtr
                        browserWindowIDPtr
                        (castPtrToStablePtr nullPtr)
                        (\browserWindow -> do
                           let allowedDragOperations =
                                 bitfieldToSet allowedDragOperationsBitfield
                               filePathCount' = fromIntegral filePathCount
                           filePathCStrings <-
                             peekArray filePathCount' filePathCStringPtr
                           filePaths <- mapM peekCString filePathCStrings
                           let dragInformation =
                                 ExternalFileDragInformation {
                                     dragInformationAllowedDragOperations =
                                       allowedDragOperations,
                                     externalFileDragInformationFilePaths =
                                       filePaths
                                   }
                           newStablePtr dragInformation)


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


foreignDragInformationBrowserItemCount
    :: StablePtr DragInformation
    -> IO Word64
foreignDragInformationBrowserItemCount dragInformationStablePtr = do
  dragInformation <- deRefStablePtr dragInformationStablePtr
  case dragInformation of
    BrowserItemDragInformation { } -> do
      let browserItems = browserItemDragInformationBrowserItems dragInformation
      return $ fromIntegral $ length browserItems
    ExternalFileDragInformation { } -> do
      return 0


foreignDragInformationBrowserItem
    :: StablePtr DragInformation
    -> Word64
    -> Ptr BrowserWindowID
    -> Ptr InodeID
    -> IO ()
foreignDragInformationBrowserItem dragInformationStablePtr
                                  index
                                  browserWindowIDPtr
                                  inodeIDPtr = do
  dragInformation <- deRefStablePtr dragInformationStablePtr
  case dragInformation of
    BrowserItemDragInformation { } -> do
      let browserItems = browserItemDragInformationBrowserItems dragInformation
          index' = fromIntegral index
      if index' < length browserItems
        then do
          let browserItem = browserItems !! index'
              browserWindow = browserItemBrowserWindow browserItem
              inode = browserItemInode browserItem
          poke browserWindowIDPtr $ browserWindowID browserWindow
          poke inodeIDPtr $ inodeID inode
        else return ()
    ExternalFileDragInformation { } -> do
      return ()


foreignDragInformationFilePathCount
    :: StablePtr DragInformation
    -> IO Word64
foreignDragInformationFilePathCount dragInformationStablePtr = do
  dragInformation <- deRefStablePtr dragInformationStablePtr
  case dragInformation of
    BrowserItemDragInformation { } -> do
      return 0
    ExternalFileDragInformation { } -> do
      let filePaths = externalFileDragInformationFilePaths dragInformation
      return $ fromIntegral $ length $ filePaths


foreignDragInformationFilePath
    :: StablePtr DragInformation
    -> Word64
    -> IO CString
foreignDragInformationFilePath dragInformationStablePtr index = do
  dragInformation <- deRefStablePtr dragInformationStablePtr
  case dragInformation of
    BrowserItemDragInformation { } -> do
      return nullPtr
    ExternalFileDragInformation { } -> do
      let filePaths = externalFileDragInformationFilePaths dragInformation
          index' = fromIntegral index
      if index' < length filePaths
        then do
          let filePath = filePaths !! index'
          stringNew filePath
        else return nullPtr
