{-# LANGUAGE TemplateHaskell #-}
module Te
  (ByteSize(..),
   Timestamp(..),
   FrontEndCallbacks(..),
   ApplicationState,
   Inode, InodeID, inodeID,
   Window, WindowID, windowID,
   WindowType(..), toWindowID, fromWindowID,
   BrowserWindow, BrowserWindowID, browserWindowID,
   BrowserItem(..),
   DocumentWindow, DocumentWindowID, documentWindowID,
   ConfirmationDialog(..),
   frontEndInternalFailure,
   versionString,
   timestampShow,
   byteSizePlaceholderString,
   byteSizeShow,
   uuidHash,
   uuidEqual,
   uuidShow,
   applicationInit,
   applicationExit,
   applicationRecentProjectCount,
   applicationRecentProjectName,
   applicationRecentProjectTimestamp,
   applicationNewProject,
   applicationOpenProject,
   applicationOpenRecentProject,
   windowClose,
   windowTitle,
   windowTitleIcon,
   browserWindowRoot,
   browserItemNewFolderInside,
   browserItemNewFileInside,
   browserItemExpanded,
   browserItemSetExpanded,
   inodeParent,
   inodeExpandable,
   inodeChildCount,
   inodeChild,
   inodeName,
   inodeKind,
   inodeSize,
   inodeCreationTimestamp,
   inodeModificationTimestamp,
   inodeIcon,
   inodeRename,
   inodeListDelete,
   inodeOpen,
   inodeValidateDrop,
   inodeAcceptDrop,
   inodesKernel,
   browserWindowDraggingSourceIntraApplicationOperations,
   browserWindowDraggingSourceInterApplicationOperations)
  where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Char
import Data.Digest.Murmur64
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.UUID
import Data.Version
import Data.Word
import System.Directory
import System.FilePath
import Prelude hiding (catch)

import Data.ByteSize
import Data.Timestamp
import Te.Database
import Te.Exceptions
import Te.FrontEndCallbacks
import Te.Identifiers
import Te.Types
import Paths_te


catchTe :: MVar ApplicationState -> a -> IO a -> IO a
catchTe applicationStateMVar defaultValue action = do
  catch (action)
        (\e -> do
           exception applicationStateMVar e
           return defaultValue)


frontEndInternalFailure :: MVar ApplicationState -> String -> Word64 -> IO ()
frontEndInternalFailure applicationStateMVar filename lineNumber =
  catchTe applicationStateMVar () $ do
    let lineNumber' = fromIntegral lineNumber
    throwIO $ TeExceptionInternal filename lineNumber'


versionString :: String
versionString = showVersion version


timestampShow :: Timestamp -> IO String
timestampShow timestamp = do
  describeTimestamp timestamp


byteSizePlaceholderString :: IO String
byteSizePlaceholderString = do
  return "—"


byteSizeShow :: ByteSize -> IO String
byteSizeShow byteSize = do
  return $ show byteSize


uuidHash :: UUID -> IO Word64
uuidHash uuid = do
  return $ asWord64 $ hash64 uuid


uuidEqual :: UUID -> UUID -> IO Bool
uuidEqual uuidA uuidB = do
  return $ uuidA == uuidB


uuidShow :: UUID -> IO String
uuidShow uuid = do
  return $ show uuid


applicationInit :: FrontEndCallbacks -> IO (MVar ApplicationState)
applicationInit callbacks = do
  let applicationState = ApplicationState {
                             applicationStateRecentProjects = [],
                             applicationStateProjects = Map.empty,
                             applicationStateFrontEndCallbacks = callbacks
                           }
  newMVar applicationState


applicationExit :: MVar ApplicationState -> IO ()
applicationExit applicationStateMVar = do
  return ()


applicationRecentProjectCount :: MVar ApplicationState -> IO Word64
applicationRecentProjectCount applicationStateMVar =
  catchTe applicationStateMVar 0 $ do
    applicationState <- readMVar applicationStateMVar
    let result = length $ applicationStateRecentProjects applicationState
    return $ fromIntegral result


applicationRecentProjectName
    :: MVar ApplicationState -> Word64 -> IO (Maybe String)
applicationRecentProjectName applicationStateMVar index =
  catchTe applicationStateMVar Nothing $ do
    applicationState <- readMVar applicationStateMVar
    let recentProjects = applicationStateRecentProjects applicationState
        index' = fromIntegral index
    if index' < length recentProjects
      then do
        let filePath = recentProjectFilePath $ recentProjects !! index'
            nameWithExtension = takeFileName filePath
            name = if isSuffixOf ".te" nameWithExtension
                     then dropExtension nameWithExtension
                     else nameWithExtension
        return $ Just name
      else return Nothing


applicationRecentProjectTimestamp
    :: MVar ApplicationState -> Word64 -> IO (Maybe Timestamp)
applicationRecentProjectTimestamp applicationStateMVar index =
  catchTe applicationStateMVar Nothing $ do
    applicationState <- readMVar applicationStateMVar
    let recentProjects = applicationStateRecentProjects applicationState
        index' = fromIntegral index
    if index' < length recentProjects
      then return $ Just $ recentProjectTimestamp $ recentProjects !! index'
      else return Nothing


applicationNewProject :: MVar ApplicationState -> IO ()
applicationNewProject applicationStateMVar =
  catchTe applicationStateMVar () $ do
    projectID <- newProjectID
    database <- newProjectDatabase
    initProjectDatabaseSchema database
    name <- getNextUntitledProjectName applicationStateMVar
    nameMVar <- newMVar name
    filePathMVar <- newMVar Nothing
    windowsMVar <- newMVar Map.empty
    let newProject = Project {
                         projectID = projectID,
                         projectApplicationState = applicationStateMVar,
                         projectDatabase = database,
                         projectName = nameMVar,
                         projectFilePath = filePathMVar,
                         projectWindows = windowsMVar
                       }
    addProjectToApplicationState applicationStateMVar newProject
    restoreProjectWindows newProject


applicationOpenProject
    :: MVar ApplicationState -> FilePath -> IO ()
applicationOpenProject applicationStateMVar filePath =
  catchTe applicationStateMVar () $ do
    applicationOpenProject' applicationStateMVar filePath


applicationOpenProject'
    :: MVar ApplicationState -> FilePath -> IO ()
applicationOpenProject' applicationStateMVar filePath = do
  exists <- doesFileExist filePath
  if not exists
    then throwIO $ TeExceptionFileDoesNotExist filePath
    else do
      database <- newProjectDatabase
      attachSucceeded <- attachFileToProjectDatabase database filePath
      if attachSucceeded
        then do
          maybeSchemaVersion <-
            getProjectDatabaseSchemaVersion database
          case maybeSchemaVersion of
            Just 1 -> do
              projectID <- newProjectID
              nameMVar <- newMVar $ computeNameForFilePath filePath
              filePathMVar <- newMVar $ Just filePath
              windowsMVar <- newMVar Map.empty
              let newProject = Project {
                                   projectID = projectID,
                                   projectApplicationState =
                                     applicationStateMVar,
                                   projectDatabase = database,
                                   projectName = nameMVar,
                                   projectFilePath = filePathMVar,
                                   projectWindows = windowsMVar
                                 }
              addProjectToApplicationState applicationStateMVar newProject
              updateProjectInRecentProjects applicationStateMVar newProject
              restoreProjectWindows newProject
            Just _ -> do
              closeProjectDatabase database
              throwIO $ TeExceptionFileCreatedByNewerVersion filePath
            Nothing -> do
              closeProjectDatabase database
              throwIO $ TeExceptionFileNotInRecognizedFormat filePath
        else do
          closeProjectDatabase database
          throwIO $ TeExceptionFileNotInRecognizedFormat filePath


applicationOpenRecentProject
    :: MVar ApplicationState -> Word64 -> IO ()
applicationOpenRecentProject applicationStateMVar index =
  catchTe applicationStateMVar () $ do
    applicationState <- readMVar applicationStateMVar
    let recentProjects = applicationStateRecentProjects applicationState
        index' = fromIntegral index
    if index' < length recentProjects
      then do
        applicationOpenProject' applicationStateMVar
                                $ recentProjectFilePath
                                   $ recentProjects !! index'
      else return ()


projectClose :: Project -> IO ()
projectClose project = do
  closeProjectDatabase $ projectDatabase project
  removeProjectFromApplicationState (projectApplicationState project) project


addProjectToApplicationState :: MVar ApplicationState -> Project -> IO ()
addProjectToApplicationState applicationStateMVar project = do
  applicationState <- takeMVar applicationStateMVar
  let projects = applicationStateProjects applicationState
      projects' = Map.insert (projectID project) project projects
      applicationState' = applicationState {
                              applicationStateProjects = projects'
                            }
  putMVar applicationStateMVar applicationState'


removeProjectFromApplicationState :: MVar ApplicationState -> Project -> IO ()
removeProjectFromApplicationState applicationStateMVar project = do
  applicationState <- takeMVar applicationStateMVar
  let projects = applicationStateProjects applicationState
      projects' = Map.delete (projectID project) projects
      applicationState' = applicationState {
                              applicationStateProjects = projects'
                            }
  putMVar applicationStateMVar applicationState'


updateProjectInRecentProjects :: MVar ApplicationState -> Project -> IO ()
updateProjectInRecentProjects applicationStateMVar project = do
  maybeFilePath <- readMVar $ projectFilePath project
  case maybeFilePath of
    Nothing -> return ()
    Just filePath -> do
      timestamp <- getTimestamp
      applicationState <- takeMVar applicationStateMVar
      let newRecentProject = RecentProject {
                                 recentProjectID = projectID project,
                                 recentProjectFilePath = filePath,
                                 recentProjectTimestamp = timestamp
                               }
          recentProjects = applicationStateRecentProjects applicationState
          recentProjects' = take 8
                                 $ newRecentProject
                                   : filter (\recentProject ->
                                               recentProjectID recentProject
                                               /= projectID project)
                                            recentProjects
          applicationState' = applicationState {
                                  applicationStateRecentProjects = recentProjects'
                                }
      putMVar applicationStateMVar applicationState'


restoreProjectWindows :: Project -> IO ()
restoreProjectWindows project = do
  windows <- takeMVar $ projectWindows project
  if Map.null windows
    then do
      putMVar (projectWindows project) windows
      newBrowserWindow project Nothing
    else do
      putMVar (projectWindows project) windows
      return ()


computeNextNumberedName :: String -> String -> [String] -> Bool -> String
computeNextNumberedName prefix suffix siblingNames caseSensitive =
  let prefix' :: String
      prefix' = caseFoldIfAppropriate prefix
      
      suffix' :: String
      suffix' = caseFoldIfAppropriate suffix
      
      prefixWithSpace' :: String
      prefixWithSpace' = prefix' ++ " "
      
      caseFoldIfAppropriate :: String -> String
      caseFoldIfAppropriate string =
        if caseSensitive
          then string
          else map toUpper string
      
      stripPrefixAndSuffix :: String -> String -> String -> Maybe String
      stripPrefixAndSuffix prefix suffix name =
        if isPrefixOf prefix name
          then let nameWithoutPrefix = drop (length prefix) name
               in if isSuffixOf suffix nameWithoutPrefix
                    then Just $ reverse $ drop (length suffix)
                                               $ reverse nameWithoutPrefix
                    else Nothing
          else Nothing
      
      nameNumber :: String -> Maybe Int
      nameNumber name =
        let name' = caseFoldIfAppropriate name
        in if name' == prefix' ++ suffix'
             then Just 1
             else case stripPrefixAndSuffix prefixWithSpace' suffix' name' of
                    Nothing -> Nothing
                    Just numberPart ->
                      if (length numberPart > 0) && (all isDigit numberPart)
                        then Just $ read numberPart
                        else Nothing
      
      number :: Int
      number = 1 + (foldl1 max
                           $ catMaybes $ [Just 0]
                                         ++ map nameNumber
                                                siblingNames)
      
      name :: String
      name = if number == 1
               then prefix ++ suffix
               else prefix ++ " " ++ show number ++ suffix
  in name


getNextUntitledProjectName :: MVar ApplicationState -> IO String
getNextUntitledProjectName applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  projectNames <- mapM (\project -> readMVar $ projectName project)
                       $ Map.elems $ applicationStateProjects applicationState
  return $ computeNextNumberedName "Untitled" "" projectNames True


computeNameForFilePath :: String -> String
computeNameForFilePath filePath =
  let fileName = takeFileName filePath
      fileNameWithoutExtension = if isSuffixOf ".te" fileName
                                   then dropExtension fileName
                                   else fileName
  in fileNameWithoutExtension


newBrowserWindow :: Project -> Maybe Inode -> IO ()
newBrowserWindow project maybeRootInode = do
  let applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    newBrowserWindowID <- newBrowserWindowID
    let newBrowserWindow' = BrowserWindow {
                                browserWindowID = newBrowserWindowID,
                                browserWindowProject = project
                              }
        newWindow = toWindow newBrowserWindow'
        newWindowID = windowID newWindow
    windows <- takeMVar $ projectWindows project
    let windows' = Map.insert newWindowID newWindow windows
    putMVar (projectWindows project) windows'
    rootInode <- case maybeRootInode of
                   Just rootInode -> return rootInode
                   Nothing -> lookupProjectRoot project
    recordNewBrowserWindow newBrowserWindow' rootInode
    noteNewBrowserWindow newBrowserWindow'


class WindowType window where
  toWindow :: window -> Window
  getFromWindow :: Window -> IO (Maybe window)


instance WindowType BrowserWindow where
  toWindow browserWindow =
             Window {
                 windowID = toWindowID $ browserWindowID browserWindow,
                 windowProject = browserWindowProject browserWindow
               }
  getFromWindow window = do
    kind <- lookupWindowKind window
    case kind of
      WindowKindBrowser ->
        return $ Just $ BrowserWindow {
                            browserWindowID = fromWindowID $ windowID window,
                            browserWindowProject = windowProject window
                          }
      _ -> return Nothing


instance WindowType DocumentWindow where
  toWindow documentWindow =
             Window {
                 windowID = toWindowID $ documentWindowID documentWindow,
                 windowProject = documentWindowProject documentWindow
               }
  getFromWindow window = do
    kind <- lookupWindowKind window
    case kind of
      WindowKindDocument ->
        return $ Just $ DocumentWindow {
                            documentWindowID = fromWindowID $ windowID window,
                            documentWindowProject = windowProject window
                          }
      _ -> return Nothing


windowClose :: Window -> IO ()
windowClose window = do
  let project = windowProject window
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    windows <- takeMVar $ projectWindows project
    let deletedWindowID = windowID window
        windows' = Map.delete deletedWindowID windows
    noteDeletedWindow window
    if Map.null windows'
      then do
        projectClose project
        putMVar (projectWindows project) windows'
      else do
        putMVar (projectWindows project) windows'


windowTitle :: Window -> IO String
windowTitle window = do
  let project = windowProject window
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    kind <- lookupWindowKind window
    case kind of
      WindowKindBrowser -> do
        maybeBrowserWindow <- getFromWindow window
        case maybeBrowserWindow of
          Just browserWindow -> do
            projectName <- readMVar $ projectName project
            folderInode <- lookupBrowserWindowRoot browserWindow
            rootInode <- lookupProjectRoot project
            if inodeID rootInode == inodeID folderInode
              then return projectName
              else do
                folderInodeInformation <- lookupInodeInformation folderInode
                let folderName = inodeInformationName folderInodeInformation
                return $ projectName ++ " - " ++ folderName
          Nothing -> throwIO $(internalFailure)
      WindowKindDocument -> do
        maybeDocumentWindow <- getFromWindow window
        case maybeDocumentWindow of
          Just documentWindow -> do
            documentInode <- lookupDocumentWindowInode documentWindow
            documentInodeInformation <- lookupInodeInformation documentInode
            let documentName = inodeInformationName documentInodeInformation
            return documentName
          Nothing -> throwIO $(internalFailure)


windowTitleIcon :: Window -> IO String
windowTitleIcon window = do
  let project = windowProject window
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Project" $ do
    kind <- lookupWindowKind window
    case kind of
      WindowKindBrowser -> do
        maybeBrowserWindow <- getFromWindow window
        case maybeBrowserWindow of
          Just browserWindow -> do
            inode <- lookupBrowserWindowRoot browserWindow
            rootInode <- lookupProjectRoot project
            if inodeID rootInode == inodeID inode
              then return "Project"
              else return "Folder"
          Nothing -> throwIO $(internalFailure)
      WindowKindDocument -> do
        maybeDocumentWindow <- getFromWindow window
        case maybeDocumentWindow of
          Just documentWindow -> do
            documentInode <- lookupDocumentWindowInode documentWindow
            documentInodeInformation <- lookupInodeInformation documentInode
            let documentKind = inodeInformationKind documentInodeInformation
            case documentKind of
              InodeKindDirectory -> return "Folder"
              InodeKindHaskell -> return "File"
          Nothing -> throwIO $(internalFailure)


browserWindowRoot :: BrowserWindow -> IO BrowserItem
browserWindowRoot browserWindow = do
  let project = browserWindowProject browserWindow
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar
          (BrowserItem {
               browserItemInode = Inode {
                                      inodeID = nullInodeID,
                                      inodeProject = project
                                    },
               browserItemBrowserWindow = browserWindow
             })
          $ do
    inode <- lookupBrowserWindowRoot browserWindow
    return $ BrowserItem {
                 browserItemInode = inode,
                 browserItemBrowserWindow = browserWindow
               }


getChildInodeNames :: Inode -> IO [String]
getChildInodeNames inode = do
  childCount <- inodeChildCount inode
  let getChildInodes resultsSoFar index = do
        if index == childCount
          then return resultsSoFar
          else do
            childInode <- lookupInodeChild inode index
            getChildInodes (resultsSoFar ++ [childInode]) (index + 1)
  childInodes <- getChildInodes [] 0
  mapM (\inode -> do
          inodeInformation <- lookupInodeInformation inode
          return $ inodeInformationName inodeInformation)
       childInodes


noteBrowserItemsChangedInAllBrowserWindows :: Project -> IO ()
noteBrowserItemsChangedInAllBrowserWindows project = do
  windows <- readMVar $ projectWindows project
  mapM_ (\window -> do
           maybeBrowserWindow <- getFromWindow window
           case maybeBrowserWindow of
             Just browserWindow -> noteBrowserItemsChanged browserWindow
             Nothing -> return ())
        $ Map.elems windows


browserItemNewFolderInside :: BrowserItem -> IO ()
browserItemNewFolderInside parentBrowserItem = do
  let parentInode = browserItemInode parentBrowserItem
      project = inodeProject parentInode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    siblingNames <- getChildInodeNames parentInode
    let newName = computeNextNumberedName "New Folder" "" siblingNames False
    inodeID' <- newInodeID
    let newInode = Inode {
                       inodeID = inodeID',
                       inodeProject = inodeProject parentInode
                     }
    recordNewInode newInode parentInode newName InodeKindDirectory Nothing
    let browserWindow = browserItemBrowserWindow parentBrowserItem
    noteBrowserItemsChanged browserWindow
    let newBrowserItem = BrowserItem {
                             browserItemInode = newInode,
                             browserItemBrowserWindow = browserWindow
                           }
    editBrowserItemName newBrowserItem


browserItemNewFileInside :: BrowserItem -> IO ()
browserItemNewFileInside parentBrowserItem = do
  let parentInode = browserItemInode parentBrowserItem
      project = inodeProject parentInode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    siblingNames <- getChildInodeNames parentInode
    let newName = computeNextNumberedName "New Module" ".hs" siblingNames False
    inodeID' <- newInodeID
    let newInode = Inode {
                       inodeID = inodeID',
                       inodeProject = inodeProject parentInode
                     }
    recordNewInode newInode parentInode newName InodeKindHaskell (Just 0)
    let browserWindow = browserItemBrowserWindow parentBrowserItem
    noteBrowserItemsChanged browserWindow
    let newBrowserItem = BrowserItem {
                             browserItemInode = newInode,
                             browserItemBrowserWindow = browserWindow
                           }
    editBrowserItemName newBrowserItem


browserItemExpanded :: BrowserItem -> IO Bool
browserItemExpanded browserItem = do
  let inode = browserItemInode browserItem
      project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar False $ do
    lookupBrowserItemExpanded browserItem


browserItemSetExpanded :: BrowserItem -> Bool -> IO ()
browserItemSetExpanded browserItem expanded = do
  let inode = browserItemInode browserItem
      project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    recordBrowserItemExpanded browserItem expanded


inodeParent :: Inode -> IO (Maybe Inode)
inodeParent inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar Nothing $ do
    lookupInodeParent inode


inodeExpandable :: Inode -> IO Bool
inodeExpandable inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar False $ do
    inodeInformation <- lookupInodeInformation inode
    case inodeInformationKind inodeInformation of
      InodeKindDirectory -> return True
      _ -> return False


inodeChildCount :: Inode -> IO Word64
inodeChildCount inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar 0 $ do
    lookupInodeChildCount inode


inodeChild :: Inode -> Word64 -> IO Inode
inodeChild inode index = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar
          (Inode {
               inodeID = nullInodeID,
               inodeProject = project
             })
          $ do
    lookupInodeChild inode index


inodeName :: Inode -> IO String
inodeName inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    inodeInformation <- lookupInodeInformation inode
    return $ inodeInformationName inodeInformation


inodeKind :: Inode -> IO String
inodeKind inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    inodeInformation <- lookupInodeInformation inode
    case inodeInformationKind inodeInformation of
      InodeKindDirectory -> return "Folder"
      InodeKindHaskell -> return "Haskell"


inodeSize :: Inode -> IO (Maybe ByteSize)
inodeSize inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar Nothing $ do
    inodeInformation <- lookupInodeInformation inode
    return $ inodeInformationSize inodeInformation


inodeCreationTimestamp :: Inode -> IO Timestamp
inodeCreationTimestamp inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar minBound $ do
    inodeInformation <- lookupInodeInformation inode
    return $ inodeInformationCreationTimestamp inodeInformation


inodeModificationTimestamp :: Inode -> IO Timestamp
inodeModificationTimestamp inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar minBound $ do
    inodeInformation <- lookupInodeInformation inode
    return $ inodeInformationModificationTimestamp inodeInformation


inodeIcon :: Inode -> IO String
inodeIcon inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "File" $ do
    inodeInformation <- lookupInodeInformation inode
    case inodeInformationKind inodeInformation of
      InodeKindDirectory -> return "Folder"
      InodeKindHaskell -> return "File"


inodeRename :: Inode -> String -> IO ()
inodeRename inode newName = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    maybeParent <- lookupInodeParent inode
    case maybeParent of
      Just parent -> do
        recordMovedInode inode newName parent
        noteBrowserItemsChangedInAllBrowserWindows project
      Nothing -> throwIO $(internalFailure)


inodeListDelete :: Maybe BrowserWindow -> [Inode] -> IO ()
inodeListDelete maybeBrowserWindow inodes = do
  inodes <- getInodesKernel inodes
  case inodes of
    [] -> return ()
    (firstInode:_) -> do
      let project = inodeProject firstInode
          applicationStateMVar = projectApplicationState project
      catchTe applicationStateMVar () $ do
        (nFolders, nFiles, totalSize) <- getInodesRecursiveStatistics inodes
        if (nFolders > 0) || (nFiles > 0)
          then do
            let pluralize :: Int -> String -> String
                pluralize n word =
                  if n == 1
                    then (show n) ++ " " ++ word
                    else (show n) ++ " " ++ word ++ "s"
                omitIfZero :: Int -> String -> Maybe String
                omitIfZero n word =
                  if n == 0
                    then Nothing
                    else Just $ pluralize n word
                message = "Do you want to delete "
                          ++ (if length inodes == 1
                                then "this item"
                                else "these items")
                          ++ "?"
                details = intercalate " and "
                                      (catMaybes $ map (uncurry omitIfZero)
                                                       [(nFolders, "folder"),
                                                        (nFiles, "file")])
                          ++ ", totalling "
                          ++ (show totalSize)
                          ++ ", will be deleted."
                          ++ "  "
                          ++ "This action is irreversible."
            confirm applicationStateMVar
                    (ConfirmationDialog {
                         confirmationDialogBrowserWindow = maybeBrowserWindow,
                         confirmationDialogMessage = message,
                         confirmationDialogDetails = details,
                         confirmationDialogDefaultButtonIndex = Just 0,
                         confirmationDialogCancelButtonIndex = Just 1,
                         confirmationDialogButtons = ["Yes", "No"]
                       })
                    (\result -> do
                       if result == 0
                         then do
                           mapM_ recordDeletedInode inodes
                           noteBrowserItemsChangedInAllBrowserWindows project
                         else return ())
          else return ()


inodeOpen :: Inode -> IO ()
inodeOpen inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    inodeInformation <- lookupInodeInformation inode
    case inodeInformationKind inodeInformation of
      InodeKindDirectory -> do
        newBrowserWindow project $ Just inode
      _ -> do
        return ()


getInodesRecursiveStatistics :: [Inode] -> IO (Int, Int, ByteSize)
getInodesRecursiveStatistics inodes = do
  inodesKernel <- getInodesKernel inodes
  let visitInode inode = do
        inodeInformation <- lookupInodeInformation inode
        let isFolder = inodeInformationKind inodeInformation
                       == InodeKindDirectory
            size = case inodeInformationSize inodeInformation of
                     Just size -> size
                     Nothing -> 0
        inodeChildren <- lookupInodeChildren inode
        visitInodeList (if isFolder
                          then 1
                          else 0,
                        if not isFolder
                          then 1
                          else 0,
                        size)
                       inodeChildren
      visitInodeList information inodes = do
        foldM (\(nFoldersSoFar, nFilesSoFar, totalSizeSoFar) inode -> do
                 (nFoldersHere, nFilesHere, totalSizeHere) <- visitInode inode
                 return (nFoldersSoFar + nFoldersHere,
                         nFilesSoFar + nFilesHere,
                         totalSizeSoFar + totalSizeHere))
              information
              inodes
  visitInodeList (0, 0, 0) inodesKernel


getInodesKernel :: [Inode] -> IO [Inode]
getInodesKernel inodes = do
  let getIsAncestorOf :: Inode -> Inode -> IO Bool
      getIsAncestorOf potentialAncestorInode scrutineeInode = do
        if inodeID potentialAncestorInode == inodeID scrutineeInode
          then return True
          else do
            maybeScrutineeParentInode <- lookupInodeParent scrutineeInode
            case maybeScrutineeParentInode of
              Nothing -> return False
              Just scrutineeParentInode ->
                getIsAncestorOf potentialAncestorInode scrutineeParentInode
      
      visit :: [Inode] -> IO [Inode]
      visit [] = return []
      visit (firstInode:rest) = do
        (firstInode, rest)
          <- foldM (\(firstInode, okayInodesSoFar) foundInode -> do
                      isAncestor <- getIsAncestorOf firstInode foundInode
                      if isAncestor
                        then return (firstInode, okayInodesSoFar)
                        else do
                          isDescendant <- getIsAncestorOf foundInode firstInode
                          if isDescendant
                            then return (foundInode, okayInodesSoFar)
                            else return (firstInode,
                                         foundInode : okayInodesSoFar))
                   (firstInode, [])
                   rest
        rest <- visit rest
        return $ firstInode : rest
  
  visit inodes


inodeValidateDrop
    :: Inode
    -> DragInformation
    -> IO (Maybe (Inode, Maybe Word64, DragOperation))
inodeValidateDrop prospectiveTargetInode dragInformation = do
  let project = inodeProject prospectiveTargetInode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar Nothing $ do
    getDropTargetAndOperation prospectiveTargetInode dragInformation


inodeAcceptDrop :: Inode -> DragInformation -> IO Bool
inodeAcceptDrop prospectiveTargetInode dragInformation = do
  let project = inodeProject prospectiveTargetInode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar False $ do
    maybeDropTargetAndOperation <-
      getDropTargetAndOperation prospectiveTargetInode dragInformation
    case maybeDropTargetAndOperation of
      Just (targetInode, _, dragOperation) -> do
        case dragInformation of
          InodeDragInformation { } -> do
            mapM (\draggedInode -> do
                    draggedInodeInformation <-
                      lookupInodeInformation draggedInode
                    let name = inodeInformationName draggedInodeInformation
                    recordMovedInode draggedInode name targetInode)
                 $ inodeDragInformationInodes dragInformation
            noteBrowserItemsChangedInAllBrowserWindows project
            return True
          _ -> return False
      _ -> throwIO $(internalFailure)


inodesKernel :: [Inode] -> IO [Inode]
inodesKernel inodes = do
  case inodes of
    [] -> return []
    (firstInode:_) -> do
      let project = inodeProject firstInode
          applicationStateMVar = projectApplicationState project
      catchTe applicationStateMVar [] $ do
        getInodesKernel inodes


getDropTargetAndOperation
    :: Inode
    -> DragInformation
    -> IO (Maybe (Inode, Maybe Word64, DragOperation))
getDropTargetAndOperation prospectiveTargetInode dragInformation = do
    case dragInformation of
      InodeDragInformation { } -> do
        let draggedInodes = inodeDragInformationInodes dragInformation
            sameProject =
              computeSameProject prospectiveTargetInode draggedInodes
            allowedDragOperations =
              dragInformationAllowedDragOperations dragInformation
        maybeTargetInode <-
          getDragTargetInode prospectiveTargetInode draggedInodes sameProject
        let maybeDragOperation =
              computeDragOperation allowedDragOperations sameProject
        case (maybeTargetInode, maybeDragOperation) of
          (Just targetInode, Just dragOperation) ->
            return $ Just (targetInode, Nothing, dragOperation)
          (Nothing, _) -> throwIO $(internalFailure)
          _ -> return Nothing
      _ -> return Nothing


computeSameProject :: Inode -> [Inode] -> Bool
computeSameProject prospectiveTargetInode draggedInodes =
  let targetProject = inodeProject prospectiveTargetInode
      sameProject = all (\draggedInode ->
                           let draggedProject = inodeProject draggedInode
                           in projectID draggedProject
                              == projectID targetProject)
                        draggedInodes
  in sameProject


getDragTargetInode :: Inode -> [Inode] -> Bool -> IO (Maybe Inode)
getDragTargetInode prospectiveTargetInode draggedInodes sameProject = do
  let considerInode prospectiveTargetInode = do
        inodeInformation <-
          lookupInodeInformation prospectiveTargetInode
        case inodeInformationKind inodeInformation of
          InodeKindDirectory -> do
            if sameProject
               && any (\draggedInode ->
                         inodeID draggedInode
                         == inodeID prospectiveTargetInode)
                      draggedInodes
              then do
                maybeParentInode <- lookupInodeParent prospectiveTargetInode
                case maybeParentInode of
                  Just parentInode -> considerInode parentInode
                  Nothing -> return Nothing
              else return $ Just prospectiveTargetInode
          _ -> do
           maybeParentInode <- lookupInodeParent prospectiveTargetInode
           case maybeParentInode of
             Just parentInode -> considerInode parentInode
             Nothing -> return Nothing
  considerInode prospectiveTargetInode


computeDragOperation :: Set DragOperation -> Bool -> Maybe DragOperation
computeDragOperation allowedDragOperations sameProject =
  let dragOperationAllowed dragOperation =
        (Set.member dragOperation allowedDragOperations)
        || (Set.member DragOperationGeneric allowedDragOperations)
      maybeDragOperation =
        foldl' (\maybeResult (condition, operation) ->
                  case maybeResult of
                    Just _ -> maybeResult
                    Nothing -> if condition
                                  && dragOperationAllowed operation
                                 then Just operation
                                 else Nothing)
               Nothing
               [(sameProject, DragOperationMove),
                (True, DragOperationCopy)]
  in maybeDragOperation


browserWindowDraggingSourceIntraApplicationOperations :: Set DragOperation
browserWindowDraggingSourceIntraApplicationOperations =
  Set.union browserWindowDraggingSourceInterApplicationOperations
            $ Set.fromList [DragOperationLink,
                            DragOperationMove]


browserWindowDraggingSourceInterApplicationOperations :: Set DragOperation
browserWindowDraggingSourceInterApplicationOperations =
  Set.fromList [DragOperationCopy,
                DragOperationGeneric,
                DragOperationDelete]
