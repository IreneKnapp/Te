{-# LANGUAGE TemplateHaskell #-}
module Te
  (Timestamp(..),
   FrontEndCallbacks(..),
   ApplicationState,
   Inode, inodeID,
   BrowserWindow, browserWindowID,
   BrowserItem(..),
   catchTe,
   frontEndInternalFailure,
   versionString,
   timestampToString,
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
   browserWindowClose,
   browserWindowTitle,
   browserWindowTitleIcon,
   browserWindowRoot,
   browserItemNewFolderInside,
   browserItemNewFileInside,
   browserItemExpanded,
   browserItemSetExpanded,
   inodeExpandable,
   inodeChildCount,
   inodeChild,
   inodeName,
   inodeKind,
   inodeSize,
   inodeCreationTimestamp,
   inodeModificationTimestamp,
   inodeRename,
   inodesDelete,
   inodeValidateDrop,
   inodeAcceptDrop)
  where

import Control.Concurrent.MVar
import Control.Exception
import Data.Char
import Data.Digest.Murmur64
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.UUID
import Data.Version
import Data.Word
import System.Directory
import System.FilePath
import Prelude hiding (catch)

import Data.Timestamp
import Te.Database
import Te.Exceptions
import Te.FrontEndCallbacks
import Te.Identifiers
import Te.Types
import Paths_te


catchTe :: MVar ApplicationState -> a -> IO a -> IO a
catchTe applicationStateMVar default' action = do
  catch (action)
        (\e -> do
           applicationState <- readMVar applicationStateMVar
           let messageString = show (e :: TeException)
               detailsString = exceptionDetails e
               callbacks = applicationStateFrontEndCallbacks applicationState
               callback = frontEndCallbacksException callbacks
           callback messageString detailsString
           return default')


frontEndInternalFailure :: MVar ApplicationState -> String -> Word64 -> IO ()
frontEndInternalFailure applicationStateMVar filename lineNumber =
  catchTe applicationStateMVar () $ do
    let lineNumber' = fromIntegral lineNumber
    throwIO $ TeExceptionInternal filename lineNumber'


versionString :: String
versionString = showVersion version


timestampToString :: Timestamp -> IO String
timestampToString timestamp = do
  describeTimestamp timestamp


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
    browserWindowsMVar <- newMVar Map.empty
    let newProject = Project {
                         projectID = projectID,
                         projectApplicationState = applicationStateMVar,
                         projectDatabase = database,
                         projectName = nameMVar,
                         projectFilePath = filePathMVar,
                         projectBrowserWindows = browserWindowsMVar
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
              browserWindowsMVar <- newMVar Map.empty
              let newProject = Project {
                                   projectID = projectID,
                                   projectApplicationState =
                                     applicationStateMVar,
                                   projectDatabase = database,
                                   projectName = nameMVar,
                                   projectFilePath = filePathMVar,
                                   projectBrowserWindows =
                                     browserWindowsMVar
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
  browserWindows <- readMVar $ projectBrowserWindows project
  if Map.null browserWindows
    then newBrowserWindow project
    else return ()


computeNextNumberedName :: String -> [String] -> Bool -> String
computeNextNumberedName prefix siblingNames caseSensitive =
  let prefix' :: String
      prefix' = caseFoldIfAppropriate prefix
      
      prefixWithSpace :: String
      prefixWithSpace = prefix' ++ " "
      
      caseFoldIfAppropriate :: String -> String
      caseFoldIfAppropriate string =
        if caseSensitive
          then string
          else map toUpper string
      
      nameNumber :: String -> Maybe Int
      nameNumber name =
        let name' = caseFoldIfAppropriate name
        in if name' == prefix'
             then Just 1
             else case stripPrefix prefixWithSpace name' of
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
               then prefix
               else prefixWithSpace ++ show number
  in name


getNextUntitledProjectName :: MVar ApplicationState -> IO String
getNextUntitledProjectName applicationStateMVar = do
  applicationState <- readMVar applicationStateMVar
  projectNames <- mapM (\project -> readMVar $ projectName project)
                       $ Map.elems $ applicationStateProjects applicationState
  return $ computeNextNumberedName "Untitled" projectNames True


computeNameForFilePath :: String -> String
computeNameForFilePath filePath =
  let fileName = takeFileName filePath
      fileNameWithoutExtension = if isSuffixOf ".te" fileName
                                   then dropExtension fileName
                                   else fileName
  in fileNameWithoutExtension


newBrowserWindow :: Project -> IO ()
newBrowserWindow project = do
  let applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    newBrowserWindowID <- newBrowserWindowID
    let newBrowserWindow' = BrowserWindow {
                                browserWindowID = newBrowserWindowID,
                                browserWindowProject = project
                              }
    browserWindows <- takeMVar $ projectBrowserWindows project
    let browserWindows' = Map.insert newBrowserWindowID
                                     newBrowserWindow'
                                     browserWindows
    putMVar (projectBrowserWindows project) browserWindows'
    rootInode <- lookupProjectRoot project
    recordNewBrowserWindow newBrowserWindow' rootInode
    noteNewBrowserWindow newBrowserWindow'


browserWindowClose :: BrowserWindow -> IO ()
browserWindowClose browserWindow = do
  let project = browserWindowProject browserWindow
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    noteDeletedBrowserWindow browserWindow
    projectClose project


browserWindowTitle :: BrowserWindow -> IO String
browserWindowTitle browserWindow = do
  let project = browserWindowProject browserWindow
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    projectName <- readMVar $ projectName project
    maybeFolderInode <- lookupBrowserWindowRoot browserWindow
    case maybeFolderInode of
      Just folderInode -> do
        rootInode <- lookupProjectRoot project
        if inodeID rootInode == inodeID folderInode
          then return projectName
          else do
            maybeFolderInodeInformation <- lookupInodeInformation folderInode
            case maybeFolderInodeInformation of
              Just folderInodeInformation -> do
                let folderName = inodeInformationName folderInodeInformation
                return $ projectName ++ " - " ++ folderName
              Nothing -> throwIO $(internalFailure)
      Nothing -> throwIO $(internalFailure)


browserWindowTitleIcon :: BrowserWindow -> IO String
browserWindowTitleIcon browserWindow = do
  let project = browserWindowProject browserWindow
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Project" $ do
    maybeInode <- lookupBrowserWindowRoot browserWindow
    case maybeInode of
      Just inode -> do
        rootInode <- lookupProjectRoot project
        if inodeID rootInode == inodeID inode
          then return "Project"
          else return "Folder"
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
    maybeInode <- lookupBrowserWindowRoot browserWindow
    case maybeInode of
      Just inode -> return $ BrowserItem {
                                 browserItemInode = inode,
                                 browserItemBrowserWindow = browserWindow
                               }
      Nothing -> throwIO $(internalFailure)


getChildInodeNames :: Inode -> IO [String]
getChildInodeNames inode = do
  childCount <- inodeChildCount inode
  let getChildInodes resultsSoFar index = do
        if index == childCount
          then return resultsSoFar
          else do
            maybeChildInode <- lookupInodeChild inode index
            case maybeChildInode of
              Just childInode ->
                getChildInodes (resultsSoFar ++ [childInode]) (index + 1)
              Nothing -> throwIO $(internalFailure)
  childInodes <- getChildInodes [] 0
  mapM (\inode -> do
          maybeInodeInformation <- lookupInodeInformation inode
          case maybeInodeInformation of
            Just inodeInformation ->
              return $ inodeInformationName inodeInformation
            Nothing -> throwIO $(internalFailure))
       childInodes


browserItemNewFolderInside :: BrowserItem -> IO ()
browserItemNewFolderInside parentBrowserItem = do
  let parentInode = browserItemInode parentBrowserItem
      project = inodeProject parentInode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    siblingNames <- getChildInodeNames parentInode
    let newName = computeNextNumberedName "New Folder" siblingNames False
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
    let newName = computeNextNumberedName "New Module.hs" siblingNames False
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


inodeExpandable :: Inode -> IO Bool
inodeExpandable inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar False $ do
    maybeInodeInformation <- lookupInodeInformation inode
    case maybeInodeInformation of
      Just inodeInformation -> do
        case inodeInformationKind inodeInformation of
          InodeKindDirectory -> return True
          _ -> return False
      Nothing -> throwIO $(internalFailure)


inodeChildCount :: Inode -> IO Word64
inodeChildCount inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar 0 $ do
    lookupInodeChildCount inode


inodeChild :: Inode -> Word64 -> IO (Maybe Inode)
inodeChild inode index = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar Nothing $ do
    lookupInodeChild inode index


inodeName :: Inode -> IO String
inodeName inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    maybeInodeInformation <- lookupInodeInformation inode
    case maybeInodeInformation of
      Just inodeInformation -> return $ inodeInformationName inodeInformation
      Nothing -> throwIO $(internalFailure)


inodeKind :: Inode -> IO String
inodeKind inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar "Unknown" $ do
    maybeInodeInformation <- lookupInodeInformation inode
    case maybeInodeInformation of
      Just inodeInformation -> do
        case inodeInformationKind inodeInformation of
          InodeKindDirectory -> return "Folder"
          InodeKindHaskell -> return "Haskell"
      Nothing -> throwIO $(internalFailure)


inodeSize :: Inode -> IO (Maybe Word64)
inodeSize inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar Nothing $ do
    maybeInodeInformation <- lookupInodeInformation inode
    case maybeInodeInformation of
      Just inodeInformation -> return $ inodeInformationSize inodeInformation
      Nothing -> throwIO $(internalFailure)


inodeCreationTimestamp :: Inode -> IO Timestamp
inodeCreationTimestamp inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar minBound $ do
    maybeInodeInformation <- lookupInodeInformation inode
    case maybeInodeInformation of
      Just inodeInformation ->
        return $ inodeInformationCreationTimestamp inodeInformation
      Nothing -> throwIO $(internalFailure)


inodeModificationTimestamp :: Inode -> IO Timestamp
inodeModificationTimestamp inode = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar minBound $ do
    maybeInodeInformation <- lookupInodeInformation inode
    case maybeInodeInformation of
      Just inodeInformation ->
        return $ inodeInformationModificationTimestamp inodeInformation
      Nothing -> throwIO $(internalFailure)


inodeRename :: Inode -> String -> IO ()
inodeRename inode newName = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    maybeParent <- lookupInodeParent inode
    case maybeParent of
      Just parent -> recordMovedInode inode newName parent
      Nothing -> throwIO $(internalFailure)


inodesDelete :: [Inode] -> IO ()
inodesDelete inodes = do
  case inodes of
    [] -> return ()
    (firstInode:_) -> do
      let project = inodeProject firstInode
          applicationStateMVar = projectApplicationState project
      catchTe applicationStateMVar () $ do
        mapM_ recordDeletedInode inodes


inodeValidateDrop
    :: Inode
    -> DragInformation
    -> IO (Maybe (Inode, DragOperation))
inodeValidateDrop inode dragInformation = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar Nothing $ do
    throwIO $(internalFailure) -- TODO


inodeAcceptDrop :: Inode -> DragInformation -> IO ()
inodeAcceptDrop inode dragInformation = do
  let project = inodeProject inode
      applicationStateMVar = projectApplicationState project
  catchTe applicationStateMVar () $ do
    throwIO $(internalFailure) -- TODO
