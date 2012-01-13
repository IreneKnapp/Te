{-# LANGUAGE TemplateHaskell #-}
module Te.LowLevel.Database
  (Database,
   newProjectDatabase,
   closeProjectDatabase,
   attachFileToProjectDatabase,
   getProjectDatabaseSchemaVersion,
   initProjectDatabaseSchema,
   lookupProjectRoot,
   recordNewInode,
   recordDeletedInode,
   recordModifiedInode,
   recordMovedInode,
   lookupInodeParent,
   lookupInodeChildren,
   lookupInodeChildCount,
   lookupInodeChild,
   lookupInodeInformation,
   lookupWindowKind,
   recordNewBrowserWindow,
   lookupBrowserWindowRoot,
   recordBrowserItemExpanded,
   lookupBrowserItemExpanded,
   recordNewDocumentWindow,
   lookupDocumentWindowInode)
  where

import Control.Exception
import Data.Word
import Database.SQLite3 (Database, StepResult(..), SQLData(..))
import qualified Database.SQLite3 as SQL
import Prelude hiding (catch)

import Data.ByteSize
import Data.SQLable
import Data.Timestamp
import Te.LowLevel.Exceptions
import Te.LowLevel.Identifiers
import Te.Types


query :: Database -> String -> [SQLData] -> IO [[SQLData]]
query database queryText bindings = do
  statement <- SQL.prepare database queryText
  catch (do
          SQL.bind statement bindings
          result <- query' statement
          SQL.finalize statement
          return result)
        (\e -> do
          catch (SQL.finalize statement)
                (\e -> do
                   return (e :: SomeException)
                   return ())
          throwIO (e :: SomeException))
  where query' statement = do
          stepResult <- SQL.step statement
          case stepResult of
            Row -> do
              row <- SQL.columns statement
              remainder <- query' statement
              return $ [row] ++ remainder
            Done -> return []


newProjectDatabase :: IO Database
newProjectDatabase = do
  SQL.open ":memory:"


closeProjectDatabase :: Database -> IO ()
closeProjectDatabase database = do
  SQL.close database


attachFileToProjectDatabase :: Database -> FilePath -> IO Bool
attachFileToProjectDatabase database filePath = do
  catch (do
          _ <- query database
                     "ATTACH DATABASE ? AS FILE"
                     [SQLText filePath]
          return True)
        (\e -> do
          return (e :: SomeException)
          return False)


getProjectDatabaseSchemaVersion :: Database -> IO (Maybe Int)
getProjectDatabaseSchemaVersion database = do
  rows <- query database
                "SELECT program, schema_version FROM settings"
                []
  return $ case rows of
             [[SQLText program, SQLInteger schemaVersion]]
               | program == "com.dankna.te" ->
                Just $ fromIntegral schemaVersion
             _ -> Nothing


initProjectDatabaseSchema :: Database -> IO ()
initProjectDatabaseSchema database = do
  timestamp <- getTimestamp
  let sqlTimestamp = toSQL timestamp
  rootInodeID <- newInodeID
  _ <- query database
             (  "PRAGMA fullfsync = 1")
             []
  _ <- query database
             (  "PRAGMA foreign_keys = 1")
             []
  _ <- query database
             (  "PRAGMA encoding = 'UTF-8'")
             []
  _ <- query database
             (  "CREATE TABLE settings ("
             ++ "  program TEXT,"
             ++ "  schema_version INTEGER,"
             ++ "  creation_timestamp INTEGER,"
             ++ "  modification_timestamp INTEGER"
             ++ ");")
             []
  _ <- query database
             (  "CREATE TABLE inodes (\n"
             ++ "  id BLOB PRIMARY KEY,\n"
             ++ "  name TEXT,\n"
             ++ "  cased_name TEXT,\n"
             ++ "  parent BLOB REFERENCES inodes(id)\n"
             ++ "              ON DELETE CASCADE\n"
             ++ "              ON UPDATE CASCADE,\n"
             ++ "  kind TEXT,\n"
             ++ "  size INTEGER,\n"
             ++ "  creation_timestamp INTEGER,"
             ++ "  modification_timestamp INTEGER,"
             ++ "  CONSTRAINT inode_name_unique_within_parent\n"
             ++ "  UNIQUE (parent, name),\n"
             ++ "  CONSTRAINT inode_id_valid\n"
             ++ "  CHECK ((typeof(id) = 'blob') AND (length(id) = 16)),\n"
             ++ "  CONSTRAINT inode_name_valid\n"
             ++ "  CHECK ((typeof(name) = 'text')\n"
             ++ "         AND (name NOT LIKE '%/%')),\n"
             ++ "  CONSTRAINT inode_cased_name_valid\n"
             ++ "  CHECK (name = upper(cased_name)),\n"
             ++ "  CONSTRAINT inode_kind_valid\n"
             ++ "  CHECK ((typeof(kind) = 'text') AND (kind != '')),\n"
             ++ "  CONSTRAINT inode_creation_timestamp_valid\n"
             ++ "  CHECK ((typeof(creation_timestamp) = 'integer')\n"
             ++ "         AND (creation_timestamp > 0)),\n"
             ++ "  CONSTRAINT inode_modification_timestamp_valid\n"
             ++ "  CHECK ((typeof(modification_timestamp) = 'integer')\n"
             ++ "         AND (modification_timestamp > 0)),\n"
             ++ "  CONSTRAINT inode_root_is_directory\n"
             ++ "  CHECK ((parent IS NOT NULL)\n"
             ++ "         OR ((kind = 'directory') AND (name = ''))),\n"
             ++ "  CONSTRAINT inode_sizes_where_appropriate\n"
             ++ "  CHECK (((kind = 'directory') AND (size IS NULL))\n"
             ++ "         OR ((kind != 'directory')\n"
             ++ "             AND (typeof(size) = 'integer')))\n"
             ++ ")")
             []
  _ <- query database
             (  "CREATE TABLE windows (\n"
             ++ "  id BLOB PRIMARY KEY,\n"
             ++ "  kind TEXT CHECK ((typeof(kind) = 'text')\n"
             ++ "                   AND ((kind = 'browser')\n"
             ++ "                        OR (kind = 'document'))),\n"
             ++ "  top INTEGER CHECK ((top IS NULL)\n"
             ++ "                     OR (typeof(top) = 'integer')),\n"
             ++ "  left INTEGER CHECK ((left IS NULL)\n"
             ++ "                      OR (typeof(left) = 'integer')),\n"
             ++ "  height INTEGER CHECK ((height IS NULL)\n"
             ++ "                        OR ((typeof(height) = 'integer')\n"
             ++ "                            AND (height > 0))),\n"
             ++ "  width INTEGER CHECK ((width IS NULL)\n"
             ++ "                       OR ((typeof(width) = 'integer')\n"
             ++ "                           AND (width > 0))),\n"
             ++ "  CONSTRAINT window_id_valid\n"
             ++ "  CHECK ((typeof(id) = 'blob') AND (length(id) = 16)),\n"
             ++ "  CONSTRAINT window_frames_all_or_none\n"
             ++ "  CHECK (((top IS NULL)\n"
             ++ "          AND (left IS NULL)\n"
             ++ "          AND (height IS NULL)\n"
             ++ "          AND (width IS NULL))\n"
             ++ "         OR\n"
             ++ "         ((top IS NOT NULL)\n"
             ++ "          AND (left IS NOT NULL)\n"
             ++ "          AND (height IS NOT NULL)\n"
             ++ "          AND (width IS NOT NULL)))\n"
             ++ ")")
             []
  _ <- query database
             (  "CREATE TABLE browser_windows (\n"
             ++ "  id BLOB PRIMARY KEY REFERENCES windows(id)\n"
             ++ "                      ON DELETE CASCADE\n"
             ++ "                      ON UPDATE CASCADE,\n"
             ++ "  root_inode BLOB REFERENCES inodes(id)\n"
             ++ "                  ON DELETE SET NULL\n"
             ++ "                  ON UPDATE CASCADE\n"
             ++ ")")
             []
  _ <- query database
             (  "CREATE TABLE browser_items (\n"
             ++ "  inode BLOB REFERENCES inodes(id)\n"
             ++ "             ON DELETE CASCADE\n"
             ++ "             ON UPDATE CASCADE,\n"
             ++ "  browser_window BLOB REFERENCES browser_windows(id)\n"
             ++ "                      ON DELETE CASCADE\n"
             ++ "                      ON UPDATE CASCADE,\n"
             ++ "  expanded INTEGER CHECK ((typeof(expanded) = 'integer')\n"
             ++ "                          AND ((expanded = 0)\n"
             ++ "                               OR (expanded = 1))),\n"
             ++ "  PRIMARY KEY (inode, browser_window)\n"
             ++ ")")
             []
  _ <- query database
             (  "CREATE TABLE document_windows (\n"
             ++ "  id BLOB PRIMARY KEY REFERENCES windows(id)\n"
             ++ "                      ON DELETE CASCADE\n"
             ++ "                      ON UPDATE CASCADE,\n"
             ++ "  inode BLOB REFERENCES inodes(id)\n"
             ++ "             ON DELETE SET NULL\n"
             ++ "             ON UPDATE CASCADE\n"
             ++ ")")
             []
  _ <- query database
             (  "INSERT INTO settings (program, schema_version,\n"
             ++ "creation_timestamp, modification_timestamp) VALUES\n"
             ++ "('com.dankna.te', 1, ?, ?);")
             [sqlTimestamp,
              sqlTimestamp]
  _ <- query database
             (  "INSERT INTO inodes (id, name, cased_name, parent,\n"
             ++ "kind, size, creation_timestamp, modification_timestamp)\n"
             ++ "VALUES (?, '', '', NULL, 'directory', NULL, ?, ?);")
             [toSQL rootInodeID,
              sqlTimestamp,
              sqlTimestamp]
  return ()


lookupProjectRoot :: Project -> IO Inode
lookupProjectRoot project = do
  let database = projectDatabase project
  rows <- query database "SELECT id FROM inodes WHERE parent IS NULL" []
  case rows of
    [[sqlRootInodeID]] ->
      case fromSQL sqlRootInodeID of
        Just rootInodeID -> return $ Inode {
                                         inodeID = rootInodeID,
                                         inodeProject = project
                                       }
        Nothing -> throwIO $(internalFailure)
    _ -> throwIO $(internalFailure)


recordNewInode
    :: Inode -> Inode -> String -> InodeKind -> Maybe ByteSize -> IO ()
recordNewInode inode parentInode name inodeKind maybeSize = do
  let project = inodeProject inode
      database = projectDatabase project
  timestamp <- getTimestamp
  let sqlTimestamp = toSQL timestamp
      sqlInodeID = toSQL $ inodeID inode
      sqlParentInodeID = toSQL $ inodeID parentInode
  _ <- query database
             (  "INSERT INTO inodes (id, name, cased_name, parent, kind,\n"
             ++ "size, creation_timestamp, modification_timestamp) VALUES\n"
             ++ "(?, upper(?), ?, ?, ?, ?, ?, ?)")
             [sqlInodeID,
              SQLText name,
              SQLText name,
              sqlParentInodeID,
              case inodeKind of
                InodeKindDirectory -> SQLText "directory"
                InodeKindHaskell -> SQLText "haskell",
              case maybeSize of
                Nothing -> SQLNull
                Just size -> toSQL size,
              sqlTimestamp,
              sqlTimestamp]
  _ <- query database
             (  "UPDATE inodes SET modification_timestamp = ? WHERE id = ?")
             [sqlTimestamp,
              sqlParentInodeID]
  return ()


recordDeletedInode :: Inode -> IO ()
recordDeletedInode inode = do
  let project = inodeProject inode
      database = projectDatabase project
  timestamp <- getTimestamp
  maybeParentInode <- lookupInodeParent inode
  case maybeParentInode of
    Just parentInode -> do
      let sqlTimestamp = toSQL timestamp
          sqlInodeID = toSQL $ inodeID inode
          sqlParentInodeID = toSQL $ inodeID parentInode
      _ <- query database
                 (  "DELETE FROM inodes WHERE id = ?")
                 [sqlInodeID]
      _ <- query database
                 (  "UPDATE inodes SET modification_timestamp = ?"
                 ++ "WHERE id = ?")
                 [sqlTimestamp,
                  sqlParentInodeID]
      return ()
    Nothing -> throwIO $(internalFailure)


recordModifiedInode :: Inode -> IO ()
recordModifiedInode inode = do
  let project = inodeProject inode
      database = projectDatabase project
  timestamp <- getTimestamp
  _ <- query database
             (  "UPDATE inodes SET modification_timestamp = ? WHERE id = ?")
             [toSQL timestamp,
              toSQL $ inodeID inode]
  return ()


recordMovedInode :: Inode -> String -> Inode -> IO ()
recordMovedInode inode newName newParentInode = do
  let project = inodeProject inode
      database = projectDatabase project
  maybeOldParentInode <- lookupInodeParent inode
  case maybeOldParentInode of
    Just oldParentInode -> do
      timestamp <- getTimestamp
      let sqlTimestamp = toSQL timestamp
          sqlNewName = SQLText newName
          sqlInodeID = toSQL $ inodeID inode
          sqlOldParentInodeID = toSQL $ inodeID oldParentInode
          sqlNewParentInodeID = toSQL $ inodeID newParentInode
      _ <- query database
                 "UPDATE inodes SET modification_timestamp = ? WHERE id = ?"
                 [sqlTimestamp,
                  sqlOldParentInodeID]
      _ <- query database
                 (  "UPDATE inodes SET name = upper(?), cased_name = ?,\n"
                 ++ " parent = ? WHERE id = ?")
                 [sqlNewName,
                  sqlNewName,
                  sqlNewParentInodeID,
                  sqlInodeID]
      _ <- query database
                 "UPDATE inodes SET modification_timestamp = ? WHERE id = ?"
                 [sqlTimestamp,
                  sqlNewParentInodeID]
      return ()


lookupInodeParent :: Inode -> IO (Maybe Inode)
lookupInodeParent inode = do
  let project = inodeProject inode
      database = projectDatabase project
  rows <- query database
                "SELECT parent FROM inodes WHERE id = ?"
                [toSQL $ inodeID inode]
  case rows of
    [[sqlInodeParentID]] ->
      case fromSQL sqlInodeParentID of
        Just inodeParentID -> return $ Just $ Inode {
                                                  inodeID = inodeParentID,
                                                  inodeProject = project
                                                }
        Nothing -> return Nothing
    _ -> return Nothing


lookupInodeChildren :: Inode -> IO [Inode]
lookupInodeChildren inode = do
  let project = inodeProject inode
      database = projectDatabase project
  rows <- query database
                (  "SELECT id FROM inodes WHERE parent = ?\n"
                ++ "ORDER BY cased_name ASC")
                [toSQL $ inodeID inode]
  mapM (\row -> do
          case row of
            [sqlChildInodeID] ->
              case fromSQL sqlChildInodeID of
                Just childInodeID -> return $ Inode {
                                                  inodeID = childInodeID,
                                                  inodeProject = project
                                                }
                Nothing -> throwIO $(internalFailure)
            _ -> throwIO $(internalFailure))
       rows


lookupInodeChildCount :: Inode -> IO Word64
lookupInodeChildCount inode = do
  let project = inodeProject inode
      database = projectDatabase project
  rows <- query database
                "SELECT count(*) FROM inodes WHERE parent = ?"
                [toSQL $ inodeID inode]
  case rows of
    [[SQLInteger count]] -> return $ fromIntegral count
    _ -> throwIO $(internalFailure)


lookupInodeChild :: Inode -> Word64 -> IO Inode
lookupInodeChild inode index = do
  let project = inodeProject inode
      database = projectDatabase project
  rows <- query database
                (  "SELECT id FROM inodes WHERE parent = ?\n"
                ++ "ORDER BY cased_name ASC LIMIT 1 OFFSET ?")
                [toSQL $ inodeID inode,
                 SQLInteger $ fromIntegral index]
  case rows of
    [[sqlChildInodeID]] ->
      case fromSQL sqlChildInodeID of
        Just childInodeID -> return $ Inode {
                                          inodeID = childInodeID,
                                          inodeProject = project
                                        }
        Nothing -> throwIO $(internalFailure)
    _ -> throwIO $(internalFailure)


lookupInodeInformation :: Inode -> IO InodeInformation
lookupInodeInformation inode = do
  let project = inodeProject inode
      database = projectDatabase project
  rows <- query database
                (  "SELECT cased_name, kind, size, creation_timestamp,\n"
                ++ "modification_timestamp FROM inodes WHERE id = ?")
                [toSQL $ inodeID inode]
  case rows of
    [[SQLText name, SQLText kindString, sqlMaybeSize,
      SQLInteger creationTimestamp, SQLInteger modificationTimestamp]] -> do
      let kind = case kindString of
                   "directory" -> InodeKindDirectory
                   "haskell" -> InodeKindHaskell
          maybeSize = case sqlMaybeSize of
                        SQLNull -> Nothing
                        _ -> fromSQL sqlMaybeSize
      return $ InodeInformation {
                   inodeInformationName = name,
                   inodeInformationKind = kind,
                   inodeInformationSize = maybeSize,
                   inodeInformationCreationTimestamp =
                     fromIntegral creationTimestamp,
                   inodeInformationModificationTimestamp =
                     fromIntegral modificationTimestamp
                 }
    _ -> throwIO $(internalFailure)


lookupWindowKind :: Window -> IO WindowKind
lookupWindowKind window = do
  let project = windowProject window
      database = projectDatabase project
  rows <- query database
                (  "SELECT kind FROM windows WHERE id = ?")
                [toSQL $ windowID window]
  case rows of
    [[SQLText kindString]] ->
      case lookup kindString [("browser", WindowKindBrowser),
                              ("document", WindowKindDocument)] of
        Just kind -> return kind
        Nothing -> throwIO $(internalFailure)
    _ -> throwIO $(internalFailure)


recordNewBrowserWindow :: BrowserWindow -> Inode -> IO ()
recordNewBrowserWindow browserWindow browserWindowRootInode = do
  let project = browserWindowProject browserWindow
      database = projectDatabase project
  _ <- query database
             (  "INSERT INTO windows (id, kind, top, left, height, width)\n"
             ++ "VALUES (?, 'browser', NULL, NULL, NULL, NULL)")
             [toSQL $ browserWindowID browserWindow]
  _ <- query database
             (  "INSERT INTO browser_windows (id, root_inode)\n"
             ++ "VALUES (?, ?)")
             [toSQL $ browserWindowID browserWindow,
              toSQL $ inodeID browserWindowRootInode]
  return ()


lookupBrowserWindowRoot :: BrowserWindow -> IO Inode
lookupBrowserWindowRoot browserWindow = do
  let project = browserWindowProject browserWindow
      database = projectDatabase project
  rows <- query database
                (  "SELECT root_inode FROM browser_windows\n"
                ++ "WHERE id = ?")
                [toSQL $ browserWindowID browserWindow]
  case rows of
    [[sqlRootInodeID]] ->
      case fromSQL sqlRootInodeID of
        Just rootInodeID -> return $ Inode {
                                         inodeID = rootInodeID,
                                         inodeProject = project
                                       }
        Nothing -> throwIO $(internalFailure)
    _ -> throwIO $(internalFailure)


recordBrowserItemExpanded :: BrowserItem -> Bool -> IO ()
recordBrowserItemExpanded browserItem expanded = do
  let inode = browserItemInode browserItem
      browserWindow = browserItemBrowserWindow browserItem
      project = browserWindowProject browserWindow
      database = projectDatabase project
  _ <- query database
             (  "INSERT OR REPLACE INTO browser_items\n"
             ++ "(inode, browser_window, expanded) VALUES (?, ?, ?)")
             [toSQL $ inodeID inode,
              toSQL $ browserWindowID browserWindow,
              SQLInteger $ if expanded
                            then 1
                            else 0]
  return ()


lookupBrowserItemExpanded :: BrowserItem -> IO Bool
lookupBrowserItemExpanded browserItem = do
  let inode = browserItemInode browserItem
      browserWindow = browserItemBrowserWindow browserItem
      project = browserWindowProject browserWindow
      database = projectDatabase project
  rows <- query database
                (  "SELECT expanded FROM browser_items\n"
                ++ "WHERE (inode = ?) AND (browser_window = ?)")
                [toSQL $ inodeID inode,
                 toSQL $ browserWindowID browserWindow]
  case rows of
    [[SQLInteger 0]] -> return False
    [[SQLInteger _]] -> return True
    [] -> return False
    _ -> throwIO $(internalFailure)


recordNewDocumentWindow :: DocumentWindow -> Inode -> IO ()
recordNewDocumentWindow documentWindow documentWindowInode = do
  let project = documentWindowProject documentWindow
      database = projectDatabase project
  _ <- query database
             (  "INSERT INTO windows (id, kind, top, left, height, width)\n"
             ++ "VALUES (?, 'document', NULL, NULL, NULL, NULL)")
             [toSQL $ documentWindowID documentWindow]
  _ <- query database
             (  "INSERT INTO document_windows (id, inode)\n"
             ++ "VALUES (?, ?)")
             [toSQL $ documentWindowID documentWindow,
              toSQL $ inodeID documentWindowInode]
  return ()


lookupDocumentWindowInode :: DocumentWindow -> IO Inode
lookupDocumentWindowInode documentWindow = do
  let project = documentWindowProject documentWindow
      database = projectDatabase project
  rows <- query database
                (  "SELECT inode FROM document_windows\n"
                ++ "WHERE id = ?")
                [toSQL $ documentWindowID documentWindow]
  case rows of
    [[sqlInodeID]] ->
      case fromSQL sqlInodeID of
        Just inodeID' -> return $ Inode {
                                      inodeID = inodeID',
                                      inodeProject = project
                                    }
        Nothing -> throwIO $(internalFailure)
    _ -> throwIO $(internalFailure)
