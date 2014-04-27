{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
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
   recordNewBrowserWindow,
   lookupBrowserWindowRoot,
   recordBrowserItemExpanded,
   lookupBrowserItemExpanded,
   recordNewDocumentWindow,
   lookupDocumentWindowInode)
  where

import Control.Exception
import Data.Text (Text)
import qualified Data.Text as Text
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


query :: Database -> Text -> [SQLData] -> IO [[SQLData]]
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


attachFileToProjectDatabase :: Database -> Text -> IO Bool
attachFileToProjectDatabase database filePath = do
  catch (do
          _ <- query database
                     (Text.intercalate "\n"
                       ["ATTACH DATABASE ? AS FILE"])
                     [SQLText filePath]
          return True)
        (\e -> do
          return (e :: SomeException)
          return False)


getProjectDatabaseSchemaVersion :: Database -> IO (Maybe Int)
getProjectDatabaseSchemaVersion database = do
  rows <- query database
                (Text.intercalate "\n"
                  ["SELECT program, schema_version FROM settings"])
                []
  return $ case rows of
             [[SQLText program, SQLInteger schemaVersion]]
               | program == "com.ireneknapp.te" ->
                Just $ fromIntegral schemaVersion
             _ -> Nothing


initProjectDatabaseSchema :: Database -> IO ()
initProjectDatabaseSchema database = do
  timestamp <- getTimestamp
  let sqlTimestamp = toSQL timestamp
  rootInodeID <- newInodeID
  _ <- query database
             (Text.intercalate "\n"
               ["PRAGMA fullfsync = 1"])
             []
  _ <- query database
             (Text.intercalate "\n"
               ["PRAGMA foreign_keys = 1"])
             []
  _ <- query database
             (Text.intercalate "\n"
               ["PRAGMA encoding = 'UTF-8'"])
             []
  _ <- query database
             (Text.intercalate "\n"
               ["CREATE TABLE settings (",
                "  program TEXT,",
                "  schema_version INTEGER,",
                "  creation_timestamp INTEGER,",
                "  modification_timestamp INTEGER",
                ");"])
             []
  _ <- query database
             (Text.intercalate "\n"
               ["CREATE TABLE inodes (",
                "  id BLOB PRIMARY KEY,",
                "  name TEXT,",
                "  cased_name TEXT,",
                "  parent BLOB REFERENCES inodes(id)",
                "              ON DELETE CASCADE",
                "              ON UPDATE CASCADE,",
                "  kind TEXT,",
                "  size INTEGER,",
                "  creation_timestamp INTEGER,",
                "  modification_timestamp INTEGER,",
                "  CONSTRAINT inode_name_unique_within_parent",
                "  UNIQUE (parent, name),",
                "  CONSTRAINT inode_id_valid",
                "  CHECK ((typeof(id) = 'blob') AND (length(id) = 16)),",
                "  CONSTRAINT inode_name_valid",
                "  CHECK ((typeof(name) = 'text')",
                "         AND (name NOT LIKE '%/%')),",
                "  CONSTRAINT inode_cased_name_valid",
                "  CHECK (name = upper(cased_name)),",
                "  CONSTRAINT inode_kind_valid",
                "  CHECK ((typeof(kind) = 'text') AND (kind != '')),",
                "  CONSTRAINT inode_creation_timestamp_valid",
                "  CHECK ((typeof(creation_timestamp) = 'integer')",
                "         AND (creation_timestamp > 0)),",
                "  CONSTRAINT inode_modification_timestamp_valid",
                "  CHECK ((typeof(modification_timestamp) = 'integer')",
                "         AND (modification_timestamp > 0)),",
                "  CONSTRAINT inode_root_is_directory",
                "  CHECK ((parent IS NOT NULL)",
                "         OR ((kind = 'directory') AND (name = ''))),",
                "  CONSTRAINT inode_sizes_where_appropriate",
                "  CHECK (((kind = 'directory') AND (size IS NULL))",
                "         OR ((kind != 'directory')",
                "             AND (typeof(size) = 'integer')))",
                ")"])
             []
  _ <- query database
             (Text.intercalate "\n"
               ["CREATE TABLE windows (",
                "  id BLOB PRIMARY KEY,",
                "  kind TEXT CHECK ((typeof(kind) = 'text')",
                "                   AND ((kind = 'browser')",
                "                        OR (kind = 'document'))),",
                "  top INTEGER CHECK ((top IS NULL)",
                "                     OR (typeof(top) = 'integer')),",
                "  left INTEGER CHECK ((left IS NULL)",
                "                      OR (typeof(left) = 'integer')),",
                "  height INTEGER CHECK ((height IS NULL)",
                "                        OR ((typeof(height) = 'integer')",
                "                            AND (height > 0))),",
                "  width INTEGER CHECK ((width IS NULL)",
                "                       OR ((typeof(width) = 'integer')",
                "                           AND (width > 0))),",
                "  CONSTRAINT window_id_valid",
                "  CHECK ((typeof(id) = 'blob') AND (length(id) = 16)),",
                "  CONSTRAINT window_frames_all_or_none",
                "  CHECK (((top IS NULL)",
                "          AND (left IS NULL)",
                "          AND (height IS NULL)",
                "          AND (width IS NULL))",
                "         OR",
                "         ((top IS NOT NULL)",
                "          AND (left IS NOT NULL)",
                "          AND (height IS NOT NULL)",
                "          AND (width IS NOT NULL)))",
                ")"])
             []
  _ <- query database
             (Text.intercalate "\n"
               ["CREATE TABLE browser_windows (",
                "  id BLOB PRIMARY KEY REFERENCES windows(id)",
                "                      ON DELETE CASCADE",
                "                      ON UPDATE CASCADE,",
                "  root_inode BLOB REFERENCES inodes(id)",
                "                  ON DELETE SET NULL",
                "                  ON UPDATE CASCADE",
                ")"])
             []
  _ <- query database
             (Text.intercalate "\n"
               ["CREATE TABLE browser_items (",
                "  inode BLOB REFERENCES inodes(id)",
                "             ON DELETE CASCADE",
                "             ON UPDATE CASCADE,",
                "  browser_window BLOB REFERENCES browser_windows(id)",
                "                      ON DELETE CASCADE",
                "                      ON UPDATE CASCADE,",
                "  expanded INTEGER CHECK ((typeof(expanded) = 'integer')",
                "                          AND ((expanded = 0)",
                "                               OR (expanded = 1))),",
                "  PRIMARY KEY (inode, browser_window)",
                ")"])
             []
  _ <- query database
             (Text.intercalate "\n"
               ["CREATE TABLE document_windows (",
                "  id BLOB PRIMARY KEY REFERENCES windows(id)",
                "                      ON DELETE CASCADE",
                "                      ON UPDATE CASCADE,",
                "  inode BLOB REFERENCES inodes(id)",
                "             ON DELETE SET NULL",
                "             ON UPDATE CASCADE",
                ")"])
             []
  _ <- query database
             (Text.intercalate "\n"
               ["INSERT INTO settings (program, schema_version,",
                "creation_timestamp, modification_timestamp) VALUES",
                "('com.ireneknapp.te', 1, ?, ?);"])
             [sqlTimestamp,
              sqlTimestamp]
  _ <- query database
             (Text.intercalate "\n"
               ["INSERT INTO inodes (id, name, cased_name, parent,",
                "kind, size, creation_timestamp, modification_timestamp)",
                "VALUES (?, '', '', NULL, 'directory', NULL, ?, ?);"])
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
    :: Inode -> Inode -> Text -> InodeType -> Maybe ByteSize -> IO ()
recordNewInode inode parentInode name inodeKind maybeSize = do
  let project = inodeProject inode
      database = projectDatabase project
  timestamp <- getTimestamp
  let sqlTimestamp = toSQL timestamp
      sqlInodeID = toSQL $ inodeID inode
      sqlParentInodeID = toSQL $ inodeID parentInode
  _ <- query database
             (Text.intercalate "\n"
               ["INSERT INTO inodes (id, name, cased_name, parent, kind,",
                "size, creation_timestamp, modification_timestamp) VALUES",
                "(?, upper(?), ?, ?, ?, ?, ?, ?)"])
             [sqlInodeID,
              SQLText name,
              SQLText name,
              sqlParentInodeID,
              case inodeKind of
                DirectoryInodeType -> SQLText "directory"
                HaskellInodeType -> SQLText "haskell",
              case maybeSize of
                Nothing -> SQLNull
                Just size -> toSQL size,
              sqlTimestamp,
              sqlTimestamp]
  _ <- query database
             (Text.intercalate "\n"
               ["UPDATE inodes SET modification_timestamp = ? WHERE id = ?"])
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
                 (Text.intercalate "\n"
                   ["DELETE FROM inodes WHERE id = ?"])
                 [sqlInodeID]
      _ <- query database
                 (Text.intercalate "\n"
                   ["UPDATE inodes SET modification_timestamp = ?",
                    "WHERE id = ?"])
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
             (Text.intercalate "\n"
               ["UPDATE inodes SET modification_timestamp = ? WHERE id = ?"])
             [toSQL timestamp,
              toSQL $ inodeID inode]
  return ()


recordMovedInode :: Inode -> Text -> Inode -> IO ()
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
                 (Text.intercalate "\n"
                   ["UPDATE inodes SET modification_timestamp = ?",
                    "WHERE id = ?"])
                 [sqlTimestamp,
                  sqlOldParentInodeID]
      _ <- query database
                 (Text.intercalate "\n"
                   ["UPDATE inodes SET name = upper(?), cased_name = ?,",
                    " parent = ? WHERE id = ?"])
                 [sqlNewName,
                  sqlNewName,
                  sqlNewParentInodeID,
                  sqlInodeID]
      _ <- query database
                 (Text.intercalate "\n"
                   ["UPDATE inodes SET modification_timestamp = ?",
                    "WHERE id = ?"])
                 [sqlTimestamp,
                  sqlNewParentInodeID]
      return ()


lookupInodeParent :: Inode -> IO (Maybe Inode)
lookupInodeParent inode = do
  let project = inodeProject inode
      database = projectDatabase project
  rows <- query database
                (Text.intercalate "\n"
                  ["SELECT parent FROM inodes WHERE id = ?"])
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
                (Text.intercalate "\n"
                  ["SELECT id FROM inodes WHERE parent = ?",
                   "ORDER BY cased_name ASC"])
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
                (Text.intercalate "\n"
                  ["SELECT count(*) FROM inodes WHERE parent = ?"])
                [toSQL $ inodeID inode]
  case rows of
    [[SQLInteger count]] -> return $ fromIntegral count
    _ -> throwIO $(internalFailure)


lookupInodeChild :: Inode -> Word64 -> IO Inode
lookupInodeChild inode index = do
  let project = inodeProject inode
      database = projectDatabase project
  rows <- query database
                (Text.intercalate "\n"
                  ["SELECT id FROM inodes WHERE parent = ?",
                   "ORDER BY cased_name ASC LIMIT 1 OFFSET ?"])
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
                (Text.intercalate "\n"
                  ["SELECT cased_name, kind, size, creation_timestamp,",
                   "modification_timestamp FROM inodes WHERE id = ?"])
                [toSQL $ inodeID inode]
  case rows of
    [[SQLText name, SQLText kindString, sqlMaybeSize,
      SQLInteger creationTimestamp, SQLInteger modificationTimestamp]] -> do
      let kind = case kindString of
                   "directory" -> DirectoryInodeType
                   "haskell" -> HaskellInodeType
          maybeSize = case sqlMaybeSize of
                        SQLNull -> Nothing
                        _ -> fromSQL sqlMaybeSize
      return $ InodeInformation {
                   inodeInformationName = name,
                   inodeInformationType = kind,
                   inodeInformationSize = maybeSize,
                   inodeInformationCreationTimestamp =
                     fromIntegral creationTimestamp,
                   inodeInformationModificationTimestamp =
                     fromIntegral modificationTimestamp
                 }
    _ -> throwIO $(internalFailure)


recordNewBrowserWindow :: BrowserWindow -> Inode -> IO ()
recordNewBrowserWindow browserWindow browserWindowRootInode = do
  let project = browserWindowProject browserWindow
      database = projectDatabase project
  _ <- query database
             (Text.intercalate "\n"
               ["INSERT INTO windows (id, kind, top, left, height, width)",
                "VALUES (?, 'browser', NULL, NULL, NULL, NULL)"])
             [toSQL $ browserWindowID browserWindow]
  _ <- query database
             (Text.intercalate "\n"
               ["INSERT INTO browser_windows (id, root_inode)",
                "VALUES (?, ?)"])
             [toSQL $ browserWindowID browserWindow,
              toSQL $ inodeID browserWindowRootInode]
  return ()


lookupBrowserWindowRoot :: BrowserWindow -> IO Inode
lookupBrowserWindowRoot browserWindow = do
  let project = browserWindowProject browserWindow
      database = projectDatabase project
  rows <- query database
                (Text.intercalate "\n"
                  ["SELECT root_inode FROM browser_windows",
                   "WHERE id = ?"])
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
             (Text.intercalate "\n"
               ["INSERT OR REPLACE INTO browser_items",
                "(inode, browser_window, expanded) VALUES (?, ?, ?)"])
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
                (Text.intercalate "\n"
                  ["SELECT expanded FROM browser_items",
                   "WHERE (inode = ?) AND (browser_window = ?)"])
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
             (Text.intercalate "\n"
               ["INSERT INTO windows (id, kind, top, left, height, width)",
                "VALUES (?, 'document', NULL, NULL, NULL, NULL)"])
             [toSQL $ documentWindowID documentWindow]
  _ <- query database
             (Text.intercalate "\n"
               ["INSERT INTO document_windows (id, inode)",
                "VALUES (?, ?)"])
             [toSQL $ documentWindowID documentWindow,
              toSQL $ inodeID documentWindowInode]
  return ()


lookupDocumentWindowInode :: DocumentWindow -> IO Inode
lookupDocumentWindowInode documentWindow = do
  let project = documentWindowProject documentWindow
      database = projectDatabase project
  rows <- query database
                (Text.intercalate "\n"
                  ["SELECT inode FROM document_windows",
                   "WHERE id = ?"])
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
