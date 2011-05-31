module Te.Database
  (Database,
   newProjectDatabase,
   closeProjectDatabase,
   attachFileToProjectDatabase,
   getProjectDatabaseSchemaVersion,
   initProjectDatabaseSchema,
   recordNewBrowserWindow)
  where

import Control.Exception
import Database.SQLite3 (Database, StepResult(..), SQLData(..))
import qualified Database.SQLite3 as SQL
import Prelude hiding (catch)

import Te.Identifiers
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
  -- TODO
  return Nothing


initProjectDatabaseSchema :: Database -> IO ()
initProjectDatabaseSchema database = do
  -- TODO
  return ()


recordNewBrowserWindow :: Project -> BrowserWindow -> IO ()
recordNewBrowserWindow project browserWindow = do
  -- TODO
  return ()
