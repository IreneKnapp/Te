module Data.SQLable
  (SQLable(..))
  where

import Data.UUID (UUID)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Database.SQLite3 (SQLData(..))
import qualified Database.SQLite3 as SQL


class SQLable a where
  toSQL :: a -> SQLData
  fromSQL :: SQLData -> Maybe a


instance SQLable UUID where
  toSQL b = SQLBlob $ BS.concat $ LBS.toChunks $ Binary.encode b
  fromSQL (SQLBlob b) = Just $ Binary.decode $ LBS.fromChunks [b]
  fromSQL _ = Nothing
