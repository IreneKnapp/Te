module Te.HighLevel.UUID
  (uuidHash,
   uuidEqual,
   uuidShow)
  where

import Data.Digest.Murmur64
import Data.UUID
import Data.Word

import Te.Types


uuidHash :: UUID -> IO Word64
uuidHash uuid = do
  return $ asWord64 $ hash64 uuid


uuidEqual :: UUID -> UUID -> IO Bool
uuidEqual uuidA uuidB = do
  return $ uuidA == uuidB


uuidShow :: UUID -> IO String
uuidShow uuid = do
  return $ show uuid

