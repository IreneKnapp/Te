{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Data.ByteSize
  (ByteSize(..))
  where

import Data.Bits
import Data.Data
import Data.Ix
import Data.Maybe
import Data.Word
import Database.SQLite3 (SQLData(..))
import Foreign.Storable
import System.Locale
import Text.Printf

import Data.SQLable


newtype ByteSize = ByteSize Word64
                   deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord,
                             Real, Ix, Typeable, Bits, Storable,
                             PrintfArg)


instance SQLable ByteSize where
  toSQL byteSize = SQLInteger $ fromIntegral byteSize
  fromSQL (SQLInteger byteSize) = Just $ fromIntegral byteSize
  fromSQL _ = Nothing


instance Show ByteSize where
  show byteSize =
    let bytes :: Word64
        bytes = fromIntegral byteSize
        kilobytes = shiftR bytes 10
        megabytes = shiftR kilobytes 10
        gigabytes = shiftR megabytes 10
        terabytes = shiftR gigabytes 10
        
        asBytes = show bytes ++ " B"
        asKilobytes = show kilobytes ++ " KiB"
        asMegabytes = show megabytes ++ " MiB"
        asGigabytes = show gigabytes ++ " GiB"
        asTerabytes = show terabytes ++ " TiB"
        
        firstWorkingAlternative alternatives =
          head $ catMaybes $ map (\(condition, result) ->
                                     if condition
                                       then Just result
                                       else Nothing)
                                 alternatives
    in firstWorkingAlternative [(kilobytes == 0, asBytes),
                                (megabytes == 0, asKilobytes),
                                (gigabytes == 0, asMegabytes),
                                (terabytes == 0, asGigabytes),
                                (True, asTerabytes)]
