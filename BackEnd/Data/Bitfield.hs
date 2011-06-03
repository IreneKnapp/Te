{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Data.Bitfield
  (Bitfield,
   Bitfieldable,
   bitfieldInformation,
   setToBitfield,
   bitfieldToSet,
   valueToBit,
   bitToValue)
  where

import Data.Bits
import Data.Data
import Data.Ix
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import Foreign.Storable
import Text.Printf


newtype (Eq a) => Bitfield a = Bitfield Word64
                  deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord,
                            Read, Real, Show, Ix, Typeable, Bits, Storable,
                            PrintfArg)


class Bitfieldable a where
  bitfieldInformation :: [(Int, a)]


setToBitfield :: (Bitfieldable a, Ord a) => Set a -> Bitfield a
setToBitfield set =
  Set.fold (\foundValue result ->
              foldl' (\result (bit, bitValue) ->
                        if foundValue == bitValue
                          then setBit result bit
                          else result)
                     result
                     bitfieldInformation)
           0
           set


bitfieldToSet :: (Bitfieldable a, Ord a) => Bitfield a -> Set a
bitfieldToSet bitfield =
  foldl' (\result (bit, bitValue) ->
            if testBit bitfield bit
              then Set.insert bitValue result
              else result)
         Set.empty
         bitfieldInformation


valueToBit :: (Bitfieldable a, Eq a) => a -> Word64
valueToBit queryValue =
  fromJust $ foldl' (\maybeResult (bit, bitValue) ->
                       case maybeResult of
                         Just _ -> maybeResult
                         Nothing -> if queryValue == bitValue
                                      then Just $ setBit 0 bit
                                      else Nothing)
                    Nothing
                    bitfieldInformation


bitToValue :: (Bitfieldable a, Eq a) => Word64 -> a
bitToValue queryValue =
  fromJust $ foldl' (\maybeResult (bit, bitValue) ->
                       case maybeResult of
                         Just _ -> maybeResult
                         Nothing -> if queryValue == setBit 0 bit
                                      then Just bitValue
                                      else Nothing)
                    Nothing
                    bitfieldInformation
