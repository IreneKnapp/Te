{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable,
             OverloadedStrings #-}
module Data.Timestamp
  (Timestamp(..),
   getTimestamp,
   describeTimestamp)
  where

import Data.Bits
import Data.Data
import Data.Ix
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Database.SQLite3 (SQLData(..))
import Foreign.Storable
import System.Locale
import Text.Printf

import Data.SQLable


newtype Timestamp = Timestamp Word64
                  deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord,
                            Read, Real, Show, Ix, Typeable, Bits, Storable,
                            PrintfArg)


instance SQLable Timestamp where
  toSQL timestamp = SQLInteger $ fromIntegral timestamp
  fromSQL (SQLInteger timestamp) = Just $ fromIntegral timestamp
  fromSQL _ = Nothing


getTimestamp :: IO Timestamp
getTimestamp = getPOSIXTime >>= return . floor


describeTimestamp :: Timestamp -> IO Text
describeTimestamp timestamp = do
  now <- getTimestamp
  let timeAgo :: Word64
      timeAgo = fromIntegral $ now - timestamp
      
      secondsAgo :: Word64
      secondsAgo = timeAgo
      
      minutesAgo :: Word64
      minutesAgo = div secondsAgo 60
      
      hoursAgo :: Word64
      hoursAgo = div minutesAgo 60
      
      daysAgo :: Word64
      daysAgo = div hoursAgo 24
      
      weeksAgo:: Word64
      weeksAgo = div daysAgo 7
            
      asSecondsAgo :: Text
      asSecondsAgo = expressTimeAgo secondsAgo "second"
      
      asMinutesAgo :: Text
      asMinutesAgo = expressTimeAgo minutesAgo "minute"
      
      asHoursAgo :: Text
      asHoursAgo = expressTimeAgo hoursAgo "hour"
      
      asYesterday :: Text
      asYesterday = "Yesterday"
      
      asWeekDay :: Text
      asWeekDay = Text.pack $ formatTime defaultTimeLocale "%A" utcTime
      
      utcTime :: UTCTime
      utcTime = posixSecondsToUTCTime $ realToFrac timestamp
      asMonthAndDay :: Text
      asMonthAndDay =
        Text.concat [Text.pack $ formatTime defaultTimeLocale "%b " utcTime,
                     appendEnglishOrdinalMarker
                       $ Text.pack $ formatTime defaultTimeLocale "%d" utcTime]
      asMonthDayAndYear :: Text
      asMonthDayAndYear =
        Text.concat [asMonthAndDay,
                     Text.pack $ formatTime defaultTimeLocale ", %Y" utcTime]
      
      expressTimeAgo :: Word64 -> Text -> Text
      expressTimeAgo duration word = Text.concat
        [Text.pack $ show duration, " ", pluralize duration word, " ago"]
      pluralize :: Word64 -> Text -> Text
      pluralize numeral word =
        if numeral == 1
          then word
          else Text.concat [word, "s"]
      appendEnglishOrdinalMarker :: Text -> Text
      appendEnglishOrdinalMarker text =
        let ordinal = if Text.null text
                          then 0
                          else read $ Text.unpack text
            marker = case (mod (div ordinal 10) 10, mod ordinal 10) of
                       (1, _) -> "th"
                       (_, 1) -> "st"
                       (_, 2) -> "nd"
                       (_, 3) -> "rd"
                       _ -> "th"
        in Text.concat [Text.pack $ show ordinal, marker]
      
      firstWorkingAlternative :: [(Bool, a)] -> a
      firstWorkingAlternative alternatives =
        head $ catMaybes $ map (\(condition, result) ->
                                   if condition
                                     then Just result
                                     else Nothing)
                               alternatives
  return $ firstWorkingAlternative [(secondsAgo < 60, asSecondsAgo),
                                    (minutesAgo < 60, asMinutesAgo),
                                    (hoursAgo < 24, asHoursAgo),
                                    (daysAgo < 2, asYesterday),
                                    (daysAgo < 7, asWeekDay),
                                    (weeksAgo < 47, asMonthAndDay),
                                    (True, asMonthDayAndYear)]
