module Data.Timestamp
  (getTimestamp,
   describeTimestamp)
  where

import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import System.Locale


getTimestamp :: IO Word64
getTimestamp = getPOSIXTime >>= return . floor


describeTimestamp :: Word64 -> IO String
describeTimestamp timestamp = do
  now <- getTimestamp
  let timeAgo = now - timestamp
      secondsAgo = timeAgo
      minutesAgo = div secondsAgo 60
      hoursAgo = div minutesAgo 60
      daysAgo = div hoursAgo 24
      weeksAgo = div daysAgo 7
            
      asSecondsAgo = expressTimeAgo secondsAgo "second"
      asMinutesAgo = expressTimeAgo minutesAgo "minute"
      asHoursAgo = expressTimeAgo hoursAgo "hour"
      
      asYesterday = "Yesterday"
      
      asWeekDay = formatTime defaultTimeLocale "%A" utcTime
      
      utcTime = posixSecondsToUTCTime $ realToFrac timestamp
      asMonthAndDay = formatTime defaultTimeLocale "%b " utcTime
                      ++ (appendEnglishOrdinalMarker
                           $ formatTime defaultTimeLocale "%d" utcTime)
      asMonthDayAndYear = asMonthAndDay
                          ++ formatTime defaultTimeLocale ", %Y" utcTime
      
      expressTimeAgo duration word =
        (show duration) ++ " " ++ (pluralize duration word) ++ " ago"
      pluralize numeral word =
        if numeral == 1
          then word
          else word ++ "s"
      appendEnglishOrdinalMarker string =
        let ordinal = if null string
                          then 0
                          else read string
            marker = case (mod (div ordinal 10) 10, mod ordinal 10) of
                       (1, _) -> "th"
                       (_, 1) -> "st"
                       (_, 2) -> "nd"
                       (_, 3) -> "rd"
                       _ -> "th"
        in (show ordinal) ++ marker
      
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
