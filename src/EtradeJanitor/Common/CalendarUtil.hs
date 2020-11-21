{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Common.CalendarUtil where

import qualified Data.Time.Calendar            as Calendar
import qualified Data.Time.Clock               as Clock
import qualified Data.Time.Clock.POSIX         as POSIX

defaultDiffTime :: Clock.DiffTime
defaultDiffTime = Clock.secondsToDiffTime 0

dayToUnixTime :: Calendar.Day -> POSIX.POSIXTime
dayToUnixTime d =
  let dx = Clock.UTCTime d defaultDiffTime in POSIX.utcTimeToPOSIXSeconds dx


unixTimeToInt :: POSIX.POSIXTime -> Int
unixTimeToInt unixTime = floor $ toRational unixTime

strToUnixTime :: String -> POSIX.POSIXTime
strToUnixTime s =
  let x = read s :: Integer in fromInteger x :: POSIX.POSIXTime

today :: IO Calendar.Day
today = Clock.getCurrentTime >>= \now -> pure $ Clock.utctDay now

