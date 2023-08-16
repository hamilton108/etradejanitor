{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Common.CalendarUtil where

import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import Data.Time.Clock.POSIX
  ( POSIXTime
  , getPOSIXTime
  , posixSecondsToUTCTime
  , utcTimeToPOSIXSeconds
  )
import Data.Time.Format
  ( defaultTimeLocale
  , formatTime
  )
import Rapanui.Common
  ( Iso8601 (..)
  , PosixTimeInt (..)
  , TimeInfo (..)
  )

defaultDiffTime :: Clock.DiffTime
defaultDiffTime = Clock.secondsToDiffTime 0

dayToUnixTime :: Calendar.Day -> POSIXTime
dayToUnixTime d =
  let dx = Clock.UTCTime d defaultDiffTime in utcTimeToPOSIXSeconds dx

unixTimeToInt :: POSIXTime -> Int
unixTimeToInt unixTime = floor $ toRational unixTime

strToUnixTime :: String -> POSIXTime
strToUnixTime s =
  let x = read s :: Integer in fromInteger x :: POSIXTime

today :: IO Calendar.Day
today = Clock.getCurrentTime >>= \now -> pure $ Clock.utctDay now

currentTimeInfo :: IO TimeInfo
currentTimeInfo =
  getPOSIXTime >>= \t ->
    let
      pt = round t :: Int
      isot = formatTime defaultTimeLocale "%FT%T" (posixSecondsToUTCTime t)
    in
      pure $ TimeInfo (PosixTimeInt pt) (Iso8601 isot)

-- pt2 <- getPOSIXTime
-- ghci> formatTime defaultTimeLocale "%FT%T" (posixSecondsToUTCTime pt2)
