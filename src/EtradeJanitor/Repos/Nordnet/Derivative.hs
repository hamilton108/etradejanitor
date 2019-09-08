{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Nordnet.Derivative where

import qualified Control.Monad.Reader as Reader
import Control.Monad.IO.Class (liftIO)

import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Text.Printf as Printf

import qualified EtradeJanitor.Params as Params
import EtradeJanitor.Common.Types (REIO,getParams)
import qualified EtradeJanitor.Common.Misc as Misc

-- https://www.nordnet.no/market/options?currency=NOK&underlyingSymbol=BAKKA&expireDate=1565906400000

nordNetUrl :: String 
nordNetUrl = "https://www.nordnet.no/market/options" -- ?currency=NOK&underlyingSymbol=BAKKA&expireDate=1565906400000

newtype Ticker = 
    Ticker { getTicker :: String }
    deriving (Show)

download_ :: Ticker -> POSIX.POSIXTime -> IO ()
download_ (Ticker ticker) unixTime = 
    pure ()


-- d = Calendar.fromGregorian 2019 3 30

-- t = Ticker "NHY"

pathNameFor :: Ticker -> Calendar.Day -> REIO String
pathNameFor (Ticker ticker) curDay = 
    Reader.ask >>= \env ->
    let 
        feed = (Params.feed . getParams) env
        (y,m,d) = Calendar.toGregorian curDay
    in
    pure $ Printf.printf "%s/%d/%d/%d/%s" feed y m d ticker

mkDir :: Ticker -> Calendar.Day -> REIO ()
mkDir (Ticker ticker) curDay = 
    pure ()

download :: Ticker -> Calendar.Day -> [POSIX.POSIXTime] -> REIO ()
download ticker curDay unixTimes = 
    mkDir ticker curDay >>
    let
        dlfn = download_ ticker 
    in 
    liftIO $ mapM_ dlfn unixTimes