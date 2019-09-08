{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Nordnet.Derivative where

import qualified Data.Time.Clock.POSIX as POSIX

-- https://www.nordnet.no/market/options?currency=NOK&underlyingSymbol=BAKKA&expireDate=1565906400000

newtype Ticker = 
    Ticker { getTicker :: String }
    deriving (Show)

newtype Url = 
    Url { getUrl :: String }
    deriving (Eq,Show)

urls :: Ticker -> [POSIX.POSIXTime] -> [Url]
urls (Ticker ticker) unixTimes = [] 