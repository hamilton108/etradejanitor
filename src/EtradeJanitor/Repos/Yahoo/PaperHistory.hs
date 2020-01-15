{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Yahoo.PaperHistory where

import qualified Data.List as L
import qualified Data.Time.Calendar as Cal

import qualified EtradeJanitor.Common.Types as T

import System.IO (openFile,hSetEncoding,hGetContents,latin1,IOMode(..))

-- https://query1.finance.yahoo.com/v7/finance/download/EQNR.OL?period1=1547506517&period2=1579042517&interval=1d&events=history&crumb=gJXukxOba2X


-- https://query1.finance.yahoo.com/v7/finance/download/EQNR.OL?period1=1577901600&period2=1579042517&interval=1d&events=history&crumb=gJXukxOba2X


asDay :: String -> Cal.Day
asDay v =
    let
        year = read (take 4 v) :: Integer

        month = read (take 2 $ drop 5 v) :: Int

        day = read (take 2 $ drop 8 v) :: Int
    in
    Cal.fromGregorian year month day

yahooDateFormat :: Cal.Day -> String
yahooDateFormat = Cal.showGregorian

parseCsv :: T.Ticker -> String -> [String]
parseCsv (T.Ticker _ _ _ dx) content =
    let
        yahooDx :: String
        yahooDx = yahooDateFormat dx
    in
    let
        lxx = tail $ L.lines content 
    in
    (init . takeWhile (\x -> x > yahooDx)) lxx

tickerCsv :: T.Ticker -> String
tickerCsv ticker = ""

fetchCsv :: T.Ticker -> IO [String]
fetchCsv ticker =
    openFile (tickerCsv ticker) ReadMode >>= \inputHandle ->
    hSetEncoding inputHandle latin1 >> -- utf8
    hGetContents inputHandle >>= \theInput ->
    pure $ parseCsv ticker theInput
