{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Yahoo.PaperHistory where

import Data.Int (Int64)
import qualified Data.List.Split as Split
import qualified Control.Monad.Reader as Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import qualified Data.Time.Calendar as Cal
import qualified Text.Printf as Printf

import qualified System.FilePath as FilePath
import System.IO (openFile,hSetEncoding,hGetContents,latin1,IOMode(..))

import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Params as Params
import EtradeJanitor.Common.Types (REIO)


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
    -- (init . takeWhile (\x -> x > yahooDx)) lxx
    (tail . dropWhile (\x -> x < yahooDx)) lxx 

csvPath :: T.Ticker -> REIO FilePath
csvPath t = 
    Reader.ask >>= \env ->
    let 
        ticker = T.ticker t
        feed = (Params.feed . T.getParams) env
    in
    pure $ Printf.printf "%s/%s.csv" feed ticker


fetchCsv :: T.Ticker -> REIO [String]
fetchCsv ticker =
    csvPath ticker >>= \tcsv ->
    liftIO $    
    openFile tcsv ReadMode >>= \inputHandle ->
    hSetEncoding inputHandle latin1 >> -- utf8
    hGetContents inputHandle >>= \theInput ->
    pure $ parseCsv ticker theInput

processLine :: T.Ticker -> String -> T.StockPrice
processLine tikr line =
        let
          [dx',opn',hi',lo',cls',_,vol'] = Split.splitOn "," line
          dxx = asDay dx' -- asDateString dx'
          opnf = read opn' :: Float
          hif = read hi' :: Float
          lof = read lo' :: Float
          clsf = read cls' :: Float
          voli = read vol' :: Int64
        in
        T.StockPrice tikr dxx opnf hif lof clsf voli

fetchStockPrices :: T.Ticker -> REIO [T.StockPrice]
fetchStockPrices tikr =
  fetchCsv tikr >>= \lx ->
  pure $ map (processLine tikr) lx
