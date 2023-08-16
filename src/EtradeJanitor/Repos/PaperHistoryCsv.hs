{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistoryCsv where

-- import Text.Printf (printf)
import Data.Int (Int64)
import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Data.Time.Calendar as Cal
import System.IO
  ( IOMode (..)
  , hGetContents
  , hSetEncoding
  , latin1
  , openFile
  )

import qualified EtradeJanitor.Common.Types as T

asDay :: String -> Cal.Day
asDay v =
  let
    year = read (take 4 v) :: Integer

    month = read (take 2 $ drop 4 v) :: Int

    day = read (take 2 $ drop 6 v) :: Int
  in
    Cal.fromGregorian year month day

processLine :: T.Ticker -> String -> T.StockPrice
processLine tikr line =
  let
    [dx', _, _, opn', hi', lo', cls', vol', _] = splitOn "," line
    dxx = asDay dx' -- asDateString dx'
    opnf = read opn' :: Float
    hif = read hi' :: Float
    lof = read lo' :: Float
    clsf = read cls' :: Float
    voli = read vol' :: Int64
  in
    -- forM_ lxx putStrLn >>
    T.StockPrice tikr dxx opnf hif lof clsf voli

netfondsDateFormat :: Cal.Day -> String
netfondsDateFormat = concat . splitOn "-" . Cal.showGregorian

fetchCsv :: T.Ticker -> IO [String]
fetchCsv (T.Ticker _ _ _ dx) =
  let
    tickerCsv :: String
    tickerCsv = "N/A" -- printf "%s/%s.csv" "T.feed" s
    netfondsDx :: String
    netfondsDx = netfondsDateFormat dx
  in
    openFile tickerCsv ReadMode >>= \inputHandle ->
      hSetEncoding inputHandle latin1
        >> hGetContents inputHandle -- utf8
        >>= \theInput ->
          let
            lxx = tail $ L.lines theInput
            result = (init . takeWhile (\x -> x > netfondsDx)) lxx
          in
            return result

fetchStockPrices :: T.Ticker -> IO [T.StockPrice]
fetchStockPrices tikr =
  fetchCsv tikr >>= \lx -> return $ map (processLine tikr) lx
