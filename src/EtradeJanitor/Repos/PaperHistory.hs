{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistory where


-- Hasql
import qualified Hasql.Session as HS

import Data.Int (Int64)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as B
import qualified Data.Time.Calendar as Cal
import qualified Data.List as L
import Data.List.Split (splitOn)
import Control.Monad (forM_)
import System.IO (openFile,hSetEncoding,hGetContents,latin1,IOMode(..))

-- Local
import qualified EtradeJanitor.Repos.Common as C
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Repos.Stocks as RS

processLine :: T.Ticker -> String -> T.StockPrice
processLine tikr line =
        let
          [dx',_,_,opn',hi',lo',cls',vol',_] = splitOn "," line
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
fetchCsv (T.Ticker _ s _ dx) =
    let
        tickerCsv :: String
        tickerCsv = printf "%s/%s.csv" T.feed s

        netfondsDx :: String
        netfondsDx = netfondsDateFormat dx
    in
      openFile tickerCsv ReadMode >>= \inputHandle ->
      hSetEncoding inputHandle latin1 >> -- utf8
      hGetContents inputHandle >>= \theInput ->
      let
        lxx = tail $ L.lines theInput
        result = (init . takeWhile (\x -> x > netfondsDx)) lxx
      in
      return result

fetchStockPrices :: T.Ticker -> IO [T.StockPrice]
fetchStockPrices tikr =
  fetchCsv tikr >>= \lx ->
  return $ map (processLine tikr) lx

asDay :: String -> Cal.Day
asDay v =
  let
    year = read (take 4 v) :: Integer

    month = read (take 2 $ drop 4 v) :: Int

    day = read (take 2 $ drop 6 v) :: Int
  in
  Cal.fromGregorian year month day


-- asDateString :: String -> String
-- asDateString v =
--   let
--     year :: String
--     year = take 4 v
--
--     month :: String
--     month = take 2 $ drop 4 v
--
--     day :: String
--     day = take 2 $ drop 6 v
--   in
--     printf "%s-%s-%s" year month day


updateStockPrices :: T.Ticker -> IO (Either C.SessionError ())
updateStockPrices tickr =
  fetchStockPrices tickr >>= \stockPrices ->
  RS.insertStockPrices stockPrices

updateStockPricesTickers :: T.Tickers -> IO ()
updateStockPricesTickers tix =
  forM_ tix updateStockPrices
