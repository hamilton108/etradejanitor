{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistory where


-- Hasql
import qualified Hasql.Session as HS

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

processLine :: String -> T.StockPrice
processLine line =
        let
          [dx',_,_,opn',hi',lo',cls',vol',_] = splitOn "," line
          dxx = asDateString dx'
        in
          -- forM_ lxx putStrLn >>
          T.StockPrice dxx opn' hi' lo' cls' vol'

netfondsDateFormat :: Cal.Day -> String
netfondsDateFormat = concat . splitOn "-" . Cal.showGregorian

fetchCsv :: T.Ticker -> IO [String]
fetchCsv (T.Ticker _ s dx) =
    let
        tickerCsv :: String
        tickerCsv = printf "%s.csv" s

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
fetchStockPrices tickr =
  fetchCsv tickr >>= \lx ->
  return $ map processLine lx

asDateString :: String -> String
asDateString v =
  let
    year :: String
    year = take 4 v

    month :: String
    month = take 2 $ drop 4 v

    day :: String
    day = take 2 $ drop 6 v
  in
    printf "%s-%s-%s" year month day

asSql :: T.Ticker -> T.StockPrice -> String -- B.ByteString
asSql (T.Ticker oid _ _) sp =
    printf
      "insert into stockmarket.stockprice (ticker_id,dx,opn,hi,lo,cls,vol) values (%d,'%s',%s,%s,%s,%s,%s)"
      oid
      (T.dx sp)
      (T.opn sp)
      (T.hi sp)
      (T.lo sp)
      (T.cls sp)
      (T.vol sp)

insertRow :: T.Ticker -> T.StockPrice -> HS.Session ()
insertRow tickr sp =
  let
    stmt = B.pack $ asSql tickr sp
  in
    HS.statement () $ C.plain $ stmt

insertRows :: T.Ticker -> [T.StockPrice] -> IO (Either C.SessionError ())
insertRows tickr stockPrices =
  C.session $
  forM_ stockPrices (insertRow tickr)

updateStockPrices :: T.Ticker -> IO (Either C.SessionError ())
updateStockPrices tickr =
  fetchStockPrices tickr >>= \stockPrices ->
  insertRows tickr stockPrices

updateStockPricesTickers :: T.Tickers -> IO ()
updateStockPricesTickers tix =
  forM_ tix updateStockPrices
