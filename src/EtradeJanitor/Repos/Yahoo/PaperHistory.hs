{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module EtradeJanitor.Repos.Yahoo.PaperHistory where

import           Control.Monad                  ( forM_ )
import           Data.Int                       ( Int64 )
import qualified Data.List.Split               as Split
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.Reader           ( MonadReader
                                                , MonadIO
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.List                     as L
import qualified Data.Time.Calendar            as Cal
import           Data.Maybe                     ( fromJust )
import qualified Text.Printf                   as Printf

--import System.FilePath 
import           System.IO                      ( openFile
                                                , hSetEncoding
                                                , hGetContents
                                                , latin1
                                                , IOMode(..)
                                                )

import qualified EtradeJanitor.Repos.Common    as C
import qualified EtradeJanitor.Repos.Stocks    as Stocks
import qualified EtradeJanitor.Common.Types    as T
import           EtradeJanitor.Common.Types     ( Env
                                                , StockPrice
                                                , Ticker
                                                , Tickers
                                                )
import qualified EtradeJanitor.Params          as Params


-- https://query1.finance.yahoo.com/v7/finance/download/EQNR.OL?period1=1547506517&period2=1579042517&interval=1d&events=history&crumb=gJXukxOba2X


-- https://query1.finance.yahoo.com/v7/finance/download/EQNR.OL?period1=1577901600&period2=1579042517&interval=1d&events=history&crumb=gJXukxOba2X



asDay :: String -> Cal.Day
asDay v =
  let year  = read (take 4 v) :: Integer

      month = read (take 2 $ drop 5 v) :: Int

      day   = read (take 2 $ drop 8 v) :: Int
  in  Cal.fromGregorian year month day

yahooDateFormat :: Cal.Day -> String
yahooDateFormat = Cal.showGregorian

parseCsv :: T.Ticker -> String -> [String]
parseCsv (T.Ticker _ _ _ dx) content =
  let yahooDx :: String
      yahooDx = yahooDateFormat dx
      lxx     = tail $ L.lines content
      result  = dropWhile (\x -> x < yahooDx) lxx
  in  case result of
        [] -> result
        _ ->
          let resultDate = (take 10 . head) result
          in  if resultDate == yahooDx then tail result else result

    --(tail . dropWhile (\x -> x < yahooDx)) lxx 

csvPath :: (MonadReader Env m) => T.Ticker -> m FilePath
csvPath t = Reader.ask >>= \env ->
  let ticker = T.ticker t
      feed   = (Params.feed . T.getParams) env
  in  pure $ Printf.printf "%s/%s.csv" feed ticker


fetchCsv :: (MonadIO m, MonadReader Env m) => Ticker -> m [String]
fetchCsv ticker = csvPath ticker >>= \tcsv ->
  liftIO $ openFile tcsv ReadMode >>= \inputHandle ->
    hSetEncoding inputHandle latin1
      >> -- utf8
          hGetContents inputHandle
      >>= \theInput -> pure $ parseCsv ticker theInput

processLine :: Ticker -> String -> Maybe StockPrice
processLine tikr line =
  let
      --[dx',opn',hi',lo',cls',vol',_,_] = Split.splitOn "," line
      splits = Split.splitOn "," line
  in  case splits of
        [dx', opn', hi', lo', cls', vol', _, _] ->
          let dxx  = asDay dx' -- asDateString dx'
              opnf = read opn' :: Float
              hif  = read hi' :: Float
              lof  = read lo' :: Float
              clsf = read cls' :: Float
              voli = read vol' :: Int64
          in  Just $ T.StockPrice tikr dxx opnf hif lof clsf voli
        _ ->
            --T.StockPrice tikr (Cal.fromGregorian 2020 3 15) 1.0 1.0 1.0 1.0 10 
          Nothing

fetchStockPrices :: (MonadIO m, MonadReader Env m) => Ticker -> m [StockPrice]
fetchStockPrices tikr = fetchCsv tikr >>= \lx ->
  let lx1    = map (processLine tikr) lx
      result = map fromJust $ filter (\x -> x /= Nothing) lx1
  in  pure $ result

printStockPrice :: StockPrice -> IO ()
printStockPrice p = Printf.printf "%s\n" (yahooDateFormat (T.dx2 p))

updateStockPrices
  :: (MonadIO m, MonadReader Env m) => Ticker -> m (Either C.SessionError ())
updateStockPrices tik =
  fetchStockPrices tik >>= \stockPrices -> Stocks.insertStockPrices stockPrices
    --liftIO $ mapM_ printStockPrice stockPrices

updateStockPricesTickers :: (MonadIO m, MonadReader Env m) => Tickers -> m ()
updateStockPricesTickers tix = Reader.ask >>= \env ->
  let doUpdate = (Params.dbUpdateStocks . T.getParams) env
  in  case doUpdate of
        True  -> forM_ tix updateStockPrices
        False -> pure ()
