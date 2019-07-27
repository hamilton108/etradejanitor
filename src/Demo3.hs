{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS -Wno-unused-imports #-}


module Demo3 where

import Data.Int (Int64)
import qualified Data.Time.Calendar as Calendar
import qualified Data.Vector as DV
import qualified Text.HTML.TagSoup as TS
import Data.List.Split (splitOn)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT,ask,runReaderT)

import qualified EtradeJanitor.EuroInvestor as EuroInvestor
import qualified EtradeJanitor.Repos.PaperHistoryEuroInvestor as PaperHistory
import qualified EtradeJanitor.Repos.PaperHistory as PaperHistoryx

import qualified EtradeJanitor.Repos.Common as RC
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Params as PA
import qualified EtradeJanitor.Common.Types as T

prms = PA.Params 
  {
    PA.databaseIp = "172.17.0.2"
  , PA.feed = "/home/rcs/opt/haskell/etradejanitor/feed2"
  , PA.downloadOnly = False
  , PA.updateDbOnly = False
  }

env = T.Env prms

-- yax = EuroInvestor.downloadPaperHistory

nhy = T.Ticker 1 "NHY" 1 (Calendar.fromGregorian 2019 4 24)

feed = "/home/rcs/opt/haskell/etradejanitor/feedtmp"

tix = DV.fromList [nhy]

soup = PaperHistory.soup nhy 

-- paper = PaperHistory.soup nhy  >>= (pure . PaperHistory.startDataLines nhy)

coll = soup >>= pure . last . PaperHistory.collect

dx = coll >>= pure . PaperHistory.asDay . head . drop 2

stok = coll >>= pure . PaperHistory.createStockPrice nhy

xx = coll >>= pure . head . drop 8

stox = runReaderT (PaperHistory.fetchStockPrices nhy) env

insertTicker :: T.Ticker -> IO () 
insertTicker tik =
  runReaderT (PaperHistory.fetchStockPrices tik) env >>= \prices ->
  runReaderT (RS.insertStockPrices prices) env >>= \result ->
    case result of
      Right resultx ->
        putStrLn $ "OK!"
      Left err ->
        putStrLn $ show err


tixx = RS.tickers "172.17.0.2"

insertTickers :: IO () 
insertTickers =
    RS.tickers (PA.databaseIp prms) >>= \tix ->
      case tix of
          Right result ->
              runReaderT (PaperHistoryx.updateStockPricesTickers result) env
              --mapM_ insertTicker result
          Left err ->
              putStrLn $ show err

downloadTickers :: IO () 
downloadTickers =
    RS.tickers (PA.databaseIp prms) >>= \tx ->
        case tx of
            Right result -> 
                runReaderT (EuroInvestor.savePaperHistoryTickers result) env
            Left err ->
                putStrLn $ show err

q = T.Ticker 29 "NAS" 1 (Calendar.fromGregorian 2019 4 24)

itq = insertTicker q