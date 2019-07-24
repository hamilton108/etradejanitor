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

import qualified EtradeJanitor.EuroInvestor as EuroInvestor
import qualified EtradeJanitor.Repos.PaperHistoryEuroInvestor as PaperHistory

import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Params as PA
import qualified EtradeJanitor.Common.Types as T

prms = PA.Params 
  {
    PA.databaseIp = "172.17.0.2"
  , PA.allPaper = False
  , PA.downloadOnly = False
  , PA.htmlOnly = False
  , PA.feed = "/home/rcs/opt/haskell/etradejanitor/feed2"
  }

-- yax = EuroInvestor.downloadPaperHistory

yux = RS.tickers (PA.databaseIp prms) >>= \tix ->
        case tix of
          Right result ->
            putStrLn $ show result
          Left err ->
            putStrLn $ show err

nhy = T.Ticker 1 "NHY" 1 (Calendar.fromGregorian 2019 4 1)

feed = "/home/rcs/opt/haskell/etradejanitor/feed2"

tix = DV.fromList [nhy]

sx = EuroInvestor.savePaperHistoryTickers feed tix 

hex = EuroInvestor.tickerUrl nhy

soup = PaperHistory.soup nhy 

-- paper = PaperHistory.soup nhy  >>= (pure . PaperHistory.startDataLines nhy)

coll = soup >>= pure . last . PaperHistory.collect

dx = coll >>= pure . PaperHistory.asDay . head . drop 2

stok = coll >>= pure . PaperHistory.createStockPrice nhy

xx = coll >>= pure . head . drop 8

stox = PaperHistory.fetchStockPrices nhy
