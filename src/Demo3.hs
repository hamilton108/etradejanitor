{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS -Wno-unused-imports #-}


module Demo3 where

import qualified EtradeJanitor.EuroInvestor as EuroInvestor

import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Params as PA

prms = PA.Params 
  {
    PA.databaseIp = "172.17.0.2"
  , PA.allPaper = False
  , PA.downloadOnly = False
  , PA.htmlOnly = False
  , PA.feed = "/home/rcs/opt/haskell/etradejanitor/feed2"
  }

yax = EuroInvestor.downloadPaperHistory

yux = RS.tickers (PA.databaseIp prms) >>= \tix ->
        case tix of
          Right result ->
            putStrLn $ show result
          Left err ->
            putStrLn $ show err
