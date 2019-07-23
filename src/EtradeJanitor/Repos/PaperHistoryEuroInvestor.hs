{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistoryEuroInvestor where

import qualified Text.HTML.TagSoup as TS

import qualified EtradeJanitor.Common.Types as T

type StringSoup = [TS.Tag String]

fetchStockPrices :: T.Ticker -> IO [T.StockPrice]
fetchStockPrices tix = pure []