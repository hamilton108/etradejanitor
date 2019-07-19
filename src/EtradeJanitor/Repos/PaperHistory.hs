{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistory where



import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)

-- Local
import qualified EtradeJanitor.Repos.Common as C
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Repos.PaperHistoryCsv as PaperHistoryCsv 
import qualified EtradeJanitor.Repos.PaperHistoryEuroInvestor as PaperHistoryEuroInvestor 


updateStockPrices :: T.Ticker -> T.REIO (Either C.SessionError ())
updateStockPrices tickr =
  liftIO (PaperHistoryCsv.fetchStockPrices tickr) >>= \stockPrices ->
  RS.insertStockPrices stockPrices


updateStockPricesTickers :: T.Tickers -> T.REIO ()
updateStockPricesTickers tix =
  forM_ tix updateStockPrices
