{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.PaperHistory where



--import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Control.Monad.Reader (ask)

-- Local
import qualified EtradeJanitor.Repos.Common as C
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Repos.Stocks as RS
--import qualified EtradeJanitor.Repos.PaperHistoryCsv as PaperHistoryCsv 
import qualified EtradeJanitor.Repos.EuroInvestor.PaperHistory as PaperHistoryEI
import qualified EtradeJanitor.Params as Params


updateStockPrices :: T.Ticker -> T.REIO (Either C.SessionError ())
updateStockPrices tickr =
  PaperHistoryEI.fetchStockPricesM tickr >>= \stockPrices ->
  RS.insertStockPrices stockPrices


updateStockPricesTickers :: T.Tickers -> T.REIO ()
updateStockPricesTickers tix =
    ask >>= \env ->
    let
        prms = T.getParams env
        skipUpdate = Params.skipDbUpdateStocks prms
    in
    case skipUpdate of  
      True -> pure ()
      False -> forM_ tix updateStockPrices
