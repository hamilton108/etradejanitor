{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module EtradeJanitor.Repos.Stocks where

import           Control.Monad                  ( forM_ )
import           Data.Functor.Contravariant     ( contramap )
import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                , ask
                                                )
import           Control.Monad.IO.Class         ( liftIO )

-- Hasql
import qualified Hasql.Session                 as HS
import qualified Hasql.Statement               as HST
import qualified Hasql.Encoders                as HE
import qualified Hasql.Decoders                as HD

-- Local
import qualified EtradeJanitor.Repos.Common    as C
import qualified EtradeJanitor.Common.Types    as T
import           EtradeJanitor.Common.Types     ( Env
                                                , Ticker(..)
                                                , Tickers
                                                , StockPrice
                                                )
import qualified EtradeJanitor.Params          as PA

tickers :: String -> IO (Either C.SessionError Tickers)
tickers dbIp = C.session dbIp $ HS.statement () selectStockTickers

stockTickerDecoder :: HD.Row Ticker
stockTickerDecoder =
  Ticker
    <$> HD.column (HD.nonNullable HD.int8)
    <*> HD.column (HD.nonNullable HD.text)
    <*> HD.column (HD.nonNullable HD.int8)
    <*> HD.column (HD.nonNullable HD.date)

selectStockTickers :: HST.Statement () Tickers
selectStockTickers = HST.Statement sql encoder decoder False
 where
  sql =
    --"select t.oid,t.ticker,t.ticker_category,max(s.dx) from stockmarket.stocktickers t join stockmarket.stockprice s on s.ticker_id=t.oid where t.status = 1 and t.oid = 3 group by t.oid,t.ticker order by t.oid"
    "select t.oid,t.ticker,t.ticker_category,max(s.dx) from stockmarket.stocktickers t join stockmarket.stockprice s on s.ticker_id=t.oid where t.status = 1 group by t.oid,t.ticker order by t.oid"
  encoder = HE.noParams
  decoder = HD.rowVector stockTickerDecoder

insertStockPriceStmt :: HST.Statement StockPrice ()
insertStockPriceStmt = HST.Statement
  "insert into stockmarket.stockprice (ticker_id,dx,opn,hi,lo,cls,vol) values ($1,$2,$3,$4,$5,$6,$7)"
  stockPriceEncoder
  HD.noResult
  True

insertStockPrices
  :: (MonadIO m, MonadReader Env m)
  => [StockPrice]
  -> m (Either C.SessionError ())
insertStockPrices prices = ask >>= \env ->
  let dbIp = (PA.databaseIp . T.getParams) env
  in  liftIO $ C.session dbIp $ forM_ prices $ \t ->
        HS.statement t insertStockPriceStmt

stockPriceEncoder :: HE.Params StockPrice
stockPriceEncoder =
  contramap T.tick (HE.param $ HE.nonNullable tickerEncoder)
    <> contramap T.dx2  (HE.param (HE.nonNullable HE.date))
    <> contramap T.opn2 (HE.param $ HE.nonNullable HE.float4)
    <> contramap T.hi2  (HE.param $ HE.nonNullable HE.float4)
    <> contramap T.lo2  (HE.param $ HE.nonNullable HE.float4)
    <> contramap T.cls2 (HE.param $ HE.nonNullable HE.float4)
    <> contramap T.vol2 (HE.param $ HE.nonNullable HE.int8)

tickerEncoder :: HE.Value Ticker
tickerEncoder = contramap T.oid HE.int8
