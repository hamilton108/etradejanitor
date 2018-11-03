{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Stocks where

import qualified Data.ByteString.Char8 as B
import Text.Printf (printf)
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.Functor.Contravariant (contramap)
import qualified Data.Time.Calendar as Cal
import qualified Data.Vector as V
import qualified Data.Maybe as M


-- Hasql
import qualified Hasql.Session as HS
import qualified Hasql.Statement as HST
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD

-- Local
import qualified EtradeJanitor.Repos.Common as C
import qualified EtradeJanitor.Common.Types as T


tickers :: IO (Either C.SessionError T.Tickers)
tickers =
  C.session $
    HS.statement () selectStockTickers

stockTickerDecoder :: HD.Row T.Ticker
stockTickerDecoder = T.Ticker <$> HD.column HD.int8 <*> HD.column HD.text <*> HD.column HD.int8 <*> HD.column HD.date

selectStockTickers :: HST.Statement () T.Tickers
selectStockTickers =
  HST.Statement sql encoder decoder False
  where
    sql =
      -- "select t.oid,t.ticker,max(s.dx) from stockmarket.stocktickers t join stockmarket.stockprice s on s.ticker_id=t.oid where t.status = 1 and t.oid = 1 group by t.oid,t.ticker order by t.ticker"
      "select t.oid,t.ticker,t.ticker_category,max(s.dx) from stockmarket.stocktickers t join stockmarket.stockprice s on s.ticker_id=t.oid where t.status = 1 group by t.oid,t.ticker order by t.oid"
    encoder =
      HE.unit
    decoder =
      HD.rowVector stockTickerDecoder

asSql :: T.Ticker -> T.StockPrice -> String -- B.ByteString
asSql (T.Ticker oid _ _ _) sp =
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
    sql = B.pack $ asSql tickr sp
  in
    HS.statement () $ C.plain sql

insertRows :: T.Ticker -> [T.StockPrice] -> IO (Either C.SessionError ())
insertRows tickr stockPrices =
  C.session $
  forM_ stockPrices (insertRow tickr)

-- insertStockPrices2 :: [T.StockPrice2] -> IO (Either C.SessionError ())
insertStockPrices2 :: V.Vector (Maybe T.StockPrice2) -> IO (Either C.SessionError ())
insertStockPrices2 prices =
  let
    p1 = V.filter (\x -> x /= Nothing) prices
    p2 = V.map (\x -> M.fromJust x) p1
    stmt =
      HST.Statement
        "insert into stockmarket.stockprice (ticker_id,dx,opn,hi,lo,cls,vol) values ($1,$2,$3,$4,$5,$6,$7)"
        stockPrice2Encoder
        HD.unit
        True
  in
  C.session $
  forM_ p2 $ (\t -> HS.statement t stmt)

stockPrice2Encoder :: HE.Params T.StockPrice2
stockPrice2Encoder =
  contramap T.tick (HE.param tickerEncoder) <>
  contramap T.dx2 (HE.param HE.date)  <>
  contramap T.opn2 (HE.param HE.float4) <>
  contramap T.hi2 (HE.param HE.float4) <>
  contramap T.lo2 (HE.param HE.float4) <>
  contramap T.cls2 (HE.param HE.float4) <>
  contramap T.vol2 (HE.param HE.int8)

tickerEncoder :: HE.Value T.Ticker
tickerEncoder =
  contramap T.oid HE.int8

  -- axEncoder :: HE.Params T.Ax
  -- axEncoder =
  --   contramap T.ar (HE.param HE.int8) <>
  --   contramap T.pp (HE.param HE.float4) <>
  --   contramap T.tt (HE.param HE.text) <>
  --   contramap T.dx3 (HE.param HE.date)
  --
-- qq =
--   -- HST.Statement "insert into stockmarket.ax (ar,p,t,dx) values ($1,$2,$3,$4)" axEncoder HD.unit True
--   HST.Statement "insert into stockmarket.stockprice (ticker_id,dx,opn,hi,lo,cls,vol) values ($1,$2,$3,$4,$5,$6,$7)" stockPrice2Encoder HD.unit True
--
-- dx = Cal.fromGregorian 2018 10 30
-- tikr = T.Ticker 1 "NHY" 1 dx
--
-- qqq =
--   -- C.session $ HS.statement (T.Ax 34099 45.24 "Demo" dx) qq
--   C.session $ HS.statement (T.StockPrice2 tikr dx 12.0 15.0 11.0 14.0 1000000) qq

  --  contramap genderText HE.text

  -- contramap getA (HE.param HE.int8)
-- demo =
--   C.session $
--         HS.statement () selectStockTickers
-- selectSum :: HST.Statement (Int64, Int64) Int64
-- selectSum =
--   HST.Statement sql encoder decoder True
--   where
--     sql =
--       "select ($1 + $2)"
--     encoder =
--       contramap fst (HE.param HE.int8) <>
--       contramap snd (HE.param HE.int8)
--     decoder =
--       HD.singleRow (HD.column HD.int8)
--
-- selectCount :: HST.Statement ()  Int64
-- selectCount =
--   HST.Statement sql encoder decoder False
--   where
--     sql =
--       "select count(1) from stockmarket.stockprice"
--     encoder =
--       HE.unit
--     decoder =
--       HD.singleRow (HD.column HD.int8)
--
-- selectCountTickers :: HST.Statement () [Int64]
-- selectCountTickers =
--   HST.Statement sql encoder decoder False
--   where
--     sql =
--       "select ticker_id,count(1) from stockmarket.stockprice group by ticker_id"
--     encoder =
--       HE.unit
--     decoder =
--       HD.rowList (HD.column HD.int8)
-- data Person =
--     Person { name :: Text, gender :: Gender, age :: Int }
--     deriving (Show)
--
-- data Gender =
--     Male | Female
--     deriving (Show)
--
-- personParams :: HE.Params Person
-- personParams =
--     contramap name (HE.param HE.text) <>
--     contramap gender (HE.param genderValue) <>
--     contramap (fromIntegral . age) (HE.param HE.int8)
--
-- genderValue :: HE.Value Gender
-- genderValue =
--     contramap genderText HE.text
--     where
--         genderText ge =
--             case ge of
--                 Male -> "male"
--                 Female -> "female"
