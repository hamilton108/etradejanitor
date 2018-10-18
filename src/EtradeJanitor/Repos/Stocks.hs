{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Stocks where

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
      "select t.oid,t.ticker,t.ticker_category,max(s.dx) from stockmarket.stocktickers t join stockmarket.stockprice s on s.ticker_id=t.oid where t.status = 1 group by t.oid,t.ticker order by t.ticker"
    encoder =
      HE.unit
    decoder =
      HD.rowVector stockTickerDecoder

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
