{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Stocks where

import Data.Int (Int64)
import Data.Functor.Contravariant (contramap)
import Data.Text (Text,pack)

-- Hasql
import qualified Hasql.Session as HS
import qualified Hasql.Statement as HST
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Data.Vector as DV

-- Local
import qualified EtradeJanitor.Repos.Common as C
import qualified EtradeJanitor.Common.Types as T

tickers :: IO [T.Ticker]
tickers = undefined

selectSum :: HST.Statement (Int64, Int64) Int64
selectSum =
  HST.Statement sql encoder decoder True
  where
    sql =
      "select ($1 + $2)"
    encoder =
      contramap fst (HE.param HE.int8) <>
      contramap snd (HE.param HE.int8)
    decoder =
      HD.singleRow (HD.column HD.int8)

selectCount :: HST.Statement ()  Int64
selectCount =
  HST.Statement sql encoder decoder False
  where
    sql =
      "select count(1) from stockmarket.stockprice"
    encoder =
      HE.unit
    decoder =
      HD.singleRow (HD.column HD.int8)

selectCountTickers :: HST.Statement () [Int64]
selectCountTickers =
  HST.Statement sql encoder decoder False
  where
    sql =
      "select ticker_id,count(1) from stockmarket.stockprice group by ticker_id"
    encoder =
      HE.unit
    decoder =
      HD.rowList (HD.column HD.int8)

-- stockTickerDecoder :: HD.Row T.Ticker
-- stockTickerDecoder = T.Ticker <$> HD.column HD.int8 <*> HD.column HD.text

stockTickerDecoder :: HD.Row (Int64, Text)
stockTickerDecoder = (,) <$> HD.column HD.int8 <*> HD.column HD.text

selectStockTickers :: HST.Statement () (DV.Vector (Int64,Text))
selectStockTickers =
  HST.Statement sql encoder decoder False
  where
    sql =
      "select oid,ticker from stockmarket.stocktickers order by ticker"
    encoder =
      HE.unit
    decoder =
      -- HD.rowList (HD.column HD.int8)
      -- HD.rowVector (HD.column HD.int8)
      HD.rowVector stockTickerDecoder

demo =
  C.session $
        HS.statement () selectStockTickers

data Person =
    Person { name :: Text, gender :: Gender, age :: Int }
    deriving (Show)

data Gender =
    Male | Female
    deriving (Show)

personParams :: HE.Params Person
personParams =
    contramap name (HE.param HE.text) <>
    contramap gender (HE.param genderValue) <>
    contramap (fromIntegral . age) (HE.param HE.int8)

genderValue :: HE.Value Gender
genderValue =
    contramap genderText HE.text
    where
        genderText ge =
            case ge of
                Male -> "male"
                Female -> "female"
