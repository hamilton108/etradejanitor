{-# LANGUAGE OverloadedStrings #-}

module Demo where

import qualified Data.Time.Calendar as Calendar
import Control.Monad.Reader (runReaderT)

import EtradeJanitor.StockExchange.OptionExpiry (expiryTimes) 
import qualified EtradeJanitor.Params as PA
import qualified EtradeJanitor.Common.Types as T

prms = PA.Params {
    PA.databaseIp = "172.20.1.3"
  , PA.redisHost = "172.20.1.2"
  , PA.feed = "/home/rcs/opt/haskell/etradejanitor/feedtmp"
  , PA.skipDownloadDerivatives = False
  , PA.skipDbUpdateStocks = False
  , PA.skipIfDownloadFileExists = True
  , PA.showStockTickers = False
  }

dx = Calendar.fromGregorian 2021 6 18

nhy = T.Ticker 
        { T.oid = 1
        , T.ticker = "NHY"
        , T.category = 1
        , T.date = dx
        }

tel = T.Ticker 
        { T.oid = 1
        , T.ticker = "TEL"
        , T.category = 1
        , T.date = dx
        }

env = T.Env prms dx

work :: T.Ticker -> IO ()
work tik = 
    putStrLn (show prms) >>
        runReaderT (expiryTimes tik) env >>= \x ->
            putStrLn (show x)

{-
import Data.ByteString.Lazy as BL
import Data.ByteString as BS
import Data.Text as TS
import Data.Text.Lazy as TL
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import Data.ByteString.UTF8 as BSU      -- from utf8-string
import Data.Text.Encoding as TSE
import Data.Text.Lazy.Encoding as TLE

-- String <-> ByteString

BLU.toString   :: BL.ByteString -> String
BLU.fromString :: String -> BL.ByteString
BSU.toString   :: BS.ByteString -> String
BSU.fromString :: String -> BS.ByteString

-- String <-> Text

TL.unpack :: TL.Text -> String
TL.pack   :: String -> TL.Text
TS.unpack :: TS.Text -> String
TS.pack   :: String -> TS.Text

-- ByteString <-> Text

TLE.encodeUtf8 :: TL.Text -> BL.ByteString
TLE.decodeUtf8 :: BL.ByteString -> TL.Text
TSE.encodeUtf8 :: TS.Text -> BS.ByteString
TSE.decodeUtf8 :: BS.ByteString -> TS.Text

-- Lazy <-> Strict

BL.fromStrict :: BS.ByteString -> BL.ByteString
BL.toStrict   :: BL.ByteString -> BS.ByteString
TL.fromStrict :: TS.Text -> TL.Text
TL.toStrict   :: TL.Text -> TS.Text


import Data.List.Split (splitOn)

import qualified EtradeJanitor.Repos.Stocks as RS

x = "a:b:c:x"

lx = splitOn ":" x

y = case lx of
    [v1,v2,v3] -> "Yep"
    _ -> "Nope"

tix = RS.tickers "172.20.1.3"
-}