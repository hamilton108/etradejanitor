module EtradeJanitor.Common.Types where

import qualified Text.Printf as TP -- (formatString,PrintfArg(..))
import qualified Data.Int as DI
import qualified Data.Text as Tx
import qualified Data.Vector as DV
import qualified Data.Time.Calendar as Cal

import qualified EtradeJanitor.Params as PA

feed :: FilePath
feed = "/home/rcs/opt/haskell/etradejanitor/feed"

newtype DbIP = DbIP { getIp :: String }

data Env = Env { getHtmlPath :: FilePath, getParams :: PA.Params } deriving (Show)

data Ticker = Ticker { oid :: DI.Int64
                     , ticker :: Tx.Text
                     , category :: DI.Int64
                     , date :: Cal.Day } deriving (Eq,Show)

type Tickers = DV.Vector Ticker

instance TP.PrintfArg Ticker where
  formatArg (Ticker _ t _ _) fmt = TP.formatString (Tx.unpack t) fmt

data IsoDate = IsoDate { year :: String
                       , month :: String
                       , day :: String} deriving (Show)

isoDateStr :: IsoDate -> String
isoDateStr (IsoDate y m d) = y ++ "-" ++ m ++ "-" ++ d

-- data StockPrice =
--   StockPrice {
--       dx :: String
--     , opn:: String
--     , hi :: String
--     , lo :: String
--     , cls :: String
--     , vol :: String
--     } deriving (Show)

-- data Ax =
--   Ax {
--     ar :: DI.Int64
--     , pp :: Float
--     , tt :: Tx.Text
--     , dx3 :: Cal.Day
--     --   dx2  :: String
--     -- , opn2 :: String
--     -- , hi2  :: String
--     -- , lo2  :: String
--     -- , cls2 :: String
--     -- , vol2 :: String
--     } deriving (Show)

data StockPrice = StockPrice { tick :: Ticker
                               , dx2 :: Cal.Day
                               , opn2 :: Float
                               , hi2 :: Float
                               , lo2 :: Float
                               , cls2 :: Float
                               , vol2 :: DI.Int64 } deriving (Eq,Show)
