module EtradeJanitor.Common.Types where

import qualified Text.Printf as TP -- (formatString,PrintfArg(..))
import qualified Data.Int as DI
import qualified Data.Text as Tx
import qualified Data.Vector as DV
import qualified Data.Time.Calendar as Cal
import Control.Monad.Reader (ReaderT)

import qualified EtradeJanitor.Params as PA

feed :: FilePath
feed = "/home/rcs/opt/haskell/etradejanitor/feed2"

type REIO = ReaderT Env IO

-- newtype DbIP = DbIP { getIp :: String }

newtype Env = Env { getParams :: PA.Params } deriving (Show)

{-
isHtmlOnly :: Env -> Bool
isHtmlOnly env =
  (PA.htmlOnly . getParams) env

isDownloadOnly :: Env -> Bool
isDownloadOnly env =
  (PA.downloadOnly . getParams) env
-}

data Ticker = 
    Ticker 
    { oid :: DI.Int64
    , ticker :: Tx.Text
    , category :: DI.Int64
    , date :: Cal.Day 
    } deriving (Eq,Show)

type Tickers = DV.Vector Ticker

instance TP.PrintfArg Ticker where
  formatArg (Ticker _ t _ _) fmt = TP.formatString (Tx.unpack t) fmt

data IsoDate = 
    IsoDate 
    { year :: String
    , month :: String
    , day :: String
    } deriving (Show)

isoDateStr :: IsoDate -> String
isoDateStr (IsoDate y m d) = y ++ "-" ++ m ++ "-" ++ d

data StockPrice = 
    StockPrice 
    { tick :: Ticker
    , dx2 :: Cal.Day
    , opn2 :: Float
    , hi2 :: Float
    , lo2 :: Float
    , cls2 :: Float
    , vol2 :: DI.Int64 
    } deriving (Eq,Show)
