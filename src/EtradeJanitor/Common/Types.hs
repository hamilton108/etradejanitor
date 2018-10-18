module EtradeJanitor.Common.Types where

import qualified Text.Printf as TP -- (formatString,PrintfArg(..))
import qualified Data.Int as DI
import qualified Data.Text as Tx
import qualified Data.Vector as DV
import qualified Data.Time.Calendar as Cal

data Ticker = Ticker DI.Int64 Tx.Text DI.Int64 Cal.Day deriving (Show)

type Tickers = DV.Vector Ticker

instance TP.PrintfArg Ticker where
  formatArg (Ticker _ t _ _) fmt = TP.formatString (Tx.unpack t) fmt

data StockPrice =
  StockPrice {
     dx :: String
    ,opn:: String
    ,hi :: String
    ,lo :: String
    ,cls :: String
    ,vol :: String
    } deriving (Show)
