module EtradeJanitor.Common.Types where

import qualified Text.Printf as TP -- (formatString,PrintfArg(..))
import qualified Data.Int as DI
import qualified Data.Text as Tx
import qualified Data.Vector as DV
import qualified Data.Time.Calendar as Cal

feed :: FilePath
feed = "/home/rcs/opt/haskell/etradejanitor/feed"

newtype Env = Env { getHtmlPath :: FilePath } deriving (Show)

data Ticker = Ticker { oid :: DI.Int64
                     , ticker :: Tx.Text
                     , category :: DI.Int64
                     , date :: Cal.Day } deriving (Show)

type Tickers = DV.Vector Ticker

instance TP.PrintfArg Ticker where
  formatArg (Ticker _ t _ _) fmt = TP.formatString (Tx.unpack t) fmt

data IsoDate = IsoDate { year :: String
                       , month :: String
                       , day :: String} deriving (Show)

isoDateStr :: IsoDate -> String
isoDateStr (IsoDate y m d) = y ++ "-" ++ m ++ "-" ++ d

data StockPrice =
  StockPrice {
     dx :: String
    ,opn:: String
    ,hi :: String
    ,lo :: String
    ,cls :: String
    ,vol :: String
    } deriving (Show)
