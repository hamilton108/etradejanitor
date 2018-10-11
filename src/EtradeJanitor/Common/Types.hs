module EtradeJanitor.Common.Types where

import Text.Printf (formatString,PrintfArg(..))

newtype Ticker = Ticker String deriving (Show)

instance PrintfArg Ticker where
  formatArg (Ticker t) fmt = formatString t fmt
  
data StockPrice =
  StockPrice {
     dx :: String
    ,opn:: String
    ,hi :: String
    ,lo :: String
    ,cls :: String
    ,vol :: String
    } deriving (Show)
