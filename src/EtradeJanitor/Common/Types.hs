module EtradeJanitor.Common.Types where

import Text.Printf (formatString,PrintfArg(..))

data Ticker = Ticker Int String deriving (Show)

instance PrintfArg Ticker where
  formatArg (Ticker _ t) fmt = formatString t fmt

data StockPrice =
  StockPrice {
     dx :: String
    ,opn:: String
    ,hi :: String
    ,lo :: String
    ,cls :: String
    ,vol :: String
    } deriving (Show)
