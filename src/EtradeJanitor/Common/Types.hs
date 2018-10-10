module EtradeJanitor.Common.Types where

import Text.Printf (formatString,PrintfArg(..))

newtype Ticker = Ticker String deriving (Show)

instance PrintfArg Ticker where
  formatArg (Ticker t) fmt = formatString t fmt
