module Demo where

import Data.List.Split (splitOn)

import qualified EtradeJanitor.Repos.Stocks as RS

x = "a:b:c:x"

lx = splitOn ":" x

y = case lx of
    [v1,v2,v3] -> "Yep"
    _ -> "Nope"

tix = RS.tickers "172.20.1.3"