
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Vector as V
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Netfonds as NF
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Repos.PaperHistory as RP

processTickers :: T.Tickers -> IO ()
processTickers tix =
    let
        cat3 = V.filter (\t -> (T.category t) == 3) tix
    in
        NF.savePaperHistoryTickers cat3 >>
        NF.saveDerivativesTickers tix >>
        RP.updateStockPricesTickers cat3


-- vol =
--     let
--         tag = TS.TagOpen ("td" :: String) [("name","ju.vo")]
--         findFn =  take 2 . dropWhile (~/= tag)
--         extractFn = TS.fromTagText . head . drop 1
--     in
--         ts $ extractFn . findFn

main :: IO ()
main =
  RS.tickers >>= \tix ->
      case tix of
        Right result -> processTickers result
        Left err -> putStrLn (show err)
