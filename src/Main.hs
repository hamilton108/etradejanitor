
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Vector as V
import qualified Data.Text as Tx
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Netfonds as NF
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Repos.PaperHistory as RP

xprocessTickers :: T.Tickers -> IO ()
xprocessTickers tix =
    let
        cat3 = V.filter (\t -> (T.category t) == 3) tix
    in
        NF.savePaperHistoryTickers cat3 >>
        NF.saveDerivativesTickers tix >>
        RP.updateStockPricesTickers cat3

processTickers :: T.Tickers -> IO ()
processTickers tix =
  let
    putStrLn_ = putStrLn . Tx.unpack . T.ticker
    one = V.head tix
    rest = V.tail tix
  in
    V.mapM_ putStrLn_ rest >>
    putStrLn "Here I come! " >>
    putStrLn_ one >>
    pure ()




main :: IO ()
main =
  RS.tickers >>= \tix ->
      case tix of
        Right result -> processTickers result
        Left err -> putStrLn (show err)
