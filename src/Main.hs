
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Vector as V
import qualified Data.Text as Tx
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Netfonds as NF
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Repos.PaperHistory as RP
import Control.Monad.Reader (ReaderT,runReaderT,ask)
import Control.Monad.IO.Class (liftIO)

-- processTickers :: T.Tickers -> IO ()
-- processTickers tix =
--     let
--         cat3 = V.filter (\t -> (T.category t) == 3) tix
--     in
--         NF.savePaperHistoryTickers cat3 >>
--         NF.saveDerivativesTickers tix >>
--         RP.updateStockPricesTickers cat3

xprocessTickers :: T.Tickers -> ReaderT T.Env IO ()
xprocessTickers tix =
  ask >>= \env ->
  liftIO $
  let
    putStrLn_ = putStrLn . (++(T.getFilePath env)) . Tx.unpack . T.ticker
    one = V.head tix
    rest = V.tail tix
  in
    V.mapM_ putStrLn_ rest >>
    putStrLn "Here I come! " >>
    putStrLn_ one >>
    pure ()


processTickers :: T.Tickers -> ReaderT T.Env IO ()
processTickers tix =
  NF.saveDerivativesTickers tix

currentFilePath :: FilePath
currentFilePath = "/home"

main :: IO ()
main =
  RS.tickers >>= \tix ->
      case tix of
        Right result ->
          runReaderT (processTickers result) $ T.Env currentFilePath
        Left err ->
          putStrLn (show err)
