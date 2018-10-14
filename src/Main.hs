
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS -Wno-unused-imports #-}

module Main (main) where

-- import Control.Monad.IO.Class (liftIO)
-- import Data.Default.Class (def)
-- import qualified Network.HTTP.Req as R
-- import Network.HTTP.Req ((=:), (/:))
-- import Data.Text (Text,pack)
-- import Text.Printf (printf)
-- import Data.Aeson (Value)
-- import qualified Data.ByteString.Char8 as B
--
-- import qualified Data.List as L
-- import Control.Monad (forM,forM_)
-- import Data.List.Split (splitOn)
-- import EtradeJanitor.Netfonds as NP
-- import EtradeJanitor.Repos.PaperHistory as RP
-- import EtradeJanitor.Common.Types (Ticker(..),StockPrice(..))
--
-- import System.IO (openFile,hSetEncoding,hGetContents,latin1,IOMode(..))

import qualified Data.Time.Calendar as Cal
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Netfonds as NF
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Repos.PaperHistory as RP


main2 :: IO ()
main2 =
    let
      ticker = T.Ticker 1 "NHY" $ Cal.fromGregorian 2018 10 1
    in
      -- NP.savePaperHistory ticker >>
      RP.updateStockPrices ticker >>= \e ->
      case e of
        Right () -> putStrLn "Done!"
        Left err -> putStrLn (show err)

processTickers :: T.Tickers -> IO ()
processTickers tix =
  NF.savePaperHistoryTickers tix >>
  RP.updateStockPricesTickers tix

main :: IO ()
main =
  RS.tickers >>= \tix ->
      case tix of
        Right result -> processTickers result
        Left err -> putStrLn (show err)
