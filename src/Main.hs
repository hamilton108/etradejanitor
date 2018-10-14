
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

import EtradeJanitor.Common.Types (Ticker(..))
import EtradeJanitor.Netfonds as NP
import EtradeJanitor.Repos.PaperHistory as RP

main :: IO ()
main =
    let
      ticker = Ticker 1 "NHY"
    in
      -- NP.savePaperHistory ticker >>
      RP.updateStockPrices ticker >>= \e ->
      case e of
        Right () -> putStrLn "Done!"
        Left err -> putStrLn (show err)

main2 :: IO ()
main2 =
    let
      ticker = Ticker 1 "NHY"
    in
      -- NP.savePaperHistory ticker >>
      RP.updateStockPricesTickers [ticker]
