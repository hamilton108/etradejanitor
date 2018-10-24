
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS -Wno-unused-imports #-}

module Demo  where

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

import qualified Data.Text as Tx
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup ((~==),(~/=))
import qualified Data.Time.Calendar as Cal
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Netfonds as NF
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Repos.PaperHistory as RP

import qualified Data.ByteString.Char8 as B

dx = Cal.fromGregorian 2018 10 1

tikr = T.Ticker 1 "NHY" 1 dx

dr = NF.derivativesResponseBody tikr >>= pure . NF.stockPriceDx . TS.parseTags . B.unpack

price = NF.derivativesResponseBody tikr >>= pure . NF.createStockPrice . TS.parseTags . B.unpack 

{-
price = NF.createStockPrice tikr

sdx = NF.soup tikr >>= return . NF.stockPriceDx

html :: IO String
html =
  NF.html tikr

xx =
  html >>= \htmlx ->
  return $ TS.parseTags htmlx


main3 =
  html >>= \htmlx ->
  putStr "Enter a term to search for: " >>
  getLine >>= \term ->
  let dict = parseDict $ TS.parseTags htmlx
  in
    putStrLn $ fromMaybe "No match found." $ lookup term dict

parseDict :: [TS.Tag String] -> [(String,String)]
parseDict = map parseItem
          . TS.sections (~== ("<dt>" :: String))
          . dropWhile (~/= ("<div class=glosslist>" :: String))

parseItem :: [TS.Tag String] -> (String,String)
parseItem xs = (TS.innerText a, unwords $ words $ TS.innerText b)
    where (a,b) = break (~== ("<dd>" :: String)) (takeWhile (~/= ("</dd>" :: String)) xs)

currentTime :: IO ()
currentTime = do
    tags <- tsx
    let time = TS.fromTagText (dropWhile (~/= ("<span id=ct>" :: String)) tags !! 1)
    putStrLn time

processTickers :: T.Tickers -> IO ()
processTickers tix =
    let
        cat1 = V.filter (\t -> (T.category t) == 1) tix
        cat3 = V.filter (\t -> (T.category t) == 3) tix
    in
        V.mapM_ (\t -> putStrLn (Tx.unpack $ T.ticker t)) cat1 >>
        putStrLn "Done 1!" >>
        V.mapM_ (\t -> putStrLn (Tx.unpack $ T.ticker t)) cat3 >>
        putStrLn "Done 3!"  >>
        NF.savePaperHistoryTickers cat3 >>
        NF.saveDerivativesTickers tix >>
        RP.updateStockPricesTickers tix
-}
