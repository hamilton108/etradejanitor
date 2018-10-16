
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

import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup ((~==),(~/=))
import qualified Data.Time.Calendar as Cal
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Netfonds as NF
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Repos.PaperHistory as RP

dx = Cal.fromGregorian 2018 10 1

tikr = T.Ticker 1 "NHY" dx

html :: IO String
html =
  NF.fetchHtml tikr

-- ts :: IO (TS.Tag String)
ts =
  -- html >>= return . TS.parseTags
  html >>= \htmlx ->
  let
    --findFn = TS.innerText . take 6 . dropWhile (~/= tag)
    findFn =  take 6 . dropWhile (~/= tag)
    soup = TS.parseTags htmlx
    -- tag = TS.TagOpen ("table" :: String) [("id","updatetable1")]
    tag = TS.TagOpen ("td" :: String) [("id","ju.l")]
    -- result = filter (~== tag) soup
    -- result = dropWhile (~/= (TS.TagOpen ("" :: String) [("id","lastmod")])) soup
    result = findFn soup
  in
    -- return $ head soup
    return result

tsx :: IO [TS.Tag String]
tsx =
  html >>= return . TS.parseTags

currentTime :: IO ()
currentTime = do
    tags <- tsx
    let time = TS.fromTagText (dropWhile (~/= ("<span id=ct>" :: String)) tags !! 1)
    putStrLn time

-- csv  =
--   RP.fetchCsv tikr
--
-- demo =
--   let
--       t = T.Ticker 1 "NHY" $ Cal.fromGregorian 2018 10 1
--   in
--     NF.saveDerivatives t

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
