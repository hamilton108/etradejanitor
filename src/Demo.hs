
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS -Wno-unused-imports #-}

module Demo (main) where

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

dx = Cal.fromGregorian 2018 10 1

tikr = T.Ticker 1 "NHY" 1 dx

price = NF.createStockPrice tikr

sdx = NF.soup tikr >>= \soupx ->
      return $ NF.stockPriceDx soupx

html :: IO String
html =
  NF.html tikr

xx =
  html >>= \htmlx ->
  return $ TS.parseTags htmlx

-- ts :: IO (TS.Tag String)
ts f =
  -- html >>= return . TS.parseTags
  html >>= \htmlx ->
  let
    --findFn = TS.innerText . take 6 . dropWhile (~/= tag)
    soup = TS.parseTags htmlx
    --tag = TS.TagOpen ("table" :: String) [("id","updatetable1")]
    --tag = TS.TagOpen ("td" :: String) [("id","ju.l")]
    --findFn =  take 2 . dropWhile (~/= tag)
    --result = filter (~== tag) soup
    --result = dropWhile (~/= (TS.TagOpen ("" :: String) [("id","lastmod")])) soup
    result = f soup -- TS.innerText $ f soup
  in
    -- return $ head soup
    return result

jul =
  ("id","ju.l")

cls =
    let
        tag = TS.TagOpen ("td" :: String) [jul]
        findFn =  take 2 . dropWhile (~/= tag)
        extractFn = TS.fromTagText . head . drop 1
    in
        ts $ extractFn . findFn

opn  =
    let
        tag = TS.TagOpen ("td" :: String) [("name","ju.op")]
        findFn =  take 2 . dropWhile (~/= tag)
        extractFn = TS.fromTagText . head . drop 1
    in
        ts $ extractFn . findFn

hi =
    let
        tag = TS.TagOpen ("td" :: String) [("name","ju.h")]
        findFn =  take 2 . dropWhile (~/= tag)
        extractFn = TS.fromTagText . head . drop 1
    in
        ts $ extractFn . findFn

lo =
    let
        tag = TS.TagOpen ("td" :: String) [("name","ju.lo")]
        findFn =  take 2 . dropWhile (~/= tag)
        extractFn = TS.fromTagText . head . drop 1
    in
        ts $ extractFn . findFn

vol =
    let
        tag = TS.TagOpen ("td" :: String) [("name","ju.vo")]
        findFn =  take 2 . dropWhile (~/= tag)
        extractFn = TS.fromTagText . head . drop 1
    in
        ts $ extractFn . findFn

ts2 =
    let
        tag = TS.TagOpen ("td" :: String) [("id","ju.b.NHY.OSE")]
        findFn =  take 3 . dropWhile (~/= tag)
        extractFn = TS.fromTagText . head . drop 2
    in
        ts $ extractFn . findFn

ts3 =
    let
        tag = TS.TagOpen ("td" :: String) [("id","ju.b.NHY.OSE")]
        findFn =  take 3 . dropWhile (~/= tag)
        extractFn = TS.fromTagText . head . drop 2
    in
        ts $ extractFn . findFn
-- ts11 =
--   ts1 >>= \x ->
--   return $ (TS.fromTagText .head . drop 1) x

-- ts2 =
--     let
--         -- tag = TS.TagOpen ("td" :: String) [("class","leftalign")]
--         tag = TS.TagOpen ("tr" :: String) [("","")]
--         findFn =  TS.sections (~== tag)
--     in
--         ts $ findFn

tsx :: IO [TS.Tag String]
tsx =
  html >>= return . TS.parseTags

{-
main3 = do
    putStr "Enter a term to search for: "
    term <- getLine
    html <- readFile "NHY.html"
    let dict = parseDict $ TS.parseTags html
    putStrLn $ fromMaybe "No match found." $ lookup term dict
-}
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
      ticker = T.Ticker 1 "NHY" 1 $ Cal.fromGregorian 2018 10 1
    in
      NF.saveDerivatives ticker >>
      putStrLn "Done!"

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

main :: IO ()
main =
  RS.tickers >>= \tix ->
      case tix of
        Right result -> processTickers result
        Left err -> putStrLn (show err)
