{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Common.Html where

import qualified System.IO as IO
import Control.Monad.IO.Class (liftIO)

import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup (Tag(..),(~==),(~/=))
import qualified Network.HTTP.Req as R
import qualified Data.ByteString.Char8 as B

import EtradeJanitor.Common.Misc (decimalStrToFloat)
import EtradeJanitor.Common.Types (REIO,Ticker)
import qualified EtradeJanitor.Common.Types as T

import EtradeJanitor.Repos.Nordnet (Prices(..))
import qualified EtradeJanitor.Repos.Nordnet as Nordnet

html :: Ticker -> REIO String
html t =
    Nordnet.fileName (OpeningPrices t) >>= \tickerHtml ->
        liftIO $
        IO.openFile tickerHtml IO.ReadMode >>= \inputHandle ->
        IO.hSetEncoding inputHandle IO.latin1 >> -- utf8
        IO.hGetContents inputHandle >>= \theInput ->
        pure theInput

soup :: Ticker -> REIO [Tag String]
soup t =
  html t >>= pure . TS.parseTags

tr :: Ticker -> REIO [Tag String]
tr t = 
    soup t >>= \soupx ->
        let
            table = dropWhile (~/= ("<table>" :: String)) soupx
            tbody = dropWhile (~/= ("<tbody>" :: String)) table
        in
        pure $ dropWhile (~/= ("<tr>" :: String)) tbody

close :: Ticker -> REIO Float
close t = 
    tr t >>= \trx ->
        let 
            td = dropWhile (~/= TagOpen ("td" :: String) [("data-title","Siste")]) trx
            txt = (TS.fromTagText . head . drop 1) $ dropWhile (~/= TagOpen ("span" :: String) [("aria-hidden","true")]) td
        in
        pure $ decimalStrToFloat txt 

{-
save :: String -> T.Ticker -> (T.Ticker -> R.Req R.BsResponse) -> IO ()
save fileName t myDownload =
  R.runReq R.defaultHttpConfig $
  myDownload  t >>= \bs ->
  liftIO $ B.writeFile fileName (R.responseBody bs)
-}