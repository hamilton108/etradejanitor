{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Common.Html where

import Data.Text (Text,pack)
import Text.Printf (printf)
import qualified System.IO as IO
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)

import qualified Text.HTML.TagSoup as TS
import qualified Network.HTTP.Req as R
import qualified Data.ByteString.Char8 as B

import Network.HTTP.Req ((=:), (/:))

import qualified EtradeJanitor.Common.Types as T

type StringSoup = [TS.Tag String]

html :: T.Ticker -> T.REIO String
html t =
    ask >>= \env ->
    liftIO $
    let
        tickerHtml = printf "%s/%s.html" (T.getHtmlPath env) (T.ticker t)
    in
    IO.openFile tickerHtml IO.ReadMode >>= \inputHandle ->
    IO.hSetEncoding inputHandle IO.latin1 >> -- utf8
    IO.hGetContents inputHandle >>= \theInput ->
    return theInput

soup :: T.Ticker -> T.REIO StringSoup
soup t =
  html t >>= pure . TS.parseTags

{-
download :: T.Ticker -> R.Url a -> R.Req R.BsResponse
download t myHttp =
  let
    ticker = (T.ticker t)
    tickerParam = printf "%s.OSE" ticker
    params = "paper" =: (pack tickerParam) <> "csv_format" =: ("csv" :: Text)
  in
  R.req R.GET myHttp R.NoReqBody R.bsResponse params
-}

save :: String -> T.Ticker -> (T.Ticker -> R.Req R.BsResponse) -> IO ()
save fileName t myDownload =
  R.runReq R.defaultHttpConfig $
  myDownload  t >>= \bs ->
  liftIO $ B.writeFile fileName (R.responseBody bs)