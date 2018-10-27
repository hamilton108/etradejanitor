{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Netfonds where

import Control.Monad (forM_)
import Data.Text (Text,pack)
import Text.Printf (printf)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT,runReaderT,ask)
import Data.Default.Class (def)
import Network.HTTP.Req ((=:), (/:))
import System.IO (IOMode(..))
import Text.HTML.TagSoup ((~/=))
import Text.Regex.TDFA ((=~))
-- import qualified Data.Vector as V
import qualified System.IO as IO
import qualified Text.Regex.TDFA as RE
import qualified Text.HTML.TagSoup as TS
import qualified Network.HTTP.Req as R
import qualified Data.ByteString.Char8 as B
import qualified EtradeJanitor.Common.Types as T

type StringSoup = [TS.Tag String]

html :: T.Ticker -> ReaderT T.Env IO String
html t =
    ask >>= \env ->
    liftIO $
    let
        tickerHtml = printf "%s/%s.html" (T.getHtmlPath env) (T.ticker t)
    in
    IO.openFile tickerHtml ReadMode >>= \inputHandle ->
    IO.hSetEncoding inputHandle IO.latin1 >> -- utf8
    IO.hGetContents inputHandle >>= \theInput ->
    return theInput

soup :: T.Ticker -> ReaderT T.Env IO StringSoup
soup t =
  html t >>= pure . TS.parseTags

stockPriceVal :: StringSoup -> TS.Attribute String -> String
stockPriceVal curSoup attr  =
    let
        tag = TS.TagOpen ("td" :: String) [attr]
        findFn =  take 2 . dropWhile (~/= tag)
        extractFn = TS.fromTagText . head . drop 1
    in
        (extractFn . findFn) curSoup

dateRe :: String
dateRe = "[0-9][0-9]/[0-9][0-9]-[0-9][0-9][0-9][0-9]"

stockPriceDx :: StringSoup -> T.IsoDate
stockPriceDx curSoup =
    let
        tag = TS.TagOpen ("span" :: String) [("id","toptime")]
        findFn =  take 2 . dropWhile (~/= tag)
        extractFn = TS.fromTagText . head . drop 1
        rawValue = (extractFn . findFn) curSoup
        match = rawValue =~ dateRe :: RE.AllTextMatches [] String
        dx = (head . RE.getAllTextMatches) match
        day = take 2 dx
        month = take 2 . drop 3 $ dx
        year = take 4 . drop 6 $ dx
    in
      T.IsoDate year month day

createStockPrice :: StringSoup -> T.StockPrice
createStockPrice soupx =
  let opn = stockPriceVal soupx ("name", "ju.op")
      hi = stockPriceVal soupx ("name", "ju.h")
      lo = stockPriceVal soupx ("name", "ju.lo")
      cls = stockPriceVal soupx ("id", "ju.l")
      vol = filter (/= ' ') $ stockPriceVal soupx ("name", "ju.vo")
      dx = (T.isoDateStr . stockPriceDx) soupx
  in
    T.StockPrice dx opn hi lo cls vol

--------------------------------------------------------------------------
------------------------------ Derivatives -------------------------------
--------------------------------------------------------------------------
downloadDerivatives :: T.Ticker -> R.Req R.BsResponse
downloadDerivatives (T.Ticker _ ticker _ _) =
  let
    params = "underlying_paper" =: ticker <> "underlying_exchange" =: ("OSE" :: Text) <> "exchange" =: ("OMFE" :: Text )
  in
  R.req R.GET (R.http "hopey.netfonds.no" /: "derivative.php") R.NoReqBody R.bsResponse params

derivativesResponseBody :: T.Ticker -> IO B.ByteString
derivativesResponseBody ticker =
  R.runReq def $
  downloadDerivatives ticker >>= pure . R.responseBody

saveDerivatives :: T.Ticker -> ReaderT T.Env IO ()
saveDerivatives ticker =
  ask >>= \env ->
  liftIO $
  derivativesResponseBody ticker >>= \bs ->
  B.writeFile (printf "%s/%s.html" (T.getHtmlPath env) ticker) bs -- (R.responseBody bs)

saveDerivativesTickers :: T.Tickers -> ReaderT T.Env IO ()
saveDerivativesTickers tix =
  forM_ tix saveDerivatives

--------------------------------------------------------------------------
--------------------------------- Common ---------------------------------
--------------------------------------------------------------------------
download_ :: T.Ticker -> R.Url a -> R.Req R.BsResponse
download_ t myHttp =
  let
    ticker = (T.ticker t)
    tickerParam = printf "%s.OSE" ticker
    params = "paper" =: (pack tickerParam) <> "csv_format" =: ("csv" :: Text)
  in
  R.req R.GET myHttp R.NoReqBody R.bsResponse params

save_ :: T.Ticker -> Maybe String -> (T.Ticker -> R.Req R.BsResponse) -> IO ()
save_ t postFix myDownload =
  R.runReq def $
  myDownload t >>= \bs ->
  let
    fileName = case postFix of
                Nothing -> printf "%s/%s.csv" T.feed t
                Just pf -> printf "%s/%s_%s.csv" T.feed t pf
  in
  liftIO $ B.writeFile fileName (R.responseBody bs)

--------------------------------------------------------------------------
-------------------------- Paper History ---------------------------------
--------------------------------------------------------------------------
-- private String paperHistoryUrl(String ticker) {
--     return String.format("http://www.netfonds.no/quotes/paperhistory.php?paper=%s.OSE&csv_format=csv", ticker);
downloadPaperHistory :: T.Ticker -> R.Req R.BsResponse
downloadPaperHistory t =
    let
      myUrl = R.http "netfonds.no" /: "quotes" /: "paperhistory.php"
    in
    download_ t myUrl

savePaperHistory :: T.Ticker -> IO ()
savePaperHistory t =
  save_ t Nothing downloadTradingDepth
  -- R.runReq def $
  -- downloadPaperHistory ticker >>= \bs ->
  -- liftIO $ B.writeFile (printf "%s/%s.csv" T.feed ticker) (R.responseBody bs)


savePaperHistoryTickers :: T.Tickers -> IO ()
savePaperHistoryTickers tix =
  forM_ tix savePaperHistory

--------------------------------------------------------------------------
------------------------------- Trading Depth-----------------------------
--------------------------------------------------------------------------
-- private String depthUrl(String ticker) {
--     return String.format("http://www.netfonds.no/quotes/posdump.php?paper=%s.OSE&csv_format=csv", ticker);
downloadTradingDepth :: T.Ticker -> R.Req R.BsResponse
downloadTradingDepth t =
    let
      myUrl = R.http "netfonds.no" /: "quotes" /: "posdump.php"
    in
    download_ t myUrl

saveTradingDepth:: T.Ticker -> IO ()
saveTradingDepth t =
  save_ t (Just "dy") downloadTradingDepth
  -- R.runReq def $
  -- downloadTradingDepth t >>= \bs ->
  -- liftIO $ B.writeFile (printf "%s/%s_dy.csv" T.feed t) (R.responseBody bs)

saveTradingDeptTickers :: T.Tickers -> IO ()
saveTradingDeptTickers tix =
  forM_ tix savePaperHistory
--------------------------------------------------------------------------
------------------------------- Byers and Sellers ------------------------
--------------------------------------------------------------------------
-- private String purchasesUrl(String ticker) {
--     return String.format("http://www.netfonds.no/quotes/tradedump.php?paper=%s.OSE&csv_format=csv", ticker);


downloadBuyersSellers :: T.Ticker -> R.Req R.BsResponse
downloadBuyersSellers t =
    let
      myUrl = R.http "netfonds.no" /: "quotes" /: "tradedump.php"
    in
    download_ t myUrl


saveBuyersSellers :: T.Ticker -> IO ()
saveBuyersSellers t =
  save_ t (Just "hndl") downloadBuyersSellers
  -- R.runReq def $
  -- downloadBuyersSellers t >>= \bs ->
  -- liftIO $ B.writeFile (printf "%s/%s_hndl.csv" T.feed t) (R.responseBody bs)


saveBuyersSellersTickers :: T.Tickers -> IO ()
saveBuyersSellersTickers tix =
  forM_ tix savePaperHistory
