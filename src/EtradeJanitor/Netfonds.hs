{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Netfonds where

import Control.Monad (forM_)
import Data.Text (Text,pack)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Default.Class (def)
import Network.HTTP.Req ((=:), (/:))
import System.IO (IOMode(..))
import Text.HTML.TagSoup ((~/=))
import Text.Regex.TDFA ((=~))
import qualified Data.Int as DI
-- import qualified Data.Vector as V
import qualified System.IO as IO
import qualified Data.Time.Calendar as Cal
import qualified Text.Regex.TDFA as RE
import qualified Data.Vector as V
import qualified Text.HTML.TagSoup as TS
import qualified Network.HTTP.Req as R
import qualified Data.ByteString.Char8 as B
import qualified EtradeJanitor.Common.Types as T
--import qualified EtradeJanitor.Params as PA

type StringSoup = [TS.Tag String]

html :: T.Ticker -> T.REIO String
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

soup :: T.Ticker -> T.REIO StringSoup
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

stockPriceDx_ :: StringSoup -> (String, String, String)
stockPriceDx_ curSoup =
    let
        tag = TS.TagOpen ("span" :: String) [("id","toptime")]
        findFn =  take 2 . dropWhile (~/= tag)
        extractFn = TS.fromTagText . head . drop 1
        rawValue = (extractFn . findFn) curSoup
        match = rawValue =~ dateRe :: RE.AllTextMatches [] String
        dx = (head . RE.getAllTextMatches) match
        -- day = read (take 2 dx) :: Int
        -- month = read (take 2 . drop 3 $ dx) :: Int
        -- year = read (take 4 . drop 6 $ dx) :: Integer
        day = take 2 dx
        month = take 2 . drop 3 $ dx
        year = take 4 . drop 6 $ dx
    in
    (year,month,day)

stockPriceDx :: StringSoup -> T.IsoDate
stockPriceDx curSoup =
    let
      (year,month,day) = stockPriceDx_ curSoup
    in
      T.IsoDate year month day

stockPriceDay :: StringSoup -> Cal.Day
stockPriceDay curSoup =
    let
      (y,m,d) = stockPriceDx_ curSoup
      year = read y :: Integer
      month = read m :: Int
      day = read d :: Int
    in
    Cal.fromGregorian year month day

-- createStockPrice :: StringSoup -> T.StockPrice
-- createStockPrice soupx =
--   let opn = stockPriceVal soupx ("name", "ju.op")
--       hi = stockPriceVal soupx ("name", "ju.h")
--       lo = stockPriceVal soupx ("name", "ju.lo")
--       cls = stockPriceVal soupx ("id", "ju.l")
--       vol = filter (/= ' ') $ stockPriceVal soupx ("name", "ju.vo")
--       dx = (T.isoDateStr . stockPriceDx) soupx
--   in
--     T.StockPrice dx opn hi lo cls vol

--maybeStockPriceVal :: StringSoup -> TS.Attribute String -> Maybe a

createStockPrice :: T.Ticker -> StringSoup -> Maybe T.StockPrice
createStockPrice tikr soupx =
  let
      readFn :: Read a => TS.Attribute String -> Maybe a
      readFn attr = readMaybe $ stockPriceVal soupx attr

      readVol =
        readMaybe (filter (/= ' ') $ stockPriceVal soupx ("name", "ju.vo")) :: Maybe DI.Int64

      dx = stockPriceDay soupx
  in
    (readFn ("name","ju.op") :: Maybe Float) >>= \opn ->
    (readFn ("name","ju.h") :: Maybe Float) >>= \hi ->
    (readFn ("name","ju.lo") :: Maybe Float) >>= \lo ->
    (readFn ("id","ju.l") :: Maybe Float) >>= \cls ->
    readVol >>= \vol ->
    Just $ T.StockPrice tikr dx opn hi lo cls vol

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

saveDerivatives :: T.Ticker -> T.REIO ()
saveDerivatives ticker =
  ask >>= \env ->
  liftIO $
  derivativesResponseBody ticker >>= \bs ->
  B.writeFile (printf "%s/%s.html" (T.getHtmlPath env) ticker) bs -- (R.responseBody bs)

saveDerivativesTickers :: T.Tickers -> T.REIO ()
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

save_ :: String -> T.Ticker -> (T.Ticker -> R.Req R.BsResponse) -> IO ()
save_ fileName t myDownload =
  R.runReq def $
  myDownload  t >>= \bs ->
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

savePaperHistory :: FilePath -> T.Ticker -> IO ()
savePaperHistory feed t =
  let
    -- fileName = printf "%s/%s.csv" T.feed (T.ticker t)
    fileName = printf "%s/%s.csv" feed (T.ticker t)
  in
  putStrLn fileName >>
  save_ fileName t downloadPaperHistory

savePaperHistoryTickers :: FilePath -> T.Tickers -> IO ()
savePaperHistoryTickers feed tix =
  -- ask >>= \env ->
  -- if isHtmlOnly env == True then
  --   return ()
  -- else
  forM_ tix (savePaperHistory feed)

fetchStockPrice :: T.Ticker -> T.REIO (Maybe T.StockPrice)
fetchStockPrice tikr =
  soup tikr >>= \soupx ->
  pure $ createStockPrice tikr soupx


fetchStockPrices :: T.Tickers -> T.REIO (V.Vector (Maybe T.StockPrice))
fetchStockPrices tix =
  V.mapM fetchStockPrice tix


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

saveTradingDepth:: T.Ticker -> T.REIO ()
saveTradingDepth t =
  ask >>= \env ->
  let
    fileName = printf "%s/%s_dy.csv" (T.getHtmlPath env) t
  in
  liftIO $ save_ fileName t downloadTradingDepth


saveTradingDepthTickers :: T.Tickers -> T.REIO ()
saveTradingDepthTickers tix =
  ask >>= \env ->
  if T.isHtmlOnly env == True then
    return ()
  else
  forM_ tix saveTradingDepth
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


saveBuyersSellers :: T.Ticker -> T.REIO ()
saveBuyersSellers t =
  ask >>= \env ->
  let
    fileName = printf "%s/%s_hndl.csv" (T.getHtmlPath env) t
  in
  liftIO $ save_ fileName t downloadBuyersSellers

saveBuyersSellersTickers :: T.Tickers -> T.REIO ()
saveBuyersSellersTickers tix =
  ask >>= \env ->
  if T.isHtmlOnly env == True then
    return ()
  else
    forM_ tix saveBuyersSellers
