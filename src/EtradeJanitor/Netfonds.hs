{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Netfonds where

import Control.Monad (forM_)
import Data.Text (Text,pack)
import Text.Printf (printf)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT,runReaderT,ask)
import Data.Default.Class (def)
import Network.HTTP.Req ((=:), (/:))
import qualified System.IO as IO
import System.IO (IOMode(..))
import Text.HTML.TagSoup ((~/=))
import Text.Regex.TDFA ((=~))
import qualified Text.Regex.TDFA as RE
import qualified Text.HTML.TagSoup as TS
import qualified Network.HTTP.Req as R
import qualified Data.ByteString.Char8 as B
import qualified EtradeJanitor.Common.Types as T
--import EtradeJanitor.Common.Types (Ticker(..))

type StringSoup = [TS.Tag String]

html :: T.Ticker -> IO String
html t =
    let
        tickerHtml = printf "%s.html" (T.ticker t)

    in
      IO.openFile tickerHtml ReadMode >>= \inputHandle ->
      IO.hSetEncoding inputHandle IO.latin1 >> -- utf8
      IO.hGetContents inputHandle >>= \theInput ->
      return theInput

soup :: T.Ticker -> IO StringSoup
soup t =
  html t >>= pure . TS.parseTags

stockPriceVal :: StringSoup -> TS.Attribute String -> String
stockPriceVal curSoup attr  =
    let
        -- tag = TS.TagOpen ("td" :: String) [("name","ju.op")]
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


{-
private String paperHistoryUrl(String ticker) {
    return String.format("http://www.netfonds.no/quotes/paperhistory.php?paper=%s.OSE&csv_format=csv", ticker);
}

private String tickerUrl(String ticker) { <<<===
    return String.format("http://hopey.netfonds.no/derivative.php?underlying_paper=%s&underlying_exchange=OSE&exchange=OMFE", ticker);
}

private String indexUrl(String ticker) {
    return String.format("http://hopey.netfonds.no/peers.php?paper=%s&exchange=OSE", ticker);
}

private String depthUrl(String ticker) {
    return String.format("http://www.netfonds.no/quotes/posdump.php?paper=%s.OSE&csv_format=csv", ticker);
}

private String purchasesUrl(String ticker) {
    return String.format("http://www.netfonds.no/quotes/tradedump.php?paper=%s.OSE&csv_format=csv", ticker);
}
-}

{-|
    downloadDerivatives returns a response BsResponse from the url like (f.ex NHY):

        http://hopey.netfonds.no/derivative.php?underlying_paper=NHY&underlying_exchange=OSE&type=&exchange=OMFE
-}
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

  --liftIO $ B.writeFile (printf "%s.html" ticker) (R.responseBody bs)

saveDerivatives :: T.Ticker -> ReaderT T.Env IO ()
saveDerivatives ticker =
  liftIO $
  derivativesResponseBody ticker >>= \bs ->
  B.writeFile (printf "%s.html" ticker) bs -- (R.responseBody bs)

saveDerivativesTickers :: T.Tickers -> ReaderT T.Env IO ()
saveDerivativesTickers tix =
  forM_ tix saveDerivatives

{-|
    downloadPaperHistory returns a response BsResponse from the url like (f.ex NHY):

        http://www.netfonds.no/quotes/paperhistory.php?paper=NHY.OSE&csv_format=csv
-}
downloadPaperHistory :: T.Ticker -> R.Req R.BsResponse
downloadPaperHistory (T.Ticker _ ticker _ _) =
  let
    tickerParam = printf "%s.OSE" ticker
    params = "paper" =: (pack tickerParam) <> "csv_format" =: ("csv" :: Text)
  in
  R.req R.GET (R.http "netfonds.no" /: "quotes" /: "paperhistory.php") R.NoReqBody R.bsResponse params

{-|
    savePaperHistory gets a http response from downloadPaperHistory
    and writes it to a csv file (f.ex NHY):

        NHY.csv
-}
savePaperHistory :: T.Ticker -> IO ()
savePaperHistory ticker =
  R.runReq def $
  downloadPaperHistory ticker >>= \bs ->
  liftIO $ B.writeFile (printf "%s.csv" ticker) (R.responseBody bs)

savePaperHistoryTickers :: T.Tickers -> IO ()
savePaperHistoryTickers tix =
  forM_ tix savePaperHistory
