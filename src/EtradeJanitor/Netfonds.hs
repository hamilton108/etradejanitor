{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Netfonds where

import Control.Monad (forM_)
import Data.Text (Text,pack)
import Text.Printf (printf)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Network.HTTP.Req ((=:), (/:))
import qualified Network.HTTP.Req as R
import qualified Data.ByteString.Char8 as B
import qualified EtradeJanitor.Common.Types as T
--import EtradeJanitor.Common.Types (Ticker(..))

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
downloadDerivatives (T.Ticker _ ticker _) =
  let
    params = "underlying_paper" =: ticker <> "underlying_exchange" =: ("OSE" :: Text) <> "exchange" =: ("OMFE" :: Text )
  in
  R.req R.GET (R.http "hopey.netfonds.no" /: "derivative.php") R.NoReqBody R.bsResponse params

saveDerivatives :: T.Ticker -> IO ()
saveDerivatives ticker =
  R.runReq def $
  downloadDerivatives ticker >>= \bs ->
  liftIO $ B.writeFile (printf "%s.html" ticker) (R.responseBody bs)

{-|
    downloadPaperHistory returns a response BsResponse from the url like (f.ex NHY):

        http://www.netfonds.no/quotes/paperhistory.php?paper=NHY.OSE&csv_format=csv
-}
downloadPaperHistory :: T.Ticker -> R.Req R.BsResponse
downloadPaperHistory (T.Ticker _ ticker _) =
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
