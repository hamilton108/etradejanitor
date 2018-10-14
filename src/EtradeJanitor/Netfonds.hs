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
