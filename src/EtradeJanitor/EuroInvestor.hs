{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.EuroInvestor where
    
import Data.Text (Text,pack)
import Text.Printf (printf)
import Network.HTTP.Req ((=:), (/:))
import qualified Network.HTTP.Req as R
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class (liftIO)

import qualified EtradeJanitor.Common.Types as T

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
  R.runReq R.defaultHttpConfig $
  myDownload  t >>= \bs ->
  liftIO $ B.writeFile fileName (R.responseBody bs)

--------------------------------------------------------------------------
-------------------------- Paper History ---------------------------------
--------------------------------------------------------------------------
downloadPaperHistory :: T.Ticker -> R.Req R.BsResponse
downloadPaperHistory t =
    let
      myUrl = R.http "netfonds.no" /: "quotes" /: "paperhistory.php"
    in
    download_ t myUrl