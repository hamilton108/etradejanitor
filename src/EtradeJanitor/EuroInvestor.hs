{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.EuroInvestor where
    
import Data.Text (Text,pack)
import Text.Printf (printf)
import Network.HTTP.Req ((=:), (/:))
import qualified Network.HTTP.Req as R
import Network.HTTP.Req (Url,Scheme(..))
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Data.Monoid (mempty)

import Data.Maybe (fromJust)

import Data.Int (Int64)
import qualified Data.Time.Calendar as Calendar

import qualified Data.Map.Strict as Map

import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Common.Html as Html

urlStem :: Url Https
urlStem = R.https "www.euroinvestor.com" /: "exchanges" /: "oslo-stock-exchange"


{-
urlMap :: Map.Map Int64 Text
urlMap = 
  Map.fromList [
    (fromIntegral 1 :: Int64, pack $ printf "%s/%s" urlStem ("norsk-hydro-asa-nok1098/340345/history" :: String))
  ]
-}

tickerUrl :: T.Ticker -> Text
tickerUrl t = 
  pack "www.vg.no"
  -- "www.euroinvestor.com" /: "exchanges" /: "oslo-stock-exchange" /: "norsk-hydro-asa-nok1098" /: "340345" /: "history"
  --pack "www.euroinvestor.com" -- /exchanges/oslo-stock-exchange/norsk-hydro-asa-nok1098/340345/history"
  -- fromJust $ Map.lookup (T.oid t) urlMap

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

-- https://www.euroinvestor.com/markets/stocks/europe/norway/obx/history

--------------------------------------------------------------------------
-------------------------- Paper History ---------------------------------
--------------------------------------------------------------------------
downloadPaperHistory :: T.Ticker -> R.Req R.BsResponse
downloadPaperHistory t =
  let
    myUrl = R.https "www.euroinvestor.com" /: "exchanges" /: "oslo-stock-exchange" /: "norsk-hydro-asa-nok1098" /: "340345" /: "history"
  in
  R.req R.GET myUrl R.NoReqBody R.bsResponse mempty

savePaperHistory :: FilePath -> T.Ticker -> IO ()
savePaperHistory feed t =
  let
    fileName = printf "%s/%s.html" feed (T.ticker t)
  in
  putStrLn fileName >>
  Html.save fileName t downloadPaperHistory

savePaperHistoryTickers :: FilePath -> T.Tickers -> IO ()
savePaperHistoryTickers feed tix =
  forM_ tix (savePaperHistory feed)