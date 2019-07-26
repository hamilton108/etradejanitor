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

-- urlStem :: Url 'Https
urlStem = R.https "www.euroinvestor.com" /: "exchanges" /: "oslo-stock-exchange"

{-
urlMap :: Map.Map Int64 Text
urlMap = 
  Map.fromList [
    (fromIntegral 1 :: Int64, pack $ printf "%s/%s" urlStem ("norsk-hydro-asa-nok1098/340345/history" :: String))
  ]
-}

-- tickerUrl :: T.Ticker -> Text
{-
tickerUrl t = 
    let 
        urlStem = R.https "www.euroinvestor.com" /: "exchanges" /: "oslo-stock-exchange"
    in 
-}
  
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

--https://www.euroinvestor.com/markets/stocks/europe/norway/obx/history 

{-
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/statoil-dmob/26207570/history 2
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/yara-international-nok17/440822/history 3
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/seadrill-ltd-usd2/514951 4
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/telenor-asa-ord-nok6/340363 6
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/marine-harvest-dmob/26207560 8
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/orkla-asa-nok125/340387 9
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/rec-silicon-asa-nok1/572189 11
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/petroleum-geo-svs-nok3/340407 12
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/storebrand-asa-seranok5/340352 14
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/tgs-nopec-geophco-nok025/340383 16
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/tomra-systems-asa-nok1/340366 17
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/aker-solutions-asa-nok108/1087272 18
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/dnb-asa-nok10/5656880 19 
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/dno-asa-nok025/340378 20
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/subsea-7-sa-com-usd2/2956006  23
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/aker-bp-asa-nok1/32707136 25
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/bw-lpg-ltd-usd001/17893004 26
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/p-f-bakkafrost-dkk1/2270837 27
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/golden-ocean-group-com-usd001/482749 28 
https://www.euroinvestor.com/exchanges/oslo-stock-exchange/norwegian-air-shut-nok010/432011 29
-}

data TickerUrl = 
    TickerUrl 
    { first :: Text
    , second :: Text
    } deriving (Show)

urlMap :: Map.Map Int64 TickerUrl 
urlMap = 
    Map.fromList 
    [ (1, TickerUrl "norsk-hydro-asa-nok1098" "340345")
    , (2, TickerUrl "statoil-dmob" "26207570" )
    , (3, TickerUrl "yara-international-nok17" "440822")
    , (4, TickerUrl "seadrill-ltd-usd2" "514951" )
    , (6, TickerUrl "telenor-asa-ord-nok6" "340363" )
    , (8, TickerUrl "marine-harvest-dmob" "26207560" )
    , (9, TickerUrl "orkla-asa-nok125" "340387" )
    , (11, TickerUrl "rec-silicon-asa-nok1" "572189" )
    , (12, TickerUrl "petroleum-geo-svs-nok3" "340407" )
    , (14, TickerUrl "storebrand-asa-seranok5" "340352" )
    , (16, TickerUrl "tgs-nopec-geophco-nok025" "340383" )
    , (17, TickerUrl "tomra-systems-asa-nok1" "340366" )
    , (18, TickerUrl "aker-solutions-asa-nok108" "1087272" )
    , (19, TickerUrl "dnb-asa-nok10" "5656880" )
    , (20, TickerUrl "dno-asa-nok025" "340378" )
    , (23, TickerUrl "subsea-7-sa-com-usd2" "2956006" )
    , (25, TickerUrl "aker-bp-asa-nok1" "32707136" )
    , (26, TickerUrl "bw-lpg-ltd-usd001" "17893004" )
    , (27, TickerUrl "p-f-bakkafrost-dkk1" "2270837" )
    , (28, TickerUrl "golden-ocean-group-com-usd001" "482749" )
    , (29, TickerUrl "norwegian-air-shut-nok010" "432011" )
    ]

--------------------------------------------------------------------------
-------------------------- Paper History ---------------------------------
--------------------------------------------------------------------------
downloadPaperHistory :: T.Ticker -> R.Req R.BsResponse
downloadPaperHistory t =
  let
    tu = fromJust $ Map.lookup (T.oid t) urlMap
    myUrl = R.https "www.euroinvestor.com" /: "exchanges" /: "oslo-stock-exchange" /: first tu /: second tu /: "history"
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