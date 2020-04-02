{-# LANGUAGE OverloadedStrings #-}

module Demo.Demo1 where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)

import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
-- import Network.HTTP.Req ((/:),(=:))
-- import qualified Network.HTTP.Req as Req
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Time.Calendar as Calendar

import qualified EtradeJanitor.StockExchange.OptionExpiry as OptionExpiry 
import qualified EtradeJanitor.Params as Params
import qualified EtradeJanitor.Common.Types as Types
import qualified EtradeJanitor.Common.Misc as Misc
import qualified EtradeJanitor.Repos.Nordnet.Derivative as Derivative
import qualified EtradeJanitor.Common.CalendarUtil as CalendarUtil
import qualified EtradeJanitor.Repos.Yahoo.PaperHistory as PaperHistory 

--import qualified Main 

testParams :: Params.Params
testParams = 
    Params.Params 
    { Params.databaseIp = "172.17.0.2"
    , Params.feed = Misc.feedRoot ++ "/test/testfeed" 
    , Params.skipDownloadStockPrices = True
    , Params.skipDownloadDerivatives = False
    , Params.skipDbUpdateStocks = True
    , Params.skipIfDownloadFileExists = True
    , Params.showStockTickers = True
    }

testDay :: Calendar.Day
testDay = 
    Calendar.fromGregorian 2020 1 19

testEnv :: Types.Env 
testEnv = Types.Env testParams testDay

-- testEnv :: IO Types.Env 
-- testEnv = Types.Env testParams CalendarUtil.today

testTicker :: Types.Ticker
testTicker = 
    Types.Ticker 1 "EQNR" 1 testDay

testTickers :: Types.Tickers
testTickers =
    Vector.fromList 
    [
          Types.Ticker 1 "NHY" 1 testDay
    ]


demo :: IO ()
demo = 
    runReaderT (PaperHistory.fetchStockPrices testTicker) testEnv >>= \prices ->
    mapM_ (putStrLn . show) prices

--demo2 :: IO ()
--demo2 = 
--    runReaderT (PaperHistory.download testTicker) testEnv


prices = 
    runReaderT (PaperHistory.fetchStockPrices testTicker) testEnv
    
showGreg :: String
showGreg = 
    PaperHistory.yahooDateFormat testDay

{-
        , Types.Ticker 2 "EQNR" 1 testDay
        , Types.Ticker 3 "YAR" 1 testDay
        , Types.Ticker 4 "SDRL" 1 testDay
        , Types.Ticker 6 "TEL" 1 testDay
        , Types.Ticker 7 "OBX" 1 testDay
        , Types.Ticker 8 "MHG" 1 testDay
        , Types.Ticker 9 "ORK" 1 testDay
        , Types.Ticker 11 "REC" 1 testDay
        , Types.Ticker 12 "PGS" 1 testDay
        , Types.Ticker 14 "STB" 1 testDay

        , Types.Ticker 16 "TGS" 1 testDay
        , Types.Ticker 17 "TOM" 1 testDay
        , Types.Ticker 18 "AKSO" 1 testDay

        , Types.Ticker 19 "DNB" 1 testDay
        , Types.Ticker 20 "DNO" 1 testDay
        , Types.Ticker 21 "GJF" 1 testDay

        , Types.Ticker 23 "SUBC" 1 testDay
        , Types.Ticker 25 "AKERBP" 1 testDay
        , Types.Ticker 26 "BWLPG" 1 testDay

        , Types.Ticker 27 "BAKKA" 1 testDay
        , Types.Ticker 28 "GOGL" 1 testDay
        , Types.Ticker 29 "NAS" 1 testDay
    ]
-}

{-
    Ticker {oid = 1, ticker = "NHY", category = 1, date = 2019-09-13}
    Ticker {oid = 2, ticker = "EQNR", category = 1, date = 2019-09-13}
    Ticker {oid = 3, ticker = "YAR", category = 1, date = 2019-09-13}
    Ticker {oid = 4, ticker = "SDRL", category = 1, date = 2019-09-13}
    Ticker {oid = 6, ticker = "TEL", category = 1, date = 2019-09-13}
    Ticker {oid = 7, ticker = "OBX", category = 3, date = 2019-09-13}
    Ticker {oid = 8, ticker = "MHG", category = 1, date = 2019-09-13}
    Ticker {oid = 9, ticker = "ORK", category = 1, date = 2019-09-13}
    Ticker {oid = 11, ticker = "REC", category = 1, date = 2019-09-13}
    Ticker {oid = 12, ticker = "PGS", category = 1, date = 2019-09-13}
    Ticker {oid = 14, ticker = "STB", category = 1, date = 2019-09-13}
    Ticker {oid = 16, ticker = "TGS", category = 1, date = 2019-09-13}
    Ticker {oid = 17, ticker = "TOM", category = 1, date = 2019-09-13}
    Ticker {oid = 18, ticker = "AKSO", category = 1, date = 2019-09-13}
    Ticker {oid = 19, ticker = "DNB", category = 1, date = 2019-09-13}
    Ticker {oid = 20, ticker = "DNO", category = 1, date = 2019-09-13}
    Ticker {oid = 21, ticker = "GJF", category = 1, date = 2019-09-13}
    Ticker {oid = 23, ticker = "SUBC", category = 1, date = 2019-09-13}
    Ticker {oid = 25, ticker = "AKERBP", category = 1, date = 2019-09-13}
    Ticker {oid = 26, ticker = "BWLPG", category = 1, date = 2019-09-13}
    Ticker {oid = 27, ticker = "BAKKA", category = 1, date = 2019-09-13}
    Ticker {oid = 28, ticker = "GOGL", category = 1, date = 2019-09-13}
    Ticker {oid = 29, ticker = "NAS", category = 1, date = 2019-09-13}
    

demo :: IO ()
demo = 
    CalendarUtil.today >>= \today ->
    let
        testEnv = Types.Env testParams today
    in
    runReaderT (OptionExpiry.expiryTimes testDay) testEnv >>= \exp ->
    runReaderT (Derivative.download testTicker exp) testEnv >>
    putStrLn (show exp)

demo2 :: IO ()
demo2 = 
    CalendarUtil.today >>= \today ->
    let
        testEnv = Types.Env testParams today
    in
    runReaderT (Derivative.downloadTickers testTickers) testEnv >>
    putStrLn "Done!" 
-}

--demo3 :: IO ()
--demo3 = Main.work testParams

 {-
dtou = CalendarUtil.dayToUnixTime
tmint = CalendarUtil.unixTimeToInt 
psd = OptionExpiry.parseStringDate 
expt = OptionExpiry.expiryTimes 
pth = OptionExpiry.expiryFileName
rfn = OptionExpiry.readExpiryFile

main :: IO ()
main = runReq defaultHttpConfig $ do
    -- This is an example of what to do when URL is given dynamically. Of
    -- course in a real application you may not want to use 'fromJust'.
    let (url, options) = fromJust (parseUrlHttps "https://httpbin.org/get?foo=bar")
    response <- req GET url NoReqBody jsonResponse $
        "from" =: (15 :: Int)           <>
        "to"   =: (67 :: Int)           <>
        basicAuth "username" "password" <>
        options                         <> -- contains the ?foo=bar part
        port 443 -- here you can put any port of course
    liftIO $ putStrLn "YES" -- print (responseBody response :: Value)

yax = 
    let 
        myurl = Req.https  "www.abc.com" 
        prm = Req.queryParam "foo" (Just 3 :: Maybe Int)
    in 
    9

main :: IO ()
main = runReq defaultHttpConfig $ do
    let n, seed :: Int
        n    = 5
        seed = 100
    bs <- req GET (https "httpbin.org" /: "bytes" /~ n) NoReqBody bsResponse $
        "seed" =: seed
    liftIO $ Char8.putStrLn "Hey ho" -- (responseBody bs)
-}