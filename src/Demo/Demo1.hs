{-# LANGUAGE OverloadedStrings #-}

module Demo.Demo1 where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)

import Data.Maybe (fromJust)
import Data.Text as Text
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
-- import qualified Main 

testParams :: Params.Params
testParams = 
    Params.Params 
    { Params.databaseIp = "172.17.0.2"
    , Params.feed = Misc.feedRoot ++ "/test/testfeed" 
    , Params.skipDownload = True
    , Params.skipDbUpdateStocks = True
    , Params.showStockTickers = True
    }

testDay :: Calendar.Day
testDay = 
    Calendar.fromGregorian 2019 9 12

testEnv :: Types.Env 
testEnv = Types.Env testParams

testTicker :: Types.Ticker
testTicker = 
    Types.Ticker 1 "NHY" 1 testDay

demo :: IO ()
demo = 
    runReaderT (OptionExpiry.expiryTimes testDay) testEnv >>= \exp ->
    runReaderT (Derivative.download testTicker exp) testEnv >>
    putStrLn (show exp)

{-
demo2 :: IO ()
demo2 = Main.work testParams
-}

dtou = CalendarUtil.dayToUnixTime
tmint = CalendarUtil.unixTimeToInt 
psd = OptionExpiry.parseStringDate 
expt = OptionExpiry.expiryTimes 
pth = OptionExpiry.expiryFileName
rfn = OptionExpiry.readExpiryFile

 {-
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