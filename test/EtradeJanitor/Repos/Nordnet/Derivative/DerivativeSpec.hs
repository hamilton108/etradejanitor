{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Nordnet.Derivative.DerivativeSpec (spec) where


import Control.Monad.Reader (runReaderT)
import Test.Hspec
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Vector as Vector
import qualified Data.Int as DI

import qualified EtradeJanitor.Repos.Nordnet.Derivative as Derivative
import qualified EtradeJanitor.Params as Params
import qualified EtradeJanitor.Common.Types as Types
import qualified EtradeJanitor.Common.Misc as Misc

testDay :: Calendar.Day
testDay = 
    let 
        year = 2019 :: Integer
        month = 9 :: Int 
        day = 1 :: Int
    in 
    Calendar.fromGregorian year month day

{-
expectedExpiryDates :: [Types.NordnetExpiry] 
expectedExpiryDates = 
    [ 1568937600
    , 1571356800
    , 1573776000
    , 1576800000
    , 1584662400
    , 1592524800
    ]

expectedExpiryDates :: [Types.NordnetExpiry] 
expectedExpiryDates = 
    [ 1568930400
    , 1571349600
    , 1573772400
    , 1576796400
    , 1584658800
    , 1592517600
    ]
-}
    
testParams :: Params.Params
testParams = 
    Params.Params 
    { Params.databaseIp = "172.17.0.2"
    , Params.redisHost = "172.20.1.2"
    , Params.feed = Misc.feedRoot ++ "/test/testfeed" 
    -- , Params.skipDownloadStockPrices = True
    , Params.skipDownloadDerivatives = True
    , Params.skipDbUpdateStocks = True
    , Params.skipIfDownloadFileExists = True
    , Params.showStockTickers = False
    , Params.openingPricesToRedis = False
    }

testEnv :: Types.Env 
testEnv = Types.Env testParams testDay

expectedPathName :: String 
expectedPathName = 
    "/home/rcs/opt/haskell/etradejanitor/test/testfeed/2019/9/1/NHY"

testTicker :: Types.Ticker
testTicker = 
    Types.Ticker 1 "NHY" 1 testDay

testTickers :: Types.Tickers
testTickers =
    let 
        makeTik :: DI.Int64 -> DI.Int64 -> Types.Ticker
        makeTik oid tickerCat = 
            Types.Ticker oid "Demo" tickerCat testDay
    in
    Vector.fromList
    [
          makeTik 1 1
        , makeTik 2 1
        , makeTik 3 1
        , makeTik 6 1
        , makeTik 7 3
    ]

spec :: Spec
spec = do
    {-
    describe "Nordnet URLs" $ do
        context "when download date is 2019-09-01" $ do
            it "expiry dates in UTC should be [..]" $ do
                testExpiryDates <- runReaderT (OptionExpiry.expiryTimes testDay) testEnv
                shouldBe testExpiryDates expectedExpiryDates 
    -}
    describe "Derivative" $ do
        context "when download date is 2019-09-01 and option ticker is NHY" $ do
            it ("path name should be " ++ expectedPathName) $ do
                testPathName <- runReaderT (Derivative.pathNameFor testTicker) testEnv
                shouldBe testPathName expectedPathName
    describe "Downloadable tickers" $ do
        context "when tickers count == 5, and they contains 1 type 3 tickers" $ do
            it ("count should be 4") $ do
                let dt = Derivative.downloadAbleTickers testTickers
                shouldBe (length dt) 4

        {-
        context "when download date is 2019-09-01 and option ticker is NHY" $ do
            it "urls should be [..]" $ do
                testExpiryDates <- runReaderT (OptionExpiry.expiryTimes testDay) testEnv
                let ticker = Derivative.Ticker "NHY"
                let testUrls = Derivative.urls ticker testExpiryDates
                shouldBe testUrls expectedUrls
        -}
