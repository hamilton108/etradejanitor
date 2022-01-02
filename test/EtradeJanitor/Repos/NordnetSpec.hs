{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.NordnetSpec
  ( spec
  )
where


import           Control.Monad.Reader           ( runReaderT )
import           Test.Hspec
import           Data.UUID                      ( nil )
import qualified Data.Time.Calendar            as Calendar
import qualified Data.Time.Clock.POSIX         as POSIX
import qualified Data.Vector                   as Vector
import qualified Data.Int                      as DI

import           EtradeJanitor.Repos.Nordnet    ( Prices(..) )
import qualified EtradeJanitor.Repos.Nordnet   as Nordnet
import qualified EtradeJanitor.Params          as Params
import           EtradeJanitor.Common.Types     ( Env(..)
                                                , OpeningPrice(..)
                                                , Ticker(..)
                                                , Tickers
                                                , runApp
                                                )
import qualified EtradeJanitor.Common.Misc     as Misc

testDay :: Calendar.Day
testDay =
  let year  = 2021 :: Integer
      month = 6 :: Int
      day   = 18 :: Int
  in  Calendar.fromGregorian year month day

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
testParams = Params.Params { Params.databaseIp               = "172.20.1.3"
                           , Params.redisHost                = "172.20.1.2"
                           , Params.redisPort                = "6379"
                           , Params.redisDatabase            = "5"
                           , Params.rabbitHost               = "172.20.1.4"
                           , Params.rabbitPort               = "5672"
                           , Params.feed = Misc.feedRoot ++ "/test/testfeed"
                           , Params.downloadDerivatives      = True
                           , Params.dbUpdateStocks           = True
                           , Params.skipIfDownloadFileExists = True
                           , Params.showStockTickers         = False
                           , Params.openingPricesToRedis     = False
                           }

testEnv :: Env
testEnv = Env testParams testDay Nothing nil

expectedPathName :: String
expectedPathName =
  "/home/rcs/opt/haskell/etradejanitor/test/testfeed/2021/6/18/EQNR"

testTicker :: Ticker
testTicker = Ticker 2 "EQNR" 1 testDay

testTickers :: Tickers
testTickers =
  let makeTik :: DI.Int64 -> DI.Int64 -> Ticker
      makeTik oid tickerCat = Ticker oid "Demo" tickerCat testDay
  in  Vector.fromList
        [makeTik 1 1, makeTik 2 1, makeTik 3 1, makeTik 6 1, makeTik 7 3]

spec :: Spec
spec = do
    {-
    describe "Nordnet URLs" $ do
        context "when download date is 2019-09-01" $ do
            it "expiry dates in UTC should be [..]" $ do
                testExpiryDates <- runReaderT (OptionExpiry.expiryTimes testDay) testEnv
                shouldBe testExpiryDates expectedExpiryDates 
    -}
  describe "Derviative Prices" $ do
    context "when download date is 2021-06-18 and option ticker is EQNR" $ do
      it ("path name should be " ++ expectedPathName) $ do
        testPathName <- runReaderT
          (runApp $ Nordnet.pathName (DerivativePrices testTicker))
          testEnv
        shouldBe testPathName expectedPathName
  describe "Downloadable tickers" $ do
    context "when tickers count == 5, and they contains 1 type 3 tickers" $ do
      it ("count should be 4") $ do
        let dt = Nordnet.downloadAbleTickers testTickers
        shouldBe (length dt) 4
  describe "Opening Prices" $ do
    context "when ticker is EQNR" $ do
      it ("opening price should be 28.26") $ do
        openingPrice <- runReaderT (runApp $ Nordnet.openingPrice testTicker)
                                   testEnv
        shouldBe openingPrice (OpeningPrice "EQNR" "235.90")


        {-
        context "when download date is 2019-09-01 and option ticker is NHY" $ do
            it "urls should be [..]" $ do
                testExpiryDates <- runReaderT (OptionExpiry.expiryTimes testDay) testEnv
                let ticker = Derivative.Ticker "NHY"
                let testUrls = Derivative.urls ticker testExpiryDates
                shouldBe testUrls expectedUrls
        -}
