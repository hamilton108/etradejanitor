{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Yahoo.PaperHistorySpec (spec) where

import Control.Monad.Reader (runReaderT)
import Test.Hspec
import qualified Data.Time.Calendar as Calendar
import qualified System.FilePath as FilePath

import qualified EtradeJanitor.Common.Types as Types
import qualified EtradeJanitor.Common.Misc as Misc
import qualified EtradeJanitor.Repos.Yahoo.PaperHistory as PaperHistory
import qualified EtradeJanitor.Params as Params

testParams :: Params.Params
testParams = 
    Params.Params 
    { Params.databaseIp = "172.17.0.2"
    , Params.redisHost = "172.20.1.2"
    , Params.feed = Misc.feedRoot ++ "/test/testfeed/paperhistory" 
    --, Params.skipDownloadStockPrices = True
    , Params.downloadDerivatives = True
    , Params.dbUpdateStocks = True
    , Params.skipIfDownloadFileExists = True
    , Params.showStockTickers = False
    , Params.openingPricesToRedis = False
    }

testDay :: Calendar.Day
testDay = 
    Calendar.fromGregorian 2019 12 30

{-
testDayNhy :: Calendar.Day
testDayNhy = 
    Calendar.fromGregorian 2020 04 20
-}

testEnv :: Types.Env 
testEnv = Types.Env testParams testDay

testTicker :: Calendar.Day -> Types.Ticker
testTicker dx = 
    Types.Ticker 1 "EQNR" 1 dx

testTickerNhy :: Calendar.Day -> Types.Ticker
testTickerNhy dx = 
    Types.Ticker 1 "NHY" 1 dx

testCsvPath :: FilePath
testCsvPath = Misc.feedRoot ++ "/test/testfeed/paperhistory/EQNR.csv"

spec :: Spec
spec = do
    describe "Yahoo Csv" $ do
        context "when Csv date is 2019-12-30" $ do
            it "dates in UTC should be [..]" $ do
                let curDate = Calendar.fromGregorian 2019 12 30 
                let result = PaperHistory.asDay "2019-12-30"
                shouldBe result curDate 
        context "when ticker is EQNR" $ do
            it ("csv path name should be " ++ testCsvPath) $ do
                result <- runReaderT (Types.runApp $ PaperHistory.csvPath (testTicker testDay)) testEnv
                shouldBe result testCsvPath 
        context "when ticker is EQNR" $ do
            it "number of stock prices should be 9" $ do
                result <- runReaderT (Types.runApp $ PaperHistory.fetchStockPrices (testTicker testDay)) testEnv
                shouldBe (length result) 9
        context "when ticker is EQNR and contains 3 faulty data" $ do
            it "number of stock prices should be 12" $ do
                let curDate = Calendar.fromGregorian 2019 12 17
                result <- runReaderT (Types.runApp $ PaperHistory.fetchStockPrices (testTicker curDate)) testEnv
                shouldBe (length result) 12
        context "when ticker is NHY and contains only one day" $ do
            it "number of stock prices should be 1" $ do
                let curDate = Calendar.fromGregorian 2020 04 20
                result <- runReaderT (Types.runApp $ PaperHistory.fetchStockPrices (testTickerNhy curDate)) testEnv
                shouldBe (length result) 1
        context "when ticker is NHY and contains only one day and ticker date greater than csv date" $ do
            it "number of stock prices should be 0" $ do
                let curDate = Calendar.fromGregorian 2020 04 22
                result <- runReaderT (Types.runApp $ PaperHistory.fetchStockPrices (testTickerNhy curDate)) testEnv
                shouldBe (length result) 0