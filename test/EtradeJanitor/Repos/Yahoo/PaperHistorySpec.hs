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
    , Params.feed = Misc.feedRoot ++ "/test/testfeed" 
    , Params.skipDownloadStockPrices = True
    , Params.skipDownloadDerivatives = True
    , Params.skipDbUpdateStocks = True
    , Params.skipIfDownloadFileExists = True
    , Params.showStockTickers = False
    }

testDay :: Calendar.Day
testDay = 
    let 
        year = 2020 :: Integer
        month = 1 :: Int 
        day = 14 :: Int
    in 
    Calendar.fromGregorian year month day

testEnv :: Types.Env 
testEnv = Types.Env testParams testDay

testTicker :: Types.Ticker
testTicker = 
    Types.Ticker 1 "EQNR" 1 testDay

testCsvPath :: FilePath
testCsvPath = Misc.feedRoot ++ "/test/testfeed/EQNR.csv"

spec :: Spec
spec = do
    describe "Yahoo Csv" $ do
        context "when Csv date is 2020-01-14" $ do
            it "dates in UTC should be [..]" $ do
                let result = PaperHistory.asDay "2020-01-14"
                shouldBe result testDay 
        context "when ticker is EQNR" $ do
            it "csv path name should be <top>/feed2/EQNR.csv" $ do
                result <- runReaderT (PaperHistory.csvPath testTicker) testEnv
                shouldBe result testCsvPath 