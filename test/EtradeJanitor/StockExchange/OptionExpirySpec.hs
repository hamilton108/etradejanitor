{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.StockExchange.OptionExpirySpec (spec) where

import Control.Monad.Reader (runReaderT)
import Test.Hspec
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock.POSIX as POSIX

import qualified EtradeJanitor.Repos.Nordnet.Derivative as Derivative
import qualified EtradeJanitor.StockExchange.OptionExpiry as OptionExpiry 
import qualified EtradeJanitor.Params as Params
import qualified EtradeJanitor.Common.Types as Types
import qualified EtradeJanitor.Common.Misc as Misc
import qualified EtradeJanitor.Common.CalendarUtil as CalendarUtil

testDay :: Calendar.Day
testDay = 
    Calendar.fromGregorian 2019 9 1

{-
testParams :: Params.Params
testParams = 
    Params.Params 
    { Params.databaseIp = "172.17.0.2"
    , Params.feed = Misc.feedRoot ++ "/test/testfeed" 
    , Params.downloadOnly = True
    , Params.updateDbOnly = True
    }

testEnv :: Types.Env 
testEnv = Types.Env testParams
-}

expectedExpiryDate :: Maybe Types.NordnetExpiry 
expectedExpiryDate =
    Just 1568937600
    -- Just $ CalendarUtil.dayToUnixTime testDay

spec :: Spec
spec = do
    describe "OptionExpiry" $ do
        context "when string-date is 2019-09-20" $ do
            it "expiry date should be (Just 1568937600)" $ do
                let testExpiryDate = OptionExpiry.parseStringDate testDay "2019-09-20"
                shouldBe testExpiryDate expectedExpiryDate
