module EtradeJanitor.Repos.Nordnet.Derivative.DerivativeSpec (spec) where


import Control.Monad.Reader (runReaderT)
import Test.Hspec
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock.POSIX as POSIX

import qualified EtradeJanitor.Repos.Nordnet.Derivative as Derivative
import qualified EtradeJanitor.StockExchange.OptionExpiry as OptionExpiry 
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

expectedExpiryDates :: [POSIX.POSIXTime]
expectedExpiryDates = 
    [ 1568937600
    , 1571356800
    , 1573776000
    , 1576800000
    , 1584662400
    , 1592524800
    ]

testParams :: Params.Params
testParams = 
    Params.Params 
    { Params.databaseIp = "172.17.0.2"
    , Params.feed = Misc.feedRoot ++ "/test/feed" 
    , Params.downloadOnly = True
    , Params.updateDbOnly = True
    }

testEnv :: Types.Env 
testEnv = Types.Env testParams

expectedUrls :: [Derivative.Url]
expectedUrls = []

spec :: Spec
spec = do
    describe "Nordnet URLs" $ do
        context "when download date is 2019-09-01" $ do
            it "expiry dates in UTC should be [..]" $ do
                testExpiryDates <- runReaderT (OptionExpiry.expiryTimes testDay) testEnv
                shouldBe testExpiryDates expectedExpiryDates 
        context "when download date is 2019-09-01 and option ticker is NHY" $ do
            it "urls should be [..]" $ do
                testExpiryDates <- runReaderT (OptionExpiry.expiryTimes testDay) testEnv
                let ticker = Derivative.Ticker "NHY"
                let testUrls = Derivative.urls ticker testExpiryDates
                shouldBe testUrls expectedUrls
