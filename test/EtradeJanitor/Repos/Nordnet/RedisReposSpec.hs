{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Nordnet.RedisReposSpec (spec) where

import Control.Monad.Reader (runReaderT)
import Test.Hspec
import Data.Sort (sort)
import qualified Data.Time.Calendar as Calendar

import qualified EtradeJanitor.Repos.Nordnet.RedisRepos as RedisRepos 
import qualified EtradeJanitor.Params as PA
import qualified EtradeJanitor.Common.Types as T


prms = PA.Params 
        { PA.databaseIp = "172.20.1.3"
        , PA.redisHost = "172.20.1.2"
        , PA.redisDatabase = "5"
        , PA.feed = "/home/rcs/opt/haskell/etradejanitor/feedtmp"
        , PA.downloadDerivatives = False
        , PA.dbUpdateStocks = False
        , PA.skipIfDownloadFileExists = True
        , PA.showStockTickers = False
        , PA.openingPricesToRedis = False
        }

dx1 :: Calendar.Day
dx1 = Calendar.fromGregorian 2020 7 24

dx2 :: Calendar.Day
dx2 = Calendar.fromGregorian 2020 12 18

dx3 :: Calendar.Day
dx3 = Calendar.fromGregorian 2021 6 19

nhy :: T.Ticker
nhy = T.Ticker 
        { T.oid = 1
        , T.ticker = "NHY"
        , T.category = 1
        , T.date = dx1
        }

tel :: T.Ticker
tel = T.Ticker 
        { T.oid = 6
        , T.ticker = "TEL"
        , T.category = 1
        , T.date = dx1
        }

expiry1 :: [T.NordnetExpiry]
expiry1 = [1597960800000,1600380000000,1602799200000,1608246000000,1616108400000,1623967200000]

expiry2 :: [T.NordnetExpiry]
expiry2 = [1595541600000,1596146400000,1596751200000,1597356000000]

expiry_all = sort $ expiry1 ++ expiry2

expiry_2020_12_18 = drop 3 expiry1

spec :: Spec
spec = do
    describe "OptionExpiry" $ do
        context "when date is 2020-07-24 and ticker is NHY" $ do
            it "expiry dates should be all expirys" $ do
                let env = T.Env prms dx1
                actual <- runReaderT (RedisRepos.expiryTimes  nhy) env
                shouldBe (sort actual) expiry_all  
        context "when date is 2020-07-24 and ticker is TEL" $ do
            it "expiry dates should be all expiry-1" $ do
                let env = T.Env prms dx1
                actual <- runReaderT (RedisRepos.expiryTimes tel) env
                shouldBe (sort actual) expiry1 
        context "when date is 2020-12-18 and ticker is NHY" $ do
            it "expiry dates should be expiry_2020_12_18" $ do
                let env = T.Env prms dx2
                actual <- runReaderT (RedisRepos.expiryTimes  nhy) env
                shouldBe (sort actual) expiry_2020_12_18 
        context "when date is 2020-12-18 and ticker is TEL" $ do
            it "expiry dates should be expiry_2020_12_18" $ do
                let env = T.Env prms dx2
                actual <- runReaderT (RedisRepos.expiryTimes  nhy) env
                shouldBe (sort actual) expiry_2020_12_18 
        context "when date is 2021-06-19 and ticker is NHY" $ do
            it "expiry dates should be empty" $ do
                let env = T.Env prms dx3
                actual <- runReaderT (RedisRepos.expiryTimes nhy) env
                shouldBe actual [] 
