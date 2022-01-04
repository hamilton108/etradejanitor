{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Nordnet.RedisReposSpec
  ( spec
  )
where

import           Control.Monad.Reader           ( runReaderT )
import           Test.Hspec
import           Data.Sort                      ( sort )
import           Data.UUID                      ( nil )
import qualified Data.Time.Calendar            as Calendar

import qualified EtradeJanitor.Repos.Nordnet.RedisRepos
                                               as RedisRepos
import qualified EtradeJanitor.Params          as PA
import qualified EtradeJanitor.Common.Types    as T


prmsRdb5 = PA.Params { PA.databaseIp               = "172.20.1.3"
                     , PA.redisHost                = "172.20.1.2"
                     , PA.redisDatabase            = "5"
                     , PA.feed = "/home/rcs/opt/haskell/etradejanitor/feedtmp"
                     , PA.downloadDerivatives      = False
                     , PA.dbUpdateStocks           = False
                     , PA.skipIfDownloadFileExists = True
                     , PA.showStockTickers         = False
                     , PA.openingPricesToRedis     = False
                     }

dx1 :: Calendar.Day
dx1 = Calendar.fromGregorian 2020 7 24

dx2 :: Calendar.Day
dx2 = Calendar.fromGregorian 2021 6 16
 
dx3 :: Calendar.Day
dx3 = Calendar.fromGregorian 2022 12 16

expiry1 :: [T.NordnetExpiry]
expiry1 =
  [ 1618524000000
  , 1621548000000
  , 1623967200000
  , 1631829600000
  , 1639695600000
  , 1645138800000
  , 1647558000000
  , 1655416800000
  , 1663279200000
  , 1671145200000]

spec :: Spec
spec = do
  describe "OptionExpiry" $ do
    context "when date is 2020-04-24" $ do
      it "expiry dates should be all expirys" $ do
        let env = T.Env prmsRdb5 dx1 Nothing nil
        actual <- runReaderT (T.runApp $ RedisRepos.expiryTimes2) env
        shouldBe (sort actual) expiry1
    context "when date is 2021-06-16" $ do
      it "count of expiry dates should be 8" $ do
        let env = T.Env prmsRdb5 dx2 Nothing nil
        actual <- runReaderT (T.runApp $ RedisRepos.expiryTimes2) env
        shouldBe (length actual) 8
    context "when date is 2022-12-16" $ do
      it "count expiry dates should be 0" $ do
        let env = T.Env prmsRdb5 dx3 Nothing nil
        actual <- runReaderT (T.runApp $ RedisRepos.expiryTimes2) env
        shouldBe actual [] 

{-
expiry1 :: [T.NordnetExpiry]
expiry1 =
  [ 1597960800000
  , 1600380000000
  , 1602799200000
  , 1608246000000
  , 1616108400000
  , 1623967200000
  ]

expiry2 :: [T.NordnetExpiry]
expiry2 = [1595541600000, 1596146400000, 1596751200000, 1597356000000]

expiry_all = sort $ expiry1 ++ expiry2

expiry_2020_12_18 = drop 3 expiry1

spec :: Spec
spec = do
  describe "OptionExpiry" $ do
    context "when date is 2020-07-24 and ticker is NHY" $ do
      it "expiry dates should be all expirys" $ do
        let env = T.Env prmsRdb5 dx1 Nothing nil
        actual <- runReaderT (T.runApp $ RedisRepos.expiryTimes nhy) env
        shouldBe (sort actual) expiry_all
    context "when date is 2020-07-24 and ticker is NHY and redis database is 6"
      $ do
          it "expiry dates should be empty" $ do
            let prms = prmsRdb5 { PA.redisDatabase = "6 " }
            let env  = T.Env prms dx1 Nothing nil
            actual <- runReaderT (T.runApp $ RedisRepos.expiryTimes nhy) env
            shouldBe (sort actual) []
    context "when date is 2020-07-24 and ticker is TEL" $ do
      it "expiry dates should be all expiry-1" $ do
        let env = T.Env prmsRdb5 dx1 Nothing nil
        actual <- runReaderT (T.runApp $ RedisRepos.expiryTimes tel) env
        shouldBe (sort actual) expiry1
    context "when date is 2020-12-18 and ticker is NHY" $ do
      it "expiry dates should be expiry_2020_12_18" $ do
        let env = T.Env prmsRdb5 dx2 Nothing nil
        actual <- runReaderT (T.runApp $ RedisRepos.expiryTimes nhy) env
        shouldBe (sort actual) expiry_2020_12_18
    context "when date is 2020-12-18 and ticker is TEL" $ do
      it "expiry dates should be expiry_2020_12_18" $ do
        let env = T.Env prmsRdb5 dx2 Nothing nil
        actual <- runReaderT (T.runApp $ RedisRepos.expiryTimes nhy) env
        shouldBe (sort actual) expiry_2020_12_18
    context "when date is 2021-06-19 and ticker is NHY" $ do
      it "expiry dates should be empty" $ do
        let env = T.Env prmsRdb5 dx3 Nothing nil
        actual <- runReaderT (T.runApp $ RedisRepos.expiryTimes nhy) env
        shouldBe actual []
-}
