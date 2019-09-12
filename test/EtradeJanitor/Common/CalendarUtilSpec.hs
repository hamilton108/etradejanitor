{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Common.CalendarUtilSpec where

import Test.Hspec
import qualified Data.Time.Calendar as Calendar
import qualified  EtradeJanitor.Common.CalendarUtil as CalendarUtil


{-
Nordnet:

    2019-7-19:0
    2019-8-16:0
    2019-9-20:1568930400000
    2019-10-18:1571349600000
    2019-11-15:1573772400000
    2019-12-20:1576796400000
    2020-3-20:1584658800000
    2020-6-19:1592517600000
-}

spec :: Spec
spec = do
    describe "POSIX" $ do
        context "when date is 2019-09-01" $ do
            it "POSIX time (seconds) should be 1567296000" $ do
                let expected = 1567296000.0
                let testDate = Calendar.fromGregorian 2019 9 1
                shouldBe (CalendarUtil.dayToUnixTime testDate) expected
{-
        context "when date is 2019-12-20" $ do
            it "POSIX time (seconds) should be 1576796400" $ do
                let expected = 1576796400.0 
                let testDate = Calendar.fromGregorian 2019 12 20
                shouldBe (CalendarUtil.dayToUnixTime testDate) expected
-}