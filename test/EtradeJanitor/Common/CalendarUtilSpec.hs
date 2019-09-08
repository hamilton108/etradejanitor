{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Common.CalendarUtilSpec where

import Test.Hspec
import qualified Data.Time.Calendar as Calendar
import qualified  EtradeJanitor.Common.CalendarUtil as CalendarUtil


spec :: Spec
spec = do
    describe "POSIX" $ do
        context "when date is 2019-09-01" $ do
            it "POSIX time (seconds) should be 1567296000" $ do
                let expected = 1567296000.0
                let testDate = Calendar.fromGregorian 2019 9 1
                shouldBe (CalendarUtil.dayToUnixTime testDate) expected