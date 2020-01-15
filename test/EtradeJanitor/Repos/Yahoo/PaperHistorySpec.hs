{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Yahoo.PaperHistorySpec (spec) where

import EtradeJanitor.Repos.Yahoo.PaperHistory as PaperHistory


import Control.Monad.Reader (runReaderT)
import Test.Hspec
import qualified Data.Time.Calendar as Calendar

testDay :: Calendar.Day
testDay = 
    let 
        year = 2020 :: Integer
        month = 1 :: Int 
        day = 14 :: Int
    in 
    Calendar.fromGregorian year month day

spec :: Spec
spec = do
    describe "Yahoo Csv" $ do
        context "when Csv date is 2020-01-14" $ do
            it "dates in UTC should be [..]" $ do
                let result = PaperHistory.asDay "2020-01-14"
                shouldBe testDay result