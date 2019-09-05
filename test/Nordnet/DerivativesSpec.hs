module Nordnet.DerivativesSpec (spec) where


import Test.Hspec
import EtradeJanitor.Repos.Nordnet.Derivatives as Derivatives

spec :: Spec
spec = do
    describe "Nordnet URLs" $ do
        context "when download date is 2019-09-01" $ do
            it "expiry dates in UTC should be [..]" $ do
                shouldBe [1,3] [1,2]
            -- strip "\t  foo bar\n" `shouldBe` "foo bar"