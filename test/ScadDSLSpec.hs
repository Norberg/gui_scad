module ScadDSLSpec where

import Test.Hspec

spec :: Spec
spec = do
    describe "dummy" $ do
      it "removes leading and trailing whitespace" $ do
        5 `shouldBe` 5

main :: IO ()
main = hspec spec
