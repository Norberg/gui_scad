{-# LANGUAGE OverloadedStrings #-}
module DSL.ParseSpec where
import Test.Hspec

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Trifecta

import DSL.Scad
import DSL.Parser

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

spec :: Spec
spec = do
    describe "Assignment Parsing" $
        it "can parse a simple assignment" $ do
            let m = parseByteString parseAssignment
                    mempty "woot=1"
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just ("woot", "1")
    describe "Header Parsing" $
        it "can parse a simple header" $ do
            let m = parseByteString parseHeader mempty "[blah]"
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "blah")
    describe "Comment parsing" $
        it "Can skip a comment before a header" $ do
            let p = skipComments >> parseHeader
                i = "; woot\n[blah]"
                m = parseByteString p mempty i
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "blah")
    describe "Parse function" $ do
        it "can parse cube(1.0)" $ do
            let m = parseByteString parseFunction mempty "cube(1.0)"
            let r' = maybeSuccess m
            print m
            r' `shouldBe` Just ("cube", ["1.0"])
        it "can parse cube(1.0,2.0,3.0)" $ do
            let m = parseByteString parseFunction mempty "cube(1.0,2.0,3.0)"
            let r' = maybeSuccess m
            print m
            r' `shouldBe` Just ("cube", ["1.0","2.0","3.0"])


main :: IO ()
main = hspec spec
