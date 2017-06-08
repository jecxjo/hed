{-# LANGUAGE OverloadedStrings #-}
module RegExSpec where

import RegEx

import Text.Parsec.Prim (parse)
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  , shouldNotBe
                  , context
                  )

spec :: Spec
spec =
  describe "RegEx" $ do
    describe "swapGen" $ do
      it "works with empty string" $ do
        let swap = swapGen "foo" "bar"
        let results = swap ""
        results `shouldBe` ""

      it "works with empty re and replace strings" $ do
        let swap = swapGen "" ""
        let results = swap "Foo"
        results `shouldBe` "Foo"

      it "leaves non-matching strings unchanged" $ do
        let swap = swapGen "foo" "bar"
        let results = swap "baz"
        results `shouldBe` "baz"

      it "replaces first instance of match" $ do
        let swap = swapGen "foo" "bar"
        let results = swap "foo foo"
        results `shouldBe` "bar foo"

      it "replaces and uses submatch" $ do
        let swap = swapGen "ba(r|z)" "\\1ap"
        let results = swap "baz"
        results `shouldBe` "zap"

    describe "swapAllGen" $ do
      it "works with empty string" $ do
        let swap = swapAllGen "foo" "bar"
        let results = swap ""
        results `shouldBe` ""

      it "works with empty re and replace strings" $ do
        let swap = swapAllGen "" ""
        let results = swap "Foo"
        results `shouldBe` "Foo"

      it "leaves non-matching strings unchanged" $ do
        let swap = swapAllGen "foo" "bar"
        let results = swap "baz"
        results `shouldBe` "baz"

      it "replaces all instance of match" $ do
        let swap = swapAllGen "foo" "bar"
        let results = swap "foo foo"
        results `shouldBe` "bar bar"

      it "replaces and uses submatch" $ do
        let swap = swapAllGen "ba(r|z)" "\\1ap"
        let results = swap "baz foo bar"
        results `shouldBe` "zap foo rap"
