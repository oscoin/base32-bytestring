{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.ByteString.Base32Spec (spec) where

import           Data.ByteString        as BS
import           Data.ByteString.Base32 as Base32
import           Data.ByteString.Char8  as BC
import           Data.Char
import           Test.Hspec
import           Test.QuickCheck


instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

spec :: Spec
spec = do
  describe "encode" $ do
    it "conform RFC examples" $ do
      encode ""       `shouldBe` ""
      encode "f"      `shouldBe` "ca"
      encode "fo"     `shouldBe` "c3zo"
      encode "foo"    `shouldBe` "c3zs6"
      encode "foob"   `shouldBe` "c3zs6ao"
      encode "fooba"  `shouldBe` "c3zs6aub"
      encode "foobar" `shouldBe` "c3zs6aubqe"

  describe "decode" $ do
    it "conform RFC examples" $ do
      decode ""           `shouldBe` Right ""
      decode "ca"         `shouldBe` Right "f"
      decode "c3zo"       `shouldBe` Right "fo"
      decode "c3zs6"      `shouldBe` Right "foo"
      decode "c3zs6ao"    `shouldBe` Right "foob"
      decode "c3zs6aub"   `shouldBe` Right "fooba"
      decode "c3zs6aubqe" `shouldBe` Right "foobar"

    it "inverse for encode" $ property $ \bs ->
      decode (encode bs) == Right bs

    it "case insensitive" $ property $ \bs ->
      decode (BC.map toLower (encode bs)) == Right bs

    it "fail gracefully if encoded data contains non alphabet chars" $ do
      decode "0======="         `shouldBe` Left "'0' is not base32 character"
      decode "AAAAAAAA0=======" `shouldBe` Left "'0' is not base32 character"

  describe "decodeLenient" $ do
    it "conform RFC examples" $ do
      decodeLenient ""           `shouldBe` Right ""
      decodeLenient "ca"         `shouldBe` Right "f"
      decodeLenient "c3zo"       `shouldBe` Right "fo"
      decodeLenient  "c3zs6"     `shouldBe` Right "foo"
      decodeLenient "c3zs6ao"    `shouldBe` Right "foob"
      decodeLenient "c3zs6aub"   `shouldBe` Right "fooba"
      decodeLenient "c3zs6aubqe" `shouldBe` Right "foobar"

    it "inverse for encode" $ property $ \bs ->
      decodeLenient (encode bs) === Right bs

    it "case insensitive" $ property $ \bs ->
      decodeLenient (BC.map toLower (encode bs)) === Right bs

    it "skip non alphabet chars" $ do
      decodeLenient "|"    `shouldBe` Right ""
      decodeLenient "M"    `shouldBe` Right ""
      decodeLenient "M|ca" `shouldBe` Right "["
