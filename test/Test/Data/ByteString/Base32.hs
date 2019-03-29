{-# LANGUAGE OverloadedStrings #-}
module Test.Data.ByteString.Base32 (tests) where

import qualified Data.ByteString        as BS
import           Data.ByteString.Base32 as Base32
import           Data.ByteString.Char8  as BC
import           Data.Char

import           System.IO.Unsafe       (unsafePerformIO)

import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.Hspec

import           Debug.Trace


-- The use of 'unsafePerformIO' seems to be \"recommended\" by the library
-- itself: http://hackage.haskell.org/package/tasty-hspec-1.1.5.1/docs/Test-Tasty-Hspec.html#g:4
-- gulp!
tests :: TestTree
tests = testGroup "Z-Base32" [
    unsafePerformIO $ testSpec "encode" encodeSpec
  , unsafePerformIO $ testSpec "decode" decodeSpec
  , unsafePerformIO $ testSpec "decodeLenient" decodeLenientSpec
  , testProperty "encode roundtrips case sensitive" propEncodeRoundtrips
  , testProperty "encode roundtrips case insensitive" propEncodeCaseInsensitiveRoundtrips
  , testProperty "decode lenient roundtrips" propDecodeLenientRoundtrips
  , testProperty "decode lenient roundtrips (case insensitive)" propDecodeLenientCaseInsensitiveRoundtrips
  ]

{------------------------------------------------------------------------------
--- Specs
------------------------------------------------------------------------------}

encodeSpec :: Spec
encodeSpec =
    it "conform RFC examples" $ do
      encode ""       `shouldBe` ""
      encode "f"      `shouldBe` "ca"
      encode "fo"     `shouldBe` "c3zo"
      encode "foo"    `shouldBe` "c3zs6"
      encode "foob"   `shouldBe` "c3zs6ao"
      encode "fooba"  `shouldBe` "c3zs6aub"
      encode "foobar" `shouldBe` "c3zs6aubqe"

decodeSpec :: Spec
decodeSpec = do
    it "conform RFC examples" $ do
      decode ""           `shouldBe` Right ""
      decode "ca"         `shouldBe` Right "f"
      decode "c3zo"       `shouldBe` Right "fo"
      decode "c3zs6"      `shouldBe` Right "foo"
      decode "c3zs6ao"    `shouldBe` Right "foob"
      decode "c3zs6aub"   `shouldBe` Right "fooba"
      decode "c3zs6aubqe" `shouldBe` Right "foobar"
    it "fail gracefully if encoded data contains non alphabet chars" $ do
      decode "0======="         `shouldBe` Left "'0' is not base32 character"
      decode "AAAAAAAA0=======" `shouldBe` Left "'0' is not base32 character"

decodeLenientSpec :: Spec
decodeLenientSpec = do
    it "conform RFC examples" $ do
      decodeLenient ""           `shouldBe` Right ""
      decodeLenient "ca"         `shouldBe` Right "f"
      decodeLenient "c3zo"       `shouldBe` Right "fo"
      decodeLenient  "c3zs6"     `shouldBe` Right "foo"
      decodeLenient "c3zs6ao"    `shouldBe` Right "foob"
      decodeLenient "c3zs6aub"   `shouldBe` Right "fooba"
      decodeLenient "c3zs6aubqe" `shouldBe` Right "foobar"

    it "skip non alphabet chars" $ do
      decodeLenient "|"    `shouldBe` Right ""
      decodeLenient "M"    `shouldBe` Right ""
      decodeLenient "M|ca" `shouldBe` Right "["

{------------------------------------------------------------------------------
--- Properties
------------------------------------------------------------------------------}

propEncodeRoundtrips :: Property
propEncodeRoundtrips = property $ do
    bs <- forAll $ Gen.bytes (Range.linear 0 500)
    decode (encode bs) === Right bs

propEncodeCaseInsensitiveRoundtrips :: Property
propEncodeCaseInsensitiveRoundtrips = property $ do
    bs <- forAll $ Gen.utf8 (Range.linear 0 500) Gen.unicodeAll
    decode (BC.map toLower (encode bs)) === Right bs

propDecodeLenientRoundtrips :: Property
propDecodeLenientRoundtrips = property $ do
    bs <- forAll $ Gen.bytes (Range.linear 0 500)
    decodeLenient (encode bs) === Right bs

propDecodeLenientCaseInsensitiveRoundtrips :: Property
propDecodeLenientCaseInsensitiveRoundtrips = property $ do
    bs <- forAll $ Gen.utf8 (Range.linear 0 500) Gen.unicodeAll
    decodeLenient (BC.map toLower (encode bs)) === Right bs
