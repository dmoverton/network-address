{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Address.TLDSpec (spec) where

import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Text (isInfixOf, toLower, toUpper)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (counterexample, (==>))

import           Network.Address.IDN (toUnicode)
import           Network.Address.TLD

spec :: Spec
spec = parallel $

  context "parseTLD" $ parallel $ do

    it "Should parse just TLD" $
      parseTLD "au" `shouldBe` Right "au"

    it "Should parse TLD with preceding dot" $
      parseTLD ".au" `shouldBe` Right "au"

    it "Should parse TLD from domain" $
      parseTLD "seek.com.au" `shouldBe` Right "au"

    it "Should parse punycode encoded tld" $
      parseTLD "xn--bck1b9a5dre4c" `shouldBe` Right "xn--bck1b9a5dre4c"

    it "Should parse punycode encoded tld with dot" $
      parseTLD ".xn--bck1b9a5dre4c" `shouldBe` Right "xn--bck1b9a5dre4c"

    it "Should parse TLD from punycode domain" $
      parseTLD "xn--p-8sbkgc5ag7bhce.xn--80aqecdr1a" `shouldBe` Right "xn--80aqecdr1a"

    it "Should parse TLD from unicode domain" $
      parseTLD "джpумлатест.католик" `shouldBe` Right "xn--80aqecdr1a"

    it "Should parse unicode tld" $
      parseTLD "ファッション" `shouldBe` Right "xn--bck1b9a5dre4c"

    prop "Should parse valid TLDs" $ \tld@TLD{..} ->
      parseTLD (decodeUtf8 _tld) `shouldBe` Right tld

    prop "Should parse valid TLDs when uppercased" $ \tld@TLD{..} ->
      parseTLD (toUpper $ decodeUtf8 _tld) `shouldBe` Right tld

    prop "Should parse valid unicode TLDs" $ \tld@TLD{..} ->
      parseTLD (decodeUtf8 $ toUnicode _tld) `shouldBe` Right tld

    prop "Should contain invalid TLD in error message" $
      \(T.filter (/= '.') -> invalidTld) ->
        not (Set.member (TLD $ encodeUtf8 $ toLower invalidTld) validTLDs) ==>
          case parseTLD invalidTld of
            Right (tld :: TLD) ->
              counterexample ("Parsed as valid TLD:" <> show tld) False
            Left errorMessage ->
              counterexample ("Doesnt contain TLD: " <> errorMessage) $
                isInfixOf invalidTld $ fromString errorMessage
