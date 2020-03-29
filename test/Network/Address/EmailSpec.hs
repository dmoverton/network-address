{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Address.EmailSpec (spec) where

import           Control.Monad (forM_)
import           Data.Maybe (isJust)
import           Data.String (fromString)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text1 (fromText1, isInfixOf)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)

import           Network.Address.Email
import           Network.Address.TLD (TLD(..))

spec :: Spec
spec = parallel $ do

  context "EmailAddress" $ parallel $ do

    prop "Should generate valid arbitrary emails" $ \emailAddress ->
      let result = parseEmailAddress $ renderEmailAddress emailAddress
      in result `shouldBe` Just emailAddress

    it "Should handle unicode in FromText1" $
      fromText1 "vodka@джpумлатест.католик"
        `shouldBe` Right ("vodka@xn--p-8sbkgc5ag7bhce.xn--80aqecdr1a" :: EmailAddress)

    it "Should contain invalid email in error message" $
      let invalidEmail = "bob.com"
      in case fromText1 invalidEmail of
        Right (_ :: EmailAddress) -> False
        Left errorMessage         -> isInfixOf invalidEmail $ fromString errorMessage

    it "Should contain invalid email with invalid TLD in error message" $
      let invalidEmail = "tom@bob.awesometld"
      in case fromText1 invalidEmail of
        Right (_ :: EmailAddress) -> False
        Left errorMessage         -> isInfixOf invalidEmail $ fromString errorMessage

  context "parseEmailAddress" $ parallel $do

    it "Should parse valid email address" $
      parseEmailAddress "tmortiboy@seek.com.au"
        `shouldBe` Just "tmortiboy@seek.com.au"

    it "Should parse email address with unicode tld" $
      parseEmailAddress "tmortiboy@seek.\xE4\xB8\xAD\xE5\x9B\xBD"
        `shouldBe` Just "tmortiboy@seek.xn--fiqs8s"

    it "Should parse email address with punycode domain" $
      parseEmailAddress "vodka@xn--p-8sbkgc5ag7bhce.xn--80aqecdr1a"
        `shouldBe` Just "vodka@xn--p-8sbkgc5ag7bhce.xn--80aqecdr1a"

    it "Should parse email address with unicode domain" $
      -- Not allowed non-ascii chars for the username
      parseEmailAddress (encodeUtf8 "vodka@джpумлатест.католик")
        `shouldBe` Just "vodka@xn--p-8sbkgc5ag7bhce.xn--80aqecdr1a"

    prop "Should parse email with valid TLD" $ \TLD{..} ->
      parseEmailAddress ("tmortiboy@seek." <> _tld)
        `shouldSatisfy` isJust

    forM_
     [ "I am not an email"
     , "tmortiboy@seek.com.au-"
     , "tom[at]domain.com"
     , encodeUtf8 "джpу@джpумлатест.католик"
     , "frank,uv@hotmail.com"
     , "dan.murphy@hayday.net.nx"
     , "atyourface@hmmmm.edu.au.Job"
     , "brisbane@strategy1hr.com.auor"
     , "henry.hover@geee.com.Your"
     , "info@idontcareaustralia.comau"
     , "jobs@nosolutions.com.auor"
     , "lucy@notsoedgeypeople.com.auk"
     , "martin@whysosalty.com.auor"
     , "mybullocks@morganjames.com.auor"
     , "monica@nofashions.ocm"
     , "online.Bob.Parkton@mywebsite.com.auGO"
     , "yourcareer@ialreadyate.edu.au.Job"
     ] $ \invalidEmail ->
      it ("Should not parse " <> show invalidEmail) $
        parseEmailAddress invalidEmail `shouldBe` Nothing
