{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Address.URISpec (spec) where

import           Control.Exception (IOException)
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (toLazyByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.String (fromString)
import           Data.Text1 (fromText1, isInfixOf)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (counterexample, (==>))
import           Text.Regex.TDFA (blankCompOpt, blankExecOpt)
import qualified Text.Regex.TDFA.ByteString as RegBS
import           URI.ByteString (urlDecode)

import           Network.Address.URI

spec :: Spec
spec = parallel $ do

  context "URL Encode" $ do

    prop "Should roundtrip URL encoding" $ \bytes ->
      containsNoEncodedEntities bytes ==>
        urlDecode False (toStrict . toLazyByteString $ urlEncode bytes)
          `shouldBe` bytes

    it "Should leave already encoded entities alone" $
      urlDecode False (toStrict . toLazyByteString $ urlEncode "%00")
        `shouldBe` "\NUL"

  context "URI" $ do

    prop "Should generate valid arbitrary URIs" $ \uri ->
      parseURI (renderURI uri) `shouldBe` Just uri

    it "Should contain invalid URI in error message" $
      let invalidURI = "$$$$://&/"
      in case fromText1 invalidURI of
        Right (uri :: URI) ->
          counterexample ("Parsed as valid URI:" <> show uri) False
        Left errorMessage ->
          counterexample ("Doesnt contain URI: " <> errorMessage) $
            isInfixOf "%24%24%24%24://&/" $ fromString errorMessage

  context "parseURIFixingEncoding" $ parallel $ do

    it "Should parse valid uri" $
      parseURIFixingEncoding "https://www.seek.com/job/12345?date=now&view=1#fragment"
        `shouldBe` Just "https://www.seek.com/job/12345?date=now&view=1#fragment"

    it "Should fix encoding issues" $
      parseURIFixingEncoding "https://www.seek.com.au/job/12345/Senior-BAS-Accountant-Tauranga-|-Rotorua?id={12345}&path=job/default"
        `shouldBe` Just "https://www.seek.com.au/job/12345/Senior-BAS-Accountant-Tauranga-%7C-Rotorua?id=%7B12345%7D&path=job%2Fdefault"

    it "Should not double encode uri" $
      parseURIFixingEncoding "https://www.seek.com.au/job/12345/Senior-BAS-Accountant-Tauranga-%7C-Rotorua?id=%7B12345%7D&path=job%2Fdefault"
        `shouldBe` Just "https://www.seek.com.au/job/12345/Senior-BAS-Accountant-Tauranga-%7C-Rotorua?id=%7B12345%7D&path=job%2Fdefault"

    it "Should parse uri with punycode domain" $
      parseURIFixingEncoding "http://xn--p-8sbkgc5ag7bhce.xn--80aqecdr1a"
        `shouldBe` Just "http://xn--p-8sbkgc5ag7bhce.xn--80aqecdr1a"

    it "Should parse uri with uppercase TLD" $
      parseURIFixingEncoding "http://SEEK.COM"
        `shouldBe` Just "http://SEEK.COM"

    it "Should fail to parse domain with invalid TLD" $
      parseURIFixingEncoding "https://queensland.xxxx"
        `shouldThrow`
          ioExceptionWithMessage
            "user error (Invalid URI: \"https://queensland.xxxx\": Invalid TLD \"xxxx\")"

    it "should to fail to parse URI with spaces" $ do
      parseURIFixingEncoding "http://  http//www.ooops.com/Vacancies/Owaka---URGENT-cover-needed.aspx" `shouldBe` Nothing

      parseURIFixingEncoding "http://Provide counselling, case management and support the well-being of pregnant women and their families."
          `shouldBe` Nothing

  context "getLastIdFromPath" $ parallel $ do

    it "Should get 149180" $
      getLastIdFromPath "http://api.seek.com.au/v2/thing/149180/"
      `shouldBe` Just 149180

    it "Should get last Id from path" $
      getLastIdFromPath "http://seek.com/advertiser/12345/job/654565/" `shouldBe` Just 654565

    it "Should get Id even when it is not at the end of the path" $
      getLastIdFromPath "http://seek.com/advertiser/12345/education-victoria/" `shouldBe` Just 12345

    it "Should get 12345 when port present" $
      getLastIdFromPath "http://seek.com.au:8080/job/12345" `shouldBe` Just 12345

    it "Should return Nothing when no path" $
      getLastIdFromPath "http://seek.com.au" `shouldBe` Nothing

    it "Should return Nothing when no numbers in path" $
      getLastIdFromPath "http://seek.com.au/some-path/boom" `shouldBe` Nothing

    it "Should return Nothing for partial numbers" $
      getLastIdFromPath "http://seek.com.au/v2/some-path/boom" `shouldBe` Nothing

    it "Should return nothing when port but no numbers in path" $
      getLastIdFromPath "http://seek.com.au:8080/job/NoID" `shouldBe` Nothing

containsNoEncodedEntities :: ByteString -> Bool
containsNoEncodedEntities =
  let Right regex = RegBS.compile blankCompOpt blankExecOpt ("%[0-9a-fA-F]{2}" :: ByteString)
  in \byteString -> case RegBS.execute regex byteString of
      Left err      -> error err
      Right Nothing -> True
      Right Just{}  -> False

ioExceptionWithMessage :: String -> Selector IOException
ioExceptionWithMessage str exception =
  str == show exception || error ("Strings don't match " ++ show (str,show exception))
