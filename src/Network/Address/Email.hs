{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Address.Email
  ( EmailAddress(..)
  , parseEmailAddress
  , renderEmailAddress
  ) where

import           Control.DeepSeq (NFData(..))
import           Control.Monad.Fail (MonadFail(..))
import           Data.Aeson (FromJSON(..), ToJSON(..), withText)
import qualified Data.Attoparsec.Text as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char (ord)
import           Data.Coerce (coerce)
import           Data.Csv (FromField(..), ToField(..))
import           Data.Maybe (fromMaybe)
import           Data.String (IsString(..))
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import           Data.Text1 (FromText1(..), ToText1(..), fromTextM, toText, unsafeFromText)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Prelude hiding (fail)
import           Test.QuickCheck (Arbitrary(..), Gen, choose, elements, vectorOf)
import qualified Text.Email.Validate as Email

import           Network.Address.IDN (toASCII)
import           Network.Address.TLD (TLD(..), parseTLD)

newtype EmailAddress = EmailAddress
  { _emailAddress :: Email.EmailAddress }
  deriving (Eq, Show, Ord, Generic)

instance NFData EmailAddress where
  rnf !_ = ()

instance IsString EmailAddress where
  fromString email = fromMaybe error' . parseEmailAddress . encodeUtf8 $ T.pack email
    where error' = error $ "IsString: Invalid EmailAddress from string literal: " <> email
  {-# INLINE fromString #-}

instance Arbitrary EmailAddress where
  arbitrary = coerce Email.unsafeEmailAddress <$> localPart <*> domainPart
    where
      localPart :: Gen ByteString
      localPart = do
        localLength <- choose (2, 46)
        local <- alphaNumericDots localLength
        if BS.null local
          then alphaNumeric localLength
          else pure local

      domainPart :: Gen ByteString
      domainPart = do
        TLD{..} <- arbitrary
        domainLength <- choose (1, 62 - BS.length _tld)
        domain <- alphaNumeric domainLength
        pure $ domain <> "." <> _tld

      alphaNumeric :: Int -> Gen ByteString
      alphaNumeric = byteString $ ['a' .. 'z' ] ++ [ '0' .. '9']

      alphaNumericDots :: Int -> Gen ByteString
      alphaNumericDots = fmap removeInvalidDots . byteString ('.' : ['a' .. 'z' ] ++ [ '0' .. '9'])
        where
          removeInvalidDots :: ByteString -> ByteString
          removeInvalidDots =
            BS.intercalate "."
            . filter (not . BS.null)
            . BS.split (w8 '.')
            . mconcat
            . fmap removeAdjacentDots
            . BS.group

          removeAdjacentDots :: ByteString -> ByteString
          removeAdjacentDots groupedWords
            | Just (head', _) <- BS.uncons groupedWords
            , head' == w8 '.' = "."
            | otherwise = groupedWords

      byteString :: String -> Int -> Gen ByteString
      byteString chars size = fmap BS.pack . vectorOf size $ elements $ w8 <$> chars

instance ToText1 EmailAddress where
  toText1 = unsafeFromText . decodeUtf8 . renderEmailAddress

instance FromText1 EmailAddress where
  parser1 = parseEmailAddress . encodeUtf8 =<< A.takeText

instance ToJSON EmailAddress where
  toJSON = toJSON . toText

instance FromJSON EmailAddress where
  parseJSON = withText "EmailAddress" fromTextM

instance ToField EmailAddress where
  toField = renderEmailAddress

instance FromField EmailAddress where
  parseField = parseEmailAddress

parseEmailAddress :: forall m. MonadFail m => ByteString -> m EmailAddress
parseEmailAddress rawEmailAddress = do
  emailAddress <- either error' (pure . coerce) $ Email.validate idnDecodedEmailAddress
  rawTld <- either (error' . show) pure $ decodeUtf8' $ coerce Email.domainPart emailAddress
  _tld <- either error' pure $ parseTLD rawTld
  pure emailAddress
  where
    error' :: String -> m a
    error' reason = fail $ "Invalid email address: " <> show rawEmailAddress <> " - " <> reason

    idnDecodedEmailAddress :: ByteString
    idnDecodedEmailAddress = case BS.split (w8 '@') rawEmailAddress of
      username : [domain] -> username <> "@" <> toASCII domain
      _                   -> rawEmailAddress

renderEmailAddress :: EmailAddress -> ByteString
renderEmailAddress = coerce Email.toByteString

w8 :: Char -> Word8
w8 = fromIntegral . ord
