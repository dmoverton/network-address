{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Address.URI
  ( URI
  , Scheme(..)
  , Host(..)
  , Query(..)
  , uriScheme
  , uriAuthority
  , uriHost
  , uriPath
  , uriPathSegments
  , uriPathIds
  , uriQuery
  , getLastIdFromPath
  , uriAuthorityHost
  , makeURI
  , parseURI
  , parseURIFixingEncoding
  , fixURIEncoding
  , renderURI
  , urlEncode
  ) where

import           Control.DeepSeq (NFData(..))
import           Control.Lens (Fold, Lens', Traversal', coerced, each, lastOf, to, (^?), _Just)
import           Control.Monad (unless)
import           Control.Monad.Fail (MonadFail(..))
import           Data.Aeson (FromJSON(..), ToJSON(..), withText)
import qualified Data.Attoparsec.Text as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Lazy (toStrict)
import           Data.Char (ord, toLower)
import           Data.Coerce (coerce)
import           Data.Csv (FromField(..), ToField(..))
import           Data.Maybe (fromMaybe)
import           Data.Set (member)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text1 (FromText1(..), ToText1(..), fromTextM, toText, unsafeFromText)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Network.HTTP.Types.URI (decodePathSegments)
import           Prelude hiding (fail)
import           Test.QuickCheck (Arbitrary(..), choose, elements, listOf, vector, vectorOf)
import           URI.ByteString (Authority(..), Host(..), Query(..), Scheme(..))
import qualified URI.ByteString as URI

import           Network.Address.TLD (TLD(..), validTLDs)

newtype URI = URI
  { _uri :: URI.URI }
  deriving (Eq, Show, Ord, Generic)

instance NFData URI where
  rnf !_ = ()

instance IsString URI where
  fromString uri = fromMaybe error' . parseURI . encodeUtf8 $ T.pack uri
    where error' = error $ "IsString: Invalid URI from string literal: " <> uri
  {-# INLINE fromString #-}

instance Arbitrary URI where
  arbitrary =
    makeURI
      <$> elements [ Scheme "http", Scheme "https" ]
      <*> genHost
      <*> do listOf $ alphaNumeric' 3 20
      <*> do fmap Query $ listOf $ (,) <$> alphaNumeric' 1 10 <*> anyByteString 1 20
    where
      genHost = do
        -- 63 characters in the maximum length of a "label"
        subDomainLength <- choose (0, 63)
        subDomain <- (\case "" -> ""; sub -> sub <> ".") <$> alphaNumeric subDomainLength
        TLD{..} <- arbitrary
        -- 253 characters is the maximum length of full domain name, including dots, but with only 3
        -- labels we cannot reach that figure
        domainLength <- choose (3, 63)
        domain <- alphaNumeric domainLength
        pure $ Host $ subDomain <> domain <> "." <> _tld

      alphaNumeric size =
        fmap BS.pack . vectorOf size $ elements $ w8 <$> ['a' .. 'z' ] ++ [ '0' .. '9']
      alphaNumeric' minSize maxSize = choose (minSize, maxSize) >>= alphaNumeric
      anyByteString minSize maxSize = choose (minSize, maxSize) >>= fmap BS.pack . vector

instance ToText1 URI where
  toText1 = unsafeFromText . decodeUtf8 . renderURI

instance FromText1 URI where
  parser1 = parseURIFixingEncoding . encodeUtf8 =<< A.takeText

instance ToJSON URI where
  toJSON = toJSON . toText

instance FromJSON URI where
  parseJSON = withText "URI" $ parseURIFixingEncoding . encodeUtf8

instance ToField URI where
  toField = renderURI

instance FromField URI where
  parseField = parseURIFixingEncoding

uriScheme :: Lens' URI Scheme
uriScheme = coerced . URI.uriSchemeL

uriHost :: Traversal' URI Host
uriHost = uriAuthority . _Just . uriAuthorityHost

uriAuthorityHost :: Lens' Authority Host
uriAuthorityHost = URI.authorityHostL

uriAuthority :: Lens' URI (Maybe Authority)
uriAuthority = coerced . URI.authorityL

uriPath :: Lens' URI ByteString
uriPath = coerced . URI.pathL

uriPathSegments :: Fold URI [Text]
uriPathSegments = uriPath . to decodePathSegments

uriPathIds :: FromText1 a => Fold URI a
uriPathIds = uriPath . to decodePathSegments . each . to fromTextM . _Just

uriQuery :: Lens' URI Query
uriQuery = coerced . URI.queryL

-- | Gets the last ID from the path: eg
--   - https://www.seek.com.au/job/123456 -> 123456
--   - https://www.seek.com.au/job/123456/apply -> 123456
getLastIdFromPath :: URI -> Maybe Int
getLastIdFromPath = lastOf uriPathIds

makeURI :: Scheme -> Host -> [ByteString] -> Query -> URI
makeURI scheme host path query = coerce
  URI.URI
    { URI.uriScheme    = scheme
    , URI.uriAuthority = Just $ Authority Nothing host Nothing
    , URI.uriPath      = case cleanPath of [] -> ""; _ -> "/" <> BS.intercalate "/" cleanPath
    , URI.uriQuery     = query
    , URI.uriFragment  = Nothing }
  where
    cleanPath = concatMap (filter (/= "") . BS.split (w8 '/')) path

parseURI :: MonadFail m => ByteString -> m URI
parseURI uri = case URI.parseURI URI.laxURIParserOptions uri of
  Left uriParseError -> error' $ show uriParseError
  Right parsedUri
    | Just hostname <- parsedUri ^? URI.authorityL . _Just . URI.authorityHostL . to hostBS -> do
      validateHostnameBytes hostname
      validateTLD hostname
      pure $ coerce parsedUri
    | otherwise -> error' "Missing hostname"
  where
    error' parseError = fail $ "Invalid URI: " <> show uri <> ": " <> parseError
    -- Due to the URL encoding which happens in parseURIFixingEncoding we can make some URIs which
    -- weren't previously valid, valid. We restrict ourselves to URIs with "normal" looking hostname
    -- and only valid TLDs
    validateHostnameBytes hostname =
      unless (BS.all validHostnameByte hostname) $ error' "Invalid byte in hostname"

    validHostnameByte b =
      (w8 'a' <= b && b <= w8 'z') ||
      (w8 'A' <= b && b <= w8 'Z') ||
      (w8 '0' <= b && b <= w8 '9') ||
      b == w8 '.' || b == w8 '-'

    validateTLD hostname =
      unless (TLD tld `member` validTLDs) $ error' $ "Invalid TLD " <> show tld
      where tld = BS8.pack . map toLower . BS8.unpack . snd $ BS.spanEnd (/= w8 '.') hostname

parseURIFixingEncoding :: MonadFail m => ByteString -> m URI
parseURIFixingEncoding = parseURI . fixURIEncoding

fixURIEncoding :: ByteString -> ByteString
fixURIEncoding = toStrict . BB.toLazyByteString . urlEncode

renderURI :: URI -> ByteString
renderURI = coerce URI.serializeURIRef'

w8 :: Char -> Word8
w8 = fromIntegral . ord
{-# INLINE w8 #-}

-- | Percent-encoding for URLs, including the extra allowed characters ":/?=#&%"
-- beyond what's usually allowed.
-- Improved version of
-- https://hackage.haskell.org/package/uri-bytestring-0.3.2.1/docs/URI-ByteString.html#v:urlEncode
urlEncode :: ByteString -> Builder
urlEncode bytes
    | BS.null bytes = mempty
    | otherwise     = BB.byteString clean <> escapedBytes <> urlEncode rest
  where
    (clean, needsEncoding) = BS.span isAllowedUrlByte bytes
    (notAllowed, rest)     = BS.break isAllowedUrlByte needsEncoding

    escapedBytes = foldMap urlBuilderForByte (BS.unpack notAllowed)

isAllowedUrlByte :: Word8 -> Bool
isAllowedUrlByte w = VU.unsafeIndex allowedUrlBytes (fromIntegral w)
{-# INLINE isAllowedUrlByte #-}

urlBuilderForByte :: Word8 -> Builder
urlBuilderForByte w = V.unsafeIndex byteUrlBuilders (fromIntegral w)
{-# INLINE urlBuilderForByte #-}

allowedUrlBytes :: VU.Vector Bool
allowedUrlBytes = VU.generate 256 isUnreserved
  where
    isUnreserved :: Int -> Bool
    isUnreserved (fromIntegral -> byte)
      | byte >= w8 'a' && byte <= w8 'z' = True
      | byte >= w8 '0' && byte <= w8 '9' = True
      | byte >= w8 'A' && byte <= w8 'Z' = True
      | otherwise = byte `BS.elem` "-_.~:/?=#&%"

byteUrlBuilders :: Vector Builder
byteUrlBuilders = flip V.imap (VG.convert allowedUrlBytes) $
  \(fromIntegral -> indexWord8) allowed ->
    if allowed
      -- These are unused due to the BS.breaks in urlEncode but left in just in case
      then BB.word8 indexWord8
      else BB.char7 '%' <> BB.word8HexFixed indexWord8
