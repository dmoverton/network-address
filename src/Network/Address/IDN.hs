{-# LANGUAGE OverloadedStrings #-}

module Network.Address.IDN
  ( toASCII
  , toUnicode
  , asciiCompatibleEncodingPrefix
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char (isAscii, ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word (Word8)

import           Data.Punycode (decodePunycode, encodePunycode)

-- Convert a Unicode domain name to an ASCII.
-- The domain name may contain several labels, separated by dots.
-- toASCII never alters a sequence of code points that are all in the ASCII range to begin with
-- (although it could fail).
-- Applying toASCII multiple times gives the same result as applying it once.
toASCII :: ByteString -> ByteString
toASCII = applyToDomainLabels labelToASCII
  where
    labelToASCII :: ByteString -> ByteString
    labelToASCII label
      | Right text <- TE.decodeUtf8' label
      , T.any (not . isAscii) text = asciiCompatibleEncodingPrefix <> encodePunycode text
      | otherwise = label

-- Convert a possibly ASCII compatible encoded domain name to Unicode.
-- The domain name may contain several labels, separated by dots.
toUnicode :: ByteString -> ByteString
toUnicode = applyToDomainLabels labelToUnicode
  where
    labelToUnicode :: ByteString -> ByteString
    labelToUnicode label
      | Just punycode <- BS.stripPrefix asciiCompatibleEncodingPrefix label
      , Right text <- decodePunycode punycode
      = TE.encodeUtf8 text
      | otherwise = label

applyToDomainLabels :: (ByteString -> ByteString) -> ByteString -> ByteString
applyToDomainLabels f = BS.intercalate "." . fmap f . BS.split (w8 '.')
  where
    w8 :: Char -> Word8
    w8 = fromIntegral . ord

asciiCompatibleEncodingPrefix :: ByteString
asciiCompatibleEncodingPrefix = "xn--"
