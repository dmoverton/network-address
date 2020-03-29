{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- Code lifted and modified from https://github.com/Cipherwraith/punycode/
   which is a fork of https://github.com/litherum/punycode

Copyright 2012, Myles C. Maxfield. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Data.Punycode
  ( decodePunycode
  , encodePunycode
  ) where

import           Control.Exception.Base (Exception)
import           Control.Lens (makeLenses, (+=), (.=))
import           Control.Monad (forM_)
import           Control.Monad.State.Strict hiding (state)
import           Control.Monad.Writer.Strict
import           Data.Attoparsec.ByteString (Parser, anyWord8, atEnd, parseOnly)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BS (toStrict)
import           Data.Char (chr, ord)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Typeable (Typeable)
import           Data.Word (Word8)

data PunycodeState = PunycodeState
  { _psN     :: {-# UNPACK #-} !Int
  , _psDelta :: {-# UNPACK #-} !Int
  , _psBias  :: {-# UNPACK #-} !Int
  , _psH     :: {-# UNPACK #-} !Int }

makeLenses ''PunycodeState

-- | Encode a string into its ascii form
encodePunycode :: Text -> ByteString
encodePunycode = BS.toStrict . Builder.toLazyByteString . execWriter . initialWriter
{-# INLINE encodePunycode #-}

initialWriter :: MonadWriter Builder m => Text -> m ()
initialWriter !input = do
  tell $ Builder.byteString basics
  when (basicsLength > 0) $ tell "-"
  evalStateT (encodeInner3 (map ord . T.unpack $! input) basicsLength) $!
    PunycodeState { _psN     = initialN
                  , _psDelta = 0
                  , _psBias  = initialBias
                  , _psH     = basicsLength }
  where
    basics :: ByteString
    basics = TE.encodeUtf8 . T.filter isBasic $! input

    basicsLength :: Int
    basicsLength = BS.length basics
{-# INLINE initialWriter #-}

encodeInner3 :: (MonadState PunycodeState m, MonadWriter Builder m) => [Int] -> Int -> m ()
encodeInner3 !input !basicsLength = get >>= helper
  where
    helper PunycodeState{..} =
      when (_psH < length input) $ do
        psN .= m
        psDelta .= delta
        forM_ input $! encodeInner2 basicsLength
        psDelta += 1
        psN += 1
        encodeInner3 input basicsLength
      where
        m :: Int
        m = minimum . filter (>= _psN) $! input

        delta :: Int
        delta = _psDelta + (m - _psN) * (_psH + 1)
{-# INLINE encodeInner3 #-}

encodeInner2 :: (MonadState PunycodeState m, MonadWriter Builder m) => Int -> Int -> m ()
encodeInner2 !basicsLength !c = get >>= helper
  where
    helper PunycodeState{..}
      | c == _psN = do
          !q <- encodeInner delta base _psBias
          tell . Builder.word8 . baseToAscii $! q
          psBias .= adapt delta (_psH + 1) (_psH == basicsLength)
          psDelta .= 0
          psH += 1
      | otherwise = psDelta .= delta
      where
        delta :: Int
        delta
          | c < _psN = 1 + _psDelta
          | otherwise = _psDelta
{-# INLINE encodeInner2 #-}

encodeInner :: (MonadWriter Builder m) => Int -> Int -> Int -> m Int
encodeInner !q !k !bias
  | q < t = pure q
  | otherwise = do
    tell . Builder.word8 . baseToAscii $! t + ((q - t) `mod` (base - t))
    !out <- encodeInner ((q - t) `div` (base - t)) (k + base) bias
    pure out
  where
    t :: Int
    t
      | k <= bias + tMin = tMin
      | k >= bias + tMax = tMax
      | otherwise = k - bias
{-# INLINE encodeInner #-}

baseToAscii :: Int -> Word8
baseToAscii !i
  | i < 26    = fromIntegral $! i + ord 'a'
  | otherwise = fromIntegral $! (i - 26) + ord '0'
{-# INLINE baseToAscii #-}

data PunycodeDecodeException
  = GenericDecodeException
  | InternalStringTooShort
  | InputTooShort
  | RightOfHyphenShouldBeAlphanumeric
  | LeftOfHyphenShouldBeBasic
  | CantStartWithDash
  | InvalidCodePoint
  deriving (Eq, Show, Typeable)
  deriving anyclass (Exception)

-- | Decode a string into its unicode form
decodePunycode :: ByteString -> Either PunycodeDecodeException Text
decodePunycode input
  | input == "--" = Right "-"
  | not (BS.null input)
  , BS.length (BS.filter (== w8 '-') input) == 1
  , BS.head input == w8 '-' = Left CantStartWithDash
  | T.any (not . isExtendedBasic) before = Left LeftOfHyphenShouldBeBasic
  | Right out <- parseOnly (decodeInner2 initialN 0 initialBias before) after = out
  | otherwise = Left InputTooShort
  where
    (before, after)
      | BS.any (== w8 '-') input = (T.pack $ map (chr . fromIntegral) $ BS.unpack $ BS.init b1, a1)
      | otherwise = (T.empty, input)
    (b1, a1) = BS.breakEnd (== w8 '-') input

decodeInner2 :: Int -> Int -> Int -> Text -> Parser (Either PunycodeDecodeException Text)
decodeInner2 n oldi bias output = do
  b <- atEnd
  helper b
  where helper False = do
          i <- decodeInner base 1 oldi bias
          helper' i
          where helper' Nothing = return $ Left RightOfHyphenShouldBeAlphanumeric
                helper' (Just i) = case output' of
                  Right output'' -> decodeInner2 n' (i' + 1) bias' output''
                  Left err       -> return $ Left err
                  where bias' = adapt (i - oldi) (T.length output + 1) (oldi == 0)
                        n' = n + i `div` (T.length output + 1)
                        i' = i `mod` (T.length output + 1)
                        output' = insertInto output n' i'
        helper True = return $ Right output

decodeInner :: Int -> Int -> Int -> Int -> Parser (Maybe Int)
decodeInner k w i bias = do
  word8 <- anyWord8
  helper $ word8ToDigit word8
  where helper Nothing = return Nothing
        helper (Just digit)
          | digit < t = return $ Just i'
          | otherwise = decodeInner (k + base) w' i' bias
          where w' = w * (base - t)
                i' = i + digit * w
                t
                  | k <= bias + tMin = tMin
                  | k >= bias + tMax = tMax
                  | otherwise = k - bias

insertInto :: Text -> Int -> Int -> Either PunycodeDecodeException Text
insertInto input n i
  | T.length input < i = Left InternalStringTooShort
  | otherwise = case safeChr n of
    Just n' -> Right $ T.concat [T.take i input, T.singleton n', T.drop i input]
    Nothing -> Left InvalidCodePoint

safeChr :: Int -> Maybe Char
safeChr x
  | x >= 0 && x <= fromEnum (maxBound :: Char) = Just $ chr x
  | otherwise = Nothing

word8ToDigit :: Word8 -> Maybe Int
word8ToDigit = helper . fromIntegral
  where
    helper word8
      | word8 >= ord 'a' && word8 <= ord 'z' = Just $ word8 - ord 'a'
      | word8 >= ord 'A' && word8 <= ord 'Z' = Just $ word8 - ord 'A'
      | word8 >= ord '0' && word8 <= ord '9' = Just $ 26 + word8 - ord '0'
      | otherwise = Nothing

isExtendedBasic :: Char -> Bool
isExtendedBasic x
  | isBasic x = True
  | ord x == 128 = True
  | otherwise = False

base :: Int
base = 36

tMin :: Int
tMin = 1

tMax :: Int
tMax = 26

skew :: Int
skew = 38

damp :: Int
damp = 700

initialBias :: Int
initialBias = 72

initialN :: Int
initialN = 128

adapt :: Int -> Int -> Bool -> Int
adapt delta numOfPoints isFirstTime = helper
  where
    helper = loop 0 $ delta' + (delta' `div` numOfPoints)
      where delta'
              | isFirstTime = delta `div` damp
              | otherwise = delta `div` 2
    loop k delta'
      | delta' > ((base - tMin) * tMax) `div` 2 = loop (k + base) $ delta' `div` (base - tMin)
      | otherwise = k + (((base - tMin + 1) * delta') `div` (delta' + skew))

isBasic :: Char -> Bool
isBasic = (< initialN) . ord

w8 :: Char -> Word8
w8 = fromIntegral . ord
