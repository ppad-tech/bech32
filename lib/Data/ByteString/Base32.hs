{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Data.ByteString.Base32
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Unpadded base32 encoding & decoding using the bech32 character set.

-- this module is an adaptation of emilypi's 'base32' library

module Data.ByteString.Base32 (
    -- * base32 encoding and decoding
    encode
  , decode
  ) where

import Control.Monad (guard)
import Data.Bits ((.|.), (.&.))
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8, Word32, Word64)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

word32be :: BS.ByteString -> Word32
word32be s =
  (fi (s `BU.unsafeIndex` 0) `B.shiftL` 24) .|.
  (fi (s `BU.unsafeIndex` 1) `B.shiftL` 16) .|.
  (fi (s `BU.unsafeIndex` 2) `B.shiftL`  8) .|.
  (fi (s `BU.unsafeIndex` 3))
{-# INLINE word32be #-}

-- realization for small builders
toStrict :: BSB.Builder -> BS.ByteString
toStrict = BS.toStrict
  . BE.toLazyByteStringWith (BE.safeStrategy 128 BE.smallChunkSize) mempty
{-# INLINE toStrict #-}

bech32_charset :: BS.ByteString
bech32_charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

word5 :: Word8 -> Maybe Word8
word5 w8 = fmap fi (BS.elemIndex w8 bech32_charset)

arrange :: Word32 -> Word8 -> BSB.Builder
arrange w32 w8 =
  let mask = 0b00011111                                 -- low 5-bit mask
      bech32_char = fi . BS.index bech32_charset . fi   -- word5 -> bech32

      -- split 40 bits into 8 w5's
      w5_0 = mask .&. (w32 `B.shiftR` 27) -- highest 5 bits
      w5_1 = mask .&. (w32 `B.shiftR` 22)
      w5_2 = mask .&. (w32 `B.shiftR` 17)
      w5_3 = mask .&. (w32 `B.shiftR` 12)
      w5_4 = mask .&. (w32 `B.shiftR` 07)
      w5_5 = mask .&. (w32 `B.shiftR` 02)
      -- combine lowest 2 bits of w32 with highest 3 bits of w8
      w5_6 = mask .&. (w32 `B.shiftL` 03 .|. fi w8 `B.shiftR` 05)
      -- lowest 5 bits of w8
      w5_7 = mask .&. fi w8

      -- get (w8) bech32 char for each w5, pack all into little-endian w64
      !w64 = bech32_char w5_0
         .|. bech32_char w5_1 `B.shiftL` 8
         .|. bech32_char w5_2 `B.shiftL` 16
         .|. bech32_char w5_3 `B.shiftL` 24
         .|. bech32_char w5_4 `B.shiftL` 32
         .|. bech32_char w5_5 `B.shiftL` 40
         .|. bech32_char w5_6 `B.shiftL` 48
         .|. bech32_char w5_7 `B.shiftL` 56

  in  BSB.word64LE w64
{-# INLINE arrange #-}

-- | Encode a base256-encoded 'ByteString' as a base32-encoded
--   'ByteString', using the bech32 character set.
--
--   >>> encode "jtobin was here!"
--   "df6x7cnfdcs8wctnyp5x2un9yy"
encode
  :: BS.ByteString -- ^ base256-encoded bytestring
  -> BS.ByteString -- ^ base32-encoded bytestring
encode dat = toStrict (go dat) where
  bech32_char = fi . BS.index bech32_charset . fi

  go bs@(BI.PS _ _ l)
    | l >= 5 = case BS.splitAt 5 bs of
        (chunk, etc) -> case BS.unsnoc chunk of
          Nothing -> error "impossible, chunk length is 5"
          Just (word32be -> w32, w8) -> arrange w32 w8 <> go etc
    | l == 0 = mempty
    | l == 1 =
        let a = BU.unsafeIndex bs 0
            t = bech32_char ((a .&. 0b11111000) `B.shiftR` 3)
            u = bech32_char ((a .&. 0b00000111) `B.shiftL` 2)

            !w16 = fi t
               .|. fi u `B.shiftL` 8

        in  BSB.word16LE w16
    | l == 2 =
        let a = BU.unsafeIndex bs 0
            b = BU.unsafeIndex bs 1
            t = bech32_char ((a .&. 0b11111000) `B.shiftR` 3)
            u = bech32_char $
                      ((a .&. 0b00000111) `B.shiftL` 2)
                  .|. ((b .&. 0b11000000) `B.shiftR` 6)
            v = bech32_char ((b .&. 0b00111110) `B.shiftR` 1)
            w = bech32_char ((b .&. 0b00000001) `B.shiftL` 4)

            !w32 = fi t
               .|. fi u `B.shiftL` 8
               .|. fi v `B.shiftL` 16
               .|. fi w `B.shiftL` 24

        in  BSB.word32LE w32
    | l == 3 =
        let a = BU.unsafeIndex bs 0
            b = BU.unsafeIndex bs 1
            c = BU.unsafeIndex bs 2
            t = bech32_char ((a .&. 0b11111000) `B.shiftR` 3)
            u = bech32_char $
                      ((a .&. 0b00000111) `B.shiftL` 2)
                  .|. ((b .&. 0b11000000) `B.shiftR` 6)
            v = bech32_char ((b .&. 0b00111110) `B.shiftR` 1)
            w = bech32_char $
                      ((b .&. 0b00000001) `B.shiftL` 4)
                  .|. ((c .&. 0b11110000) `B.shiftR` 4)
            x = bech32_char ((c .&. 0b00001111) `B.shiftL` 1)

            !w32 = fi t
               .|. fi u `B.shiftL` 8
               .|. fi v `B.shiftL` 16
               .|. fi w `B.shiftL` 24

        in  BSB.word32LE w32 <> BSB.word8 x
    | l == 4 =
        let a = BU.unsafeIndex bs 0
            b = BU.unsafeIndex bs 1
            c = BU.unsafeIndex bs 2
            d = BU.unsafeIndex bs 3
            t = bech32_char ((a .&. 0b11111000) `B.shiftR` 3)
            u = bech32_char $
                      ((a .&. 0b00000111) `B.shiftL` 2)
                  .|. ((b .&. 0b11000000) `B.shiftR` 6)
            v = bech32_char ((b .&. 0b00111110) `B.shiftR` 1)
            w = bech32_char $
                      ((b .&. 0b00000001) `B.shiftL` 4)
                  .|. ((c .&. 0b11110000) `B.shiftR` 4)
            x = bech32_char $
                      ((c .&. 0b00001111) `B.shiftL` 1)
                  .|. ((d .&. 0b10000000) `B.shiftR` 7)
            y = bech32_char ((d .&. 0b01111100) `B.shiftR` 2)
            z = bech32_char ((d .&. 0b00000011) `B.shiftL` 3)

            !w32 = fi t
               .|. fi u `B.shiftL` 8
               .|. fi v `B.shiftL` 16
               .|. fi w `B.shiftL` 24

            !w16 = fi x
               .|. fi y `B.shiftL` 8

        in  BSB.word32LE w32 <> BSB.word16LE w16 <> BSB.word8 z

    | otherwise =
        error "impossible"

-- | Decode a 'ByteString', encoded as base32 using the bech32 character
--   set, to a base256-encoded 'ByteString'.
--
--   >>> decode "df6x7cnfdcs8wctnyp5x2un9yy"
--   Just "jtobin was here!"
--   >>> decode "dfOx7cnfdcs8wctnyp5x2un9yy" -- s/6/O (non-bech32 character)
--   Nothing
decode
  :: BS.ByteString        -- ^ base32-encoded bytestring
  -> Maybe BS.ByteString  -- ^ base256-encoded bytestring
decode = handle . go mempty where
  handle = \case
    Nothing -> Nothing
    Just s -> Just (toStrict s)

  go acc bs@(BI.PS _ _ l)
    | l < 8 = do
        fin <- finalize bs
        pure (acc <> fin)
    | otherwise = case BS.splitAt 8 bs of
        (chunk, etc) -> do
           res <- decode_chunk chunk
           go (acc <> res) etc

finalize :: BS.ByteString -> Maybe BSB.Builder
finalize bs@(BI.PS _ _ l)
  | l == 0 = Just mempty
  | otherwise = do
      guard (l >= 2)
      w5_0 <- word5 (BU.unsafeIndex bs 0)
      w5_1 <- word5 (BU.unsafeIndex bs 1)
      let w8_0 = w5_0 `B.shiftL` 3
             .|. w5_1 `B.shiftR` 2

      -- https://datatracker.ietf.org/doc/html/rfc4648#section-6
      if | l == 2 -> do -- 2 w5's, need 1 w8; 2 bits remain
             guard (w5_1 `B.shiftL` 6 == 0)
             pure (BSB.word8 w8_0)

         | l == 4 -> do -- 4 w5's, need 2 w8's; 4 bits remain
             w5_2 <- word5 (BU.unsafeIndex bs 2)
             w5_3 <- word5 (BU.unsafeIndex bs 3)
             let w8_1 = w5_1 `B.shiftL` 6
                    .|. w5_2 `B.shiftL` 1
                    .|. w5_3 `B.shiftR` 4

                 w16 = fi w8_1
                   .|. fi w8_0 `B.shiftL` 8

             guard (w5_3 `B.shiftL` 4 == 0)
             pure (BSB.word16BE w16)

         | l == 5 -> do -- 5 w5's, need 3 w8's; 1 bit remains
             w5_2 <- word5 (BU.unsafeIndex bs 2)
             w5_3 <- word5 (BU.unsafeIndex bs 3)
             w5_4 <- word5 (BU.unsafeIndex bs 4)
             let w8_1 = w5_1 `B.shiftL` 6
                    .|. w5_2 `B.shiftL` 1
                    .|. w5_3 `B.shiftR` 4
                 w8_2 = w5_3 `B.shiftL` 4
                    .|. w5_4 `B.shiftR` 1

                 w16  = fi w8_1
                    .|. fi w8_0 `B.shiftL` 8

             guard (w5_4 `B.shiftL` 7 == 0)
             pure (BSB.word16BE w16 <> BSB.word8 w8_2)

         | l == 7 -> do -- 7 w5's, need 4 w8's; 3 bits remain
             w5_2 <- word5 (BU.unsafeIndex bs 2)
             w5_3 <- word5 (BU.unsafeIndex bs 3)
             w5_4 <- word5 (BU.unsafeIndex bs 4)
             w5_5 <- word5 (BU.unsafeIndex bs 5)
             w5_6 <- word5 (BU.unsafeIndex bs 6)
             let w8_1 = w5_1 `B.shiftL` 6
                    .|. w5_2 `B.shiftL` 1
                    .|. w5_3 `B.shiftR` 4
                 w8_2 = w5_3 `B.shiftL` 4
                    .|. w5_4 `B.shiftR` 1
                 w8_3 = w5_4 `B.shiftL` 7
                    .|. w5_5 `B.shiftL` 2
                    .|. w5_6 `B.shiftR` 3

                 w32  = fi w8_3
                    .|. fi w8_2 `B.shiftL` 8
                    .|. fi w8_1 `B.shiftL` 16
                    .|. fi w8_0 `B.shiftL` 24

             guard (w5_6 `B.shiftL` 5 == 0)
             pure (BSB.word32BE w32)

         | otherwise -> Nothing

-- assumes length 8 input
decode_chunk :: BS.ByteString -> Maybe BSB.Builder
decode_chunk bs = do
  w5_0 <- word5 (BU.unsafeIndex bs 0)
  w5_1 <- word5 (BU.unsafeIndex bs 1)
  w5_2 <- word5 (BU.unsafeIndex bs 2)
  w5_3 <- word5 (BU.unsafeIndex bs 3)
  w5_4 <- word5 (BU.unsafeIndex bs 4)
  w5_5 <- word5 (BU.unsafeIndex bs 5)
  w5_6 <- word5 (BU.unsafeIndex bs 6)
  w5_7 <- word5 (BU.unsafeIndex bs 7)

  let w40 :: Word64
      w40 = fi w5_0 `B.shiftL` 35
        .|. fi w5_1 `B.shiftL` 30
        .|. fi w5_2 `B.shiftL` 25
        .|. fi w5_3 `B.shiftL` 20
        .|. fi w5_4 `B.shiftL` 15
        .|. fi w5_5 `B.shiftL` 10
        .|. fi w5_6 `B.shiftL` 05
        .|. fi w5_7
      w32 = fi (w40 `B.shiftR` 8)   :: Word32
      w8  = fi (0b11111111 .&. w40) :: Word8

  pure $ BSB.word32BE w32 <> BSB.word8 w8

