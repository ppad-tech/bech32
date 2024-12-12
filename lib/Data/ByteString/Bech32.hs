{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.Bech32 where

import Data.Bits ((.|.), (.&.))
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BE
import Data.Word (Word32)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

word32be :: BS.ByteString -> Word32
word32be s =
  (fi (s `BS.index` 0) `B.shiftL` 24) .|.
  (fi (s `BS.index` 1) `B.shiftL` 16) .|.
  (fi (s `BS.index` 2) `B.shiftL`  8) .|.
  (fi (s `BS.index` 3))
{-# INLINE word32be #-}

-- realization for small builders
toStrict :: BSB.Builder -> BS.ByteString
toStrict = BS.toStrict
  . BE.toLazyByteStringWith (BE.safeStrategy 128 BE.smallChunkSize) mempty

-- (maybe) pad to a multiple of 40 bits
maybe_pad :: BS.ByteString -> BS.ByteString
maybe_pad bs
    | l `rem` 5 == 0 = bs
    | otherwise = bs <> BS.replicate k 0x00
  where
    l = BS.length bs
    k = let r = fi l `rem` 5
        in  if r == 0 then 0 else r + 5

bech32_charset :: BS.ByteString
bech32_charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

-- adapted from emilypi's 'base32' library
base32 :: BS.ByteString -> BS.ByteString
base32 dat = toStrict (go dat) where
  mask = 0b11111

  go bs = case BS.splitAt 5 bs of
    (chunk, etc)
      | BS.length chunk /= 5 -> mempty
      | otherwise -> case BS.unsnoc chunk of
          Nothing -> error "impossible, chunk length is 5"
          Just (word32be -> w32, fi -> w8) ->
            let i0 = fi (mask .&. (w32 `B.shiftR` 27))
                i1 = fi (mask .&. (w32 `B.shiftR` 22))
                i2 = fi (mask .&. (w32 `B.shiftR` 17))
                i3 = fi (mask .&. (w32 `B.shiftR` 12))
                i4 = fi (mask .&. (w32 `B.shiftR` 07))
                i5 = fi (mask .&. (w32 `B.shiftR` 02))
                i6 = fi (mask .&. (w32 `B.shiftL` 03 .|. w8 `B.shiftR` 05))
                i7 = fi (mask .&. w8)

                w8_0 = fi (BS.index bech32_charset i0)
                w8_1 = fi (BS.index bech32_charset i1)
                w8_2 = fi (BS.index bech32_charset i2)
                w8_3 = fi (BS.index bech32_charset i3)
                w8_4 = fi (BS.index bech32_charset i4)
                w8_5 = fi (BS.index bech32_charset i5)
                w8_6 = fi (BS.index bech32_charset i6)
                w8_7 = fi (BS.index bech32_charset i7)

                w64 = w8_0
                  .|. w8_1 `B.shiftL` 8
                  .|. w8_2 `B.shiftL` 16
                  .|. w8_3 `B.shiftL` 24
                  .|. w8_4 `B.shiftL` 32
                  .|. w8_5 `B.shiftL` 40
                  .|. w8_6 `B.shiftL` 48
                  .|. w8_7 `B.shiftL` 56

            in  BSB.word64LE w64 <> go etc

