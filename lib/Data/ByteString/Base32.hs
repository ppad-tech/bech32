{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.Base32 (
    encode
  , as_word5
  , as_bech32
  , as_base32

  -- not actually base32-related, but convenient to put here
  , Encoding(..)
  , create_checksum
  , verify
  , valid_hrp
  ) where

import Data.Bits ((.|.), (.&.))
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Primitive.PrimArray as PA
import Data.Word (Word32)

_BECH32M_CONST :: Word32
_BECH32M_CONST = 0x2bc830a3

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

bech32_charset :: BS.ByteString
bech32_charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

-- adapted from emilypi's 'base32' library
encode :: BS.ByteString -> BS.ByteString
encode dat = toStrict (go dat) where
  bech32_char = fi . BS.index bech32_charset . fi
  go bs = case BS.splitAt 5 bs of
    (chunk, etc) -> case BS.length etc of
      -- https://datatracker.ietf.org/doc/html/rfc4648#section-6
      0 | BS.length chunk == 5 -> case BS.unsnoc chunk of
            Nothing -> error "impossible, chunk length is 5"
            Just (word32be -> w32, fi -> w8) -> arrange w32 w8

        | BS.length chunk == 1 ->
            let a = BU.unsafeIndex chunk 0
                t = bech32_char ((a .&. 0b11111000) `B.shiftR` 3)
                u = bech32_char ((a .&. 0b00000111) `B.shiftL` 2)
            in  BSB.word8 t <> BSB.word8 u

        | BS.length chunk == 2 ->
            let a = BU.unsafeIndex chunk 0
                b = BU.unsafeIndex chunk 1
                t = bech32_char ((a .&. 0b11111000) `B.shiftR` 3)
                u = bech32_char $
                          ((a .&. 0b00000111) `B.shiftL` 2)
                      .|. ((b .&. 0b11000000) `B.shiftR` 6)
                v = bech32_char ((b .&. 0b00111110) `B.shiftR` 1)
                w = bech32_char ((b .&. 0b00000001) `B.shiftL` 4)
            in  BSB.word8 t <> BSB.word8 u <> BSB.word8 v <> BSB.word8 w

        | BS.length chunk == 3 ->
            let a = BU.unsafeIndex chunk 0
                b = BU.unsafeIndex chunk 1
                c = BU.unsafeIndex chunk 2
                t = bech32_char ((a .&. 0b11111000) `B.shiftR` 3)
                u = bech32_char $
                          ((a .&. 0b00000111) `B.shiftL` 2)
                      .|. ((b .&. 0b11000000) `B.shiftR` 6)
                v = bech32_char ((b .&. 0b00111110) `B.shiftR` 1)
                w = bech32_char $
                          ((b .&. 0b00000001) `B.shiftL` 4)
                      .|. ((c .&. 0b11110000) `B.shiftR` 4)
                x = bech32_char ((c .&. 0b00001111) `B.shiftL` 1)
            in  BSB.word8 t <> BSB.word8 u <> BSB.word8 v <> BSB.word8 w
                <> BSB.word8 x

        | BS.length chunk == 4 ->
            let a = BU.unsafeIndex chunk 0
                b = BU.unsafeIndex chunk 1
                c = BU.unsafeIndex chunk 2
                d = BU.unsafeIndex chunk 3
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
            in  BSB.word8 t <> BSB.word8 u <> BSB.word8 v <> BSB.word8 w
                <> BSB.word8 x <> BSB.word8 y <> BSB.word8 z

        | otherwise -> mempty

      _ -> case BS.unsnoc chunk of
        Nothing -> error "impossible, chunk length is 5"
        Just (word32be -> w32, fi -> w8) -> arrange w32 w8 <> go etc

-- adapted from emilypi's 'base32' library
arrange :: Word32 -> Word32 -> BSB.Builder
arrange w32 w8 =
  let mask = 0b00011111
      bech32_char = fi . BS.index bech32_charset . fi

      w8_0 = bech32_char (mask .&. (w32 `B.shiftR` 27))
      w8_1 = bech32_char (mask .&. (w32 `B.shiftR` 22))
      w8_2 = bech32_char (mask .&. (w32 `B.shiftR` 17))
      w8_3 = bech32_char (mask .&. (w32 `B.shiftR` 12))
      w8_4 = bech32_char (mask .&. (w32 `B.shiftR` 07))
      w8_5 = bech32_char (mask .&. (w32 `B.shiftR` 02))
      w8_6 = bech32_char (mask .&. (w32 `B.shiftL` 03 .|. w8 `B.shiftR` 05))
      w8_7 = bech32_char (mask .&. w8)

      w64 = w8_0
        .|. w8_1 `B.shiftL` 8
        .|. w8_2 `B.shiftL` 16
        .|. w8_3 `B.shiftL` 24
        .|. w8_4 `B.shiftL` 32
        .|. w8_5 `B.shiftL` 40
        .|. w8_6 `B.shiftL` 48
        .|. w8_7 `B.shiftL` 56

  in  BSB.word64LE w64

-- naive base32 -> word5
as_word5 :: BS.ByteString -> BS.ByteString
as_word5 = BS.map f where
  f b = case BS.elemIndex (fi b) bech32_charset of
    Nothing -> error "ppad-bech32 (as_word5): input not bech32-encoded"
    Just w -> fi w

-- naive word5 -> base32
as_base32 :: BS.ByteString -> BS.ByteString
as_base32 = BS.map (BS.index bech32_charset . fi)

polymod :: BS.ByteString -> Word32
polymod = BS.foldl' alg 1 where
  generator = PA.primArrayFromListN 5
    [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]

  alg !chk v =
    let !b = chk `B.shiftR` 25
        c = (chk .&. 0x1ffffff) `B.shiftL` 5 `B.xor` fi v
    in  loop_gen 0 b c

  loop_gen i b !chk
    | i > 4 = chk
    | otherwise =
        let sor | B.testBit (b `B.shiftR` i) 0 =
                    PA.indexPrimArray generator i
                | otherwise = 0
        in  loop_gen (succ i) b (chk `B.xor` sor)

valid_hrp :: BS.ByteString -> Bool
valid_hrp hrp
    | l == 0 || l > 83 = False
    | otherwise = BS.all (\b -> (b > 32) && (b < 127)) hrp
  where
    l = BS.length hrp

hrp_expand :: BS.ByteString -> BS.ByteString
hrp_expand bs = toStrict
  $  BSB.byteString (BS.map (`B.shiftR` 5) bs)
  <> BSB.word8 0
  <> BSB.byteString (BS.map (.&. 0b11111) bs)

data Encoding =
    Bech32
  | Bech32m

create_checksum :: Encoding -> BS.ByteString -> BS.ByteString -> BS.ByteString
create_checksum enc hrp dat =
  let pre = hrp_expand hrp <> dat
      pay = toStrict $
           BSB.byteString pre
        <> BSB.byteString "\NUL\NUL\NUL\NUL\NUL\NUL"
      pm = polymod pay `B.xor` case enc of
        Bech32  -> 1
        Bech32m -> _BECH32M_CONST

      code i = (fi (pm `B.shiftR` fi i) .&. 0b11111)

  in  BS.map code "\EM\DC4\SI\n\ENQ\NUL" -- BS.pack [25, 20, 15, 10, 5, 0]

verify :: Encoding -> BS.ByteString -> Bool
verify enc b32 = case BS.elemIndexEnd 0x31 b32 of
  Nothing  -> False
  Just idx ->
    let (hrp, BS.drop 1 -> dat) = BS.splitAt idx b32
        bs = hrp_expand hrp <> as_word5 dat
    in  polymod bs == case enc of
          Bech32 -> 1
          Bech32m -> _BECH32M_CONST

