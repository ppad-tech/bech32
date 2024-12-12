{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.Bech32 where

import Control.Monad (guard)
import Data.Bits ((.|.), (.&.))
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.Primitive.PrimArray as PA
import Data.Word (Word8, Word32, Word64)

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

bech32_charset :: BS.ByteString
bech32_charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

bech32_char_w64 :: Word32 -> Word64
bech32_char_w64 = fi . BS.index bech32_charset . fi

bech32_char :: Word8 -> Word8
bech32_char = fi . BS.index bech32_charset . fi

-- adapted from emilypi's 'base32' library
w40_to_w64 :: Word32 -> Word32 -> BSB.Builder
w40_to_w64 w32 w8 =
  let mask = 0b00011111

      w8_0 = bech32_char_w64 (mask .&. (w32 `B.shiftR` 27))
      w8_1 = bech32_char_w64 (mask .&. (w32 `B.shiftR` 22))
      w8_2 = bech32_char_w64 (mask .&. (w32 `B.shiftR` 17))
      w8_3 = bech32_char_w64 (mask .&. (w32 `B.shiftR` 12))
      w8_4 = bech32_char_w64 (mask .&. (w32 `B.shiftR` 07))
      w8_5 = bech32_char_w64 (mask .&. (w32 `B.shiftR` 02))
      w8_6 = bech32_char_w64 (mask .&. (w32 `B.shiftL` 03 .|. w8 `B.shiftR` 05))
      w8_7 = bech32_char_w64 (mask .&. w8)

      w64 = w8_0
        .|. w8_1 `B.shiftL` 8
        .|. w8_2 `B.shiftL` 16
        .|. w8_3 `B.shiftL` 24
        .|. w8_4 `B.shiftL` 32
        .|. w8_5 `B.shiftL` 40
        .|. w8_6 `B.shiftL` 48
        .|. w8_7 `B.shiftL` 56

  in  BSB.word64LE w64

-- adapted from emilypi's 'base32' library
base32 :: BS.ByteString -> BS.ByteString
base32 dat = toStrict (go dat) where
  go bs = case BS.splitAt 5 bs of
    (chunk, etc) -> case BS.length etc of
      0 | BS.length chunk == 5 -> case BS.unsnoc chunk of
            Nothing -> error "impossible, chunk length is 5"
            Just (word32be -> w32, fi -> w8) -> w40_to_w64 w32 w8

        | BS.length chunk == 1 ->
            let a = BS.index chunk 0
                t = bech32_char ((a .&. 0b11111000) `B.shiftR` 3)
                u = bech32_char ((a .&. 0b00000111) `B.shiftL` 2)
            in  BSB.word8 t <> BSB.word8 u

        | BS.length chunk == 2 ->
            let a = BS.index chunk 0
                b = BS.index chunk 1
                t = bech32_char ((a .&. 0b11111000) `B.shiftR` 3)
                u = bech32_char $
                          ((a .&. 0b00000111) `B.shiftL` 2)
                      .|. ((b .&. 0b11000000) `B.shiftR` 6)
                v = bech32_char ((b .&. 0b00111110) `B.shiftR` 1)
                w = bech32_char ((b .&. 0b00000001) `B.shiftL` 4)
            in  BSB.word8 t <> BSB.word8 u <> BSB.word8 v <> BSB.word8 w

        | BS.length chunk == 3 ->
            let a = BS.index chunk 0
                b = BS.index chunk 1
                c = BS.index chunk 2
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
            let a = BS.index chunk 0
                b = BS.index chunk 1
                c = BS.index chunk 2
                d = BS.index chunk 3
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
        Just (word32be -> w32, fi -> w8) -> w40_to_w64 w32 w8 <> go etc

-- naive base32 -> word5
as_w5s :: BS.ByteString -> BS.ByteString
as_w5s bs = BS.map f bs where
  f b = case BS.elemIndex (fi b) bech32_charset of
    Nothing -> error "ppad-bech32 (as_w5s): input not bech32-encoded"
    Just w -> fi w

-- naive word5 -> bech32
as_bech32 :: BS.ByteString -> BS.ByteString
as_bech32 bs = BS.map f bs where
  f b = BS.index bech32_charset (fi b)

valid_hrp :: BS.ByteString -> Bool
valid_hrp hrp
    | l == 0 || l > 83 = False
    | otherwise = BS.all (\b -> (b > 32) && (b < 127)) hrp
  where
    l = BS.length hrp

bech32_polymod :: BS.ByteString -> Word32
bech32_polymod = BS.foldl' alg 1 where
  generator = PA.primArrayFromListN 5
    [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]

  alg !chk v =
    let !b = chk `B.shiftR` 25
        c = (chk .&. 0x1ffffff) `B.shiftL` 5 `B.xor` fi v
    in  loop_gen 0 b c

  loop_gen i b !chk
    | i > 4 = chk
    | otherwise =
        let sor | B.testBit (b `B.shiftR` i) 0 = PA.indexPrimArray generator i
                | otherwise = 0
        in  loop_gen (succ i) b (chk `B.xor` sor)

bech32_hrp_expand :: BS.ByteString -> BS.ByteString
bech32_hrp_expand bs = toStrict
  $  BSB.byteString (BS.map (`B.shiftR` 5) bs)
  <> BSB.word8 0
  <> BSB.byteString (BS.map (.&. 0b11111) bs)

bech32_verify_checksum :: BS.ByteString -> BS.ByteString -> Bool
bech32_verify_checksum hrp dat =
  let bs = bech32_hrp_expand hrp <> dat
  in  bech32_polymod bs == 1

bech32_create_checksum :: BS.ByteString -> BS.ByteString -> BS.ByteString
bech32_create_checksum hrp dat =
  let pre = bech32_hrp_expand hrp <> dat
      pay = toStrict $
           BSB.byteString pre
        <> BSB.byteString "\NUL\NUL\NUL\NUL\NUL\NUL"
      pm = bech32_polymod pay `B.xor` 1

      code i = (fi (pm `B.shiftR` fi i) .&. 0b11111)

  in  BS.map code "\EM\DC4\SI\n\ENQ\NUL" -- BS.pack [25, 20, 15, 10, 5, 0]

-- base255 -> bech32
encode :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
encode hrp (base32 -> dat) = do
  guard (valid_hrp hrp)
  let check = bech32_create_checksum hrp (as_w5s dat)
      res = toStrict $
           BSB.byteString hrp
        <> BSB.word8 49 -- 1
        <> BSB.byteString dat
        <> BSB.byteString (as_bech32 check)
  guard (BS.length res < 91)
  pure res

