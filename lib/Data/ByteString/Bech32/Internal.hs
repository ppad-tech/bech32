{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.Bech32.Internal (
    as_word5
  , as_base32
  , Encoding(..)
  , create_checksum
  , verify
  , valid_hrp
  ) where

import Data.Bits ((.&.))
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Primitive.PrimArray as PA
import Data.Word (Word32)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- realization for small builders
toStrict :: BSB.Builder -> BS.ByteString
toStrict = BS.toStrict
  . BE.toLazyByteStringWith (BE.safeStrategy 128 BE.smallChunkSize) mempty
{-# INLINE toStrict #-}

_BECH32M_CONST :: Word32
_BECH32M_CONST = 0x2bc830a3

bech32_charset :: BS.ByteString
bech32_charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

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
    let (hrp, BU.unsafeDrop 1 -> dat) = BS.splitAt idx b32
        bs = hrp_expand hrp <> as_word5 dat
    in  polymod bs == case enc of
          Bech32 -> 1
          Bech32m -> _BECH32M_CONST

