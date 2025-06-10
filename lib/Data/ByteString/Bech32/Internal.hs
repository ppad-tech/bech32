{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8, Word32)

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

word5 :: Word8 -> Maybe Word8
word5 = \case
  113 -> pure $! 00 -- 'q'
  112 -> pure $! 01 -- 'p'
  122 -> pure $! 02 -- 'z'
  114 -> pure $! 03 -- 'r'
  121 -> pure $! 04 -- 'y'
  57  -> pure $! 05 -- '9'
  120 -> pure $! 06 -- 'x'
  56  -> pure $! 07 -- '8'
  103 -> pure $! 08 -- 'g'
  102 -> pure $! 09 -- 'f'
  50  -> pure $! 10 -- '2'
  116 -> pure $! 11 -- 't'
  118 -> pure $! 12 -- 'v'
  100 -> pure $! 13 -- 'd'
  119 -> pure $! 14 -- 'w'
  48  -> pure $! 15 -- '0'
  115 -> pure $! 16 -- 's'
  51  -> pure $! 17 -- '3'
  106 -> pure $! 18 -- 'j'
  110 -> pure $! 19 -- 'n'
  53  -> pure $! 20 -- '5'
  52  -> pure $! 21 -- '4'
  107 -> pure $! 22 -- 'k'
  104 -> pure $! 23 -- 'h'
  99  -> pure $! 24 -- 'c'
  101 -> pure $! 25 -- 'e'
  54  -> pure $! 26 -- '6'
  109 -> pure $! 27 -- 'm'
  117 -> pure $! 28 -- 'u'
  97  -> pure $! 29 -- 'a'
  55  -> pure $! 30 -- '7'
  108 -> pure $! 31 -- 'l'
  _   -> Nothing
{-# INLINE word5 #-}

-- base32 -> word5
as_word5 :: BS.ByteString -> Maybe BS.ByteString
as_word5 = go mempty where
  go acc bs = case BS.uncons bs of
    Nothing -> pure (toStrict acc)
    Just (h, t) -> do
      w5 <- word5 (fi h)
      go (acc <> BSB.word8 w5) t

-- word5 -> base32
as_base32 :: BS.ByteString -> BS.ByteString
as_base32 = BS.map (BU.unsafeIndex bech32_charset . fi)

polymod :: BS.ByteString -> Word32
polymod = BS.foldl' alg 1 where
  generator :: Int -> Word32
  generator = \case
    0 -> 0x3b6a57b2
    1 -> 0x26508e6d
    2 -> 0x1ea119fa
    3 -> 0x3d4233dd
    4 -> 0x2a1462b3
    _ -> error "ppad-bech32: internal error (please report this as a bug!)"

  alg !chk v =
    let !b = chk `B.shiftR` 25
        c = (chk .&. 0x1ffffff) `B.shiftL` 5 `B.xor` fi v
    in  loop_gen 0 b c

  loop_gen i b !chk
    | i > 4 = chk
    | otherwise =
        let sor | B.testBit (b `B.shiftR` i) 0 = generator i
                | otherwise = 0
        in  loop_gen (succ i) b (chk `B.xor` sor)

valid_hrp :: BS.ByteString -> Bool
valid_hrp hrp@(BI.PS _ _ l)
  | l == 0 || l > 83 = False
  | otherwise = BS.all (\b -> (b > 32) && (b < 127)) hrp

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
        w5s = as_word5 dat
    in  case w5s of
          Nothing -> False
          Just ws ->
            let bs = hrp_expand hrp <> ws
            in  polymod bs == case enc of
                  Bech32 -> 1
                  Bech32m -> _BECH32M_CONST

