{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.Bech32.Internal (
    as_word5
  , as_base32
  , Encoding(..)
  , create_checksum
  , verify
  , valid_hrp
  ) where

import Data.Bits ((.&.), (.|.))
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.ByteString.Base32.Internal (enc_tab, dec_tab)
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

_BECH32M_CONST :: Word32
_BECH32M_CONST = 0x2bc830a3

-- | Translate base32 bytestring to its 5-bit-value bytestring.  Each
--   input byte is looked up in 'dec_tab'; if any byte is not a valid
--   bech32 char, returns 'Nothing'.
as_word5 :: BS.ByteString -> Maybe BS.ByteString
as_word5 (BI.PS sfp soff l) = case dec_tab of
  BI.PS tfp toff _ -> unsafeDupablePerformIO $ do
    fp <- BI.mallocByteString l
    ok <- withForeignPtr fp  $ \dst ->
          withForeignPtr sfp $ \sp0 ->
          withForeignPtr tfp $ \tp0 -> do
            let !sp = sp0 `plusPtr` soff :: Ptr Word8
                !tp = tp0 `plusPtr` toff :: Ptr Word8
                loop !i !acc
                  | i == l    = pure $! acc .&. 0x40 == 0
                  | otherwise = do
                      c <- peekElemOff sp i
                      n <- peekElemOff tp (fi c)
                      pokeElemOff dst i (n .&. 0x1f)
                      loop (i + 1) (acc .|. n)
            loop 0 0
    pure $! if ok then Just (BI.PS fp 0 l) else Nothing

-- | Translate a 5-bit-value bytestring to its bech32 base32
--   bytestring.
as_base32 :: BS.ByteString -> BS.ByteString
as_base32 (BI.PS sfp soff l) = case enc_tab of
  BI.PS tfp toff _ ->
    BI.unsafeCreate l $ \dst ->
      withForeignPtr sfp $ \sp0 ->
      withForeignPtr tfp $ \tp0 -> do
        let !sp = sp0 `plusPtr` soff :: Ptr Word8
            !tp = tp0 `plusPtr` toff :: Ptr Word8
            loop !i
              | i == l    = pure ()
              | otherwise = do
                  v <- peekElemOff sp i
                  c <- peekElemOff tp (fi v)
                  pokeElemOff dst i c
                  loop (i + 1)
        loop 0

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

-- | Build the bech32 HRP expansion: high-5-bits of each HRP byte,
--   then a single 0, then low-5-bits of each HRP byte.
hrp_expand :: BS.ByteString -> BS.ByteString
hrp_expand (BI.PS sfp soff l) =
  BI.unsafeCreate (2 * l + 1) $ \dst ->
    withForeignPtr sfp $ \sp0 -> do
      let !sp = sp0 `plusPtr` soff :: Ptr Word8
          loop_hi !i
            | i == l    = pure ()
            | otherwise = do
                c <- peekElemOff sp i
                pokeElemOff dst i (c `B.shiftR` 5)
                loop_hi (i + 1)
          loop_lo !i
            | i == l    = pure ()
            | otherwise = do
                c <- peekElemOff sp i
                pokeElemOff dst (l + 1 + i) (c .&. 0x1f)
                loop_lo (i + 1)
      loop_hi 0
      pokeElemOff dst l (0 :: Word8)
      loop_lo 0

data Encoding =
    Bech32
  | Bech32m

zero6 :: BS.ByteString
zero6 = BS.replicate 6 0
{-# NOINLINE zero6 #-}

create_checksum
  :: Encoding -> BS.ByteString -> BS.ByteString -> BS.ByteString
create_checksum enc hrp dat =
  let !pay = BS.concat [hrp_expand hrp, dat, zero6]
      !pm  = polymod pay `B.xor` case enc of
        Bech32  -> 1
        Bech32m -> _BECH32M_CONST
  in  BI.unsafeCreate 6 $ \dst -> do
        pokeElemOff dst 0 (fi (pm `B.shiftR` 25) .&. 0x1f :: Word8)
        pokeElemOff dst 1 (fi (pm `B.shiftR` 20) .&. 0x1f :: Word8)
        pokeElemOff dst 2 (fi (pm `B.shiftR` 15) .&. 0x1f :: Word8)
        pokeElemOff dst 3 (fi (pm `B.shiftR` 10) .&. 0x1f :: Word8)
        pokeElemOff dst 4 (fi (pm `B.shiftR`  5) .&. 0x1f :: Word8)
        pokeElemOff dst 5 (fi  pm               .&. 0x1f :: Word8)

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
