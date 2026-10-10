{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.Bech32.Internal (
    as_word5
  , as_base32
  , Encoding(..)
  , create_checksum
  , verify
  , valid_hrp
  , encode
  , decode
  ) where

import Control.Monad (guard)
import Data.Bits ((.&.), (.|.))
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32 as B32
import Data.ByteString.Base32.Internal
  (enc_tab, dec_tab, w5_tab, encode_with)
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
--   bytestring.  Only the low 5 bits of each input byte are used.
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
                  c <- peekElemOff tp (fi (v .&. 0x1f))
                  pokeElemOff dst i c
                  loop (i + 1)
        loop 0

polymod :: BS.ByteString -> Word32
polymod = BS.foldl' alg 1 where
  alg !chk v =
    let !b = chk `B.shiftR` 25
        !c = (chk .&. 0x1ffffff) `B.shiftL` 5 `B.xor` fi v
    in  c `B.xor` gen b 0 0x3b6a57b2
          `B.xor` gen b 1 0x26508e6d
          `B.xor` gen b 2 0x1ea119fa
          `B.xor` gen b 3 0x3d4233dd
          `B.xor` gen b 4 0x2a1462b3

  -- the generator 'g' if bit 'i' of 'b' is set, else zero
  gen :: Word32 -> Int -> Word32 -> Word32
  gen b i g = negate ((b `B.shiftR` i) .&. 1) .&. g
  {-# INLINE gen #-}

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

-- | Lowercase an all-uppercase string.  Mixed-case strings (some
--   ASCII uppercase and some ASCII lowercase characters) are rejected,
--   per BIP173.
normalize_case :: BS.ByteString -> Maybe BS.ByteString
normalize_case bs
  | has_upper && BS.any is_lower bs = Nothing
  | has_upper = Just $! BS.map to_lower bs
  | otherwise = Just bs
  where
    has_upper = BS.any is_upper bs

is_upper :: Word8 -> Bool
is_upper c = c >= 0x41 && c <= 0x5A
{-# INLINE is_upper #-}

is_lower :: Word8 -> Bool
is_lower c = c >= 0x61 && c <= 0x7A
{-# INLINE is_lower #-}

to_lower :: Word8 -> Word8
to_lower c
  | is_upper c = c + 0x20
  | otherwise  = c
{-# INLINE to_lower #-}

-- | Does a lowercase hrp and data part (5-bit values plus a 6-value
--   checksum, as bech32 characters) carry a valid checksum?
valid_checksum :: Encoding -> BS.ByteString -> BS.ByteString -> Bool
valid_checksum enc hrp dat
  | BS.length dat < 6 = False
  | otherwise = case as_word5 dat of
      Nothing -> False
      Just ws -> polymod (hrp_expand hrp <> ws) == case enc of
        Bech32  -> 1
        Bech32m -> _BECH32M_CONST

-- | Verify that a bech32 or bech32m string has a valid checksum.
--   All-uppercase input is accepted; mixed-case input is not.
verify :: Encoding -> BS.ByteString -> Bool
verify enc b32 = case normalize_case b32 of
  Nothing -> False
  Just bs -> case BS.elemIndexEnd 0x31 bs of
    Nothing  -> False
    Just idx ->
      let (hrp, BU.unsafeDrop 1 -> dat) = BS.splitAt idx bs
      in  valid_checksum enc hrp dat

-- | Encode a human-readable part and base256 data as bech32 or
--   bech32m.  The human-readable part is lowercased.
encode
  :: Encoding
  -> BS.ByteString        -- ^ base256-encoded human-readable part
  -> BS.ByteString        -- ^ base256-encoded data part
  -> Maybe BS.ByteString  -- ^ bech32-encoded bytestring
encode enc (BS.map to_lower -> hrp) dat = do
  guard (valid_hrp hrp)
  let !ws = encode_with w5_tab dat
  guard (BS.length hrp + BS.length ws + 7 <= 90)
  let !chk = create_checksum enc hrp ws
  pure $! BS.concat [hrp, BS.singleton 0x31, as_base32 (ws <> chk)]

-- | Decode a bech32 or bech32m string into its (lowercase)
--   human-readable part and base256 data part.  All-uppercase input
--   is accepted; mixed-case input is not.
decode
  :: Encoding
  -> BS.ByteString                        -- ^ bech32-encoded bytestring
  -> Maybe (BS.ByteString, BS.ByteString) -- ^ (hrp, data less checksum)
decode enc b32 = do
  guard (BS.length b32 <= 90)
  bs  <- normalize_case b32
  sep <- BS.elemIndexEnd 0x31 bs
  let (hrp, BU.unsafeDrop 1 -> raw) = BS.splitAt sep bs
  guard (valid_hrp hrp)
  guard (valid_checksum enc hrp raw)
  dat <- B32.decode (BS.dropEnd 6 raw)
  pure (hrp, dat)
