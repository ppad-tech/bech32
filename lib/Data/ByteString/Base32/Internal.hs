{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Data.ByteString.Base32.Internal
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Tables and the table-driven encoding loop for the bech32 base32
-- charset, shared by 'Data.ByteString.Base32' and
-- 'Data.ByteString.Bech32.Internal'.

module Data.ByteString.Base32.Internal (
    enc_tab
  , dec_tab
  , w5_tab
  , encode_with
  ) where

import qualified Data.Bits as B
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)

fi :: (Num a, Integral b) => b -> a
fi = fromIntegral
{-# INLINE fi #-}

-- 32-byte encoding table: the bech32 character set.  Maps a 5-bit
-- value (0..31) to its bech32 character.  ASCII-only with no embedded
-- NUL, so the bytestring 'IsString' rule rewrites the literal to
-- 'unsafePackAddress' and the bytes live in static rodata.
enc_tab :: BS.ByteString
enc_tab = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
{-# NOINLINE enc_tab #-}

-- 256-byte reverse table.  Index by an ASCII byte to obtain its
-- 5-bit value (biased into bit 5); valid bech32 chars map to
-- 0x20..0x3f, every other byte maps to 0x40.
--
-- The encoding is chosen so the literal is strictly ASCII and
-- contains no embedded NUL, which is what the bytestring 'IsString'
-- rule needs to rewrite it into 'unsafePackAddress' (cf. 'enc_tab')
-- - the bytes end up in static rodata, with no CAF allocation.
--
-- The 0x40 sentinel is distinguished by bit 6; no value 0x20..0x3f
-- carries that bit, so callers OR-fold every lookup into an
-- accumulator and test 'acc .&. 0x40 == 0' once at the end.  The
-- 5-bit value is extracted as 'b .&. 0x1f'.
dec_tab :: BS.ByteString
dec_tab =
  "\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x2f\x40\x2a\x31\x35\x34\x3a\x3e\x27\x25\x40\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x40\x3d\x40\x38\x2d\x39\x29\x28\x37\x40\x32\x36\x3f\x3b\x33\x40\
  \\x21\x20\x23\x30\x2b\x3c\x2c\x2e\x26\x24\x22\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\
  \\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40"
{-# NOINLINE dec_tab #-}

-- 32-byte identity table: maps a 5-bit value to itself.  Encoding
-- through it yields the 5-bit values themselves rather than their
-- bech32 characters.
w5_tab :: BS.ByteString
w5_tab = BS.pack [0 .. 31]
{-# NOINLINE w5_tab #-}

-- | Regroup a base256 'ByteString' into 5-bit values, mapping each
--   through the supplied 32-byte table.
encode_with
  :: BS.ByteString -- ^ 32-byte table
  -> BS.ByteString -- ^ base256-encoded bytestring
  -> BS.ByteString
encode_with (BI.PS tfp toff _) (BI.PS sfp soff l) =
  let !outlen = (l * 8 + 4) `quot` 5
  in  BI.unsafeCreate outlen $ \dst ->
        withForeignPtr sfp $ \sp0 ->
        withForeignPtr tfp $ \tp0 -> do
          let !sp = sp0 `plusPtr` soff :: Ptr Word8
              !tp = tp0 `plusPtr` toff :: Ptr Word8
          encode_loop sp tp dst l 0 0

encode_loop
  :: Ptr Word8 -> Ptr Word8 -> Ptr Word8
  -> Int -> Int -> Int -> IO ()
encode_loop !sp !tp !dst !len !i !j
  | i + 5 <= len = do
      a <- peekElemOff sp i
      b <- peekElemOff sp (i + 1)
      c <- peekElemOff sp (i + 2)
      d <- peekElemOff sp (i + 3)
      e <- peekElemOff sp (i + 4)
      let !w0 = (a `B.shiftR` 3) .&. 0x1f
          !w1 = (a `B.shiftL` 2 .|. b `B.shiftR` 6) .&. 0x1f
          !w2 = (b `B.shiftR` 1) .&. 0x1f
          !w3 = (b `B.shiftL` 4 .|. c `B.shiftR` 4) .&. 0x1f
          !w4 = (c `B.shiftL` 1 .|. d `B.shiftR` 7) .&. 0x1f
          !w5 = (d `B.shiftR` 2) .&. 0x1f
          !w6 = (d `B.shiftL` 3 .|. e `B.shiftR` 5) .&. 0x1f
          !w7 = e .&. 0x1f
      peekElemOff tp (fi w0) >>= pokeElemOff dst j
      peekElemOff tp (fi w1) >>= pokeElemOff dst (j + 1)
      peekElemOff tp (fi w2) >>= pokeElemOff dst (j + 2)
      peekElemOff tp (fi w3) >>= pokeElemOff dst (j + 3)
      peekElemOff tp (fi w4) >>= pokeElemOff dst (j + 4)
      peekElemOff tp (fi w5) >>= pokeElemOff dst (j + 5)
      peekElemOff tp (fi w6) >>= pokeElemOff dst (j + 6)
      peekElemOff tp (fi w7) >>= pokeElemOff dst (j + 7)
      encode_loop sp tp dst len (i + 5) (j + 8)
  | otherwise = encode_tail sp tp dst len i j

encode_tail
  :: Ptr Word8 -> Ptr Word8 -> Ptr Word8
  -> Int -> Int -> Int -> IO ()
encode_tail !sp !tp !dst !len !i !j = case len - i of
  0 -> pure ()
  1 -> do
    a <- peekElemOff sp i
    let !w0 = (a `B.shiftR` 3) .&. 0x1f
        !w1 = (a `B.shiftL` 2) .&. 0x1f
    peekElemOff tp (fi w0) >>= pokeElemOff dst j
    peekElemOff tp (fi w1) >>= pokeElemOff dst (j + 1)
  2 -> do
    a <- peekElemOff sp i
    b <- peekElemOff sp (i + 1)
    let !w0 = (a `B.shiftR` 3) .&. 0x1f
        !w1 = (a `B.shiftL` 2 .|. b `B.shiftR` 6) .&. 0x1f
        !w2 = (b `B.shiftR` 1) .&. 0x1f
        !w3 = (b `B.shiftL` 4) .&. 0x1f
    peekElemOff tp (fi w0) >>= pokeElemOff dst j
    peekElemOff tp (fi w1) >>= pokeElemOff dst (j + 1)
    peekElemOff tp (fi w2) >>= pokeElemOff dst (j + 2)
    peekElemOff tp (fi w3) >>= pokeElemOff dst (j + 3)
  3 -> do
    a <- peekElemOff sp i
    b <- peekElemOff sp (i + 1)
    c <- peekElemOff sp (i + 2)
    let !w0 = (a `B.shiftR` 3) .&. 0x1f
        !w1 = (a `B.shiftL` 2 .|. b `B.shiftR` 6) .&. 0x1f
        !w2 = (b `B.shiftR` 1) .&. 0x1f
        !w3 = (b `B.shiftL` 4 .|. c `B.shiftR` 4) .&. 0x1f
        !w4 = (c `B.shiftL` 1) .&. 0x1f
    peekElemOff tp (fi w0) >>= pokeElemOff dst j
    peekElemOff tp (fi w1) >>= pokeElemOff dst (j + 1)
    peekElemOff tp (fi w2) >>= pokeElemOff dst (j + 2)
    peekElemOff tp (fi w3) >>= pokeElemOff dst (j + 3)
    peekElemOff tp (fi w4) >>= pokeElemOff dst (j + 4)
  4 -> do
    a <- peekElemOff sp i
    b <- peekElemOff sp (i + 1)
    c <- peekElemOff sp (i + 2)
    d <- peekElemOff sp (i + 3)
    let !w0 = (a `B.shiftR` 3) .&. 0x1f
        !w1 = (a `B.shiftL` 2 .|. b `B.shiftR` 6) .&. 0x1f
        !w2 = (b `B.shiftR` 1) .&. 0x1f
        !w3 = (b `B.shiftL` 4 .|. c `B.shiftR` 4) .&. 0x1f
        !w4 = (c `B.shiftL` 1 .|. d `B.shiftR` 7) .&. 0x1f
        !w5 = (d `B.shiftR` 2) .&. 0x1f
        !w6 = (d `B.shiftL` 3) .&. 0x1f
    peekElemOff tp (fi w0) >>= pokeElemOff dst j
    peekElemOff tp (fi w1) >>= pokeElemOff dst (j + 1)
    peekElemOff tp (fi w2) >>= pokeElemOff dst (j + 2)
    peekElemOff tp (fi w3) >>= pokeElemOff dst (j + 3)
    peekElemOff tp (fi w4) >>= pokeElemOff dst (j + 4)
    peekElemOff tp (fi w5) >>= pokeElemOff dst (j + 5)
    peekElemOff tp (fi w6) >>= pokeElemOff dst (j + 6)
  _ -> pure ()  -- impossible: 0 <= len - i < 5
