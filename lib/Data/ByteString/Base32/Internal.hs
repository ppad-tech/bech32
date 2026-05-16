{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Data.ByteString.Base32.Internal
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Static rodata tables for the bech32 base32 charset, shared by
-- 'Data.ByteString.Base32' and 'Data.ByteString.Bech32.Internal'.

module Data.ByteString.Base32.Internal (
    enc_tab
  , dec_tab
  ) where

import qualified Data.ByteString as BS

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
