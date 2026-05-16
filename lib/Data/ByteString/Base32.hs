{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Data.ByteString.Base32
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Unpadded base32 encoding & decoding using the bech32 character set.

module Data.ByteString.Base32 (
    -- * base32 encoding and decoding
    encode
  , decode
  ) where

import qualified Data.Bits as B
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as BS
import Data.ByteString.Base32.Internal (enc_tab, dec_tab)
import qualified Data.ByteString.Internal as BI
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

fi :: (Num a, Integral b) => b -> a
fi = fromIntegral
{-# INLINE fi #-}

-- | Encode a base256-encoded 'ByteString' as a base32-encoded
--   'ByteString', using the bech32 character set.
--
--   >>> encode "jtobin was here!"
--   "df6x7cnfdcs8wctnyp5x2un9yy"
encode
  :: BS.ByteString -- ^ base256-encoded bytestring
  -> BS.ByteString -- ^ base32-encoded bytestring
encode (BI.PS sfp soff l) = case enc_tab of
  BI.PS tfp toff _ ->
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
decode (BI.PS sfp soff l) = case l `rem` 8 of
  1 -> Nothing
  3 -> Nothing
  6 -> Nothing
  _ -> case dec_tab of
    BI.PS tfp toff _ -> unsafeDupablePerformIO $ do
      let !n = (l * 5) `B.shiftR` 3
      fp <- BI.mallocByteString n
      ok <- withForeignPtr fp  $ \dst ->
            withForeignPtr sfp $ \sp0 ->
            withForeignPtr tfp $ \tp0 -> do
              let !sp = sp0 `plusPtr` soff :: Ptr Word8
                  !tp = tp0 `plusPtr` toff :: Ptr Word8
              decode_loop sp tp dst l 0 0 0
      pure $! if ok then Just (BI.PS fp 0 n) else Nothing

decode_loop
  :: Ptr Word8 -> Ptr Word8 -> Ptr Word8
  -> Int -> Int -> Int -> Word8 -> IO Bool
decode_loop !sp !tp !dst !len !i !j !acc
  | i + 8 <= len = do
      c0 <- peekElemOff sp i
      c1 <- peekElemOff sp (i + 1)
      c2 <- peekElemOff sp (i + 2)
      c3 <- peekElemOff sp (i + 3)
      c4 <- peekElemOff sp (i + 4)
      c5 <- peekElemOff sp (i + 5)
      c6 <- peekElemOff sp (i + 6)
      c7 <- peekElemOff sp (i + 7)
      n0 <- peekElemOff tp (fi c0)
      n1 <- peekElemOff tp (fi c1)
      n2 <- peekElemOff tp (fi c2)
      n3 <- peekElemOff tp (fi c3)
      n4 <- peekElemOff tp (fi c4)
      n5 <- peekElemOff tp (fi c5)
      n6 <- peekElemOff tp (fi c6)
      n7 <- peekElemOff tp (fi c7)
      let !v0 = n0 .&. 0x1f
          !v1 = n1 .&. 0x1f
          !v2 = n2 .&. 0x1f
          !v3 = n3 .&. 0x1f
          !v4 = n4 .&. 0x1f
          !v5 = n5 .&. 0x1f
          !v6 = n6 .&. 0x1f
          !v7 = n7 .&. 0x1f
          !b0 = (v0 `B.shiftL` 3) .|. (v1 `B.shiftR` 2)
          !b1 = (v1 `B.shiftL` 6) .|. (v2 `B.shiftL` 1) .|.
                (v3 `B.shiftR` 4)
          !b2 = (v3 `B.shiftL` 4) .|. (v4 `B.shiftR` 1)
          !b3 = (v4 `B.shiftL` 7) .|. (v5 `B.shiftL` 2) .|.
                (v6 `B.shiftR` 3)
          !b4 = (v6 `B.shiftL` 5) .|. v7
      pokeElemOff dst j       b0
      pokeElemOff dst (j + 1) b1
      pokeElemOff dst (j + 2) b2
      pokeElemOff dst (j + 3) b3
      pokeElemOff dst (j + 4) b4
      decode_loop sp tp dst len (i + 8) (j + 5)
        (acc .|. n0 .|. n1 .|. n2 .|. n3 .|. n4 .|. n5 .|. n6 .|. n7)
  | otherwise = decode_tail sp tp dst len i j acc

decode_tail
  :: Ptr Word8 -> Ptr Word8 -> Ptr Word8
  -> Int -> Int -> Int -> Word8 -> IO Bool
decode_tail !sp !tp !dst !len !i !j !acc = case len - i of
  0 -> pure $! acc .&. 0x40 == 0
  2 -> do
    c0 <- peekElemOff sp i
    c1 <- peekElemOff sp (i + 1)
    n0 <- peekElemOff tp (fi c0)
    n1 <- peekElemOff tp (fi c1)
    let !v0 = n0 .&. 0x1f
        !v1 = n1 .&. 0x1f
        !b0 = (v0 `B.shiftL` 3) .|. (v1 `B.shiftR` 2)
        -- canonical-form check: bits dropped from v1 must be zero
        !slack = v1 `B.shiftL` 6
    pokeElemOff dst j b0
    pure $! (acc .|. n0 .|. n1) .&. 0x40 == 0 && slack == 0
  4 -> do
    c0 <- peekElemOff sp i
    c1 <- peekElemOff sp (i + 1)
    c2 <- peekElemOff sp (i + 2)
    c3 <- peekElemOff sp (i + 3)
    n0 <- peekElemOff tp (fi c0)
    n1 <- peekElemOff tp (fi c1)
    n2 <- peekElemOff tp (fi c2)
    n3 <- peekElemOff tp (fi c3)
    let !v0 = n0 .&. 0x1f
        !v1 = n1 .&. 0x1f
        !v2 = n2 .&. 0x1f
        !v3 = n3 .&. 0x1f
        !b0 = (v0 `B.shiftL` 3) .|. (v1 `B.shiftR` 2)
        !b1 = (v1 `B.shiftL` 6) .|. (v2 `B.shiftL` 1) .|.
              (v3 `B.shiftR` 4)
        !slack = v3 `B.shiftL` 4
    pokeElemOff dst j       b0
    pokeElemOff dst (j + 1) b1
    pure $! (acc .|. n0 .|. n1 .|. n2 .|. n3) .&. 0x40 == 0
         && slack == 0
  5 -> do
    c0 <- peekElemOff sp i
    c1 <- peekElemOff sp (i + 1)
    c2 <- peekElemOff sp (i + 2)
    c3 <- peekElemOff sp (i + 3)
    c4 <- peekElemOff sp (i + 4)
    n0 <- peekElemOff tp (fi c0)
    n1 <- peekElemOff tp (fi c1)
    n2 <- peekElemOff tp (fi c2)
    n3 <- peekElemOff tp (fi c3)
    n4 <- peekElemOff tp (fi c4)
    let !v0 = n0 .&. 0x1f
        !v1 = n1 .&. 0x1f
        !v2 = n2 .&. 0x1f
        !v3 = n3 .&. 0x1f
        !v4 = n4 .&. 0x1f
        !b0 = (v0 `B.shiftL` 3) .|. (v1 `B.shiftR` 2)
        !b1 = (v1 `B.shiftL` 6) .|. (v2 `B.shiftL` 1) .|.
              (v3 `B.shiftR` 4)
        !b2 = (v3 `B.shiftL` 4) .|. (v4 `B.shiftR` 1)
        !slack = v4 `B.shiftL` 7
    pokeElemOff dst j       b0
    pokeElemOff dst (j + 1) b1
    pokeElemOff dst (j + 2) b2
    pure $! (acc .|. n0 .|. n1 .|. n2 .|. n3 .|. n4) .&. 0x40 == 0
         && slack == 0
  7 -> do
    c0 <- peekElemOff sp i
    c1 <- peekElemOff sp (i + 1)
    c2 <- peekElemOff sp (i + 2)
    c3 <- peekElemOff sp (i + 3)
    c4 <- peekElemOff sp (i + 4)
    c5 <- peekElemOff sp (i + 5)
    c6 <- peekElemOff sp (i + 6)
    n0 <- peekElemOff tp (fi c0)
    n1 <- peekElemOff tp (fi c1)
    n2 <- peekElemOff tp (fi c2)
    n3 <- peekElemOff tp (fi c3)
    n4 <- peekElemOff tp (fi c4)
    n5 <- peekElemOff tp (fi c5)
    n6 <- peekElemOff tp (fi c6)
    let !v0 = n0 .&. 0x1f
        !v1 = n1 .&. 0x1f
        !v2 = n2 .&. 0x1f
        !v3 = n3 .&. 0x1f
        !v4 = n4 .&. 0x1f
        !v5 = n5 .&. 0x1f
        !v6 = n6 .&. 0x1f
        !b0 = (v0 `B.shiftL` 3) .|. (v1 `B.shiftR` 2)
        !b1 = (v1 `B.shiftL` 6) .|. (v2 `B.shiftL` 1) .|.
              (v3 `B.shiftR` 4)
        !b2 = (v3 `B.shiftL` 4) .|. (v4 `B.shiftR` 1)
        !b3 = (v4 `B.shiftL` 7) .|. (v5 `B.shiftL` 2) .|.
              (v6 `B.shiftR` 3)
        !slack = v6 `B.shiftL` 5
    pokeElemOff dst j       b0
    pokeElemOff dst (j + 1) b1
    pokeElemOff dst (j + 2) b2
    pokeElemOff dst (j + 3) b3
    pure $!
         (acc .|. n0 .|. n1 .|. n2 .|. n3 .|. n4 .|. n5 .|. n6)
         .&. 0x40 == 0
         && slack == 0
  _ -> pure False -- impossible: tail-length guard already rejected
