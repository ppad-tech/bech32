{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Data.ByteString.Bech32m
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- The
-- [BIP350](https://github.com/bitcoin/bips/blob/master/bip-0350.mediawiki)
-- bech32m checksummed base32 encoding, with decoding and checksum
-- verification.

module Data.ByteString.Bech32m (
    -- * Encoding and Decoding
    encode
  , decode

    -- * Checksum
  , verify
  ) where

import Control.Monad (guard)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base32 as B32
import qualified Data.ByteString.Bech32.Internal as BI
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Internal as BSI
import qualified Data.Char as C (toLower)

-- realization for small builders
toStrict :: BSB.Builder -> BS.ByteString
toStrict = BS.toStrict
  . BE.toLazyByteStringWith (BE.safeStrategy 128 BE.smallChunkSize) mempty
{-# INLINE toStrict #-}

create_checksum :: BS.ByteString -> BS.ByteString -> BS.ByteString
create_checksum = BI.create_checksum BI.Bech32m

-- | Encode a base256 human-readable part and input as bech32m.
--
--   >>> let Just bech32m = encode "bc" "my string"
--   >>> bech32m
--   "bc1d4ujqum5wf5kuecwqlxtg"
encode
  :: BS.ByteString        -- ^ base256-encoded human-readable part
  -> BS.ByteString        -- ^ base256-encoded data part
  -> Maybe BS.ByteString  -- ^ bech32m-encoded bytestring
encode hrp (B32.encode -> dat) = do
  guard (BI.valid_hrp hrp)
  let check = create_checksum hrp (BI.as_word5 dat)
      res = toStrict $
           BSB.byteString (B8.map C.toLower hrp)
        <> BSB.word8 49 -- 1
        <> BSB.byteString dat
        <> BSB.byteString (BI.as_base32 check)
  guard (BS.length res < 91)
  pure res

-- | Decode a bech32m-encoded 'ByteString' into its human-readable and data
--   parts.
--
--   >>> decode "hi1df6x7cnfdcs8wctnyp5x2un9m9ac4f"
--   Just ("hi","jtobin was here")
--   >>> decode "hey1df6x7cnfdcs8wctnyp5x2un9m9ac4f" -- s/hi/hey
--   Nothing
decode
  :: BS.ByteString                        -- ^ bech23-encoded bytestring
  -> Maybe (BS.ByteString, BS.ByteString)
decode bs@(BSI.PS _ _ l) = do
  guard (l <= 90)
  guard (verify bs)
  sep <- BS.elemIndexEnd 0x31 bs
  case BS.splitAt sep bs of
    (hrp, raw) -> do
      guard (BI.valid_hrp hrp)
      guard (BS.length raw >= 6)
      (_, BS.dropEnd 6 -> bech32dat) <- BS.uncons raw
      dat <- B32.decode bech32dat
      pure (hrp, dat)

-- | Verify that a bech32m string has a valid checksum.
--
--   >>> verify "bc1d4ujqum5wf5kuecwqlxtg"
--   True
--   >>> verify "bc1d4ujquw5wf5kuecwqlxtg" -- s/m/w
--   False
verify
  :: BS.ByteString -- ^ bech32m-encoded bytestring
  -> Bool
verify = BI.verify BI.Bech32m

