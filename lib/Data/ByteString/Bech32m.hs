{-# OPTIONS_HADDOCK prune #-}

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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Bech32.Internal as BI

-- | Encode a base256 human-readable part and input as bech32m.
--
--   >>> let Just bech32m = encode "bc" "my string"
--   >>> bech32m
--   "bc1d4ujqum5wf5kuecwqlxtg"
encode
  :: BS.ByteString        -- ^ base256-encoded human-readable part
  -> BS.ByteString        -- ^ base256-encoded data part
  -> Maybe BS.ByteString  -- ^ bech32m-encoded bytestring
encode = BI.encode BI.Bech32m

-- | Decode a bech32m-encoded 'ByteString' into its human-readable and data
--   parts.
--
--   All-uppercase input is accepted, and its human-readable part is
--   returned in lowercase. Mixed-case input produces 'Nothing'.
--
--   >>> decode "hi1df6x7cnfdcs8wctnyp5x2un9m9ac4f"
--   Just ("hi","jtobin was here")
--   >>> decode "hey1df6x7cnfdcs8wctnyp5x2un9m9ac4f" -- s/hi/hey
--   Nothing
decode
  :: BS.ByteString                        -- ^ bech32m-encoded bytestring
  -> Maybe (BS.ByteString, BS.ByteString) -- ^ (hrp, data less checksum)
decode = BI.decode BI.Bech32m

-- | Verify that a bech32m string has a valid checksum.
--
--   All-uppercase input is accepted; mixed-case input is not.
--
--   >>> verify "bc1d4ujqum5wf5kuecwqlxtg"
--   True
--   >>> verify "bc1d4ujquw5wf5kuecwqlxtg" -- s/m/w
--   False
verify
  :: BS.ByteString -- ^ bech32m-encoded bytestring
  -> Bool
verify = BI.verify BI.Bech32m

