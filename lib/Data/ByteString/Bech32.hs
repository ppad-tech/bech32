{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.Bech32 (
    encode
  , verify_checksum
  ) where

import Control.Monad (guard)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32 as B32
import Data.ByteString.Base32 (Encoding(..))
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BE

-- realization for small builders
toStrict :: BSB.Builder -> BS.ByteString
toStrict = BS.toStrict
  . BE.toLazyByteStringWith (BE.safeStrategy 128 BE.smallChunkSize) mempty

create_checksum :: BS.ByteString -> BS.ByteString -> BS.ByteString
create_checksum = B32.create_checksum Bech32

-- | Verify a Bech32 checksum, given human-readable and data parts.
--
--   >>> let Just bech32 = encode "bc" "my string"
--   >>> verify_checksum "bc" "d4ujqum5wf5kuecmu02w2"
--   True
--   >>> verify_checksum "bc" "d4ujquw5wf5kuecmu02w2" -- s/m/w
--   False
verify_checksum
  :: BS.ByteString -- ^ base255-encoded human-readable part
  -> BS.ByteString -- ^ bech32-encoded data part
  -> Bool
verify_checksum = B32.verify_checksum Bech32

-- | Encode a base255 human-readable part and input as Bech32.
--
--   >>> let Just bech32 = encode "bc" "my string"
--   >>> bech32
--   "bc1d4ujqum5wf5kuecmu02w2"
encode
  :: BS.ByteString        -- ^ base255-encoded human-readable part
  -> BS.ByteString        -- ^ base255-encoded data part
  -> Maybe BS.ByteString  -- ^ bech32-encoded bytestring
encode hrp (B32.encode -> dat) = do
  guard (B32.valid_hrp hrp)
  let check = create_checksum hrp (B32.as_word5 dat)
      res = toStrict $
           BSB.byteString hrp
        <> BSB.word8 49 -- 1
        <> BSB.byteString dat
        <> BSB.byteString (B32.as_bech32 check)
  guard (BS.length res < 91)
  pure res

