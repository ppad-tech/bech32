{-# LANGUAGE BangPatterns #-}

module Data.ByteString.Bech32 where

-- this entire module is an adaptation of the official haskell
-- reference, which can be found at:
--
-- github.com/sipa/bech32/blob/master/ref/haskell/src/Codec/Binary/Bech32.hs

import Data.Bits ((.|.), (.&.))
import qualified Data.Bits as B
import qualified Data.ByteString as BS

base256_to_base32 = undefined

base32_to_base256 = undefined

data Pad = Pad | NoPad

-- XX e0 ~ 5, e1 ~ 8 (or vice versa)
convert_base2 :: BS.ByteString -> Int -> Int -> Pad -> BS.ByteString
convert_base2 bs e0 e1 pad = loop 0 mempty 0 0 where
  mask = 2 ^ e1 - 1
  len = BS.length bs

  loop j !acc !car !pos
    | j == len = BS.pack . reverse $ case pad of
        Pad | pos > 0 ->
          let car0 = (car `B.unsafeShiftL` (e1 - pos)) .&. mask
          in  car0 : acc
        _ -> acc

    | otherwise =
        let word = BS.index bs j
            car0 = (car `B.unsafeShiftL` e0) .|. word
            pos0 = pos + e0
            (nacc, pos1) = loop_pos car0 pos0 acc
            car1 = car0 .&. (2 ^ pos1 - 1)
        in  loop (succ j) nacc car1 pos1

  loop_pos !car !pos !acc
    | pos < e1 = (acc, pos)
    | otherwise =
        let nacc = ((car `B.unsafeShiftR` (pos - e1)) .&. mask) : acc
        in  loop_pos car (pos - e1) nacc

