{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32 as Base32
import qualified Data.ByteString.Bech32 as Bech32
import GHC.Generics
import qualified Reference.Bech32 as R
import Control.DeepSeq

deriving instance Generic R.Word5
instance NFData R.Word5

main :: IO ()
main = defaultMain [
    suite
  ]

base32_encode :: Benchmark
base32_encode = bgroup "base32 encode" [
    bench "120b" $ nf Base32.encode "jtobin was here"
  , bench "128b (non 40-bit multiple length)" $
      nf Base32.encode "jtobin was here!"
  , bench "240b" $ nf Base32.encode "jtobin was herejtobin was here"
  ]

base32_decode :: Benchmark
base32_decode = bgroup "base32 decode" [
    bench "120b" $ nf Base32.decode "df6x7cnfdcs8wctnyp5x2un9"
  , bench "128b (non 40-bit multiple length)" $
      nf Base32.decode "df6x7cnfdcs8wctnyp5x2un9yy"
  ]

bech32_encode :: Benchmark
bech32_encode = bgroup "bech32 encode" [
    bench "120b" $ nf (Bech32.encode "bc") "jtobin was here"
  ]

bech32_decode :: Benchmark
bech32_decode = bgroup "bech32 decode" [
    bench "120b" $ nf Bech32.decode "bc1df6x7cnfdcs8wctnyp5x2un9f0pw8y"
  ]

suite :: Benchmark
suite = bgroup "benchmarks" [
      bgroup "ppad-bech32" [
          base32_encode
        , base32_decode
        , bech32_encode
        , bech32_decode
      ]
    , bgroup "reference" [
        bgroup "bech32 encode" [
          bench "120b" $ nf (refEncode "bc") "jtobin was here"
        ]
      ]
    ]
  where
    refEncode h a = R.bech32Encode h (R.toBase32 (BS.unpack a))

