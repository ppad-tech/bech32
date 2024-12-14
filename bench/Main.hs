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

base32 :: Benchmark
base32 = bgroup "base32 encode" [
    bench "120b" $ nf Base32.encode "jtobin was here"
  , bench "128b (non 40-bit multiple length)" $
      nf Base32.encode "jtobin was here!"
  , bench "240b" $ nf Base32.encode "jtobin was herejtobin was here"
  ]

bech32 :: Benchmark
bech32 = bgroup "bech32 encode" [
    bench "120b" $ nf (Bech32.encode "bc") "jtobin was here"
  , bench "128b (non 40-bit multiple length)" $
      nf (Bech32.encode "bc") "jtobin was here!"
  , bench "240b" $ nf (Bech32.encode "bc") "jtobin was herejtobin was here"
  ]

suite :: Benchmark
suite = env setup $ \ ~(a, b, c) -> bgroup "benchmarks" [
      bgroup "ppad-bech32" [
          base32
        , bech32
      ]
    , bgroup "reference" [
        bgroup "bech32" [
            bench "120b" $ nf (R.bech32Encode "bc") a
          , bench "128b (non 40-bit multiple length)" $
              nf (R.bech32Encode "bc") b
          , bench "240b" $ nf (R.bech32Encode "bc") c
        ]
      ]
    ]
  where
    setup = do
      let a = R.toBase32 (BS.unpack "jtobin was here")
          b = R.toBase32 (BS.unpack "jtobin was here!")
          c = R.toBase32 (BS.unpack "jtobin was herejtobin was here")
      pure (a, b, c)
