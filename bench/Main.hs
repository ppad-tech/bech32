{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Data.ByteString.Base32 as Base32
import qualified Data.ByteString.Bech32 as Bech32

main :: IO ()
main = defaultMain [
    suite
  ]

suite :: Benchmark
suite = env setup $ \big ->
    bgroup "ppad-bech32" [
        bgroup "base32" [
            bench "base32 120b" $  whnf Base32.encode
              "jtobin was here"
          , bench "base32 128b" $  whnf Base32.encode
              "jtobin was here!"
          , bench "base32 240b" $  whnf Base32.encode
              "jtobin was herejtobin was here"
          , bench "base32 1200b" $ whnf Base32.encode big
        ]
      , bgroup "bech32" [
            bench "bech32 120b" $ nf (Bech32.encode "bc")
              "jtobin was here"
          , bench "bech32 128b" $ nf (Bech32.encode "bc")
              "jtobin was here!"
        ]
    ]
  where
    setup = pure . mconcat . take 10 $ repeat "jtobin was here"

