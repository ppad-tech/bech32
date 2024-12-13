{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Data.ByteString.Bech32 as Bech32

main :: IO ()
main = defaultMain [
    suite
  ]

suite :: Benchmark
suite =
  bgroup "ppad-bech32" [
    bgroup "bech32" [
        bench "120b" $ nf (Bech32.encode "bc")
          "jtobin was here"
      , bench "128b (non 40-bit multiple length)" $ nf (Bech32.encode "bc")
          "jtobin was here!"
      , bench "240b" $ nf (Bech32.encode "bc")
          "jtobin was herejtobin was here"
    ]
  ]

