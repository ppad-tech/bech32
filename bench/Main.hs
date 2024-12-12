{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Data.ByteString.Bech32 as B32

main :: IO ()
main = defaultMain [
    suite
  ]

suite :: Benchmark
suite = env setup $ \big ->
    bgroup "ppad-bech32" [
        bench "base32 120b" $  whnf B32.base32 "jtobin was here"
      , bench "base32 240b" $  whnf B32.base32 "jtobin was herejtobin was here"
      , bench "base32 1200b" $ whnf B32.base32 big
      ]
  where
    setup = pure . mconcat . take 10 $ repeat "jtobin was here"

