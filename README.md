# bech32

[![](https://img.shields.io/hackage/v/ppad-bech32?color=blue)](https://hackage.haskell.org/package/ppad-bech32)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-bech32-lightblue)](https://docs.ppad.tech/bech32)

A pure Haskell implementation of bech32m and bech32 encoding/decoding on
strict ByteStrings, as specified by [BIP350][bi350] and [BIP173][bi173].

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > -- import qualified
  > import qualified Data.ByteString.Bech32m as Bech32m
  >
  > -- create a bech32m-encoded string using a human-readable part (HRP)
  > -- and some input
  > let Just bech32m = Bech32m.encode "bc" "a standard word8 bytestring"
  > bech32m
  "bc1vys8xarpdejxzunyypmk7uny8qsxy7t5v4ehgunfdenswyuz0e"
  >
  > -- verify that a bech32m string has a valid checksum
  > Bech32m.verify bech32
  True
  >
  > -- tweaked stuff will obviously fail to verify (s/m/w below)
  > Bech32m.verify "bc1vys8xarpdejxzunyypwk7uny8qsxy7t5v4ehgunfdenswyuz0e"
  False
  >
  > -- decode bech32m-encoded input
  > Bech32m.decode bech32m
  Just ("bc","a standard word8 bytestring")
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/bech32](https://docs.ppad.tech/bech32).

## Performance

The aim is best-in-class performance for pure Haskell code.

Current benchmark figures on a M4 Silicon MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking benchmarks/ppad-bech32/bech32 encode/120b
  time                 462.7 ns   (460.8 ns .. 465.5 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 465.5 ns   (464.3 ns .. 466.6 ns)
  std dev              3.955 ns   (3.485 ns .. 4.602 ns)

  benchmarking benchmarks/ppad-bech32/bech32 decode/120b
  time                 499.4 ns   (497.5 ns .. 502.3 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 508.0 ns   (505.2 ns .. 510.8 ns)
  std dev              9.101 ns   (7.828 ns .. 11.23 ns)
  variance introduced by outliers: 21% (moderately inflated)
```

You should compile with the 'llvm' flag for maximum performance.

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be challenging to achieve.

If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-bech32
```

to get a REPL for the main library.

## Attribution

The base32 implementation used internally is more or less a pure
translation of the [base32][bas32] package on Hackage.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[bi173]: https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
[bi350]: https://github.com/bitcoin/bips/blob/master/bip-0350.mediawiki
[bas32]: https://hackage.haskell.org/package/base32
