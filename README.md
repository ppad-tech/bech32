# ppad-bech32

A pure Haskell implementation of the bech32m and bech32 encodings on
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
```

## Performance

The eventual aim is best-in-class performance for pure, highly-auditable
Haskell code. At present we're roughly equivalent to (perhaps slightly
faster than) the official BIP173 reference implementation.

Current benchmark figures on my mid-2020 MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking ppad-bech32/bech32/120b
  time                 1.364 μs   (1.354 μs .. 1.374 μs)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 1.355 μs   (1.344 μs .. 1.366 μs)
  std dev              34.32 ns   (28.03 ns .. 43.94 ns)

  benchmarking ppad-bech32/bech32/128b (non 40-bit multiple length)
  time                 1.451 μs   (1.439 μs .. 1.463 μs)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 1.441 μs   (1.433 μs .. 1.451 μs)
  std dev              29.46 ns   (24.15 ns .. 36.49 ns)

  benchmarking ppad-bech32/bech32/240b
  time                 2.085 μs   (2.066 μs .. 2.104 μs)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 2.093 μs   (2.076 μs .. 2.113 μs)
  std dev              58.92 ns   (49.45 ns .. 78.03 ns)
```

## Security

**NOTE** This library is still in a pre-release state!

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
