# ppad-bech32

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

The aim is best-in-class performance for pure, highly-auditable Haskell
code. At present we're a little over twice as fast as the official
BIP173 reference implementation.

Current benchmark figures on my mid-2020 MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking benchmarks/ppad-bech32/bech32 encode/120b
  time                 1.278 μs   (1.264 μs .. 1.293 μs)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 1.271 μs   (1.261 μs .. 1.283 μs)
  std dev              37.83 ns   (31.42 ns .. 45.63 ns)

  benchmarking benchmarks/ppad-bech32/bech32 decode/120b
  time                 1.567 μs   (1.519 μs .. 1.611 μs)
                       0.996 R²   (0.994 R² .. 0.998 R²)
  mean                 1.535 μs   (1.511 μs .. 1.565 μs)
  std dev              88.19 ns   (71.27 ns .. 108.7 ns)

  benchmarking benchmarks/reference/bech32 encode/120b
  time                 2.953 μs   (2.785 μs .. 3.143 μs)
                       0.975 R²   (0.958 R² .. 0.991 R²)
  mean                 2.817 μs   (2.723 μs .. 2.998 μs)
  std dev              415.9 ns   (287.2 ns .. 640.3 ns)
```

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
