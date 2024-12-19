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

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/bech32](https://docs.ppad.tech/bech32).

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code. At present we're slightly faster than the official BIP173
reference implementation.

Current benchmark figures on my mid-2020 MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking ppad-bech32/bech32 encode/120b
  time                 1.230 μs   (1.204 μs .. 1.255 μs)
                       0.998 R²   (0.996 R² .. 0.999 R²)
  mean                 1.209 μs   (1.194 μs .. 1.224 μs)
  std dev              51.37 ns   (42.90 ns .. 64.59 ns)

  benchmarking ppad-bech32/bech32 encode/128b (non 40-bit multiple length)
  time                 1.335 μs   (1.312 μs .. 1.354 μs)
                       0.997 R²   (0.995 R² .. 0.998 R²)
  mean                 1.343 μs   (1.318 μs .. 1.386 μs)
  std dev              101.4 ns   (72.87 ns .. 161.6 ns)

  benchmarking ppad-bech32/bech32 encode/240b
  time                 1.851 μs   (1.822 μs .. 1.879 μs)
                       0.999 R²   (0.998 R² .. 0.999 R²)
  mean                 1.848 μs   (1.825 μs .. 1.872 μs)
  std dev              77.27 ns   (63.06 ns .. 94.54 ns)
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
