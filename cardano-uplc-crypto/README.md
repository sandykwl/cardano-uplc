# cardano-uplc-crypto

The three signature builtins of Plutus Core for `cardano-uplc`:
`verifyEd25519Signature`, `verifyEcdsaSecp256k1Signature` and
`verifySchnorrSecp256k1Signature`. Argument lengths and the other rules that
need no library are in the core package. This one gives the verdicts, from
libsodium and libsecp256k1, the C libraries the Cardano node runs.

```haskell
import Cardano.UPLC.Crypto qualified as Crypto
import Cardano.UPLC.Machine (Params (..), defaultParams, evaluateWith)

run = evaluateWith defaultParams {signatureChecks = Crypto.libraryChecks}
```

## System libraries

The package needs `pkg-config` and two libraries it can find:

- libsodium 1.0.18 or later. Ed25519 acceptance is libsodium's rule, not the
  specification's. It is the same from 1.0.18 to 1.0.21 and has changed on
  libsodium's unreleased `master`. The tests in
  `test/ed25519-edge-vectors.tsv` fail on a library that decides differently.
- libsecp256k1 0.3.2 or later, built with the `schnorrsig` and `extrakeys`
  modules. Distribution packages are often older; build it from source into a
  prefix and set `PKG_CONFIG_PATH` to its `lib/pkgconfig`.

## Tests

This directory has its own `cabal.project`, so run cabal from here; the root
project builds the core alone.

`cabal test` in this directory runs 440 tests:

- the 83 signature cases of Plutus's conformance suite, in `test/conformance`;
- 74 chosen inputs: the ways a check fails or answers `False`, and valid
  signatures at the edges of its rules;
- three messages at or just past the 65,536-byte bound, each under the five
  semantics variants, 15 tests in all;
- 268 Ed25519 edge cases, with verdicts from libsodium as `cardano-crypto-class`
  links it.

## Continuous integration

`.github/workflows/cardano-uplc-crypto.yml` builds and tests on `ubuntu-latest`
and `macos-latest` with GHC 9.6.7 and 9.8.4, taking libsodium from the
distribution and building libsecp256k1 v0.3.2 from source into `~/.local`,
which is cached. `windows-latest` runs with `continue-on-error`, on the MSYS2
packages. A separate job checks Haddock coverage, `@since` and `cabal check`.
