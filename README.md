# cardano-uplc

Untyped Plutus Core (UPLC) in Haskell, the language every Cardano smart
contract compiles to.

## Why this exists

Plutarch compiles to UPLC but does not implement UPLC itself, taking its term
representation, its evaluator and its cost model from `plutus-core`. Building
`plutus-core` requires Nix, a requirement that carries into Plutarch's own
build, and anyone working on Plutarch has to learn `plutus-core`'s
implementation choices as well as Plutarch's own.

Amaru instead wrote its own UPLC implementation, a complete CEK machine
designed for performance that does not require Nix. This package is a UPLC
implementation in Haskell, without Nix, built with reference to `amaru-uplc`,
`plutus-core` and the Plutus Core specification. It depends on nothing from
`plutus-core`, so it can be used independently.

## Where it stands

The library provides the syntax, under `Cardano.UPLC`: the term and program
types, binders, builtins, constants and their types, and `Data`. It depends
on `bytestring`, `containers`, `text`, `transformers`, `vector` and `cborg`,
which bring `half`, `primitive` and `vector-stream`. `containers` ships with
GHC. Nothing needs a system library, and the package builds with plain `cabal`
against Hackage, without Nix.

`Cardano.UPLC.Flat` is a first version of the flat codec, in both directions,
and is still under review. Flat is the form a script takes on chain, and the
format is described in the module's Haddock.

`Cardano.UPLC.Data` defines `Data`, the type datums, redeemers and the script
context are built from. CBOR is its wire form wherever it appears: in a
transaction's datum and redeemer fields, with no script involved, and inside a
flat program that holds one as a constant, where the payload is a CBOR byte
string. `Cardano.UPLC.Data.Cbor` reads and writes it, byte for byte as
plutus-core does, and the flat codec uses it for `data` constants.
`Cardano.UPLC.Value` is the ledger's multi-asset `value` type, built on
nested maps as plutus-core's is, and the flat codec reads and writes `value`
constants with plutus-core's refusals.

`Cardano.UPLC.Machine` is the CEK machine. It runs every term except
`constr` and `case`, and of the builtins it runs the integer, bytestring and
bitwise ones and `ifThenElse`, under any of plutus-core's five semantics
variants. Anything else it refuses with `NotSupported`. The other builtins,
`constr` and `case`, and version gating follow in later releases. It has no
execution budget: costing is outside the package's scope.

## Correctness

Where `plutus-core` and the Plutus Core specification differ, behaviour
matches `plutus-core`, the implementation the network runs. The test suite
runs the official conformance cases for everything the machine implements.

The test suite includes cases copied from the Plutus repository's conformance
suite, which is under the Apache License 2.0. They are in `test/conformance`,
with their `LICENSE` and `NOTICE`; the package itself is MIT.

## Running the tests

From the package root, since the conformance runner reads `test/conformance`
relative to it:

```bash
cabal test all                      # the whole suite, 931 tests
cabal test all -w ghc-9.6.7         # the other compiler CI uses
cabal run -v0 test:tests -- -l      # list every test's name without running it
```

## Comparing with plutus-core

The comparison with plutus-core lives in its own repository,
[cardano-uplc-compare](https://github.com/sandykwl/cardano-uplc-compare),
because it depends on plutus-core, which this package exists to avoid. It
compares the flat codec, `Data` as CBOR and the `value` type, and reads
cardano-uplc from a checkout beside it:

```bash
git clone -b cek-machine https://github.com/sandykwl/cardano-uplc.git
git clone https://github.com/sandykwl/cardano-uplc-compare.git
cd cardano-uplc-compare
cabal update                    # the package index is pinned to 2026-09-24
cabal run -w ghc-9.6.7 cardano-uplc-compare -- 1000         # 1000 cases from each random family
cabal run -w ghc-9.6.7 cardano-uplc-compare -- corpus DIR   # every .flat file under DIR
```

The first build compiles plutus-core and about 150 other packages. The last
line of a run is `RESULT: no disagreements` or `RESULT: disagreements found`.

## Acknowledgements

The design follows Amaru's `amaru-uplc` crate, plutus-core and the Plutus
Core specification. `amaru-uplc` and plutus-core are licensed under Apache
2.0; see `NOTICE`.
