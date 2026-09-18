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
designed for performance that does not require Nix. This package is that
implementation in Haskell, ported from Amaru's `amaru-uplc` crate, and it
depends on nothing from `plutus-core`, so it can be used independently.

## Where it stands

The library provides the syntax, under `Cardano.UPLC`: the term and program
types, binders, builtins, constants and their types, and `Data`. It depends
on `bytestring` and `text` and nothing further, and it builds with plain
`cabal` against Hackage, without Nix.

`Cardano.UPLC.Flat` is a first version of the flat codec, in both directions,
and is still under review. Flat is the form a script takes on chain, and the
format is described in the module's Haddock. A `data` constant crosses the
wire as CBOR, and the CBOR codec is the next release; until then the codec
refuses one with a named error in each direction. The ledger's `value` type
is not handled yet either.

The CEK machine, cost metering, the builtins and version gating follow in
later releases, roughly in that order, since each one needs the ones before
it.

## Correctness

Semantics follow the Plutus specification rather than any single
implementation of it, and correctness is to be measured against the official
Plutus Conformance Test Suite.
