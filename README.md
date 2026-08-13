# plutarch-uplc

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

The library currently provides two modules: `Plutarch.UPLC.Ty`, for the type a
constant carries, and `Plutarch.UPLC.Data`, for the datums, redeemers and
other structured values that pass between the ledger and a script. It depends
on `bytestring` and nothing further, and it builds with plain `cabal` against
Hackage, without Nix.

The flat binary codec, `Data` to CBOR and back, the CEK machine, cost
metering, the builtins and version gating follow in later releases, roughly in
that order, since each one needs the ones before it.

## Correctness

Semantics follow the Plutus specification rather than any single
implementation of it, and correctness is to be measured against the official
Plutus Conformance Test Suite.
