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
builtins, `constr` and `case`, which it refuses with `NotSupported`, and it has
no execution budget. The builtins, `constr` and `case`, cost metering and
version gating follow in later releases.

## Correctness

Where `plutus-core` and the Plutus Core specification differ, behaviour
matches `plutus-core`, the implementation the network runs. Correctness is to
be measured against the official Plutus Conformance Test Suite.
