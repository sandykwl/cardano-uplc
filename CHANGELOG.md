# Changelog for `cardano-uplc`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## Unreleased

### Added

- `Cardano.UPLC.Ty`, the type a constant carries
- `Cardano.UPLC.Data`, the datums, redeemers and script context passed
  between the ledger and a script
- `Cardano.UPLC.Constant`, literal values
- `Cardano.UPLC.Name`, the two binder representations
- `Cardano.UPLC.Builtin`, the builtin functions in flat tag order
- `Cardano.UPLC.Term`, terms, versions and programs, each carrying an
  annotation for tools; the codec writes none and decodes `()`. `case`
  branches and array constants are vectors, as in plutus-core, so the package
  depends on `vector`
- `Cardano.UPLC.Data.Cbor`, `Data` as CBOR, byte for byte as plutus-core
  writes and reads it
- `Cardano.UPLC.Value`, the ledger's multi-asset value as plutus-core defines
  it, with a constant (`CValue`), a type (`TyValue`, flat tag 13) and flat
  codec support that refuses what plutus-core refuses: a key over 32 bytes, a
  quantity of zero or outside 128 bits, an empty token list, and keys out of
  order; the package now depends on `containers`
- `Cardano.UPLC.Machine`, the CEK machine for every term but `constr` and
  `case`
- `Cardano.UPLC.Flat`, the flat codec, including `data` constants; the
  encoder cannot fail
- `SemanticsVariant` and `evaluateWith`, to run the builtins under any of the
  five semantics variants; `evaluate` runs the latest
- The integer builtins, from `addInteger` to `expModInteger`, and
  `ifThenElse`
