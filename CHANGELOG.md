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
- `Params` and `evaluateWith`, to run the builtins under any of the five
  semantics variants and with signature verifiers; `evaluate` runs the latest
  variant with no verifiers
- The integer builtins, from `addInteger` to `expModInteger`, and
  `ifThenElse`
- The bytestring builtins, from `appendByteString` to `byteStringToInteger`,
  and the bitwise ones, from `andByteString` to `findFirstSetBit`
- The hash builtins: `sha2_256`, `sha3_256`, `blake2b_256`, `keccak_256`,
  `blake2b_224` and `ripemd_160`, on `crypton`
- The signature builtins check their argument lengths and, in the newer
  variants, the message bound; the verdict comes from a verifier the caller
  passes in `SignatureChecks`
- Plutus conformance cases for the builtins the machine runs, in
  `test/conformance`, with a runner and `scripts/conformance/refresh.py`; they
  are under the Apache License 2.0, with their own `LICENSE` and `NOTICE`

### Changed

- `evaluateWith` takes a `Params` record, where it took a `SemanticsVariant`,
  so that it can also carry the signature verifiers. Code that passed a
  variant `v` passes `defaultParams { semanticsVariant = v }`; `evaluate` is
  unchanged
