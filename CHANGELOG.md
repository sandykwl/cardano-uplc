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
  annotation for tools; the codec writes none and decodes `()`
- `Cardano.UPLC.Data.Cbor`, `Data` as CBOR, byte for byte as plutus-core
  writes and reads it
- `Cardano.UPLC.Flat`, the flat codec, including `data` constants; the
  encoder cannot fail
