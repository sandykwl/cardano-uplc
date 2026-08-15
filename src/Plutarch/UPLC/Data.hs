{- | The universal on-chain data format. Datums, redeemers and the script
context all reach a script as a 'Data' value.

@since 0.1.0
-}
module Plutarch.UPLC.Data (
  Data (..),
) where

import Data.ByteString (ByteString)

{- | A datum, a redeemer, or any other structured value carried between the
ledger and a script.

The declaration order encodes nothing: 'Data' crosses the wire as CBOR, which
has tags of its own, so the variants stand in plutus-core's order for the
reader arriving from there.

@since 0.1.0
-}
data Data
  = {- | A constructor application. The index is an 'Integer' because the CBOR
    fallback for tags outside 121-127 and 1280-1400 is not bounded by 2^64.
    -}
    Constr !Integer ![Data]
  | {- | An association list, not a map: The encoding preserves insertion order
    and permits duplicate keys, so normalising would change the bytes.
    -}
    Map ![(Data, Data)]
  | List ![Data]
  | I !Integer
  | B !ByteString
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )
