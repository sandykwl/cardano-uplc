{- | The universal on-chain data format. Datums, redeemers and the script
context all reach a script as a 'Data' value.

@since 0.1.0
-}
module Cardano.UPLC.Data (
  Data (..),
) where

import Data.ByteString (ByteString)

{- | A datum, a redeemer, or any other structured value passing between the
ledger and a script.

Order means nothing here: 'Data' goes over the wire as CBOR, with tags of its
own. The variants follow plutus-core's.

@since 0.1.0
-}
data Data
  = {- | A constructor application. The index is an 'Integer' because the CBOR
    fallback for tags outside 121-127 and 1280-1400 has no 2^64 bound.
    -}
    Constr !Integer ![Data]
  | {- | An association list, not a map: the encoding keeps insertion order and
    allows duplicate keys, so normalising would change the bytes.
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
