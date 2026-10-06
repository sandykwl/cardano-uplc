{- | 'Data' as CBOR, the form it takes everywhere on chain: in a datum, a
redeemer, and a @data@ constant inside a script.

The bytes are plutus-core's, exactly, because a datum's hash is taken over
them. Some of the rules are plutus-core's own, not CBOR's: byte strings are
cut into chunks of at most 64 bytes, and a reader refuses a longer one;
constructors take tags 121 to 127 and 1280 to 1400, then 102 with the index;
a non-empty list is written with an indefinite length, a map never is.

Reading accepts what plutus-core accepts, which is more than it writes: a
head written longer than it needs, a definite list, an indefinite map. So
decoding then encoding does not always give back the same bytes.

@since 0.1.0
-}
module Cardano.UPLC.Data.Cbor (
  encodeData,
  decodeData,
  DataDecodeError (..),
) where

import Cardano.UPLC.Data.Cbor.Internal (DataDecodeError (..), decodeData, encodeData)
