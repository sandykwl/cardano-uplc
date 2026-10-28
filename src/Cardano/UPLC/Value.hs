{- | The ledger's multi-asset value, as a constant.

A value maps a currency symbol and a token name to a quantity. This is
plutus-core's type, with the same invariants: no inner map is empty, no
quantity is zero, a key is at most 32 bytes and a quantity fits a signed
128-bit integer. The constructor is hidden and 'pack' removes zeros and empty
currencies, so every t'Value' satisfies them. The flat decoder still refuses a
zero quantity or an empty currency instead of dropping it, since that would
accept a second encoding of the same value.

plutus-core's @Value@ also caches the sizes of its inner maps and the number
of negative quantities. They let plutus-core cost the value builtins, and
answer some of them, without walking the map. All of them follow from the
map, so leaving them out changes nothing about what a t'Value' means.

@since 0.1.0
-}
module Cardano.UPLC.Value (
  K,
  k,
  unK,
  Quantity,
  quantity,
  unQuantity,
  NestedMap,
  Value,
  pack,
  unpack,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

{- | A currency symbol or token name: at most 32 bytes. Real currency symbols
are empty or 28 bytes, but the type allows any length up to 32, as
plutus-core's does.

@since 0.1.0
-}
newtype K = UnsafeK
  { unK :: ByteString
  -- ^ The bytes of a key.
  --
  -- @since 0.1.0
  }
  deriving newtype
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Ord
    , -- | @since 0.1.0
      Show
    )

{- | 'Nothing' for more than 32 bytes.

@since 0.1.0
-}
k :: ByteString -> Maybe K
k b
  | BS.length b <= 32 = Just (UnsafeK b)
  | otherwise = Nothing

{- | An amount of one token, within -2^127 to 2^127 - 1. It is an 'Integer'
under a bounds check, as in plutus-core, so exceeding the range is refused
rather than wrapped.

@since 0.1.0
-}
newtype Quantity = UnsafeQuantity
  { unQuantity :: Integer
  -- ^ The integer inside a quantity.
  --
  -- @since 0.1.0
  }
  deriving newtype
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Ord
    , -- | @since 0.1.0
      Show
    )

{- | 'Nothing' outside -2^127 to 2^127 - 1.

@since 0.1.0
-}
quantity :: Integer -> Maybe Quantity
quantity i
  | i >= -(2 ^ (127 :: Int)) && i < 2 ^ (127 :: Int) = Just (UnsafeQuantity i)
  | otherwise = Nothing

{- | Currency symbol to token name to quantity.

@since 0.1.0
-}
type NestedMap = Map K (Map K Quantity)

{- | A multi-asset value.

@since 0.1.0
-}
newtype Value = Value NestedMap
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )

{- | Build a value, dropping zero quantities and then any currency left with
no tokens.

@since 0.1.0
-}
pack :: NestedMap -> Value
pack = Value . Map.filter (not . Map.null) . Map.map (Map.filter ((/= 0) . unQuantity))

{- | The map inside a value. It has no empty inner map and no zero quantity.

@since 0.1.0
-}
unpack :: Value -> NestedMap
unpack (Value m) = m
