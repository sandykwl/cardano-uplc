{- | The types a constant can have.

UPLC erases types everywhere else. Constants keep theirs because the flat
encoding writes the type before the value, and the value cannot be read
without it.

@since 0.1.0
-}
module Cardano.UPLC.Ty (
  Ty (..),
) where

{- | In flat tag order, but with no 'Enum' instance: a list, array or pair
encodes as a sequence of tags, so position is not the tag here.

The three BLS12-381 types are here although "Cardano.UPLC.Constant" has no
value of them. A type can occur without a value, as in an empty list of G1
elements, and that program is legal. The ledger's @value@ type, tag 13, is
not here yet.

@since 0.1.0
-}
data Ty
  = TyInteger
  | TyByteString
  | TyString
  | TyUnit
  | TyBool
  | TyList !Ty
  | TyPair !Ty !Ty
  | TyData
  | TyBLS12_381_G1_Element
  | TyBLS12_381_G2_Element
  | TyBLS12_381_MlResult
  | TyArray !Ty
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )
