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

@since 0.1.0
-}
data Ty
  = TyInteger
  | TyByteString
  | TyString
  | TyUnit
  | TyBool
  | TyList !Ty
  | TyArray !Ty
  | TyPair !Ty !Ty
  | TyData
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )
