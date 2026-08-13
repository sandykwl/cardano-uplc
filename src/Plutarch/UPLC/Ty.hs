{- | The types a constant can have. Untyped Plutus Core erases types everywhere else: lambdas, variables and
applications carry none. Constants alone keep theirs, because the flat
encoding writes a value's type before the value, and the value's bytes
cannot be read without it.

@since 0.1.0
-}
module Plutarch.UPLC.Ty (
  Ty (..),
) where

{- | The type of a constant, which a decoder must know before it can read the
value itself. Declared in flat tag order, and with no 'Enum' instance: A list,
an array or a pair encodes as a sequence of tags, so a position here is not a
tag.

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
