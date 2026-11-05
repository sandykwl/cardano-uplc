{- | Why a saturated builtin gave no value. Apart from the builtin modules it
is reached through "Cardano.UPLC.Machine.Builtins".

@since 0.1.0
-}
module Cardano.UPLC.Machine.Builtins.Error (
  BuiltinError (..),
) where

{- | Why a saturated builtin gave no value.

@since 0.1.0
-}
data BuiltinError
  = -- | An argument is not a constant, or not one of the type expected.
    --
    -- @since 0.1.0
    ArgumentType
  | -- | An argument is out of range, or the builtin failed.
    --
    -- @since 0.1.0
    Failed
  | -- | The builtin is not run by this release.
    --
    -- @since 0.1.0
    Unsupported
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )
