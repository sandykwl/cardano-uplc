{- | Binder representations for Untyped Plutus Core.

Parsed terms carry t'Name's, terms ready to evaluate carry t'DeBruijn'
indices. Separate types leave the distinction to the compiler.

@since 0.1.0
-}
module Cardano.UPLC.Name (
  -- * Runnable binders
  DeBruijn (..),

  -- * Parsed binders
  Name (..),
) where

import Data.Text (Text)
import Data.Word (Word64)

{- | A de Bruijn index: the number of enclosing lambdas between a variable and
its binder, counting outward from 1.

Names are gone, so programs differing only in their binder names come out
identical and every node agrees on what a script means. 'Word64' rather than
'Int' keeps the range off the decoder's machine.

@since 0.1.0
-}
newtype DeBruijn = DeBruijn Word64
  deriving stock
    ( -- | @since 0.1.0
      Show
    )
  deriving newtype
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Ord
    )

{- | A binder name from the source text, carried only until the conversion to
t'DeBruijn' indices.

An inner @\\x@ can hide an outer @\\x@; 'nameUnique' is the serial number that
tells the two apart.

@since 0.1.0
-}
data Name = Name
  { nameText :: !Text
  -- ^ The identifier as written.
  --
  -- @since 0.1.0
  , nameUnique :: !Word64
  -- ^ Tells apart binders that share a 'nameText'.
  --
  -- @since 0.1.0
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Ord
    , -- | @since 0.1.0
      Show
    )
