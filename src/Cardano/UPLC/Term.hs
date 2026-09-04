{- | Untyped Plutus Core terms, and the programs built from them.

@since 0.1.0
-}
module Cardano.UPLC.Term (
  -- * Terms
  Term (..),

  -- * Programs
  Program (..),
  Version (..),
) where

import Data.Word (Word64)
import Cardano.UPLC.Builtin (DefaultFun)
import Cardano.UPLC.Constant (Constant)

{- | The ten term shapes of Untyped Plutus Core.

The binder is a parameter, so the compiler knows the phase: @Term Name@ is
parsed and cannot be evaluated, @Term DeBruijn@ can.

Constructors are in flat tag order, 0 through 9. Position is the tag, so
reordering them changes what a program decodes to. Fields are strict, because
evaluation is metered.

@since 0.1.0
-}
data Term name
  = -- | @since 0.1.0
    Var !name
  | {- | Suspend a term, deferring work the script may not need.

    @since 0.1.0
    -}
    Delay !(Term name)
  | {- | Lambda abstraction. One argument only; several are curried.

    @since 0.1.0
    -}
    LamAbs !name !(Term name)
  | -- | @since 0.1.0
    Apply !(Term name) !(Term name)
  | -- | @since 0.1.0
    Constant !Constant
  | {- | Resume a term that 'Delay' suspended.

    @since 0.1.0
    -}
    Force !(Term name)
  | {- | Abort. The whole script fails, which is how a validator rejects a
    transaction.

    @since 0.1.0
    -}
    Error
  | -- | @since 0.1.0
    Builtin !DefaultFun
  | {- | A tagged constructor application, from Plutus V3. The tag is 'Word64'
    here but 'Integer' in "Cardano.UPLC.Data": this one a compiler writes, that one
    arrives off the chain.

    @since 0.1.0
    -}
    Constr !Word64 ![Term name]
  | {- | Scrutinise a 'Constr', taking the branch its tag indexes. From
    Plutus V3.

    @since 0.1.0
    -}
    Case !(Term name) ![Term name]
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Functor
    , -- | @since 0.1.0
      Foldable
    , -- | @since 0.1.0
      Traversable
    )

{- | The language version a program was compiled for, which gates the term
shapes and builtins legal within it.

Named fields, because @Version 1 1 0@ and @Version 1 0 1@ read alike at a call
site. Nothing here checks the version; the decoder and the machine do.

@since 0.1.0
-}
data Version = Version
  { versionMajor :: !Word64
  -- ^ @since 0.1.0
  , versionMinor :: !Word64
  -- ^ @since 0.1.0
  , versionPatch :: !Word64
  -- ^ @since 0.1.0
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Ord
    , -- | @since 0.1.0
      Show
    )

{- | A complete script: a version and the term it applies to.

@since 0.1.0
-}
data Program name = Program
  { programVersion :: !Version
  -- ^ @since 0.1.0
  , programTerm :: !(Term name)
  -- ^ @since 0.1.0
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Functor
    , -- | @since 0.1.0
      Foldable
    , -- | @since 0.1.0
      Traversable
    )
