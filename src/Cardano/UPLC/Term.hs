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

The annotation, the last parameter and the first field of every constructor,
is for whatever a tool wants to carry, such as a source position. It never
reaches the wire: decoding fills it with @()@. The layout is plutus-core's,
so code written against plutus-core's terms carries over.

Constructors are in flat tag order, 0 through 9, so the declaration reads as
the format's table; the codec writes each tag out by number. Fields are
strict, because evaluation is metered.

@since 0.1.0
-}
data Term name ann
  = -- | @since 0.1.0
    Var !ann !name
  | {- | Suspend a term, deferring work the script may not need.

    @since 0.1.0
    -}
    Delay !ann !(Term name ann)
  | {- | Lambda abstraction. One argument only; several are curried.

    @since 0.1.0
    -}
    LamAbs !ann !name !(Term name ann)
  | -- | @since 0.1.0
    Apply !ann !(Term name ann) !(Term name ann)
  | -- | @since 0.1.0
    Constant !ann !Constant
  | {- | Resume a term that 'Delay' suspended.

    @since 0.1.0
    -}
    Force !ann !(Term name ann)
  | {- | Abort. The whole script fails, which is how a validator rejects a
    transaction.

    @since 0.1.0
    -}
    Error !ann
  | -- | @since 0.1.0
    Builtin !ann !DefaultFun
  | {- | A tagged constructor application, from Plutus V3. The tag is 'Word64'
    here but 'Integer' in "Cardano.UPLC.Data": this one a compiler writes, that one
    arrives off the chain.

    @since 0.1.0
    -}
    Constr !ann !Word64 ![Term name ann]
  | {- | Scrutinise a 'Constr', taking the branch its tag indexes. From
    Plutus V3.

    @since 0.1.0
    -}
    Case !ann !(Term name ann) ![Term name ann]
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

{- | A complete script: a version and the term it applies to, with an
annotation for the whole program.

@since 0.1.0
-}
data Program name ann = Program
  { programAnn :: !ann
  -- ^ @since 0.1.0
  , programVersion :: !Version
  -- ^ @since 0.1.0
  , programTerm :: !(Term name ann)
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
