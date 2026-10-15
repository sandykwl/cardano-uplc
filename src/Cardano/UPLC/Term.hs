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

import Data.Vector (Vector)
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Cardano.UPLC.Builtin (DefaultFun)
import Cardano.UPLC.Constant (Constant)

{- | The ten term shapes of Untyped Plutus Core.

The binder is a parameter, so the compiler knows the phase: @Term Name@ is
parsed and cannot be evaluated, @Term DeBruijn@ can.

The annotation, the last parameter and the first field of every constructor,
is for whatever a tool wants to carry, such as a source position. It never
reaches the wire: decoding fills it with @()@. The layout is plutus-core's,
so code written against plutus-core's terms carries over.

plutus-core's @Term@ also takes the constant universe and the builtin type as
parameters. Here both are fixed, because the chain fixes them: adding a type
or a builtin is a hard fork.

Constructors are in plutus-core's order, which is not the flat tag order; the
codec writes each tag out by number. Fields are strict, because evaluation is
metered.

@since 0.1.0
-}
data Term name ann
  = -- | @since 0.1.0
    Var !ann !name
  | {- | Lambda abstraction. One argument only; several are curried.

    @since 0.1.0
    -}
    LamAbs !ann !name !(Term name ann)
  | -- | @since 0.1.0
    Apply !ann !(Term name ann) !(Term name ann)
  | {- | Resume a term that 'Delay' suspended.

    @since 0.1.0
    -}
    Force !ann !(Term name ann)
  | {- | Suspend a term, deferring work the script may not need.

    @since 0.1.0
    -}
    Delay !ann !(Term name ann)
  | -- | @since 0.1.0
    Constant !ann !Constant
  | -- | @since 0.1.0
    Builtin !ann !DefaultFun
  | {- | Abort. The whole script fails, which is how a validator rejects a
    transaction.

    @since 0.1.0
    -}
    Error !ann
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
    Case !ann !(Term name ann) !(Vector (Term name ann))
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
site. Nothing here checks the version; the decoder and the machine do. The
numbers are naturals, as in plutus-core, so a program the ledger would refuse
for its version still decodes.

@since 0.1.0
-}
data Version = Version
  { versionMajor :: !Natural
  -- ^ @since 0.1.0
  , versionMinor :: !Natural
  -- ^ @since 0.1.0
  , versionPatch :: !Natural
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
