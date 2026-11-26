{- | Evaluating a term with the CEK machine.

The machine is the one in the Plutus Core specification: a term under
evaluation (C), the environment that gives its variables their values (E), and
a stack of frames saying what to do with the result (K). It walks down a term
in 'compute', pushing frames, and hands each value it reaches to the frame on
top in 'returnValue'. One case per rule, so the code reads beside the
specification's.

This release runs @var@, @lam@, @apply@, @delay@, @force@, constants, @error@
and @builtin@, for the integer, bytestring, bitwise and hash builtins and
@ifThenElse@, and for the signature builtins when t'Params' carries their
verifiers. @constr@ and @case@, and the builtins of later releases, are
refused with 'NotSupported'. There is no execution budget: a term that never
finishes keeps the machine running.

@since 0.1.0
-}
module Cardano.UPLC.Machine (
  evaluate,
  evaluateWith,
  Params (..),
  SignatureChecks (..),
  defaultParams,
  SemanticsVariant (..),
  EvalError (..),
) where

import Data.Word (Word64)
import Cardano.UPLC.Builtin (DefaultFun)
import Cardano.UPLC.Machine.Builtins (
  BuiltinError (..),
  Params (..),
  SemanticsVariant (..),
  SignatureChecks (..),
  arity,
  defaultParams,
  forceCount,
  runBuiltin,
 )
import Cardano.UPLC.Machine.CekValue (CekValue (..), Env, discharge, lookupVar)
import Cardano.UPLC.Name (DeBruijn (DeBruijn))
import Cardano.UPLC.Term (Term (..))

{- | Why evaluation stopped without a value. On chain all but 'NotSupported'
are the same failure, and the names are for the person reading the error.
'NotSupported' is different: it marks what this release does not run yet.

Apart from 'NotSupported', where an error carries a term it is the term
plutus-core reports as the cause.

@since 0.1.0
-}
data EvalError
  = -- | The term reached @error@.
    --
    -- @since 0.1.0
    ErrorTerm
  | -- | A variable with nothing bound to it: index 0, or an index past the
    -- enclosing lambdas.
    --
    -- @since 0.1.0
    FreeVariable !Word64
  | -- | Something that is not a lambda was applied to an argument.
    --
    -- @since 0.1.0
    NotAFunction !(Term DeBruijn ())
  | -- | Something that is not a delay was forced.
    --
    -- @since 0.1.0
    NotDelayed !(Term DeBruijn ())
  | -- | A builtin that still expects an argument was forced. The term is the
    -- builtin as it was, with the @force@ around it.
    --
    -- @since 0.1.0
    BuiltinNeedsArgument !(Term DeBruijn ())
  | -- | A builtin that still expects a force was applied to an argument. The
    -- term is the builtin as it was, applied to the argument.
    --
    -- @since 0.1.0
    BuiltinNeedsForce !(Term DeBruijn ())
  | -- | The last argument of a builtin arrived and one of the arguments was
    -- not a constant, or not a constant of the type the builtin takes. The
    -- term is the whole application.
    --
    -- @since 0.1.0
    BuiltinArgumentType !(Term DeBruijn ())
  | -- | The last argument of a builtin arrived and the builtin failed: an
    -- argument was out of range for it, or it refused its arguments. The term
    -- is the whole application.
    --
    -- @since 0.1.0
    BuiltinFailure !(Term DeBruijn ())
  | -- | @constr@, @case@, or a builtin this release does not run yet.
    -- Evaluation stops when the machine reaches one; what runs before that
    -- has run. For a builtin the term is the saturated application.
    --
    -- @since 0.1.0
    NotSupported !(Term DeBruijn ())
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )

-- The stack is a list of frames. plutus-core links each frame to the rest of
-- the stack instead; the frames are the same.
data Frame
  = -- (force _): force the value about to arrive.
    FrameForce
  | -- [_ (M, ρ)]: the function is being computed; the argument M waits.
    FrameAwaitFunTerm !Env !(Term DeBruijn ())
  | -- [V _]: the function is V; the argument is being computed.
    FrameAwaitArg !CekValue

{- | Evaluate a term under 'defaultParams': the semantics of 'VariantE', the
one the conformance suite runs, and no signature verifiers. Annotations are
dropped, so the result carries @()@. The result is the value the term reduces
to, turned back into a term, with the variables its closures captured
replaced by what they were bound to.

@since 0.1.0
-}
evaluate :: Term DeBruijn ann -> Either EvalError (Term DeBruijn ())
evaluate = evaluateWith defaultParams

{- | Evaluate a term under the given parameters. Only builtins read them.

@since 0.1.0
-}
evaluateWith :: Params -> Term DeBruijn ann -> Either EvalError (Term DeBruijn ())
evaluateWith params = compute params [] [] . fmap (const ())

compute ::
  Params -> [Frame] -> Env -> Term DeBruijn () -> Either EvalError (Term DeBruijn ())
compute params stack env term = case term of
  Var _ (DeBruijn i) -> maybe (Left (FreeVariable i)) (returnValue params stack) (lookupVar i env)
  Constant _ c -> returnValue params stack (VCon c)
  LamAbs _ _ body -> returnValue params stack (VLamAbs body env)
  Delay _ body -> returnValue params stack (VDelay body env)
  Force _ body -> compute params (FrameForce : stack) env body
  Apply _ f x -> compute params (FrameAwaitFunTerm env x : stack) env f
  Builtin _ f -> returnValue params stack (VBuiltin f 0 [])
  Error _ -> Left ErrorTerm
  t -> Left (NotSupported t)

returnValue ::
  Params -> [Frame] -> CekValue -> Either EvalError (Term DeBruijn ())
returnValue params stack v = case stack of
  [] -> Right (discharge v)
  FrameForce : rest -> case v of
    VDelay body env -> compute params rest env body
    VBuiltin f forces args
      | forces < forceCount f -> builtinStep params rest f (forces + 1) args
      | otherwise -> Left (BuiltinNeedsArgument (Force () (discharge v)))
    _ -> Left (NotDelayed (discharge v))
  FrameAwaitFunTerm env x : rest -> compute params (FrameAwaitArg v : rest) env x
  FrameAwaitArg f : rest -> case f of
    VLamAbs body env -> compute params rest (v : env) body
    VBuiltin g forces args
      | forces == forceCount g -> builtinStep params rest g forces (v : args)
      | otherwise -> Left (BuiltinNeedsForce (Apply () (discharge f) (discharge v)))
    _ -> Left (NotAFunction (discharge f))

-- A builtin has taken one more force or argument. If it now has them all it
-- runs, and otherwise it is a value.
builtinStep ::
  Params ->
  [Frame] ->
  DefaultFun ->
  Int ->
  [CekValue] ->
  Either EvalError (Term DeBruijn ())
builtinStep params stack f forces args
  | forces == forceCount f && length args == arity f =
      case runBuiltin params f (reverse args) of
        -- Forced here, so a builtin does its work when it runs, not when
        -- something reads the result.
        Right !result -> returnValue params stack result
        Left ArgumentType -> Left (BuiltinArgumentType application)
        Left Failed -> Left (BuiltinFailure application)
        Left Unsupported -> Left (NotSupported application)
  | otherwise = returnValue params stack partial
  where
    partial = VBuiltin f forces args
    application = discharge partial
