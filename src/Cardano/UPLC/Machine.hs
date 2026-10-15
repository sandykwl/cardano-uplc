{- | Evaluating a term with the CEK machine.

The machine is the one in the Plutus Core specification: a term under
evaluation (C), the environment that gives its variables their values (E), and
a stack of frames saying what to do with the result (K). It walks down a term
in 'compute', pushing frames, and hands each value it reaches to the frame on
top in 'returnValue'. One case per rule, so the code reads beside the
specification's.

This release runs @var@, @lam@, @apply@, @delay@, @force@, constants and
@error@. Builtins, @constr@ and @case@ are refused with 'NotSupported' until
the releases that bring them. There is no execution budget: a term that never
finishes keeps the machine running.

@since 0.1.0
-}
module Cardano.UPLC.Machine (
  evaluate,
  EvalError (..),
) where

import Data.Word (Word64)
import Cardano.UPLC.Constant (Constant)
import Cardano.UPLC.Name (DeBruijn (DeBruijn))
import Cardano.UPLC.Term (Term (..))

{- | Why evaluation stopped without a value. On chain the first four are the
same failure, and the names are for the person reading the error.
'NotSupported' is different: it marks what this release does not run yet.

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
  | -- | A builtin, @constr@ or @case@, which this release does not run yet.
    -- It is reported before any part of the term runs.
    --
    -- @since 0.1.0
    NotSupported !(Term DeBruijn ())
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )

-- A lambda holds its body and the environment it was made in; the binder
-- itself holds nothing, since variables count their way to it.
data Value
  = VCon !Constant
  | VDelay !(Term DeBruijn ()) !Env
  | VLamAbs !(Term DeBruijn ()) !Env

-- The innermost binder's value first, so variable 1 is the head. plutus-core
-- uses a skew-binary random-access list, for faster lookup of deep variables.
-- That changes no result, so a plain list will do until a benchmark says the
-- lookups matter.
type Env = [Value]

-- The stack is a list of frames. plutus-core links each frame to the rest of
-- the stack instead, which is faster to match on but holds the same frames.
data Frame
  = -- (force _): force the value about to arrive.
    FrameForce
  | -- [_ (M, ρ)]: the function is being computed; the argument M waits.
    FrameAwaitFunTerm !Env !(Term DeBruijn ())
  | -- [V _]: the function is V; the argument is being computed.
    FrameAwaitArg !Value

{- | Evaluate a term. Annotations are dropped, so the result carries @()@. The
result is the value the term reduces to, turned back into a term, with the
variables its closures captured replaced by what they were bound to.

@since 0.1.0
-}
evaluate :: Term DeBruijn ann -> Either EvalError (Term DeBruijn ())
evaluate = compute [] [] . fmap (const ())

compute :: [Frame] -> Env -> Term DeBruijn () -> Either EvalError (Term DeBruijn ())
compute stack env term = case term of
  Var _ (DeBruijn i) -> maybe (Left (FreeVariable i)) (returnValue stack) (lookupVar i env)
  Constant _ c -> returnValue stack (VCon c)
  LamAbs _ _ body -> returnValue stack (VLamAbs body env)
  Delay _ body -> returnValue stack (VDelay body env)
  Force _ body -> compute (FrameForce : stack) env body
  Apply _ f x -> compute (FrameAwaitFunTerm env x : stack) env f
  Error _ -> Left ErrorTerm
  t -> Left (NotSupported t)

returnValue :: [Frame] -> Value -> Either EvalError (Term DeBruijn ())
returnValue stack v = case stack of
  [] -> Right (discharge v)
  FrameForce : rest -> case v of
    VDelay body env -> compute rest env body
    _ -> Left (NotDelayed (discharge v))
  FrameAwaitFunTerm env x : rest -> compute (FrameAwaitArg v : rest) env x
  FrameAwaitArg f : rest -> case f of
    VLamAbs body env -> compute rest (v : env) body
    _ -> Left (NotAFunction (discharge f))

-- Variables count from 1; index 0 never names anything.
lookupVar :: Word64 -> Env -> Maybe Value
lookupVar 0 _ = Nothing
lookupVar 1 (v : _) = Just v
lookupVar i (_ : rest) = lookupVar (i - 1) rest
lookupVar _ [] = Nothing

discharge :: Value -> Term DeBruijn ()
discharge = \case
  VCon c -> Constant () c
  VDelay body env -> Delay () (substitute env 0 body)
  VLamAbs body env -> LamAbs () (DeBruijn 0) (substitute env 1 body)

-- Replace each variable the environment binds with its value, discharged.
-- `depth` counts the lambdas between here and the environment: a variable at
-- or below it is bound inside the term and stays. A variable the environment
-- does not have stays as it is, which only an open term can produce.
substitute :: Env -> Word64 -> Term DeBruijn () -> Term DeBruijn ()
substitute env = go
  where
    go depth = \case
      v@(Var _ (DeBruijn i))
        | i <= depth -> v
        | otherwise -> maybe v discharge (lookupVar (i - depth) env)
      LamAbs a b body -> LamAbs a b (go (depth + 1) body)
      Apply a f x -> Apply a (go depth f) (go depth x)
      Delay a body -> Delay a (go depth body)
      Force a body -> Force a (go depth body)
      Constr a i fields -> Constr a i (map (go depth) fields)
      Case a s branches -> Case a (go depth s) (fmap (go depth) branches)
      t@Constant {} -> t
      t@Builtin {} -> t
      t@Error {} -> t
