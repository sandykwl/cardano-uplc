{- | The values the machine computes with, shared by the machine and the
builtins.

A closure keeps its environment, and 'discharge' turns a value back into a
term by putting what the environment bound back into the body. A partial
builtin keeps no environment, only the values it has taken.

@since 0.1.0
-}
module Cardano.UPLC.Machine.CekValue (
  CekValue (..),
  Env,
  lookupVar,
  discharge,
) where

import Data.Word (Word64)
import Cardano.UPLC.Builtin (DefaultFun)
import Cardano.UPLC.Constant (Constant)
import Cardano.UPLC.Name (DeBruijn (DeBruijn))
import Cardano.UPLC.Term (Term (..))

-- A lambda holds its body and the environment it was made in; the binder
-- itself holds nothing, since variables count their way to it.
--
-- A builtin holds how many forces it has taken and the arguments it has
-- taken, newest first. Every builtin takes all its forces before its first
-- argument, so the count and the list are the whole state.
data CekValue
  = VCon !Constant
  | VDelay !(Term DeBruijn ()) !Env
  | VLamAbs !(Term DeBruijn ()) !Env
  | VBuiltin !DefaultFun !Int ![CekValue]

-- The innermost binder's value first, so variable 1 is the head. plutus-core
-- uses a skew-binary random-access list, for faster lookup of deep variables.
-- That changes no result, so a plain list will do until a benchmark says the
-- lookups matter.
type Env = [CekValue]

-- Variables count from 1; index 0 never names anything.
lookupVar :: Word64 -> Env -> Maybe CekValue
lookupVar 0 _ = Nothing
lookupVar 1 (v : _) = Just v
lookupVar i (_ : rest) = lookupVar (i - 1) rest
lookupVar _ [] = Nothing

-- A partial builtin comes back as its forces, then its arguments, in the
-- order they arrived. The specification's figure drops the forces; the
-- conformance cases keep them.
discharge :: CekValue -> Term DeBruijn ()
discharge = \case
  VCon c -> Constant () c
  VDelay body env -> Delay () (substitute env 0 body)
  VLamAbs body env -> LamAbs () (DeBruijn 0) (substitute env 1 body)
  VBuiltin f forces args ->
    foldl (Apply ()) (iterate (Force ()) (Builtin () f) !! forces) (map discharge (reverse args))

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
