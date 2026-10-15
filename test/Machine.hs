{- Tests for the CEK machine.

Each test is a small term whose result follows from the specification's rules
by hand: one or more per rule, one per error.
-}
module Machine (machineTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Cardano.UPLC.Builtin (DefaultFun (AddInteger))
import Cardano.UPLC.Constant (Constant (..))
import Cardano.UPLC.Machine (EvalError (..), evaluate)
import Cardano.UPLC.Name (DeBruijn (DeBruijn))
import Cardano.UPLC.Term (Term (..))

machineTests :: TestTree
machineTests = testGroup "Machine" [rules, discharging, failures]

type T = Term DeBruijn ()

var :: Word -> T
var = Var () . DeBruijn . fromIntegral

lam :: T -> T
lam = LamAbs () (DeBruijn 0)

app :: T -> T -> T
app = Apply ()

int :: Integer -> T
int = Constant () . CInteger

rules :: TestTree
rules =
  testGroup
    "Rules"
    [ testCase "a constant is its own value" $
        evaluate (int 7) @?= Right (int 7)
    , testCase "a lambda applied to an argument computes its body with the argument bound" $
        evaluate (app (lam (var 1)) (int 7)) @?= Right (int 7)
    , testCase "the function is computed before the argument" $
        -- If the argument came first, its error would be the one reported.
        evaluate (app (Error ()) (Force () (int 1))) @?= Left ErrorTerm
    , testCase "variable 1 is the nearest binder, 2 the next one out" $
        evaluate (app (app (lam (lam (var 2))) (int 1)) (int 2)) @?= Right (int 1)
    , testCase "force runs a delayed term" $
        evaluate (Force () (Delay () (int 7))) @?= Right (int 7)
    , testCase "a delay runs in the environment it was made in, not where it is forced" $
        -- The delay captures 5; it is forced inside another lambda, whose own
        -- environment holds only the delay.
        evaluate (app (lam (Force () (var 1))) (app (lam (Delay () (var 1))) (int 5))) @?= Right (int 5)
    , testCase "a delay is a value: its body is not run" $
        evaluate (Delay () (Error ())) @?= Right (Delay () (Error ()))
    , testCase "a lambda is a value: its body is not run" $
        evaluate (lam (Error ())) @?= Right (lam (Error ()))
    , testCase "an argument is computed even when the body ignores it" $
        evaluate (app (lam (int 1)) (Error ())) @?= Left ErrorTerm
    ]

discharging :: TestTree
discharging =
  testGroup
    "Discharge"
    [ testCase "a returned lambda carries what its variables were bound to" $
        evaluate (app (lam (lam (var 2))) (int 5)) @?= Right (lam (int 5))
    , testCase "variables bound inside the returned term stay" $
        evaluate (app (lam (lam (app (var 1) (var 2)))) (int 5)) @?= Right (lam (app (var 1) (int 5)))
    , testCase "a returned delay carries its environment too" $
        evaluate (app (lam (Delay () (var 1))) (int 5)) @?= Right (Delay () (int 5))
    , testCase "a captured value that is itself a closure is discharged in turn" $
        -- (lam x (lam y x)) applied to a closure that captured 5: the result's
        -- x is that closure, with its own 5 put back in.
        evaluate (app (lam (lam (var 2))) (app (lam (lam (var 2))) (int 5))) @?= Right (lam (lam (int 5)))
    , testCase "delay and force inside a returned lambda do not change the depth" $
        evaluate (app (lam (lam (Delay () (var 2)))) (int 5)) @?= Right (lam (Delay () (int 5)))
    , testCase "terms the machine does not run yet are still discharged inside a closure" $
        evaluate (app (lam (lam (Constr () 0 [var 2]))) (int 5)) @?= Right (lam (Constr () 0 [int 5]))
    ]

failures :: TestTree
failures =
  testGroup
    "Failures"
    [ testCase "error fails" $
        evaluate (Error ()) @?= Left ErrorTerm
    , testCase "a variable with no binder" $
        evaluate (var 1) @?= Left (FreeVariable 1)
    , testCase "index 0 never names anything" $
        evaluate (app (lam (var 0)) (int 1)) @?= Left (FreeVariable 0)
    , testCase "an index past the enclosing lambdas" $
        evaluate (app (lam (var 2)) (int 1)) @?= Left (FreeVariable 2)
    , testCase "applying a constant" $
        evaluate (app (int 1) (int 2)) @?= Left (NotAFunction (int 1))
    , testCase "applying a delay, reported discharged" $
        evaluate (app (app (lam (Delay () (var 1))) (int 5)) (int 2)) @?= Left (NotAFunction (Delay () (int 5)))
    , testCase "forcing a constant" $
        evaluate (Force () (int 1)) @?= Left (NotDelayed (int 1))
    , testCase "forcing a lambda, reported discharged" $
        evaluate (app (lam (Force () (lam (var 2)))) (int 5)) @?= Left (NotDelayed (lam (int 5)))
    , testCase "a builtin is not run yet" $
        evaluate (Builtin () AddInteger) @?= Left (NotSupported (Builtin () AddInteger))
    , testCase "constr is not run yet" $
        evaluate (Constr () 0 []) @?= Left (NotSupported (Constr () 0 []))
    , testCase "case is not run yet" $
        evaluate (Case () (int 0) mempty) @?= Left (NotSupported (Case () (int 0) mempty))
    ]
