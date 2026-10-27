{- Tests for the CEK machine.

Each test is a small term whose result follows from the specification's rules,
and plutus-core's where they differ: one or more per rule, one per error.
-}
module Machine (machineTests) where

import Data.ByteString qualified as BS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Cardano.UPLC.Builtin (DefaultFun (..))
import Cardano.UPLC.Constant (Constant (..))
import Cardano.UPLC.Machine (EvalError (..), SemanticsVariant (..), evaluate, evaluateWith)
import Cardano.UPLC.Name (DeBruijn (DeBruijn))
import Cardano.UPLC.Term (Term (..))
import Cardano.UPLC.Ty (Ty (TyInteger))

machineTests :: TestTree
machineTests = testGroup "Machine" [rules, discharging, failures, builtinRules, builtinErrors, ifThenElse, variants]

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
    , testCase "constr is not run yet" $
        evaluate (Constr () 0 []) @?= Left (NotSupported (Constr () 0 []))
    , testCase "case is not run yet" $
        evaluate (Case () (int 0) mempty) @?= Left (NotSupported (Case () (int 0) mempty))
    ]

builtin :: DefaultFun -> T
builtin = Builtin ()

-- A builtin applied to arguments, as a term.
call :: DefaultFun -> [T] -> T
call f = foldl app (builtin f)

force :: T -> T
force = Force ()

-- ifThenElse with its one force, applied to arguments.
ite :: [T] -> T
ite = foldl app (force (builtin IfThenElse))

bool :: Bool -> T
bool = Constant () . CBool

unit :: T
unit = Constant () CUnit

bytes :: Int -> T
bytes n = Constant () (CByteString (BS.replicate n 7))

consByte :: Integer -> T
consByte n = call ConsByteString [int n, bytes 0]

-- writeBits on a bytestring of the given length, with no indices to set.
writeBits :: Int -> T
writeBits n = call WriteBits [bytes n, Constant () (CList TyInteger []), bool True]

shift :: Integer -> T
shift k = call ShiftByteString [bytes 1, int k]

slice :: Integer -> T
slice start = call SliceByteString [int start, int 1, bytes 1]

-- The smallest positive integer the bounded variants refuse.
bound :: Integer
bound = 2 ^ (262143 :: Int)

builtinRules :: TestTree
builtinRules =
  testGroup
    "Builtin rules"
    [ testCase "a builtin is a value" $
        evaluate (builtin AddInteger) @?= Right (builtin AddInteger)
    , testCase "an argument is taken and the builtin stays a value" $
        evaluate (call AddInteger [int 1]) @?= Right (call AddInteger [int 1])
    , testCase "the last argument runs the builtin" $
        evaluate (call AddInteger [int 1, int 2]) @?= Right (int 3)
    , testCase "arguments are computed before the builtin runs" $
        evaluate (call AddInteger [app (lam (var 1)) (int 1), int 2]) @?= Right (int 3)
    , testCase "a result is returned to the frames waiting for it" $
        evaluate (app (lam (var 1)) (call AddInteger [int 1, int 2])) @?= Right (int 3)
    , testCase "a force is taken and the builtin stays a value" $
        evaluate (force (builtin IfThenElse)) @?= Right (force (builtin IfThenElse))
    , testCase "arguments after the force run the builtin" $
        evaluate (ite [bool True, int 1, int 2]) @?= Right (int 1)
    , testCase "a partial application is a value even with an ill-typed argument" $
        evaluate (call AddInteger [unit]) @?= Right (call AddInteger [unit])
    , testCase "a partial builtin is discharged with its forces, then its arguments" $
        evaluate (ite [bool True]) @?= Right (ite [bool True])
    , testCase "a partial builtin without a force is discharged with its arguments" $
        evaluate (app (lam (call AddInteger [var 1])) (int 5)) @?= Right (call AddInteger [int 5])
    , testCase "a partial builtin in a closure has its arguments discharged" $
        evaluate (app (lam (lam (ite [bool True, var 2]))) (int 5)) @?= Right (lam (ite [bool True, int 5]))
    , testCase "a builtin this release does not run is refused when saturated" $
        evaluate (call Sha2_256 [bytes 1]) @?= Left (NotSupported (call Sha2_256 [bytes 1]))
    , testCase "an unsupported builtin is a value until saturated" $
        evaluate (call Sha2_256 []) @?= Right (builtin Sha2_256)
    ]

builtinErrors :: TestTree
builtinErrors =
  testGroup
    "Builtin errors"
    [ testCase "forcing a builtin that expects an argument" $
        evaluate (force (builtin AddInteger)) @?= Left (BuiltinNeedsArgument (force (builtin AddInteger)))
    , testCase "forcing past its forces, the cause holds the forces taken" $
        evaluate (force (force (builtin IfThenElse))) @?= Left (BuiltinNeedsArgument (force (force (builtin IfThenElse))))
    , testCase "forcing after an argument" $
        evaluate (force (call AddInteger [int 1])) @?= Left (BuiltinNeedsArgument (force (call AddInteger [int 1])))
    , testCase "applying a builtin that expects a force" $
        evaluate (call IfThenElse [bool True]) @?= Left (BuiltinNeedsForce (call IfThenElse [bool True]))
    , testCase "the cause of the apply is discharged" $
        evaluate (app (lam (call IfThenElse [var 1])) (int 5)) @?= Left (BuiltinNeedsForce (call IfThenElse [int 5]))
    , testCase "an argument that is not a constant" $
        evaluate (call AddInteger [lam (var 1), int 1]) @?= Left (BuiltinArgumentType (call AddInteger [lam (var 1), int 1]))
    , testCase "a constant of the wrong type" $
        evaluate (call AddInteger [int 1, unit]) @?= Left (BuiltinArgumentType (call AddInteger [int 1, unit]))
    , testCase "the cause of an argument error is discharged" $
        evaluate (app (lam (call AddInteger [var 1, unit])) (int 5)) @?= Left (BuiltinArgumentType (call AddInteger [int 5, unit]))
    , testCase "the cause keeps the forces" $
        evaluate (ite [int 1, int 2, int 3]) @?= Left (BuiltinArgumentType (ite [int 1, int 2, int 3]))
    , testCase "the first argument that fails decides: out of range before wrong type" $
        evaluateWith VariantD (call AddInteger [int bound, unit])
          @?= Left (BuiltinFailure (call AddInteger [int bound, unit]))
    , testCase "the first argument that fails decides: wrong type before out of range" $
        evaluateWith VariantD (call AddInteger [unit, int bound])
          @?= Left (BuiltinArgumentType (call AddInteger [unit, int bound]))
    ]

ifThenElse :: TestTree
ifThenElse =
  testGroup
    "ifThenElse"
    [ testCase "true takes the first branch" $
        evaluate (ite [bool True, int 1, int 2]) @?= Right (int 1)
    , testCase "false takes the second" $
        evaluate (ite [bool False, int 1, int 2]) @?= Right (int 2)
    , testCase "a lambda passes through with its environment" $
        evaluate (app (lam (ite [bool True, lam (var 2), int 0])) (int 5)) @?= Right (lam (int 5))
    , testCase "a partial builtin passes through" $
        evaluate (ite [bool False, int 0, call AddInteger [int 1]]) @?= Right (call AddInteger [int 1])
    , testCase "the branch not taken is still computed" $
        evaluate (ite [bool True, int 1, Error ()]) @?= Left ErrorTerm
    , testCase "the condition must be a bool" $
        evaluate (ite [int 1, int 2, int 3]) @?= Left (BuiltinArgumentType (ite [int 1, int 2, int 3]))
    , testCase "a lambda is not a bool" $
        evaluate (ite [lam (var 1), int 2, int 3]) @?= Left (BuiltinArgumentType (ite [lam (var 1), int 2, int 3]))
    ]

variants :: TestTree
variants =
  testGroup
    "Variants"
    [ testCase "evaluate runs variant E" $
        evaluate (call AddInteger [int bound, int 0]) @?= evaluateWith VariantE (call AddInteger [int bound, int 0])
    , testCase "A, B and C accept an integer at the bound" $
        mapM_
          (\v -> evaluateWith v (call AddInteger [int bound, int 0]) @?= Right (int bound))
          [VariantA, VariantB, VariantC]
    , testCase "D and E refuse it" $
        mapM_
          (\v -> evaluateWith v (call AddInteger [int bound, int 0]) @?= Left (BuiltinFailure (call AddInteger [int bound, int 0])))
          [VariantD, VariantE]
    , testCase "the largest integer D and E accept" $
        evaluateWith VariantE (call AddInteger [int (bound - 1), int 0]) @?= Right (int (bound - 1))
    , testCase "the smallest integer D and E accept" $
        evaluateWith VariantE (call AddInteger [int (negate bound), int 0]) @?= Right (int (negate bound))
    , testCase "one below it is refused" $
        evaluateWith VariantD (call AddInteger [int (negate bound - 1), int 0])
          @?= Left (BuiltinFailure (call AddInteger [int (negate bound - 1), int 0]))
    , testCase "a sum past the bound is a result, and only the next builtin refuses it" $
        evaluateWith VariantE (call AddInteger [call AddInteger [int (bound - 1), int 1], int 0])
          @?= Left (BuiltinFailure (call AddInteger [int bound, int 0]))
    , testCase "equalsInteger takes any integer in every variant" $
        mapM_
          (\v -> evaluateWith v (call EqualsInteger [int bound, int bound]) @?= Right (bool True))
          [minBound .. maxBound]
    , testCase "a bytestring of 65536 bytes is accepted in D and E, one more is not" $
        mapM_
          (\v -> do
              evaluateWith v (call AppendByteString [bytes 65536, bytes 0]) @?= Right (bytes 65536)
              evaluateWith v (call AppendByteString [bytes 65537, bytes 0])
                @?= Left (BuiltinFailure (call AppendByteString [bytes 65537, bytes 0])))
          [VariantD, VariantE]
    , testCase "A, B and C accept it" $
        mapM_
          (\v -> evaluateWith v (call AppendByteString [bytes 65537, bytes 0]) @?= Right (bytes 65537))
          [VariantA, VariantB, VariantC]
    , testCase "consByteString refuses a first argument past 255 in C and E, and wraps it in A, B and D" $ do
        mapM_ (\v -> evaluateWith v (consByte 256) @?= Left (BuiltinFailure (consByte 256))) [VariantC, VariantE]
        mapM_ (\v -> evaluateWith v (consByte 256) @?= Right (Constant () (CByteString (BS.pack [0])))) [VariantA, VariantB, VariantD]
    , testCase "writeBits refuses more than 4096 bytes in D and E" $ do
        mapM_ (\v -> evaluateWith v (writeBits 4096) @?= Right (bytes 4096)) [minBound .. maxBound]
        mapM_ (\v -> evaluateWith v (writeBits 4097) @?= Left (BuiltinFailure (writeBits 4097))) [VariantD, VariantE]
        mapM_ (\v -> evaluateWith v (writeBits 4097) @?= Right (bytes 4097)) [VariantA, VariantB, VariantC]
    , testCase "a shift amount is an Int64 in D and E, and any integer before" $ do
        let top = 2 ^ (63 :: Int) :: Integer
        evaluateWith VariantE (shift (top - 1)) @?= Right (Constant () (CByteString (BS.singleton 0)))
        evaluateWith VariantE (shift top) @?= Left (BuiltinFailure (shift top))
        evaluateWith VariantD (shift (negate top)) @?= Right (Constant () (CByteString (BS.singleton 0)))
        evaluateWith VariantD (shift (negate top - 1)) @?= Left (BuiltinFailure (shift (negate top - 1)))
        evaluateWith VariantC (shift top) @?= Right (Constant () (CByteString (BS.singleton 0)))
    , testCase "an index past Int64 fails in every variant, the last Int64 does not" $ do
        let top = 2 ^ (63 :: Int) :: Integer
        mapM_ (\v -> evaluateWith v (slice (top - 1)) @?= Right (Constant () (CByteString BS.empty))) [minBound .. maxBound]
        mapM_ (\v -> evaluateWith v (slice top) @?= Left (BuiltinFailure (slice top))) [minBound .. maxBound]
    , testCase "lengthOfByteString has no bound" $
        mapM_
          (\v -> evaluateWith v (call LengthOfByteString [bytes 65537]) @?= Right (int 65537))
          [minBound .. maxBound]
    ]
