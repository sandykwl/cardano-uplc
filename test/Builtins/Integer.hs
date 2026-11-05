{- Tests for the integer builtins, each run through the machine on a real term.

The expected values come from plutus-core 1.70.0.0 or its conformance cases,
some computed with the Prelude's operations. A failure is always the builtin's
own, so the cause the machine reports is the saturated call itself.
-}
module Builtins.Integer (integerTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (Gen, NonNegative (..), NonPositive (..), Positive (..), arbitrary, choose, forAll, frequency, suchThat, testProperty, (=/=), (===), (==>))
import Cardano.UPLC.Builtin (DefaultFun (..))
import Cardano.UPLC.Constant (Constant (..))
import Cardano.UPLC.Machine (EvalError (..), SemanticsVariant (..), evaluate, evaluateWith)
import Cardano.UPLC.Name (DeBruijn (DeBruijn))
import Cardano.UPLC.Term (Term (..))
import Cardano.UPLC.Value (pack)

integerTests :: TestTree
integerTests = testGroup "Integer builtins" [arithmetic, comparisons, division, divisionBounds, bounds, expMod, expModBounds, arithmeticLaws, divisionLaws, expModLaws]

type T = Term DeBruijn ()

int :: Integer -> T
int = Constant () . CInteger

bool :: Bool -> T
bool = Constant () . CBool

-- A builtin applied to arguments.
call :: DefaultFun -> [T] -> T
call f = foldl (Apply ()) (Builtin () f)

-- N = 2^262143, the smallest positive integer the bounded variants refuse.
bound :: Integer
bound = 2 ^ (262143 :: Int)

allVariants, oldVariants, newVariants :: [SemanticsVariant]
allVariants = [minBound .. maxBound]
oldVariants = [VariantA, VariantB, VariantC]
newVariants = [VariantD, VariantE]

-- The call gives this result in the given variants.
givesIn :: [SemanticsVariant] -> DefaultFun -> [Integer] -> T -> Assertion
givesIn vs f args r = mapM_ (\v -> evaluateWith v (call f (map int args)) @?= Right r) vs

-- The call gives this integer in every variant.
gives :: DefaultFun -> [Integer] -> Integer -> Assertion
gives f args r = givesIn allVariants f args (int r)

givesBool :: DefaultFun -> [Integer] -> Bool -> Assertion
givesBool f args r = givesIn allVariants f args (bool r)

-- The call fails in the given variants.
failsIn :: [SemanticsVariant] -> DefaultFun -> [Integer] -> Assertion
failsIn vs f args =
  mapM_ (\v -> evaluateWith v t @?= Left (BuiltinFailure t)) vs
  where
    t = call f (map int args)

-- The call fails in every variant.
fails :: DefaultFun -> [Integer] -> Assertion
fails = failsIn allVariants

arithmetic :: TestTree
arithmetic =
  testGroup
    "Arithmetic"
    [ testCase "addInteger" $ gives AddInteger [2, 3] 5
    , testCase "addInteger with negatives" $ gives AddInteger [-2, -3] (-5)
    , testCase "subtractInteger 1 2 is -1" $ gives SubtractInteger [1, 2] (-1)
    , testCase "subtractInteger of a negative" $ gives SubtractInteger [1, -2] 3
    , testCase "multiplyInteger of two negatives" $
        gives MultiplyInteger [-793479793478939, -166266268485555555] 131928924380432445633603606956145
    , testCase "multiplyInteger by zero" $ gives MultiplyInteger [-5, 0] 0
    ]

comparisons :: TestTree
comparisons =
  testGroup
    "Comparisons"
    [ testCase "lessThanInteger 4 4 is False" $ givesBool LessThanInteger [4, 4] False
    , testCase "lessThanInteger 3 4 is True" $ givesBool LessThanInteger [3, 4] True
    , testCase "lessThanInteger of negatives" $ givesBool LessThanInteger [-4, -3] True
    , testCase "lessThanEqualsInteger 4 4 is True" $ givesBool LessThanEqualsInteger [4, 4] True
    , testCase "lessThanEqualsInteger 5 4 is False" $ givesBool LessThanEqualsInteger [5, 4] False
    , testCase "equalsInteger on equal 38-digit integers" $
        givesBool EqualsInteger [12345678901234567890123456789012345678, 12345678901234567890123456789012345678] True
    , testCase "equalsInteger on different integers" $ givesBool EqualsInteger [1, -1] False
    ]

division :: TestTree
division =
  testGroup
    "Division"
    [ testCase "divideInteger rounds towards negative infinity" $ do
        gives DivideInteger [7, 2] 3
        gives DivideInteger [-7, 2] (-4)
        gives DivideInteger [7, -2] (-4)
        gives DivideInteger [-7, -2] 3
    , testCase "quotientInteger rounds towards zero" $ do
        gives QuotientInteger [7, 2] 3
        gives QuotientInteger [-7, 2] (-3)
        gives QuotientInteger [7, -2] (-3)
        gives QuotientInteger [-7, -2] 3
    , testCase "modInteger takes the sign of the divisor" $ do
        gives ModInteger [7, 2] 1
        gives ModInteger [-7, 2] 1
        gives ModInteger [7, -2] (-1)
        gives ModInteger [-7, -2] (-1)
    , testCase "remainderInteger takes the sign of the dividend" $ do
        gives RemainderInteger [7, 2] 1
        gives RemainderInteger [-7, 2] (-1)
        gives RemainderInteger [7, -2] 1
        gives RemainderInteger [-7, -2] (-1)
    , testCase "-1 by 2" $ do
        gives DivideInteger [-1, 2] (-1)
        gives ModInteger [-1, 2] 1
        gives QuotientInteger [-1, 2] 0
        gives RemainderInteger [-1, 2] (-1)
    , testCase "an exact division leaves nothing" $ do
        gives DivideInteger [-6, 3] (-2)
        gives ModInteger [-6, 3] 0
        gives QuotientInteger [-6, 3] (-2)
        gives RemainderInteger [-6, 3] 0
    , testCase "zero divided by anything is zero" $
        mapM_ (\f -> gives f [0, -5] 0) [DivideInteger, QuotientInteger, RemainderInteger, ModInteger]
    , testCase "divideInteger, conformance rows" $ do
        gives DivideInteger [-503, 1777777777] (-1)
        gives DivideInteger [503, -1777777777] (-1)
        gives DivideInteger [-503, -1777777777] 0
    , testCase "modInteger, conformance rows" $ do
        gives ModInteger [-503, 1777777777] 1777777274
        gives ModInteger [503, -1777777777] (-1777777274)
        gives ModInteger [-503, -1777777777] (-503)
    , testCase "remainderInteger, conformance rows" $ do
        gives RemainderInteger [-503, 1777777777] (-503)
        gives RemainderInteger [503, -1777777777] 503
    , testCase "quotientInteger, a large dividend" $
        gives QuotientInteger [-503783783785265728700234277, 1777777777] (-283378378503190012)
    , testCase "1 by 2^64, from amaru-uplc's extra conformance cases" $ do
        gives DivideInteger [1, 2 ^ (64 :: Int)] 0
        gives ModInteger [1, 2 ^ (64 :: Int)] 1
    , testCase "a zero divisor fails" $
        mapM_ (\f -> fails f [1, 0]) divisions
    , testCase "a zero divisor fails when the dividend is zero too" $
        mapM_ (\f -> fails f [0, 0]) divisions
    , testCase "a computed dividend over zero fails" $ do
        let t = call RemainderInteger [call AddInteger [int 6, int 4], int 0]
            cause = call RemainderInteger [int 10, int 0]
        evaluate t @?= Left (BuiltinFailure cause)
    , testCase "a value constant is not an integer" $ do
        let t = call DivideInteger [int 1, Constant () (CValue (pack mempty))]
        evaluate t @?= Left (BuiltinArgumentType t)
    , testCase "a division by zero in an argument the function ignores still fails" $ do
        let t = Apply () (LamAbs () (DeBruijn 0) (int 1)) (call DivideInteger [int 1, int 0])
        evaluate t @?= Left (BuiltinFailure (call DivideInteger [int 1, int 0]))
    ]

divisions :: [DefaultFun]
divisions = [DivideInteger, QuotientInteger, RemainderInteger, ModInteger]

divisionBounds :: TestTree
divisionBounds =
  testGroup
    "Division and the D and E bound"
    [ testCase "N as the dividend is refused in D and E only" $
        mapM_
          (\(f, r) -> do
              givesIn oldVariants f [bound, 1] (int r)
              failsIn newVariants f [bound, 1])
          [(DivideInteger, bound), (QuotientInteger, bound), (RemainderInteger, 0), (ModInteger, 0)]
    , testCase "N as the divisor is refused in D and E only" $
        mapM_
          (\(f, r) -> do
              givesIn oldVariants f [1, bound] (int r)
              failsIn newVariants f [1, bound])
          [(DivideInteger, 0), (QuotientInteger, 0), (RemainderInteger, 1), (ModInteger, 1)]
    , testCase "N over zero fails in every variant" $
        mapM_ (\f -> fails f [bound, 0]) divisions
    , testCase "N - 1 is accepted" $
        mapM_
          (\(f, r) -> gives f [bound - 1, 1] r)
          [(DivideInteger, bound - 1), (QuotientInteger, bound - 1), (RemainderInteger, 0), (ModInteger, 0)]
    ]

expMod :: TestTree
expMod =
  testGroup
    "expModInteger"
    [ testCase "2^10 mod 1000" $ em 2 10 1000 24
    , testCase "a zero exponent gives 1" $ em 500 0 500 1
    , testCase "a multiple of the modulus gives 0" $ em 500 5 500 0
    , testCase "0^0 is 1 for a modulus above 1" $ em 0 0 2123478956273846578234523475237 1
    , testCase "0^0 mod 1 is 0" $ em 0 0 1 0
    , testCase "modulus 1 gives 0 for a negative exponent" $ em 12389 (-9) 1 0
    , testCase "modulus 1 gives 0 for a zero base and a negative exponent" $ em 0 (-1) 1 0
    , testCase "a zero base with a negative exponent has no inverse" $ emFails 0 (-1) 17
    , testCase "a zero base with a large negative exponent has no inverse" $ emFails 0 (-1134134121) 17
    , testCase "inverse of 2 mod 7" $ em 2 (-1) 7 4
    , testCase "inverse of a negative base" $ em (-2) (-1) 7 3
    , testCase "the square of the inverse of 3 mod 7" $ em 3 (-2) 7 4
    , testCase "4^-5 mod 9" $ em 4 (-5) 9 4
    , testCase "2^-3 mod 3" $ em 2 (-3) 3 2
    , testCase "inverse of -1719 mod 17" $ em (-1719) (-1) 17 8
    , testCase "a negative base with a positive exponent" $ em (-3) 3 7 1
    , testCase "a base sharing a factor with the modulus has no inverse" $ emFails 2 (-3) 4
    , testCase "a base equal to the modulus has no inverse" $ emFails 17 (-1) 17
    , testCase "500^-5 mod 5" $ emFails 500 (-5) 5
    , testCase "modulus 0" $ emFails 1 1 0
    , testCase "everything zero" $ emFails 0 0 0
    , testCase "a negative modulus" $ emFails 1 1 (-3)
    , testCase "a negative modulus with no inverse" $ emFails 2 (-3) (-4)
    ]
  where
    em b e m r = gives ExpModInteger [b, e, m] r
    emFails b e m = fails ExpModInteger [b, e, m]

-- K is 2^8191. The modulus must be below K, and the base and exponent in
-- [-K, K), except that a modulus of 1 is answered first.
expModBounds :: TestTree
expModBounds =
  testGroup
    "expModInteger size limits"
    [ testCase "the largest modulus is accepted" $ gives ExpModInteger [2, 1, k - 1] 2
    , testCase "a modulus of K fails" $ fails ExpModInteger [2, 1, k]
    , testCase "the largest base is accepted" $ gives ExpModInteger [k - 1, 1, 7] (mod (k - 1) 7)
    , testCase "the smallest base is accepted" $ gives ExpModInteger [negate k, 1, 7] (mod (negate k) 7)
    , testCase "a base of K fails" $ fails ExpModInteger [k, 1, 7]
    , testCase "a base below -K fails" $ fails ExpModInteger [negate k - 1, 1, 7]
    , testCase "an exponent of K fails" $ fails ExpModInteger [2, k, 7]
    , testCase "an exponent below -K fails" $ fails ExpModInteger [3, negate k - 1, 7]
    , testCase "modulus 1 comes before the size of the base" $ gives ExpModInteger [k, 1, 1] 0
    , testCase "a zero base with a huge negative exponent fails" $ fails ExpModInteger [0, negate (2 ^ (9000 :: Int)), 7]
    ]
  where
    k = 2 ^ (8191 :: Int)

-- With N = 2^262143, A, B and C take any integer; D and E refuse an argument
-- outside [-N, N - 1], but a result outside it is returned.
bounds :: TestTree
bounds =
  testGroup
    "The D and E bound"
    [ testCase "N - 1 is accepted everywhere" $ gives AddInteger [bound - 1, 0] (bound - 1)
    , testCase "-N is accepted everywhere" $ gives AddInteger [negate bound, 0] (negate bound)
    , testCase "N is refused by D and E only" $ do
        givesIn oldVariants AddInteger [bound, 0] (int bound)
        failsIn newVariants AddInteger [bound, 0]
    , testCase "-N - 1 is refused by D and E only" $ do
        givesIn oldVariants AddInteger [negate bound - 1, 0] (int (negate bound - 1))
        failsIn newVariants AddInteger [negate bound - 1, 0]
    , testCase "the second argument is checked too" $
        failsIn newVariants AddInteger [0, bound]
    , testCase "a result past the bound is returned" $
        gives AddInteger [bound - 1, 1] bound
    , testCase "(N - 1) * (N - 1) is returned" $
        gives MultiplyInteger [bound - 1, bound - 1] ((bound - 1) * (bound - 1))
    , testCase "subtractInteger refuses N in D and E" $ do
        givesIn oldVariants SubtractInteger [bound, 1] (int (bound - 1))
        failsIn newVariants SubtractInteger [bound, 1]
    , testCase "multiplyInteger refuses N in D and E" $ do
        givesIn oldVariants MultiplyInteger [1, bound] (int bound)
        failsIn newVariants MultiplyInteger [1, bound]
    , testCase "lessThanInteger refuses N in D and E" $ do
        givesIn oldVariants LessThanInteger [bound, 0] (bool False)
        failsIn newVariants LessThanInteger [bound, 0]
    , testCase "lessThanEqualsInteger refuses N in D and E" $ do
        givesIn oldVariants LessThanEqualsInteger [0, bound] (bool True)
        failsIn newVariants LessThanEqualsInteger [0, bound]
    , testCase "equalsInteger takes N in every variant" $ givesBool EqualsInteger [bound, bound] True
    , testCase "an unsaturated call does not look at its argument" $
        mapM_
          (\v -> evaluateWith v (call AddInteger [int bound]) @?= Right (call AddInteger [int bound]))
          allVariants
    ]

-- QuickCheck's own integers stay small. These also reach past 64 bits, where
-- a machine word no longer holds them, and up to the D and E bound.
genInteger :: Gen Integer
genInteger =
  frequency
    [ (3, arbitrary)
    , (1, choose (2 ^ (62 :: Int), 2 ^ (66 :: Int)))
    , (1, negate <$> choose (2 ^ (62 :: Int), 2 ^ (66 :: Int)))
    , (1, choose (negate bound, bound - 1))
    ]

pairs :: Gen (Integer, Integer)
pairs = (,) <$> genInteger <*> genInteger

-- The divisor is never zero.
dividends :: Gen (Integer, Integer)
dividends = (,) <$> genInteger <*> (genInteger `suchThat` (/= 0))

-- The denotations are the Prelude's, so these check what is around them:
-- unlifting, the result constant, and that nothing is lost past 64 bits.
arithmeticLaws :: TestTree
arithmeticLaws =
  testGroup
    "Arithmetic properties"
    [ testProperty "addInteger agrees with (+)" $ forAll pairs $ \(x, y) -> run2 AddInteger x y === Right (int (x + y))
    , testProperty "subtractInteger agrees with (-)" $ forAll pairs $ \(x, y) -> run2 SubtractInteger x y === Right (int (x - y))
    , testProperty "multiplyInteger agrees with (*)" $ forAll pairs $ \(x, y) -> run2 MultiplyInteger x y === Right (int (x * y))
    , testProperty "subtractInteger undoes addInteger while the sum is in bounds" $ forAll pairs $ \(x, y) ->
        abs (x + y) < bound ==> run2 SubtractInteger (x + y) y === Right (int x)
    , testProperty "lessThanInteger agrees with (<)" $ forAll pairs $ \(x, y) -> run2 LessThanInteger x y === Right (bool (x < y))
    , testProperty "lessThanEqualsInteger agrees with (<=)" $ forAll pairs $ \(x, y) ->
        run2 LessThanEqualsInteger x y === Right (bool (x <= y))
    , testProperty "equalsInteger agrees with (==)" $ forAll pairs $ \(x, y) -> run2 EqualsInteger x y === Right (bool (x == y))
    , testProperty "an integer equals itself" $ forAll genInteger $ \x -> run2 EqualsInteger x x === Right (bool True)
    ]

run2 :: DefaultFun -> Integer -> Integer -> Either EvalError T
run2 f x y = evaluate (call f [int x, int y])

divisionLaws :: TestTree
divisionLaws =
  testGroup
    "Division properties"
    [ testProperty "divideInteger and modInteger agree with Haskell's div and mod" $
        forAll dividends $ \(x, y) ->
          (run2 DivideInteger x y, run2 ModInteger x y)
            === (Right (int (x `div` y)), Right (int (x `mod` y)))
    , testProperty "x == y * divideInteger x y + modInteger x y" $
        forAll dividends $ \(x, y) -> case (run2 DivideInteger x y, run2 ModInteger x y) of
          (Right (Constant () (CInteger q)), Right (Constant () (CInteger m))) -> x === y * q + m
          r -> error (show r)
    , testProperty "x == y * quotientInteger x y + remainderInteger x y" $
        forAll dividends $ \(x, y) -> case (run2 QuotientInteger x y, run2 RemainderInteger x y) of
          (Right (Constant () (CInteger q)), Right (Constant () (CInteger r))) -> x === y * q + r
          r -> error (show r)
    , testProperty "the modulus has the sign of the divisor and is smaller than it" $
        forAll dividends $ \(x, y) -> case run2 ModInteger x y of
          Right (Constant () (CInteger m)) -> (m == 0 || signum m == signum y) && abs m < abs y
          r -> error (show r)
    , testProperty "the remainder has the sign of the dividend and is smaller than the divisor" $
        forAll dividends $ \(x, y) -> case run2 RemainderInteger x y of
          Right (Constant () (CInteger r)) -> (r == 0 || signum r == signum x) && abs r < abs y
          r -> error (show r)
    , testProperty "quotientInteger and remainderInteger agree with Haskell's quot and rem" $
        forAll dividends $ \(x, y) ->
          (run2 QuotientInteger x y, run2 RemainderInteger x y)
            === (Right (int (x `quot` y)), Right (int (x `rem` y)))
    , testProperty "quotient and divide agree when the signs agree" $
        forAll dividends $ \(x, y) -> (x >= 0) == (y > 0) ==> run2 QuotientInteger x y === run2 DivideInteger x y
    , testProperty "a zero divisor always fails" $
        forAll genInteger $ \x -> all (\f -> run2 f x 0 == Left (BuiltinFailure (call f [int x, int 0]))) divisions
    ]

expModLaws :: TestTree
expModLaws =
  testGroup
    "expModInteger properties"
    [ testProperty "a non-negative exponent agrees with repeated multiplication" $
        \b (NonNegative e) (Positive m) ->
          e <= 40 ==> run3 b e m === Right (int (mod (b ^ e) m))
    , testProperty "the result of a successful call lies in [0, m)" $
        \b e (Positive m) -> case run3 b e m of
          Right (Constant () (CInteger r)) -> 0 <= r && r < m
          Left (BuiltinFailure _) -> e < 0 && gcd b m /= 1
          r -> error (show r)
    , testProperty "a negative exponent gives the inverse of the power when the base is coprime" $
        \b (Positive e) (Positive m) ->
          m > 1 && gcd b m == 1 && e <= 40 ==> case run3 b (negate e) m of
            Right (Constant () (CInteger r)) -> mod (r * b ^ e) m === 1
            r -> error (show r)
    , testProperty "a negative exponent fails exactly when the base is not coprime" $
        \b (Positive e) (Positive m) ->
          m > 1 && b /= 0 ==> case run3 b (negate e) m of
            Right _ -> gcd b m === 1
            Left (BuiltinFailure _) -> gcd b m =/= 1
            r -> error (show r)
    , testProperty "a non-positive modulus fails" $
        \b e (NonPositive m) -> run3 b e m === Left (BuiltinFailure (expCall b e m))
    ]

run3 :: Integer -> Integer -> Integer -> Either EvalError T
run3 b e m = evaluate (expCall b e m)

expCall :: Integer -> Integer -> Integer -> T
expCall b e m = call ExpModInteger [int b, int e, int m]
