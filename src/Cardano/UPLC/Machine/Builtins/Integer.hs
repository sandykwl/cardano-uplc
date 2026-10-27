{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}

{- | The integer builtins, on plain 'Integer'.

Arguments arrive already unlifted and range-checked for the semantics variant,
so these functions know nothing of the machine or of the variants. A @Left@ is
the builtin's own failure.

@since 0.1.0
-}
module Cardano.UPLC.Machine.Builtins.Integer (
  addInteger,
  subtractInteger,
  multiplyInteger,
  divideInteger,
  quotientInteger,
  remainderInteger,
  modInteger,
  equalsInteger,
  lessThanInteger,
  lessThanEqualsInteger,
  expModInteger,
) where

import GHC.Num.Integer (integerPowMod#)
import Numeric.Natural (Natural)
import Cardano.UPLC.Machine.Builtins.Error (BuiltinError (..))

-- A zero divisor fails, whatever the dividend.
nonZero :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Either BuiltinError Integer
nonZero _ _ 0 = Left Failed
nonZero f x y = Right (f x y)

{- | Addition.

@since 0.1.0
-}
addInteger :: Integer -> Integer -> Either BuiltinError Integer
addInteger x y = Right (x + y)

{- | Subtraction.

@since 0.1.0
-}
subtractInteger :: Integer -> Integer -> Either BuiltinError Integer
subtractInteger x y = Right (x - y)

{- | Multiplication.

@since 0.1.0
-}
multiplyInteger :: Integer -> Integer -> Either BuiltinError Integer
multiplyInteger x y = Right (x * y)

{- | Division rounding towards negative infinity; fails on a zero divisor.

@since 0.1.0
-}
divideInteger :: Integer -> Integer -> Either BuiltinError Integer
divideInteger = nonZero div

{- | Division rounding towards zero; fails on a zero divisor.

@since 0.1.0
-}
quotientInteger :: Integer -> Integer -> Either BuiltinError Integer
quotientInteger = nonZero quot

{- | Remainder with the sign of the dividend; fails on a zero divisor.

@since 0.1.0
-}
remainderInteger :: Integer -> Integer -> Either BuiltinError Integer
remainderInteger = nonZero rem

{- | Remainder with the sign of the divisor; fails on a zero divisor.

@since 0.1.0
-}
modInteger :: Integer -> Integer -> Either BuiltinError Integer
modInteger = nonZero mod

{- | Equality.

@since 0.1.0
-}
equalsInteger :: Integer -> Integer -> Either BuiltinError Bool
equalsInteger x y = Right (x == y)

{- | Strictly less than.

@since 0.1.0
-}
lessThanInteger :: Integer -> Integer -> Either BuiltinError Bool
lessThanInteger x y = Right (x < y)

{- | Less than or equal.

@since 0.1.0
-}
lessThanEqualsInteger :: Integer -> Integer -> Either BuiltinError Bool
lessThanEqualsInteger x y = Right (x <= y)

{- | Base to the exponent modulo the modulus, in @[0, modulus)@. A negative
exponent takes the modular inverse of the base, which fails when there is
none. Arguments are base, exponent, modulus.

@since 0.1.0
-}
expModInteger :: Integer -> Integer -> Integer -> Either BuiltinError Integer
expModInteger b e m
  | m <= 0 || m >= expModBound = Left Failed
  -- GHC's modular arithmetic gets these two cases wrong, so they are answered
  -- first, and a modulus of 1 gives 0 even for a base out of range.
  | m == 1 = Right 0
  | b == 0 && e < 0 = Left Failed
  | outside b || outside e = Left Failed
  | otherwise = case integerPowMod# b e (fromInteger m :: Natural) of
      (# r | #) -> Right (toInteger r)
      (# | () #) -> Left Failed
  where
    outside x = x < negate expModBound || x >= expModBound

-- The modulus must be below 2^8191, and the base and exponent in
-- [-2^8191, 2^8191). Kept at the top level and not inlined, as in
-- plutus-core, so the number is built once.
expModBound :: Integer
expModBound = 2 ^ (8191 :: Int)
{-# NOINLINE expModBound #-}
