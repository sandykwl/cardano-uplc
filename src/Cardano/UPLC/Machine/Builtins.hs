{- | Running a builtin: how many forces and arguments each takes, and what
happens when the last argument arrives.

Arguments are unlifted at saturation, left to right, so the first one that is
wrong decides the error. Two kinds of failure are kept apart, as in
plutus-core: an argument that is not a constant of the type the builtin takes
('ArgumentType'), and one that is the right type but out of range, or that the
builtin itself refuses ('Failed'). On chain both are the same failure. A
builtin with no denotation yet is 'Unsupported'.

@since 0.1.0
-}
module Cardano.UPLC.Machine.Builtins (
  SemanticsVariant (..),
  BuiltinError (..),
  forceCount,
  arity,
  runBuiltin,
) where

import Cardano.UPLC.Builtin (DefaultFun (..))
import Cardano.UPLC.Constant (Constant (..))
import Cardano.UPLC.Machine.Builtins.Error (BuiltinError (..))
import Cardano.UPLC.Machine.Builtins.Integer qualified as I
import Cardano.UPLC.Machine.CekValue (CekValue (..))

{- | Which builtin semantics to run. The ledger picks one from the ledger
language and the protocol version, and this package leaves that choice to the
caller. plutus-ledger-api 1.70.0.0 gives Plutus V1 and V2 scripts A before
the Chang hard fork, B until van Rossem and D from then on. Plutus V3 scripts
get C, then E from van Rossem.

A and B behave the same and differ only in cost, which this package does not
count. D and E bound most integer arguments. E is the one the conformance
suite runs.

@since 0.1.0
-}
data SemanticsVariant
  = -- | @since 0.1.0
    VariantA
  | -- | @since 0.1.0
    VariantB
  | -- | @since 0.1.0
    VariantC
  | -- | @since 0.1.0
    VariantD
  | -- | @since 0.1.0
    VariantE
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Ord
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Enum
    , -- | @since 0.1.0
      Bounded
    )

-- Forces, then arguments.
shape :: DefaultFun -> (Int, Int)
shape = \case
  AddInteger                      -> (0, 2)
  SubtractInteger                 -> (0, 2)
  MultiplyInteger                 -> (0, 2)
  DivideInteger                   -> (0, 2)
  QuotientInteger                 -> (0, 2)
  RemainderInteger                -> (0, 2)
  ModInteger                      -> (0, 2)
  EqualsInteger                   -> (0, 2)
  LessThanInteger                 -> (0, 2)
  LessThanEqualsInteger           -> (0, 2)
  AppendByteString                -> (0, 2)
  ConsByteString                  -> (0, 2)
  SliceByteString                 -> (0, 3)
  LengthOfByteString              -> (0, 1)
  IndexByteString                 -> (0, 2)
  EqualsByteString                -> (0, 2)
  LessThanByteString              -> (0, 2)
  LessThanEqualsByteString        -> (0, 2)
  Sha2_256                        -> (0, 1)
  Sha3_256                        -> (0, 1)
  Blake2b_256                     -> (0, 1)
  VerifyEd25519Signature          -> (0, 3)
  AppendString                    -> (0, 2)
  EqualsString                    -> (0, 2)
  EncodeUtf8                      -> (0, 1)
  DecodeUtf8                      -> (0, 1)
  IfThenElse                      -> (1, 3)
  ChooseUnit                      -> (1, 2)
  Trace                           -> (1, 2)
  FstPair                         -> (2, 1)
  SndPair                         -> (2, 1)
  ChooseList                      -> (2, 3)
  MkCons                          -> (1, 2)
  HeadList                        -> (1, 1)
  TailList                        -> (1, 1)
  NullList                        -> (1, 1)
  ChooseData                      -> (1, 6)
  ConstrData                      -> (0, 2)
  MapData                         -> (0, 1)
  ListData                        -> (0, 1)
  IData                           -> (0, 1)
  BData                           -> (0, 1)
  UnConstrData                    -> (0, 1)
  UnMapData                       -> (0, 1)
  UnListData                      -> (0, 1)
  UnIData                         -> (0, 1)
  UnBData                         -> (0, 1)
  EqualsData                      -> (0, 2)
  MkPairData                      -> (0, 2)
  MkNilData                       -> (0, 1)
  MkNilPairData                   -> (0, 1)
  SerialiseData                   -> (0, 1)
  VerifyEcdsaSecp256k1Signature   -> (0, 3)
  VerifySchnorrSecp256k1Signature -> (0, 3)
  Bls12_381_G1_add                -> (0, 2)
  Bls12_381_G1_neg                -> (0, 1)
  Bls12_381_G1_scalarMul          -> (0, 2)
  Bls12_381_G1_equal              -> (0, 2)
  Bls12_381_G1_compress           -> (0, 1)
  Bls12_381_G1_uncompress         -> (0, 1)
  Bls12_381_G1_hashToGroup        -> (0, 2)
  Bls12_381_G2_add                -> (0, 2)
  Bls12_381_G2_neg                -> (0, 1)
  Bls12_381_G2_scalarMul          -> (0, 2)
  Bls12_381_G2_equal              -> (0, 2)
  Bls12_381_G2_compress           -> (0, 1)
  Bls12_381_G2_uncompress         -> (0, 1)
  Bls12_381_G2_hashToGroup        -> (0, 2)
  Bls12_381_millerLoop            -> (0, 2)
  Bls12_381_mulMlResult           -> (0, 2)
  Bls12_381_finalVerify           -> (0, 2)
  Keccak_256                      -> (0, 1)
  Blake2b_224                     -> (0, 1)
  IntegerToByteString             -> (0, 3)
  ByteStringToInteger             -> (0, 2)
  AndByteString                   -> (0, 3)
  OrByteString                    -> (0, 3)
  XorByteString                   -> (0, 3)
  ComplementByteString            -> (0, 1)
  ReadBit                         -> (0, 2)
  WriteBits                       -> (0, 3)
  ReplicateByte                   -> (0, 2)
  ShiftByteString                 -> (0, 2)
  RotateByteString                -> (0, 2)
  CountSetBits                    -> (0, 1)
  FindFirstSetBit                 -> (0, 1)
  Ripemd_160                      -> (0, 1)
  ExpModInteger                   -> (0, 3)
  DropList                        -> (1, 2)
  LengthOfArray                   -> (1, 1)
  ListToArray                     -> (1, 1)
  IndexArray                      -> (1, 2)
  Bls12_381_G1_multiScalarMul     -> (0, 2)
  Bls12_381_G2_multiScalarMul     -> (0, 2)

{- | How many forces a builtin takes.

@since 0.1.0
-}
forceCount :: DefaultFun -> Int
forceCount = fst . shape

{- | How many arguments a builtin takes.

@since 0.1.0
-}
arity :: DefaultFun -> Int
arity = snd . shape

{- | Apply a builtin to its arguments, given in the order they were passed.
The caller has counted them: there are 'arity' of them.

@since 0.1.0
-}
runBuiltin :: SemanticsVariant -> DefaultFun -> [CekValue] -> Either BuiltinError CekValue
runBuiltin var f args = case (f, args) of
  (AddInteger, [x, y]) -> integerOp I.addInteger x y
  (SubtractInteger, [x, y]) -> integerOp I.subtractInteger x y
  (MultiplyInteger, [x, y]) -> integerOp I.multiplyInteger x y
  (DivideInteger, [x, y]) -> integerOp I.divideInteger x y
  (QuotientInteger, [x, y]) -> integerOp I.quotientInteger x y
  (RemainderInteger, [x, y]) -> integerOp I.remainderInteger x y
  (ModInteger, [x, y]) -> integerOp I.modInteger x y
  -- Unbounded in every variant, unlike its neighbours, as in plutus-core.
  (EqualsInteger, [x, y]) -> do
    a <- integer x
    b <- integer y
    con CBool (I.equalsInteger a b)
  (LessThanInteger, [x, y]) -> do
    a <- boundedInteger x
    b <- boundedInteger y
    con CBool (I.lessThanInteger a b)
  (LessThanEqualsInteger, [x, y]) -> do
    a <- boundedInteger x
    b <- boundedInteger y
    con CBool (I.lessThanEqualsInteger a b)
  -- Unbounded here: it has size limits of its own, the same in every variant.
  (ExpModInteger, [b, e, m]) -> do
    base <- integer b
    expo <- integer e
    modulus <- integer m
    con CInteger (I.expModInteger base expo modulus)
  -- The branches are passed through unchecked and unchanged.
  (IfThenElse, [c, t, e]) -> do
    cond <- bool c
    Right (if cond then t else e)
  _ -> Left Unsupported
  where
    limited = bounded var

    integerOp op x y = do
      a <- boundedInteger x
      b <- boundedInteger y
      con CInteger (op a b)

    boundedInteger v = integer v >>= \n ->
      if limited && not (integerInBounds n)
        then Left Failed
        else Right n

-- D and E bound the integer arguments builtins read. Every variant is listed,
-- unlike plutus-core's wildcard, so a new one has to be placed by hand.
bounded :: SemanticsVariant -> Bool
bounded = \case
  VariantA -> False
  VariantB -> False
  VariantC -> False
  VariantD -> True
  VariantE -> True

-- The D and E bound: integers from -(2^262143) to 2^262143 - 1 (plutus-core's
-- Cardano.hs). Kept at the top level and not inlined, as there, so the big
-- number is built once.
integerBound :: Integer
integerBound = 2 ^ (262143 :: Int)
{-# NOINLINE integerBound #-}

integerInBounds :: Integer -> Bool
integerInBounds n = n >= negate integerBound && n < integerBound

-- Unlifting. A value that is not a constant, and a constant of another type,
-- are the same failure.
constant :: CekValue -> Either BuiltinError Constant
constant = \case
  VCon c -> Right c
  _ -> Left ArgumentType

integer :: CekValue -> Either BuiltinError Integer
integer v = constant v >>= \case
  CInteger n -> Right n
  _ -> Left ArgumentType

bool :: CekValue -> Either BuiltinError Bool
bool v = constant v >>= \case
  CBool b -> Right b
  _ -> Left ArgumentType

-- A denotation's result becomes a constant; its failure passes through.
con :: (r -> Constant) -> Either BuiltinError r -> Either BuiltinError CekValue
con wrap = fmap (VCon . wrap)
