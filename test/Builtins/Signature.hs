{- Tests for the rules the core keeps for the three signature builtins, with
stub verifiers in place of a library.

A stub that calls error proves a rule fired first: the machine would crash if
it asked. The verdicts themselves are the library's, and are tested where the
library is.
-}
module Builtins.Signature (signatureTests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Cardano.UPLC.Builtin (DefaultFun (..))
import Cardano.UPLC.Constant (Constant (..))
import Cardano.UPLC.Machine (
  EvalError (..),
  Params (..),
  SemanticsVariant (..),
  SignatureChecks (..),
  defaultParams,
  evaluate,
  evaluateWith,
 )
import Cardano.UPLC.Name (DeBruijn)
import Cardano.UPLC.Term (Term (..))

signatureTests :: TestTree
signatureTests =
  testGroup "Signature builtins" $
    [testGroup name (map ($ spec) (rules spec)) | (name, spec) <- specs]
      <> [argumentsInOrder, argumentTypes, testGroup "No verifier" noVerifier]
  where
    rules spec = [lengths, noVerifierForLengths, verdicts] <> [bound | not (fixedMessage spec)]

type T = Term DeBruijn ()
type Check = ByteString -> ByteString -> ByteString -> Maybe Bool

-- A builtin, its key length, whether the message is fixed at 32 bytes, and
-- how to put a verifier in the record.
data Spec = Spec
  { builtin :: DefaultFun
  , keyLen :: Int
  , fixedMessage :: Bool
  , install :: Check -> SignatureChecks
  }

specs :: [(String, Spec)]
specs =
  [ ("verifyEd25519Signature", Spec VerifyEd25519Signature 32 False (\c -> none {checkEd25519 = Just c}))
  , ("verifyEcdsaSecp256k1Signature", Spec VerifyEcdsaSecp256k1Signature 33 True (\c -> none {checkEcdsa = Just c}))
  , ("verifySchnorrSecp256k1Signature", Spec VerifySchnorrSecp256k1Signature 32 False (\c -> none {checkSchnorr = Just c}))
  ]

none :: SignatureChecks
none = signatureChecks defaultParams

neverAsked :: Check
neverAsked _ _ _ = error "the verifier was asked"

bytes :: Int -> ByteString
bytes n = BS.replicate n 7

con :: ByteString -> T
con = Constant () . CByteString

call :: DefaultFun -> [T] -> T
call f = foldl (Apply ()) (Builtin () f)

-- A call with a key, a message and a signature of the given sizes.
sized :: Spec -> Int -> Int -> Int -> T
sized spec k m s = call (builtin spec) [con (bytes k), con (bytes m), con (bytes s)]

-- The message size that is right for the builtin.
okMessage :: Spec -> Int
okMessage spec = if fixedMessage spec then 32 else 100

under :: SemanticsVariant -> SignatureChecks -> T -> Either EvalError T
under v checks = evaluateWith (Params v checks)

allVariants :: [SemanticsVariant]
allVariants = [minBound .. maxBound]

failure :: T -> Either EvalError T
failure t = Left (BuiltinFailure t)

lengths :: Spec -> TestTree
lengths spec =
  testCase "wrong lengths fail without asking the verifier" $
    mapM_
      check
      ( [(keyLen spec + d, okMessage spec, 64) | d <- [-1, 1]]
          <> [(keyLen spec, okMessage spec, s) | s <- [0, 63, 65]]
          <> [(0, okMessage spec, 64)]
          <> [(keyLen spec, m, 64) | fixedMessage spec, m <- [0, 31, 33]]
      )
  where
    checks = install spec neverAsked
    check (k, m, s) =
      mapM_
        (\v -> under v checks (sized spec k m s) @?= failure (sized spec k m s))
        allVariants

-- Lengths come before the question of a missing verifier.
noVerifierForLengths :: Spec -> TestTree
noVerifierForLengths spec =
  testCase "wrong lengths fail even with no verifier" $
    evaluate (sized spec (keyLen spec + 1) (okMessage spec) 64)
      @?= failure (sized spec (keyLen spec + 1) (okMessage spec) 64)

verdicts :: Spec -> TestTree
verdicts spec =
  testCase "the verifier's answer is the result" $ do
    let t = sized spec (keyLen spec) (okMessage spec) 64
        answer a = install spec (\_ _ _ -> a)
    mapM_
      ( \v -> do
          under v (answer (Just True)) t @?= Right (Constant () (CBool True))
          under v (answer (Just False)) t @?= Right (Constant () (CBool False))
          under v (answer Nothing) t @?= failure t
      )
      allVariants

-- Only Ed25519 and Schnorr have a message bound (ECDSA fixes the length at
-- 32), and only D and E apply it.
bound :: Spec -> TestTree
bound spec =
  testCase "the message bound" $ do
    let at n = sized spec (keyLen spec) n 64
        yes = install spec (\_ _ _ -> Just True)
        ok = Right (Constant () (CBool True))
    mapM_ (\v -> under v yes (at 65536) @?= ok) allVariants
    mapM_ (\v -> under v yes (at 65537) @?= ok) [VariantA, VariantB, VariantC]
    mapM_
      (\v -> under v (install spec neverAsked) (at 65537) @?= failure (at 65537))
      [VariantD, VariantE]

argumentsInOrder :: TestTree
argumentsInOrder =
  testCase "verifiers get the key, the message and the signature" $
    mapM_
      ( \(_, spec) -> do
          let key = BS.replicate (keyLen spec) 1
              msg = BS.replicate (okMessage spec) 2
              sig = BS.replicate 64 3
              checks = install spec (\k m s -> Just (k == key && m == msg && s == sig))
          evaluateWith (Params VariantE checks) (call (builtin spec) [con key, con msg, con sig])
            @?= Right (Constant () (CBool True))
      )
      specs

-- An argument of the wrong type is reported before any length rule.
argumentTypes :: TestTree
argumentTypes =
  testCase "an argument that is not a bytestring is a type error" $
    mapM_
      ( \(_, spec) -> do
          let t = call (builtin spec) [con (bytes 1), Constant () (CInteger 1), con (bytes 64)]
          evaluateWith (Params VariantE (install spec neverAsked)) t @?= Left (BuiltinArgumentType t)
      )
      specs

noVerifier :: [TestTree]
noVerifier =
  [ testCase name $ do
      let t = sized spec (keyLen spec) (okMessage spec) 64
      evaluate t @?= Left (NotSupported t)
  | (name, spec) <- specs
  ]
