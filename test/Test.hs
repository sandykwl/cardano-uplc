{- Tests for the flat codec.

Round trips check that the encoder and decoder agree with each other. The
golden test checks that they agree with the reference, using the worked
example from the specification. The negative cases are copied byte for byte
from the conformance suite, and each checks the exact error and where it
happened, not just that decoding failed. The last group pins down what this
release doesn't support yet.
-}
module Main (main) where

import Data.ByteString qualified as BS
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests, counterexample, testProperty, (===))
import Cardano.UPLC.Builtin (DefaultFun (AddInteger))
import Cardano.UPLC.Constant (Constant (..))
import Cardano.UPLC.Data qualified as Data
import Cardano.UPLC.Flat (
  DecodeError (..),
  EncodeError (EncodeDataConstant),
  decodeProgram,
  encodeProgram,
 )
import Cardano.UPLC.Name (DeBruijn (DeBruijn))
import Cardano.UPLC.Term (Program (Program), Term (..), Version (Version))
import Cardano.UPLC.Test.Gen (Encodable (Encodable), Nested (Nested))
import Cardano.UPLC.Ty (Ty (TyBLS12_381_G1_Element))

main :: IO ()
main =
  defaultMain . adjustOption (max (1_000 :: QuickCheckTests)) $
    testGroup "Flat" [roundTrips, golden, negatives, padding, gaps]

roundTrips :: TestTree
roundTrips =
  testGroup
    "Round trips"
    [ testProperty "programs" $
        \(Encodable p) -> roundTrip p
    , testProperty "constants at nested types" $
        \(Nested c) -> roundTrip (Program (Version 1 1 0) (Constant c))
    ]
  where
    roundTrip p = case encodeProgram p of
      Left e -> counterexample (show e) False
      Right bytes -> decodeProgram bytes === Right p

{- (program 1.1.0
     [ [ (lam x (lam y [ [ (builtin addInteger) x ] y ])) (con integer 2) ]
       (con integer 3) ])

   The specification's worked example, with the fifteen bytes it gives. -}
golden :: TestTree
golden =
  testGroup
    "Golden"
    [ testCase "the worked example encodes to the reference bytes" $
        encodeProgram example @?= Right exampleBytes
    , testCase "the reference bytes decode to the worked example" $
        decodeProgram exampleBytes @?= Right example
    ]
  where
    example =
      Program (Version 1 1 0) $
        Apply
          ( Apply
              ( LamAbs (DeBruijn 0) . LamAbs (DeBruijn 0) $
                  Apply (Apply (Builtin AddInteger) (Var (DeBruijn 2))) (Var (DeBruijn 1))
              )
              (Constant (CInteger 2))
          )
          (Constant (CInteger 3))
    exampleBytes =
      BS.pack [0x01, 0x01, 0x00, 0x33, 0x22, 0x33, 0x70, 0x00, 0x04, 0x00, 0x29, 0x00, 0x22, 0x40, 0x0d]

-- The conformance suite's negative cases, byte for byte.
negatives :: TestTree
negatives =
  testGroup
    "Conformance negatives"
    [ -- bls/g1-element: the type tag [9] decodes, the value doesn't. Flat has
      -- no encoding for a BLS point, and the reference refuses it too.
      testCase "a BLS G1 value is refused" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x4c, 0x81, 0x00])
          @?= Left (UnsupportedValue TyBLS12_381_G1_Element 34)
    , testCase "case below 1.1.0 is refused" $
        decodeProgram (BS.pack [0x01, 0x00, 0x00, 0x96, 0x01])
          @?= Left (TermNotInVersion 9 24)
    , testCase "constr below 1.1.0 is refused" $
        decodeProgram (BS.pack [0x01, 0x00, 0x00, 0x80, 0x01])
          @?= Left (TermNotInVersion 8 24)
    , -- constr/tag-overflow: a tag of exactly 2^64. Truncated, it would
      -- have decoded as tag 0.
      testCase "a constr tag of 2^64 overflows" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x88, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x00, 0x21])
          @?= Left (WordOverflow 28)
    , -- var/free: index 1 with no enclosing lambda decodes, because scope
      -- checking is a later pass. Upstream files it under evaluation
      -- failure.
      testCase "a free variable decodes" $
        decodeProgram (BS.pack [0x01, 0x00, 0x00, 0x00, 0x11])
          @?= Right (Program (Version 1 0 0) (Var (DeBruijn 1)))
    ]

-- Both references read padding as 0s up to the first 1 and then require
-- byte alignment, so extra zero bytes pass and a 1 mid-byte does not.
padding :: TestTree
padding =
  testGroup
    "Padding"
    [ testCase "whole extra bytes of 0s are accepted" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x60, 0x00, 0x01])
          @?= Right (Program (Version 1 1 0) Error)
    , testCase "a closing 1 that does not end a byte is refused" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x62])
          @?= Left (BadFiller 28)
    ]

-- The edges: a BLS type with no value, which works, and a data constant,
-- which waits for a CBOR codec.
gaps :: TestTree
gaps =
  testGroup
    "Release gaps"
    [ testCase "an empty list of BLS elements round-trips" $ do
        let p = Program (Version 1 1 0) (Constant (CList TyBLS12_381_G1_Element []))
        case encodeProgram p of
          Left e -> assertFailure (show e)
          Right bytes -> decodeProgram bytes @?= Right p
    , testCase "a data constant is refused by the encoder" $
        encodeProgram (Program (Version 1 1 0) (Constant (CData (Data.I 0))))
          @?= Left EncodeDataConstant
    ]

