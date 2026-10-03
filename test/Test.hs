{- Tests for the flat codec.

Round trips check that the encoder and decoder agree with each other. The
golden test checks that they agree with the reference, on a small program
whose bytes plutus-core reads and writes back unchanged. The negative cases
are copied byte for byte from amaru-uplc's conformance suite, and each checks
the exact error and where it happened, not just that decoding failed. The
last group pins down what this release doesn't support yet.
-}
module Main (main) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests, counterexample, testProperty, (===))
import Cardano.UPLC.Builtin (DefaultFun (..))
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
import Cardano.UPLC.Ty (Ty (..))

main :: IO ()
main =
  defaultMain . adjustOption (max (1_000 :: QuickCheckTests)) $
    testGroup "Flat" [tagTables, roundTrips, golden, goldenShapes, goldenChunks, negatives, padding, readingRules, gaps]

-- Position in the declaration is the wire tag, so a dropped or reordered
-- line would renumber everything after it and no round trip would notice.
-- The builtin tags are ones the conformance corpus pins by its bytes; the
-- term tags come from the specification's table.
tagTables :: TestTree
tagTables =
  testGroup
    "Tag tables"
    [ testCase "the builtin table ends at tag 93" $
        fromEnum (maxBound :: DefaultFun) @?= 93
    , testCase "builtin tags match the corpus" $
        map fromEnum [Sha2_256, SerialiseData, Bls12_381_G1_uncompress, Bls12_381_G1_hashToGroup, Keccak_256, IndexArray]
          @?= [18, 51, 59, 60, 71, 91]
    ]

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

   Its fifteen bytes were worked out by hand from the specification, and
   plutus-core decodes and re-encodes them unchanged. -}
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

{- (program 1.1.0
     (case (constr 1 (delay (force (error))) (con bool True))
           (con unit ())
           (con (list (pair bytestring string)) [(#ab, "a")])))

   Worked out by hand from the specification's rules, to cover the term
   and constant shapes the worked example leaves out: delay, force, error,
   constr, case, bool, unit, bytestring, string, list and pair. -}
goldenShapes :: TestTree
goldenShapes =
  testGroup
    "Golden, the other shapes"
    [ testCase "encodes to the hand-computed bytes" $
        encodeProgram shapes @?= Right shapesBytes
    , testCase "the hand-computed bytes decode to it" $
        decodeProgram shapesBytes @?= Right shapes
    ]
  where
    shapes =
      Program (Version 1 1 0) $
        Case
          (Constr 1 [Delay (Force Error), Constant (CBool True)])
          [ Constant CUnit
          , Constant
              ( CList
                  (TyPair TyByteString TyString)
                  [CPair TyByteString TyString (CByteString (BS.pack [0xab])) (CString (T.pack "a"))]
              )
          ]
    shapesBytes =
      BS.pack
        [ 0x01, 0x01, 0x00 -- version
        , 0x98 -- case, constr
        , 0x01 -- constr tag 1
        , 0x8a, 0xb5, 0x28, 0xa9, 0x35 -- fields: delay force error, bool true; branch 1: unit; branch 2 begins
        , 0x2f, 0x5b, 0xde, 0xd1, 0x93 -- type tags [7,5,7,7,6,1,2]; list cons; padding
        , 0x01, 0xab, 0x00 -- bytestring #ab
        , 0x01, 0x01, 0x61, 0x00 -- padding, string "a"
        , 0x01 -- list end, branches end, final padding
        ]

-- A 300-byte string: the writer must split it 255 then 45, because that is
-- the split the reference writes and the script hash depends on it. The
-- generators never reach 255 bytes, so this is pinned by hand.
goldenChunks :: TestTree
goldenChunks =
  testGroup
    "Golden, chunking"
    [ testCase "a 300-byte string is written as 255 and 45" $
        encodeProgram program @?= Right bytes
    , testCase "and reads back" $
        decodeProgram bytes @?= Right program
    ]
  where
    program = Program (Version 1 1 0) (Constant (CByteString (BS.replicate 300 0xab)))
    bytes =
      BS.concat
        [ BS.pack [0x01, 0x01, 0x00, 0x48, 0x81] -- version; constant, type [1], padding
        , BS.pack [0xff], BS.replicate 255 0xab
        , BS.pack [0x2d], BS.replicate 45 0xab
        , BS.pack [0x00, 0x01] -- end of chunks, final padding
        ]

-- amaru-uplc's negative conformance cases, byte for byte.
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

-- How numbers written with extra zero groups are read, matched to
-- plutus-core's decoders at 1.70.0.0. Indices and constr tags take at most
-- ten groups, as its Word64 reader does. Versions and integer constants are
-- unbounded naturals there, so any padding is accepted.
readingRules :: TestTree
readingRules =
  testGroup
    "Reading rules"
    [ testCase "an index padded to two groups is read" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x08, 0x10, 0x01])
          @?= Right (Program (Version 1 1 0) (Var (DeBruijn 1)))
    , testCase "an index padded to ten groups is read" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x08, 0x18, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x00, 0x01])
          @?= Right (Program (Version 1 1 0) (Var (DeBruijn 1)))
    , testCase "an index padded to eleven groups is refused" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x08, 0x18, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x00, 0x01])
          @?= Left (WordOverflow 28)
    , testCase "a constr tag padded to two groups is read" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x88, 0x10, 0x01])
          @?= Right (Program (Version 1 1 0) (Constr 1 []))
    , testCase "a constr tag padded to eleven groups is refused" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x88, 0x18, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x00, 0x01])
          @?= Left (WordOverflow 28)
    , testCase "a version padded to eleven groups is read" $
        decodeProgram (BS.pack [0x81, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00, 0x01, 0x00, 0x61])
          @?= Right (Program (Version 1 1 0) Error)
    , -- plutus-core's flat layer accepts this and its ledger refuses it, as
      -- it refuses every version but 1.0.0 and 1.1.0.
      testCase "a version past 2^64 is refused" $
        decodeProgram (BS.pack [0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x02, 0x01, 0x00, 0x61])
          @?= Left (WordOverflow 0)
    , -- "abc" split 1 + 2: the reader takes any chunking, as the
      -- reference's does; only the writer is held to 255-byte chunks.
      testCase "a non-canonically chunked byte string is read" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x48, 0x81, 0x01, 0x61, 0x02, 0x62, 0x63, 0x00, 0x01])
          @?= Right (Program (Version 1 1 0) (Constant (CByteString (BS.pack [0x61, 0x62, 0x63]))))
    , testCase "a string that is not UTF-8 is refused" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x49, 0x01, 0x01, 0xff, 0x00, 0x01])
          @?= Left (InvalidUtf8 34)
    , testCase "a byte after the final padding is refused" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x61, 0x00])
          @?= Left (TrailingInput 32)
    , -- Index zero points at no binder; like the reference, the decoder
      -- leaves that to scope checking.
      testCase "index zero is read" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x00, 0x01])
          @?= Right (Program (Version 1 1 0) (Var (DeBruijn 0)))
    , testCase "a padded integer constant is read" $
        decodeProgram (BS.pack [0x01, 0x01, 0x00, 0x48, 0x21, 0x20, 0x20, 0x00, 0x01])
          @?= Right (Program (Version 1 1 0) (Constant (CInteger 2)))
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

