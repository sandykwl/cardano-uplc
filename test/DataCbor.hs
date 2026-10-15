{- Tests for Data as CBOR.

The golden bytes are plutus-core's, printed by its own encoder, never worked
out by hand. The negatives check the exact error
and where it points: this codec's own rules report the start of the item at
fault, and anything the CBOR reader refuses keeps the reader's message.
-}
module DataCbor (dataCborTests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Numeric (readHex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (forAll, testProperty, (===))
import Cardano.UPLC.Data (Data (..))
import Cardano.UPLC.Data.Cbor (DataDecodeError (..), decodeData, encodeData)
import Cardano.UPLC.Test.Gen (genData)

dataCborTests :: TestTree
dataCborTests = testGroup "Data as CBOR" [roundTrip, golden, reading, refusals]

roundTrip :: TestTree
roundTrip =
  testProperty "decoding what was encoded gives the value back" $
    forAll genData $ \d -> decodeData (encodeData d) === Right d

-- One value either side of each writing rule's boundary.
golden :: TestTree
golden =
  testGroup
    "Golden, from plutus-core"
    [ testCase label (encodeData d @?= hex bytes)
    | (label, d, bytes) <-
        [ ("Constr 6, the last short tag", Constr 6 [], "d87f80")
        , ("Constr 7, the first long tag", Constr 7 [], "d9050080")
        , ("Constr 127, the last long tag", Constr 127 [], "d9057880")
        , ("Constr 128, under tag 102", Constr 128 [], "d86682188080")
        , ("Constr 2^64, written though nothing reads it", Constr (2 ^ (64 :: Int)) [], "d86682c24901000000000000000080")
        , ("Constr -1, written though nothing reads it", Constr (-1) [], "d866822080")
        , ("fields, indefinite when not empty", Constr 0 [I 1], "d8799f01ff")
        , ("an empty list, definite", List [], "80")
        , ("a map, always definite", Map [(I 1, I 2)], "a10102")
        , ("I 23, in the head", I 23, "17")
        , ("I 24, one byte after", I 24, "1818")
        , ("I (2^64 - 1), the last native", I (2 ^ (64 :: Int) - 1), "1bffffffffffffffff")
        , ("I 2^64, the first big", I (2 ^ (64 :: Int)), "c249010000000000000000")
        , ("I -2^64, the last native", I (negate (2 ^ (64 :: Int))), "3bffffffffffffffff")
        , ("I (-2^64 - 1), the first big", I (negate (2 ^ (64 :: Int)) - 1), "c349010000000000000000")
        , ("I 2^512, a 65-byte magnitude, chunked", I (2 ^ (512 :: Int)), "c25f5840" <> "01" <> zeros 63 <> "4100ff")
        , ("B of 64 bytes, one string", B (BS.replicate 64 0xab), "5840" <> abs' 64)
        , ("B of 65 bytes, chunked", B (BS.replicate 65 0xab), "5f5840" <> abs' 64 <> "41abff")
        ]
    ]
  where
    zeros n = concat (replicate n "00")
    abs' n = concat (replicate n "ab")

-- Forms plutus-core accepts but never writes.
reading :: TestTree
reading =
  testGroup
    "Reading what plutus-core accepts"
    [ testCase label (decodeData (hex bytes) @?= Right d)
    | (label, bytes, d) <-
        [ ("a head written longer than it needs", "1b0000000000000001", I 1)
        , ("a big integer that would fit natively", "c24101", I 1)
        , ("a big integer's payload in chunks", "c25f4101ff", I 1)
        , ("a byte string in chunks, one of them empty", "5f41ab40ff", B (BS.pack [0xab]))
        , ("a definite non-empty list", "8101", List [I 1])
        , ("an indefinite map", "bf0102ff", Map [(I 1, I 2)])
        , ("a map with a key twice, both kept", "a201020103", Map [(I 1, I 2), (I 1, I 3)])
        , ("a small index under tag 102", "d866820080", Constr 0 [])
        , ("tag 102 with an indefinite pair", "d8669f188080ff", Constr 128 [])
        ]
    ]

refusals :: TestTree
refusals =
  testGroup
    "Refusals"
    [ testCase label (decodeData (hex bytes) @?= Left e)
    | (label, bytes, e) <-
        [ ("a definite byte string of 65 bytes", "5841" <> concat (replicate 65 "ab"), ByteStringTooLong 0 65)
        , ("a 65-byte chunk", "5f5841" <> concat (replicate 65 "ab") <> "ff", ByteStringTooLong 1 65)
        , ("tag 2 written with a long head", "d8020101", UnknownTag 0 2)
        , ("tag 120, just below the constructors", "d87880", UnknownTag 0 120)
        , ("tag 1401, just above them", "d9057980", UnknownTag 0 1401)
        , ("tag 102 with three items", "d8668318808000", BadConstrPair 0)
        , ("tag 102 with an index past Word64", "d86682c24901000000000000000080", CborError 3 (T.pack "expected word"))
        , ("a big integer over text", "c26161", BadBigInteger 1)
        , ("text", "6161", NotData 0 (T.pack "TypeString"))
        , ("a byte after the value", "0000", TrailingBytes 1)
        , ("no bytes", "", CborError 0 (T.pack "end of input"))
        ]
    ]

hex :: String -> ByteString
hex = BS.pack . go
  where
    go (a : b : rest) = case readHex [a, b] of
      [(w, "")] -> w : go rest
      _ -> error ("not hex: " <> [a, b])
    go _ = []
