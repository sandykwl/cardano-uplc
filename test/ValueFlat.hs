{-# LANGUAGE OverloadedStrings #-}

{- Tests for the value constant in flat.

The golden bytes and the accepted and refused inputs are plutus-core's, printed
by plutus-core 1.70.0.0's own flat encoder and decoder, never worked out by
hand. The refused inputs are flat lists on their own, so each is put into a
program by 'inProgram'. The refusals check the exact rule and the bit where
the offending key or quantity starts.
-}
module ValueFlat (valueFlatTests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Numeric (readHex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (forAll, testProperty, (===))
import Cardano.UPLC.Constant (Constant (..))
import Cardano.UPLC.Flat (DecodeError (..), ValueError (..), decodeProgram, encodeProgram)
import Cardano.UPLC.Name (DeBruijn)
import Cardano.UPLC.Term (Program (Program), Term (Constant), Version (Version))
import Cardano.UPLC.Test.Gen (genValue)
import Cardano.UPLC.Ty (Ty (TyValue))
import Cardano.UPLC.Value (Value, k, pack, quantity)

valueFlatTests :: TestTree
valueFlatTests = testGroup "Value in flat" [roundTrip, golden, reading, refusals]

program :: Constant -> Program DeBruijn ()
program = Program () (Version 1 1 0) . Constant ()

roundTrip :: TestTree
roundTrip =
  testProperty "decoding what was encoded gives the value back" $
    forAll genValue $ \v -> decodeProgram (encodeProgram (program (CValue v))) === Right (program (CValue v))

-- Build a value from plain bytes and integers, failing the test on an
-- invalid key or quantity.
mk :: [(ByteString, [(ByteString, Integer)])] -> Value
mk xs = pack (Map.fromList [(key c, Map.fromList [(key t, q n) | (t, n) <- ts]) | (c, ts) <- xs])
  where
    key = maybe (error "mk: key") id . k
    q = maybe (error "mk: quantity") id . quantity

golden :: TestTree
golden =
  testGroup
    "Golden, from plutus-core"
    ( concat
        [ [ testCase (label <> ", encoded") (encodeProgram (program (CValue v)) @?= hex bytes)
          , testCase (label <> ", decoded") (decodeProgram (hex bytes) @?= Right (program (CValue v)))
          ]
        | (label, v, bytes) <-
            [ ("empty", mk [], "0101004e81")
            , ("one coin", mk [(hex "aa", [("T", 5)])], "0101004ea101aa00810154000a01")
            , ( "two currencies"
              , mk [(hex "aa", [("A", 1), ("B", -2)]), (hex "bbcc", [("", 7)])]
              , "0101004ea101aa00810141000281014200034102bbcc0081000e01"
              )
            , ("empty keys, largest quantity", mk [("", [("", 2 ^ (127 :: Int) - 1)])], "0101004ea1008100feffffffffffffffffffffffffffffffffff0301")
            , ( "32-byte keys, smallest quantity"
              , mk [(BS.replicate 32 0xff, [(BS.replicate 32 0x01, negate (2 ^ (127 :: Int)))])]
              , "0101004ea120" <> ff 32 <> "008120" <> concat (replicate 32 "01") <> "00ffffffffffffffffffffffffffffffffffff0301"
              )
            ]
        ]
        <> [ testCase "a list of values, so the type tags are [7,5,13]" $ do
               let p = program (CList TyValue [CValue (mk []), CValue (mk [(hex "aa", [("T", 5)])])])
               encodeProgram p @?= hex "0101004bd7ab0101aa00810154000a01"
               decodeProgram (hex "0101004bd7ab0101aa00810154000a01") @?= Right p
           ]
    )
  where
    ff n = concat (replicate n "ff")

-- Inputs the probe's decoder accepts. The last is written with an extra zero
-- group, which plutus-core reads and then writes shorter.
reading :: TestTree
reading =
  testGroup
    "Reading what plutus-core accepts"
    [ testCase label (decodeProgram (inProgram bytes) @?= Right (program (CValue v)))
    | (label, bytes, v) <-
        [ ("a currency of 32 bytes", "8120" <> concat (replicate 32 "01") <> "00810174000201", mk [(BS.replicate 32 1, [("t", 1)])])
        , ("the largest quantity", "8101630081017400feffffffffffffffffffffffffffffffffff0301", mk [("c", [("t", 2 ^ (127 :: Int) - 1)])])
        , ("the smallest quantity", "8101630081017400ffffffffffffffffffffffffffffffffffff0301", mk [("c", [("t", negate (2 ^ (127 :: Int)))])])
        , ("a key and the same key plus a zero byte", "8101610081017400024102610000810174000201", mk [("a", [("t", 1)]), ("a\0", [("t", 1)])])
        , ("a quantity written with an extra group", "81016300810174008a0001", mk [("c", [("t", 5)])])
        ]
    ]

-- Offsets are in bits from the start of the program, taken before the
-- offending item's padding.
refusals :: TestTree
refusals =
  testGroup
    "Refusals"
    [ testCase label (decodeProgram (inProgram bytes) @?= Left (InvalidValue e at))
    | (label, bytes, e, at) <-
        [ ("a currency of 33 bytes", "812101010101010101010101010101010101010101010101010101010101010101010100810174000201", KeyTooLong,35)
        , ("a token name of 33 bytes", "810163008121010101010101010101010101010101010101010101010101010101010101010101000201", KeyTooLong,65)
        , ("a zero quantity", "81016300810174000001", ZeroQuantity,96)
        , ("a zero quantity written with an extra group", "8101630081017400800001", ZeroQuantity,96)
        , ("a quantity of 2^127", "81016300810174008080808080808080808080808080808080800401", QuantityOutOfRange,96)
        , ("a quantity of -2^127 - 1", "81016300810174008180808080808080808080808080808080800401", QuantityOutOfRange,96)
        , ("a currency with no tokens", "8101630001", EmptyTokenList,35)
        , ("currencies descending", "81016200810174000241016100810174000201", CurrenciesNotAscending,106)
        , ("a currency twice", "81016100810174000241016100810175000201", CurrenciesNotAscending,106)
        , ("tokens descending", "810163008101620002810161000201", TokensNotAscending,105)
        , ("a token twice", "810163008101610002810161000401", TokensNotAscending,105)
        ]
    ]

-- The probe prints a value on its own, a flat list that starts with its
-- first cons bit and a filler. A program puts the same list after the type
-- tags, which end a few bits into a byte, so after the first byte the two
-- are the same bytes. This does not work for the empty list.
inProgram :: String -> ByteString
inProgram valueAlone = hex "0101004ea1" <> BS.drop 1 (hex valueAlone)

hex :: String -> ByteString
hex = BS.pack . go
  where
    go (a : b : rest) = case readHex [a, b] of
      [(w, "")] -> w : go rest
      _ -> error ("not hex: " <> [a, b])
    go _ = []
