{- Tests for the bitwise builtins, run through the machine.

The unit tests are edge cases checked against plutus-core 1.70.0.0, each in
the variants it names. The properties compare every builtin with a model in
which a bytestring is an unsigned big-endian Integer.
-}
module Builtins.Bitwise (bitwiseTests) where

import Data.Bits (clearBit, popCount, setBit, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (digitToInt)
import Data.List (foldl')
import Test.QuickCheck (Gen, choose, elements, forAll, frequency, listOf, vectorOf, (===), (==>), (.&&.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Cardano.UPLC.Builtin (DefaultFun (..))
import Cardano.UPLC.Constant (Constant (..))
import Cardano.UPLC.Machine (EvalError (..), SemanticsVariant (..), evaluateWith)
import Cardano.UPLC.Name (DeBruijn)
import Cardano.UPLC.Term (Term (..))
import Cardano.UPLC.Ty (Ty (TyBool, TyInteger))

bitwiseTests :: TestTree
bitwiseTests =
  testGroup
    "Bitwise"
    [ logicalRows
    , bitRows
    , replicateRows
    , shiftRows
    , rotateRows
    , countRows
    , bounds
    , models
    ]

type T = Term DeBruijn ()

call :: DefaultFun -> [T] -> T
call f = foldl (Apply ()) (Builtin () f)

bs :: ByteString -> T
bs = Constant () . CByteString

int :: Integer -> T
int = Constant () . CInteger

bool :: Bool -> T
bool = Constant () . CBool

ints :: [Integer] -> T
ints = Constant () . CList TyInteger . map CInteger

-- A bytestring written as hex digits, for writing expected values.
hex :: String -> ByteString
hex = BS.pack . go
  where
    go (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go _ = []

-- A, B and C behave alike, and so do D and E.
cVariants, eVariants, allVariants :: [SemanticsVariant]
cVariants = [VariantA, VariantB, VariantC]
eVariants = [VariantD, VariantE]
allVariants = cVariants ++ eVariants

gives :: [SemanticsVariant] -> T -> T -> Assertion
gives vs t expected = mapM_ (\v -> evaluateWith v t @?= Right expected) vs

fails :: [SemanticsVariant] -> T -> Assertion
fails vs t = mapM_ check vs
  where
    check v = case evaluateWith v t of
      Left (BuiltinFailure cause) | cause == t -> pure ()
      other -> assertFailure (show v ++ ": expected a builtin failure, got " ++ show other)

logicalRows :: TestTree
logicalRows =
  testGroup
    "and, or, xor, complement"
    [ testCase "and truncating takes the shorter length" $
        gives allVariants (call AndByteString [bool False, bs (hex "4f00"), bs (hex "f4")]) (bs (hex "44"))
    , testCase "and padding keeps the rest of the longer" $
        gives allVariants (call AndByteString [bool True, bs (hex "4f00"), bs (hex "f4")]) (bs (hex "4400"))
    , testCase "or padding" $
        gives allVariants (call OrByteString [bool True, bs (hex "4f00"), bs (hex "f4")]) (bs (hex "ff00"))
    , testCase "xor padding" $
        gives allVariants (call XorByteString [bool True, bs (hex "4f00"), bs (hex "f4")]) (bs (hex "bb00"))
    , testCase "and padding an empty first argument" $
        gives allVariants (call AndByteString [bool True, bs BS.empty, bs (hex "ff")]) (bs (hex "ff"))
    , testCase "and truncating an empty first argument" $
        gives allVariants (call AndByteString [bool False, bs BS.empty, bs (hex "ff")]) (bs BS.empty)
    , testCase "and padding, the first argument longer" $
        gives allVariants (call AndByteString [bool True, bs (hex "00f0ff"), bs (hex "fff0")]) (bs (hex "00f0ff"))
    , testCase "or padding, the first argument longer" $
        gives allVariants (call OrByteString [bool True, bs (hex "00f0ff"), bs (hex "fff0")]) (bs (hex "fff0ff"))
    , testCase "xor truncating" $
        gives allVariants (call XorByteString [bool False, bs (hex "00f0ff"), bs (hex "fff0")]) (bs (hex "ff00"))
    , testCase "complement of the empty bytestring" $
        gives allVariants (call ComplementByteString [bs BS.empty]) (bs BS.empty)
    , testCase "complement flips every bit" $
        gives allVariants (call ComplementByteString [bs (hex "b00b")]) (bs (hex "4ff4"))
    ]

bitRows :: TestTree
bitRows =
  testGroup
    "readBit, writeBits"
    [ testCase "readBit counts from the least significant end of the last byte" $
        mapM_
          (\(i, b) -> gives allVariants (call ReadBit [bs (hex "f4"), int i]) (bool b))
          [(0, False), (1, False), (2, True), (7, True)]
    , testCase "readBit past the last bit fails" $
        fails allVariants (call ReadBit [bs (hex "f4"), int 8])
    , testCase "readBit at a negative index fails" $
        fails allVariants (call ReadBit [bs (hex "f4"), int (-1)])
    , testCase "readBit on the empty bytestring fails" $
        fails allVariants (call ReadBit [bs BS.empty, int 0])
    , testCase "readBit reaches into the first byte" $
        gives allVariants (call ReadBit [bs (hex "f4ff"), int 10]) (bool True)
    , testCase "readBit at an index outside Int64 fails" $
        fails allVariants (call ReadBit [bs (hex "ff"), int (2 ^ (63 :: Int))])
    , testCase "writeBits clears a bit" $
        gives allVariants (call WriteBits [bs (hex "ff"), ints [0], bool False]) (bs (hex "fe"))
    , testCase "writeBits sets the top bit of a byte" $
        gives allVariants (call WriteBits [bs (hex "00"), ints [7], bool True]) (bs (hex "80"))
    , testCase "writeBits with indices in either order" $
        mapM_
          (\is -> gives allVariants (call WriteBits [bs (hex "f4ff"), ints is, bool False]) (bs (hex "f0fd")))
          [[10, 1], [1, 10]]
    , testCase "writeBits fails if any index is out of range" $
        fails allVariants (call WriteBits [bs (hex "ff"), ints [0, 8], bool False])
    , testCase "writeBits with no indices returns the input" $
        gives allVariants (call WriteBits [bs BS.empty, ints [], bool True]) (bs BS.empty)
    , testCase "writeBits on the empty bytestring fails for any index" $
        fails allVariants (call WriteBits [bs BS.empty, ints [0], bool True])
    , testCase "writeBits fails for an index outside Int64" $
        fails allVariants (call WriteBits [bs (hex "ff"), ints [2 ^ (64 :: Int)], bool True])
    , testCase "writeBits fails for a negative index" $
        fails allVariants (call WriteBits [bs (hex "ff"), ints [-1], bool True])
    , testCase "writeBits takes a list of integers, not of booleans" $ do
        let t = call WriteBits [bs (hex "00"), Constant () (CList TyBool [CBool True]), bool True]
        evaluateWith VariantE t @?= Left (BuiltinArgumentType t)
    , testCase "writeBits with a repeated index" $
        gives allVariants (call WriteBits [bs (hex "00"), ints [3, 3], bool True]) (bs (hex "08"))
    ]

replicateRows :: TestTree
replicateRows =
  testGroup
    "replicateByte"
    [ testCase "length 0" $
        gives allVariants (call ReplicateByte [int 0, int 255]) (bs BS.empty)
    , testCase "length 4" $
        gives allVariants (call ReplicateByte [int 4, int 255]) (bs (hex "ffffffff"))
    , testCase "a byte of 256 fails" $
        fails allVariants (call ReplicateByte [int 0, int 256])
    , testCase "a negative byte fails" $
        fails allVariants (call ReplicateByte [int 1, int (-1)])
    , testCase "a negative length fails" $
        fails allVariants (call ReplicateByte [int (-1), int 0])
    , testCase "8192 bytes is the longest" $
        gives allVariants (call ReplicateByte [int 8192, int 0]) (bs (BS.replicate 8192 0))
    , testCase "8193 bytes fails" $
        fails allVariants (call ReplicateByte [int 8193, int 0])
    ]

shiftRows :: TestTree
shiftRows =
  testGroup
    "shiftByteString"
    [ shiftCase "by 1" 1 "d7f8"
    , shiftCase "left by 5" 5 "7f80"
    , shiftCase "right by 5" (-5) "075f"
    , shiftCase "left by a whole byte" 8 "fc00"
    , shiftCase "right by a whole byte" (-8) "00eb"
    , shiftCase "left by 9" 9 "f800"
    , shiftCase "left by 15" 15 "0000"
    , shiftCase "by the bit length" 16 "0000"
    , shiftCase "right by the bit length" (-16) "0000"
    , shiftCase "by the largest Int64" (2 ^ (63 :: Int) - 1) "0000"
    , shiftCase "by the smallest Int64" (negate (2 ^ (63 :: Int))) "0000"
    , testCase "by one more than an Int64: zeros in A to C, a failure in D and E" $ do
        gives cVariants (call ShiftByteString [bs (hex "ebfc"), int (2 ^ (63 :: Int))]) (bs (hex "0000"))
        fails eVariants (call ShiftByteString [bs (hex "ebfc"), int (2 ^ (63 :: Int))])
    , testCase "by one less than an Int64: the same split" $ do
        gives cVariants (call ShiftByteString [bs (hex "ebfc"), int (negate (2 ^ (63 :: Int)) - 1)]) (bs (hex "0000"))
        fails eVariants (call ShiftByteString [bs (hex "ebfc"), int (negate (2 ^ (63 :: Int)) - 1)])
    , testCase "the empty bytestring by 2^64" $ do
        gives cVariants (call ShiftByteString [bs BS.empty, int (2 ^ (64 :: Int))]) (bs BS.empty)
        fails eVariants (call ShiftByteString [bs BS.empty, int (2 ^ (64 :: Int))])
    , testCase "by 0 returns the input" $
        gives allVariants (call ShiftByteString [bs (hex "ebfc"), int 0]) (bs (hex "ebfc"))
    ]
  where
    shiftCase name k out =
      testCase name $ gives allVariants (call ShiftByteString [bs (hex "ebfc"), int k]) (bs (hex out))

rotateRows :: TestTree
rotateRows =
  testGroup
    "rotateByteString"
    [ rotateCase "by 1" 1 "d7f9"
    , rotateCase "left by 5" 5 "7f9d"
    , rotateCase "right by 5" (-5) "e75f"
    , rotateCase "by a whole byte" 8 "fceb"
    , rotateCase "by the bit length" 16 "ebfc"
    , rotateCase "by 21, which is 5 round again" 21 "7f9d"
    , rotateCase "by -21" (-21) "e75f"
    , rotateCase "by the largest Int64" (2 ^ (63 :: Int) - 1) "75fe"
    , rotateCase "by the smallest Int64, a multiple of the bit length" (negate (2 ^ (63 :: Int))) "ebfc"
    , testCase "by 2^63: unchanged in A to C, a failure in D and E" $ do
        gives cVariants (call RotateByteString [bs (hex "ebfc"), int (2 ^ (63 :: Int))]) (bs (hex "ebfc"))
        fails eVariants (call RotateByteString [bs (hex "ebfc"), int (2 ^ (63 :: Int))])
    , testCase "the empty bytestring by 2^64" $ do
        gives cVariants (call RotateByteString [bs BS.empty, int (2 ^ (64 :: Int))]) (bs BS.empty)
        fails eVariants (call RotateByteString [bs BS.empty, int (2 ^ (64 :: Int))])
    ]
  where
    rotateCase name k out =
      testCase name $ gives allVariants (call RotateByteString [bs (hex "ebfc"), int k]) (bs (hex out))

countRows :: TestTree
countRows =
  testGroup
    "countSetBits, findFirstSetBit"
    [ testCase "countSetBits" $
        mapM_
          (\(s, n) -> gives allVariants (call CountSetBits [bs (hex s)]) (int n))
          [("", 0), ("0100", 1), ("0001", 1), ("ffff", 16), ("010000000000000000", 1)]
    , testCase "findFirstSetBit is -1 when no bit is set" $
        mapM_ (\s -> gives allVariants (call FindFirstSetBit [bs (hex s)]) (int (-1))) ["", "0000"]
    , testCase "findFirstSetBit is the lowest set index" $
        mapM_
          (\(s, n) -> gives allVariants (call FindFirstSetBit [bs (hex s)]) (int n))
          [("0002", 1), ("fff2", 1), ("0100", 8), ("8000", 15), ("f4", 2), ("f400", 10)]
    , -- plutus-core's regression 6453: a set byte followed by whole words of zeros.
      testCase "findFirstSetBit past the first 64 bits" $
        mapM_
          (\(s, n) -> gives allVariants (call FindFirstSetBit [bs (hex s)]) (int n))
          [("01" <> replicate 16 '0', 64), ("01" <> replicate 32 '0', 128)]
    ]

-- The sizes at which variants D and E start to refuse an argument.
bounds :: TestTree
bounds =
  testGroup
    "Bounds"
    [ testCase "and takes 65536 bytes in every variant" $
        gives allVariants (call AndByteString [bool False, bs (full 65536), bs (hex "ff")]) (bs (hex "ff"))
    , testCase "and takes 65537 bytes in A to C, not in D and E" $ do
        gives cVariants (call AndByteString [bool False, bs (full 65537), bs (hex "ff")]) (bs (hex "ff"))
        fails eVariants (call AndByteString [bool False, bs (full 65537), bs (hex "ff")])
        fails eVariants (call AndByteString [bool False, bs (hex "ff"), bs (full 65537)])
    , testCase "complement, countSetBits, findFirstSetBit and readBit share the 65536 limit" $ do
        fails eVariants (call ComplementByteString [bs (full 65537)])
        fails eVariants (call CountSetBits [bs (full 65537)])
        fails eVariants (call FindFirstSetBit [bs (full 65537)])
        fails eVariants (call ReadBit [bs (full 65537), int 0])
        gives cVariants (call CountSetBits [bs (full 65537)]) (int (8 * 65537))
    , testCase "shift and rotate take 65537 bytes in every variant" $ do
        gives allVariants (call ShiftByteString [bs (full 65537), int 1000000]) (bs (BS.replicate 65537 0))
        gives allVariants (call RotateByteString [bs (full 65537), int 0]) (bs (full 65537))
    , testCase "writeBits takes 4096 bytes in every variant" $
        gives allVariants (call WriteBits [bs (BS.replicate 4096 0), ints [], bool True]) (bs (BS.replicate 4096 0))
    , testCase "writeBits takes 4097 bytes in A to C, not in D and E" $ do
        gives cVariants (call WriteBits [bs (BS.replicate 4097 0), ints [], bool True]) (bs (BS.replicate 4097 0))
        fails eVariants (call WriteBits [bs (BS.replicate 4097 0), ints [], bool True])
    ]
  where
    full n = BS.replicate n 0xff

-- A bytestring as an unsigned big-endian Integer, and back.
toInt :: ByteString -> Integer
toInt = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0

fromInt :: Int -> Integer -> ByteString
fromInt n x = BS.pack [fromInteger ((x `shiftR` (8 * i)) .&. 255) | i <- [n - 1, n - 2 .. 0]]

-- Bytestrings of up to 12 bytes, leaning on the bytes that exercise single
-- bits.
genBytes :: Gen ByteString
genBytes = do
  n <- choose (0, 12)
  BS.pack <$> vectorOf n (frequency [(1, elements [0, 1, 0x80, 0xff]), (1, choose (0, 255))])

genPair :: Gen (ByteString, ByteString)
genPair = do
  a <- genBytes
  b <- BS.pack <$> vectorOf (BS.length a) (choose (0, 255))
  pure (a, b)

-- Run a builtin in variant E and read the constant back.
run :: DefaultFun -> [T] -> Maybe Constant
run f args = case evaluateWith VariantE (call f args) of
  Right (Constant () c) -> Just c
  _ -> Nothing

runBytes :: DefaultFun -> [T] -> Maybe ByteString
runBytes f args = case run f args of
  Just (CByteString s) -> Just s
  _ -> Nothing

models :: TestTree
models =
  testGroup
    "Against the Integer model"
    [ testProperty "and, or and xor of equal lengths are .&., .|. and xor" $
        forAll genPair $ \(a, b) ->
          let n = BS.length a
           in and
                [ runBytes f [bool p, bs a, bs b] == Just (fromInt n (op (toInt a) (toInt b)))
                | (f, op) <- [(AndByteString, (.&.)), (OrByteString, (.|.)), (XorByteString, xor)]
                , p <- [False, True]
                ]
    , testProperty "padding gives the longer length and truncating the shorter" $
        forAll genBytes $ \a -> forAll genBytes $ \b ->
          fmap BS.length (runBytes AndByteString [bool True, bs a, bs b]) === Just (max (BS.length a) (BS.length b))
            .&&. fmap BS.length (runBytes OrByteString [bool False, bs a, bs b]) === Just (min (BS.length a) (BS.length b))
    , testProperty "padding is the shorter extended with the operation's neutral byte" $
        forAll genBytes $ \a -> forAll genBytes $ \b ->
          let n = max (BS.length a) (BS.length b)
              ext fill s = s <> BS.replicate (n - BS.length s) fill
           in runBytes AndByteString [bool True, bs a, bs b] === Just (BS.pack (BS.zipWith (.&.) (ext 0xff a) (ext 0xff b)))
                .&&. runBytes OrByteString [bool True, bs a, bs b] === Just (BS.pack (BS.zipWith (.|.) (ext 0 a) (ext 0 b)))
                .&&. runBytes XorByteString [bool True, bs a, bs b] === Just (BS.pack (BS.zipWith xor (ext 0 a) (ext 0 b)))
    , testProperty "complement twice is the identity" $
        forAll genBytes $ \a ->
          (runBytes ComplementByteString [bs a] >>= \c -> runBytes ComplementByteString [bs c]) === Just a
    , testProperty "complement is xor with all ones" $
        forAll genBytes $ \a ->
          runBytes ComplementByteString [bs a] === Just (fromInt (BS.length a) (toInt a `xor` (2 ^ (8 * BS.length a) - 1)))
    , testProperty "shift is shiftL or shiftR, cut to the length" $
        forAll genBytes $ \a -> forAll (choose (-110, 110)) $ \k ->
          let n = BS.length a
              x = toInt a
              expected
                | k >= 0 = fromInt n (x `shiftL` fromInteger k)
                | otherwise = fromInt n (x `shiftR` fromInteger (negate k))
           in runBytes ShiftByteString [bs a, int k] === Just expected
    , testProperty "shift by a very large amount gives zeros" $
        forAll genBytes $ \a -> forAll (elements [1, -1]) $ \sign ->
          runBytes ShiftByteString [bs a, int (sign * 2 ^ (62 :: Int))] === Just (BS.replicate (BS.length a) 0)
    , testProperty "rotate is the Integer rotation within the length" $
        forAll genBytes $ \a -> forAll (choose (-300, 300)) $ \k ->
          not (BS.null a) ==>
            let bits = 8 * toInteger (BS.length a)
                r = k `mod` bits
                x = toInt a
                rotated = ((x `shiftL` fromInteger r) .|. (x `shiftR` fromInteger (bits - r))) .&. (2 ^ bits - 1)
             in runBytes RotateByteString [bs a, int k] === Just (fromInt (BS.length a) rotated)
    , testProperty "rotating by the length is the identity" $
        forAll genBytes $ \a -> forAll (choose (-3, 3)) $ \m ->
          runBytes RotateByteString [bs a, int (m * 8 * toInteger (BS.length a))] === Just a
    , testProperty "rotating one way then the other is the identity" $
        forAll genBytes $ \a -> forAll (choose (-300, 300)) $ \k ->
          (runBytes RotateByteString [bs a, int k] >>= \r -> runBytes RotateByteString [bs r, int (negate k)]) === Just a
    , testProperty "countSetBits is popCount" $
        forAll genBytes $ \a ->
          run CountSetBits [bs a] === Just (CInteger (toInteger (popCount (toInt a))))
    , testProperty "readBit is testBit inside the length, a failure outside" $
        forAll genBytes $ \a -> forAll (choose (-3, 8 * 12 + 3)) $ \i ->
          let inside = i >= 0 && i < 8 * toInteger (BS.length a)
              t = call ReadBit [bs a, int i]
           in evaluateWith VariantE t
                === if inside then Right (bool (testBit (toInt a) (fromInteger i))) else Left (BuiltinFailure t)
    , testProperty "findFirstSetBit is the trailing zero count, or -1" $
        forAll genBytes $ \a ->
          let x = toInt a
              expected = if x == 0 then -1 else toInteger (length (takeWhile (not . testBit x) [0 ..]))
           in run FindFirstSetBit [bs a] === Just (CInteger expected)
    , testProperty "writeBits sets or clears each listed bit" $
        forAll genBytes $ \a -> forAll (listOf (choose (0, 8 * 12))) $ \is -> forAll (elements [False, True]) $ \u ->
          let n = BS.length a
              valid = all (< 8 * toInteger n) is
              write acc i = if u then setBit acc (fromInteger i) else clearBit acc (fromInteger i)
              t = call WriteBits [bs a, ints is, bool u]
           in evaluateWith VariantE t
                === if valid then Right (bs (fromInt n (foldl' write (toInt a) is))) else Left (BuiltinFailure t)
    , testProperty "a written bit reads back" $
        forAll genBytes $ \a -> forAll (elements [False, True]) $ \u ->
          not (BS.null a) ==> forAll (choose (0, 8 * toInteger (BS.length a) - 1)) $ \i ->
            (runBytes WriteBits [bs a, ints [i], bool u] >>= \w -> run ReadBit [bs w, int i]) === Just (CBool u)
    ]
