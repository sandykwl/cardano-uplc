{- Tests for the bytestring builtins, through the machine.

Each edge case is one test, run under every semantics variant, with the result
plutus-core 1.70.0.0 gave or its conformance cases state. The properties are
the laws CIP-0121 states and the order laws of the specification.
-}
module Builtins.ByteString (byteStringTests) where

import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.QuickCheck (
  Gen,
  choose,
  counterexample,
  forAll,
  listOf,
  (.&&.),
  (===),
  (==>),
 )
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Cardano.UPLC.Builtin (DefaultFun (..))
import Cardano.UPLC.Constant (Constant (..))
import Cardano.UPLC.Machine (EvalError (..), SemanticsVariant (..), evaluateWith)
import Cardano.UPLC.Name (DeBruijn)
import Cardano.UPLC.Term (Term (..))

byteStringTests :: TestTree
byteStringTests =
  testGroup
    "ByteString builtins"
    [ appendRows
    , consRows
    , sliceRows
    , lengthRows
    , indexRows
    , comparisonRows
    , integerToByteStringRows
    , byteStringToIntegerRows
    , properties
    ]

type T = Term DeBruijn ()

call :: DefaultFun -> [T] -> T
call f = foldl (Apply ()) (Builtin () f)

int :: Integer -> T
int = Constant () . CInteger

bool :: Bool -> T
bool = Constant () . CBool

bytes :: [Word8] -> T
bytes = Constant () . CByteString . BS.pack

-- n bytes of 0x01.
ones :: Int -> T
ones n = bytes (replicate n 1)

-- What a row expects: a result, or the builtin's failure with its cause, which
-- is the term itself unless the term's arguments had to be computed first.
data Out = Ok T | Fail | Failed T

-- A row whose outcome is the same in every variant.
same :: String -> T -> Out -> TestTree
same name term out = row name term out out out out

-- A row by variant: A and B (which never differ here), C, D, E.
row :: String -> T -> Out -> Out -> Out -> Out -> TestTree
row name term ab c d e =
  testCase name $
    mapM_
      (\(v, out) -> check v out)
      [(VariantA, ab), (VariantB, ab), (VariantC, c), (VariantD, d), (VariantE, e)]
  where
    check v out = evaluateWith v term @?= expected out
    expected = \case
      Ok t -> Right t
      Fail -> Left (BuiltinFailure term)
      Failed cause -> Left (BuiltinFailure cause)

appendRows :: TestTree
appendRows =
  testGroup
    "appendByteString"
    [ same "joins two bytestrings" (append (bytes [0, 0xaa, 0xbb, 0xcc]) (bytes [0xff, 0, 0x33])) (Ok (bytes [0, 0xaa, 0xbb, 0xcc, 0xff, 0, 0x33]))
    , same "empty and empty" (append (bytes []) (bytes [])) (Ok (bytes []))
    , row "an argument past 65536 bytes is refused in D and E" (len (append (ones 65537) (bytes []))) (Ok (int 65537)) (Ok (int 65537)) (Failed inner) (Failed inner)
    , same "a result past 65536 bytes is not checked" (len (append (ones 65536) (ones 65536))) (Ok (int 131072))
    ]
  where
    inner = append (ones 65537) (bytes [])
    append x y = call AppendByteString [x, y]
    len x = call LengthOfByteString [x]

consRows :: TestTree
consRows =
  testGroup
    "consByteString"
    [ same "puts a byte in front" (cons 84 (bytes [1, 2])) (Ok (bytes [84, 1, 2]))
    , same "the largest byte" (cons 255 (bytes [])) (Ok (bytes [255]))
    , same "the smallest byte" (cons 0 (bytes [])) (Ok (bytes [0]))
    , row "256 wraps in A, B and D, fails in C and E" (cons 256 (bytes [])) (Ok (bytes [0])) Fail (Ok (bytes [0])) Fail
    , row "-1 wraps to 255" (cons (-1) (bytes [])) (Ok (bytes [255])) Fail (Ok (bytes [255])) Fail
    , row "-256 wraps to 0" (cons (-256) (bytes [])) (Ok (bytes [0])) Fail (Ok (bytes [0])) Fail
    , row "2^64 + 1 wraps to 1" (cons (2 ^ (64 :: Int) + 1) (bytes [])) (Ok (bytes [1])) Fail (Ok (bytes [1])) Fail
    , row "2^262143 is past D's integer bound" (cons bound (bytes [])) (Ok (bytes [0])) Fail Fail Fail
    , row "one below it is inside the bound" (cons (bound - 1) (bytes [])) (Ok (bytes [255])) Fail (Ok (bytes [255])) Fail
    , row "-2^262143 is inside the bound" (cons (negate bound) (bytes [])) (Ok (bytes [0])) Fail (Ok (bytes [0])) Fail
    , row "one below -2^262143 is refused in D" (cons (negate bound - 1) (bytes [])) (Ok (bytes [255])) Fail Fail Fail
    , row "the bytestring past 65536 bytes is refused in D and E" (len (cons 1 (ones 65537))) (Ok (int 65538)) (Ok (int 65538)) (Failed inner) (Failed inner)
    , same "the result may be longer than 65536 bytes" (len (cons 1 (ones 65536))) (Ok (int 65537))
    ]
  where
    inner = cons 1 (ones 65537)
    cons n s = call ConsByteString [int n, s]
    len x = call LengthOfByteString [x]
    bound = 2 ^ (262143 :: Int)

thirteen :: T
thirteen = bytes [1 .. 13]

sliceRows :: TestTree
sliceRows =
  testGroup
    "sliceByteString"
    [ same "from an index" (slice 3 5 thirteen) (Ok (bytes [4 .. 8]))
    , same "a negative start counts as 0, so five bytes come back where the specification's formula gives two" (slice (-3) 5 thirteen) (Ok (bytes [1 .. 5]))
    , same "-2 and 3 on four bytes" (slice (-2) 3 (bytes [0, 1, 2, 3])) (Ok (bytes [0, 1, 2]))
    , same "a long length stops at the end" (slice (-3) 1234 thirteen) (Ok thirteen)
    , same "from the middle" (slice 5 3 thirteen) (Ok (bytes [6, 7, 8]))
    , same "a negative length is empty" (slice 1 (-1) four) (Ok (bytes []))
    , same "start at the end" (slice 4 1 four) (Ok (bytes []))
    , same "length past the end" (slice 3 100 four) (Ok (bytes [3]))
    , same "large start and length inside Int64" (slice 123456789123456789 123456789123456789 thirteen) (Ok (bytes []))
    , same "the largest Int64 for both" (slice maxInt maxInt four) (Ok (bytes []))
    , same "the smallest start, the largest length" (slice minInt maxInt four) (Ok four)
    , same "start past Int64" (slice (maxInt + 1) 1 four) Fail
    , same "length past Int64" (slice 0 (maxInt + 1) four) Fail
    , same "start below Int64" (slice (minInt - 1) 1 four) Fail
    , same "length below Int64" (slice 0 (minInt - 1) four) Fail
    , row "a bytestring past 65536 bytes is refused in D and E" (slice 0 1 (ones 65537)) (Ok (bytes [1])) (Ok (bytes [1])) Fail Fail
    , same "65536 bytes are accepted" (slice 0 1 (ones 65536)) (Ok (bytes [1]))
    ]
  where
    slice s k b = call SliceByteString [int s, int k, b]
    four = bytes [0, 1, 2, 3]
    maxInt = 2 ^ (63 :: Int) - 1
    minInt = negate (2 ^ (63 :: Int))

lengthRows :: TestTree
lengthRows =
  testGroup
    "lengthOfByteString"
    [ same "three bytes" (call LengthOfByteString [bytes [0, 0xff, 0xaa]]) (Ok (int 3))
    , same "empty" (call LengthOfByteString [bytes []]) (Ok (int 0))
    , same "no bound in any variant" (call LengthOfByteString [ones 65537]) (Ok (int 65537))
    ]

indexRows :: TestTree
indexRows =
  testGroup
    "indexByteString"
    [ same "a byte as an integer" (index (bytes [0, 0xff, 0xaa]) 1) (Ok (int 255))
    , same "the first" (index (bytes [0, 0xff, 0xaa]) 0) (Ok (int 0))
    , same "the last" (index (bytes [0, 0xff, 0xaa]) 2) (Ok (int 170))
    , same "past the end" (index (bytes [0]) 1) Fail
    , same "negative" (index (bytes [0, 0xff, 0xaa]) (-1)) Fail
    , same "empty bytestring" (index (bytes []) 0) Fail
    , same "past Int64" (index (bytes [0]) (2 ^ (63 :: Int))) Fail
    , same "far past Int64" (index (bytes [0]) (2 ^ (200 :: Int))) Fail
    , row "a bytestring past 65536 bytes is refused in D and E" (index (ones 65537) 0) (Ok (int 1)) (Ok (int 1)) Fail Fail
    ]
  where
    index b i = call IndexByteString [b, int i]

comparisonRows :: TestTree
comparisonRows =
  testGroup
    "equalsByteString, lessThanByteString, lessThanEqualsByteString"
    [ same "empty equals empty" (eq (bytes []) (bytes [])) (Ok (bool True))
    , same "a longer one is not equal" (eq (bytes [0, 0]) (bytes [0])) (Ok (bool False))
    , same "32 bytes against 1, from amaru-uplc's extra conformance cases" (eq (bytes (replicate 32 0)) (bytes [0])) (Ok (bool False))
    , row "equal bytestrings past 65536 bytes are refused in D and E" (eq (ones 65537) (ones 65537)) (Ok (bool True)) (Ok (bool True)) Fail Fail
    , same "empty is not below empty" (lt (bytes []) (bytes [])) (Ok (bool False))
    , same "empty is below anything else" (lt (bytes []) (bytes [0])) (Ok (bool True))
    , same "a proper prefix is smaller" (lt (bytes [0]) (bytes [0, 0])) (Ok (bool True))
    , same "bytes are unsigned and compared first" (lt (bytes [0xff]) (bytes [0, 0])) (Ok (bool False))
    , same "0x7f is below 0x80" (lt (bytes [0x7f]) (bytes [0x80])) (Ok (bool True))
    , same "a prefix with a longer other" (lt (bytes [0, 0xff]) (bytes [0, 0xff, 0xaa])) (Ok (bool True))
    , same "the specification's example" (lt (bytes [0x23, 0x45, 0x67, 0x89]) (bytes [0x24])) (Ok (bool True))
    , same "and its second" (lt (bytes [0x23, 0x45]) (bytes [0x23, 0x45, 0])) (Ok (bool True))
    , same "empty is at most empty" (le (bytes []) (bytes [])) (Ok (bool True))
    , same "00ff is not at most 00" (le (bytes [0, 0xff]) (bytes [0])) (Ok (bool False))
    , same "0100 is not at most 01" (le (bytes [1, 0]) (bytes [1])) (Ok (bool False))
    , same "equal is at most" (le (bytes [1]) (bytes [1])) (Ok (bool True))
    , row "lessThan refuses a bytestring past 65536 bytes in D and E" (lt (ones 65537) (bytes [])) (Ok (bool False)) (Ok (bool False)) Fail Fail
    , row "and lessThanEquals" (le (bytes []) (ones 65537)) (Ok (bool True)) (Ok (bool True)) Fail Fail
    ]
  where
    eq x y = call EqualsByteString [x, y]
    lt x y = call LessThanByteString [x, y]
    le x y = call LessThanEqualsByteString [x, y]

integerToByteStringRows :: TestTree
integerToByteStringRows =
  testGroup
    "integerToByteString"
    [ same "zero at width 0 is empty" (itb True 0 0) (Ok (bytes []))
    , same "zero keeps its width, where the specification's formal definition gives an empty string" (itb True 12 0) (Ok (zeros 12))
    , same "and at the largest width, little-endian" (itb False 8192 0) (Ok (zeros 8192))
    , same "little-endian, 404 is 94 01" (itb False 0 404) (Ok (bytes [0x94, 0x01]))
    , same "big-endian, 404 is 01 94" (itb True 0 404) (Ok (bytes [0x01, 0x94]))
    , same "padded at the end when little-endian" (itb False 5 404) (Ok (bytes [0x94, 1, 0, 0, 0]))
    , same "padded in front when big-endian" (itb True 5 404) (Ok (bytes [0, 0, 0, 1, 0x94]))
    , same "too narrow" (itb True 1 404) Fail
    , same "exact width" (itb True 6 20016001699311) (Ok (bytes [0x12, 0x34, 0x56, 0xab, 0xcd, 0xef]))
    , same "extra width, little-endian" (itb False 12 20016001699311) (Ok (bytes ([0xef, 0xcd, 0xab, 0x56, 0x34, 0x12] ++ replicate 6 0)))
    , same "width one short" (itb True 5 20016001699311) Fail
    , same "2^64 takes nine bytes" (itb True 0 (2 ^ (64 :: Int))) (Ok (bytes (1 : replicate 8 0)))
    , same "negative input" (itb True 20 (-5)) Fail
    , same "very negative input at width 0" (itb True 0 (negate (2 ^ (65536 :: Int)))) Fail
    , same "negative width" (itb True (-20) 5) Fail
    , same "width past 8192" (itb True 8193 0) Fail
    , same "width 2^29" (itb True (2 ^ (29 :: Int)) 0) Fail
    , same "width 2^64" (itb True (2 ^ (64 :: Int)) 0) Fail
    , same "the largest input at width 0, big-endian" (itb True 0 maxInput) (Ok (ffs 8192))
    , same "and little-endian" (itb False 0 maxInput) (Ok (ffs 8192))
    , same "one more, big-endian" (itb True 0 (maxInput + 1)) Fail
    , same "and little-endian" (itb False 0 (maxInput + 1)) Fail
    , same "the largest input at width 8192" (itb True 8192 maxInput) (Ok (ffs 8192))
    , same "and little-endian" (itb False 8192 maxInput) (Ok (ffs 8192))
    , same "width 8191 is too narrow, big-endian" (itb True 8191 maxInput) Fail
    , same "and little-endian" (itb False 8191 maxInput) Fail
    , same "2^65536 does not fit width 8192, big-endian" (itb True 8192 (maxInput + 1)) Fail
    , same "and little-endian" (itb False 8192 (maxInput + 1)) Fail
    , same "width 8193 fails little-endian too" (itb False 8193 0) Fail
    , -- The failing call is reported with its argument already computed.
      same "the output of a long bytestring is over the limit"
        (call IntegerToByteString [bool True, int 0, call ByteStringToInteger [bool True, ones 8193]])
        (Failed (call IntegerToByteString [bool True, int 0, int ((256 ^ (8193 :: Int) - 1) `div` 255)]))
    , localOption (mkTimeout 2000000) $
        testCase "the largest input is not quadratic in its length" $
          mapM_
            (\k -> evaluateWith VariantE (itb True 0 (maxInput - k)) @?= Right (bytes (replicate 8191 0xff ++ [0xff - fromInteger k])))
            [0 .. 99]
    , same "a bytestring just inside the limit comes back" (call IntegerToByteString [bool True, int 8192, call ByteStringToInteger [bool True, ones 8192]]) (Ok (ones 8192))
    ]
  where
    itb e w n = call IntegerToByteString [bool e, int w, int n]
    zeros n = bytes (replicate n 0)
    ffs n = bytes (replicate n 0xff)
    maxInput = 2 ^ (65536 :: Int) - 1

byteStringToIntegerRows :: TestTree
byteStringToIntegerRows =
  testGroup
    "byteStringToInteger"
    [ same "empty, big-endian" (bti True (bytes [])) (Ok (int 0))
    , same "empty, little-endian" (bti False (bytes [])) (Ok (int 0))
    , same "big-endian" (bti True six) (Ok (int 20016001699311))
    , same "little-endian" (bti False six) (Ok (int 263666621887506))
    , same "leading zero, big-endian" (bti True (bytes [0, 1, 1])) (Ok (int 257))
    , same "leading zero, little-endian" (bti False (bytes [0, 1, 1])) (Ok (int 65792))
    , same "zeros, big-endian" (bti True (zeros 13)) (Ok (int 0))
    , same "zeros, little-endian" (bti False (zeros 13)) (Ok (int 0))
    , -- The result is far past D's integer bound, and only the next builtin
      -- that reads it would refuse it.
      row "an input past 65536 bytes is refused in D and E" (bti True (ones 65537)) (Ok (int allOnes)) (Ok (int allOnes)) Fail Fail
    , same "65536 bytes are accepted" (bti False (ones 65536)) (Ok (int (allOnes `div` 256)))
    ]
  where
    bti e b = call ByteStringToInteger [bool e, b]
    six = bytes [0x12, 0x34, 0x56, 0xab, 0xcd, 0xef]
    zeros n = bytes (replicate n 0)
    -- 65537 bytes of 0x01 is the sum of 256^i for i below 65537.
    allOnes = (256 ^ (65537 :: Int) - 1) `div` 255

properties :: TestTree
properties =
  testGroup
    "Properties"
    [ testProperty "CIP-0121: a number survives a trip through bytes of any width that holds it" $
        forAll ((,,) <$> arbitrary' <*> choose (0, 40) <*> arbitraryNatural) $ \(big, extra, n) ->
          let w = minimalWidth n + extra
              enc = evalBytes (call IntegerToByteString [bool big, int (toInteger w), int n])
           in counterexample (show enc) $
                (enc >>= \b -> evalInt (call ByteStringToInteger [bool big, bytes (BS.unpack b)])) === Just n
    , testProperty "CIP-0121: width 0 gives the fewest bytes, and they decode back" $
        forAll ((,) <$> arbitrary' <*> arbitraryNatural) $ \(big, n) ->
          let enc = evalBytes (call IntegerToByteString [bool big, int 0, int n])
           in (BS.length <$> enc) === Just (minimalWidth n)
                .&&. (enc >>= \b -> evalInt (call ByteStringToInteger [bool big, bytes (BS.unpack b)])) === Just n
    , testProperty "CIP-0121: bytes survive a trip through the integer at their own length" $
        forAll ((,) <$> arbitrary' <*> (listOf (choose (0, 255)) :: Gen [Word8])) $ \(big, bs) ->
          not (null bs) ==>
            let int' = evalInt (call ByteStringToInteger [bool big, bytes bs])
                back = int' >>= \n -> evalBytes (call IntegerToByteString [bool big, int (toInteger (length bs)), int n])
             in back === Just (BS.pack bs)
    , testProperty "the two endiannesses read the reverse of each other" $
        forAll (listOf (choose (0, 255)) :: Gen [Word8]) $ \bs ->
          evalInt (call ByteStringToInteger [bool True, bytes bs])
            === evalInt (call ByteStringToInteger [bool False, bytes (reverse bs)])
    , testProperty "the order is lexicographic on unsigned bytes" $
        forAll pairGen $ \(x, y) ->
          evalBool (call LessThanByteString [bytes x, bytes y]) === Just (x < y)
    , testProperty "lessThan is irreflexive" $
        forAll smallBytes $ \x -> evalBool (call LessThanByteString [bytes x, bytes x]) === Just False
    , testProperty "exactly one of less, equal, greater" $
        forAll pairGen $ \(x, y) ->
          let lt = evalBool (call LessThanByteString [bytes x, bytes y])
              eq = evalBool (call EqualsByteString [bytes x, bytes y])
              gt = evalBool (call LessThanByteString [bytes y, bytes x])
           in length (filter (== Just True) [lt, eq, gt]) === 1
    , testProperty "lessThanEquals is lessThan or equals" $
        forAll pairGen $ \(x, y) ->
          evalBool (call LessThanEqualsByteString [bytes x, bytes y])
            === ((||) <$> evalBool (call LessThanByteString [bytes x, bytes y]) <*> evalBool (call EqualsByteString [bytes x, bytes y]))
    , testProperty "lessThan is transitive" $
        forAll ((,,) <$> smallBytes <*> smallBytes <*> smallBytes) $ \(x, y, z) ->
          let lt a b = evalBool (call LessThanByteString [bytes a, bytes b]) == Just True
           in (lt x y && lt y z) ==> lt x z
    , testProperty "slice matches the list model with the start and length clamped to zero" $
        forAll ((,,) <$> choose (-20, 20) <*> choose (-20, 20) <*> smallBytes) $ \(s, k, bs) ->
          evalBytes (call SliceByteString [int s, int k, bytes bs])
            === Just (BS.pack (take (fromInteger k) (drop (fromInteger s) bs)))
    , testProperty "index agrees with the list, and fails outside it" $
        forAll ((,) <$> choose (-3, 12) <*> smallBytes) $ \(i, bs) ->
          let t = call IndexByteString [bytes bs, int i]
           in evaluateWith VariantE t
                === if i >= 0 && fromInteger i < length bs
                  then Right (int (toInteger (bs !! fromInteger i)))
                  else Left (BuiltinFailure t)
    ]
  where
    arbitrary' = choose (False, True)
    smallBytes = listOf (choose (0, 3)) :: Gen [Word8]
    pairGen = (,) <$> smallBytes <*> smallBytes
    arbitraryNatural = do
      len <- choose (0, 40)
      bs <- mapM (const (choose (0, 255))) [1 .. len :: Int]
      pure (foldl (\a b -> a * 256 + toInteger (b :: Word8)) 0 bs)

-- How many bytes width 0 should produce.
minimalWidth :: Integer -> Int
minimalWidth = go 0
  where
    go acc 0 = acc
    go acc m = go (acc + 1) (m `div` 256)

evalBool :: T -> Maybe Bool
evalBool t = case evaluateWith VariantE t of
  Right (Constant () (CBool b)) -> Just b
  _ -> Nothing

evalInt :: T -> Maybe Integer
evalInt t = case evaluateWith VariantE t of
  Right (Constant () (CInteger n)) -> Just n
  _ -> Nothing

evalBytes :: T -> Maybe BS.ByteString
evalBytes t = case evaluateWith VariantE t of
  Right (Constant () (CByteString b)) -> Just b
  _ -> Nothing
