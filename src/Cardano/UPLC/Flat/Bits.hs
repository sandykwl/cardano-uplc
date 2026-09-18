{- | Reading and writing the pieces the format is made of: bits, naturals,
integers, byte strings, lists and padding. Nothing here knows about terms.

Flat packs to the bit. A four-bit tag takes four bits and the next field
starts mid-byte, so both the writer and the reader keep track of a bit
position. Within a byte, bits go most significant first.

@since 0.1.0
-}
module Cardano.UPLC.Flat.Bits (
  -- * Writing
  Out,
  emptyOut,
  runOut,
  pushBit,
  pushBits,
  pushFiller,
  pushWord64,
  pushInteger,
  pushByteString,
  pushText,
  pushListWith,

  -- * Reading
  Get,
  runGet,
  getOffset,
  getFail,
  getBit,
  getBits,
  getWord64,
  getInteger,
  getFiller,
  getByteString,
  getText,
  getListWith,
  getEnd,
) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.List (foldl')
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word64, Word8)
import Numeric.Natural (Natural)
import Cardano.UPLC.Flat.Error (DecodeError (..))

{- | Writer state: the bytes finished so far, plus the byte being filled and
how many of its bits are used. The small fields are strict, so a large
script turns into bytes as it goes instead of piling up thunks.

Only this module can look inside an t'Out'. That keeps byte alignment
safe: 'pushByteString' is the only writer that needs it, and it pads
first.

@since 0.1.0
-}
data Out = Out !Builder !Word8 !Int

{- | Nothing written yet.

@since 0.1.0
-}
emptyOut :: Out
emptyOut = Out mempty 0 0

{- | The bytes written so far. A half-filled last byte comes out with its
spare bits set to 0, though a whole program never ends mid-byte, because
its encoder finishes with 'pushFiller'.

@since 0.1.0
-}
runOut :: Out -> ByteString
runOut (Out emitted partial used) =
  LBS.toStrict . B.toLazyByteString $
    if used == 0 then emitted else emitted <> B.word8 partial

{- | Write one bit.

@since 0.1.0
-}
pushBit :: Bool -> Out -> Out
pushBit b (Out emitted partial used) =
  let partial' = if b then partial .|. shiftL 1 (7 - used) else partial
   in if used == 7
        then Out (emitted <> B.word8 partial') 0 0
        else Out emitted partial' (used + 1)

{- | Write the low @n@ bits of a byte, 1 to 8 of them, most significant
first. Tags use this: four bits for term and type tags, seven for
builtins.

@since 0.1.0
-}
pushBits :: Int -> Word8 -> Out -> Out
pushBits n w out = foldl' (\o i -> pushBit (testBit w i) o) out (reverse [0 .. n - 1])

{- | Pad to the next byte boundary: zero or more 0 bits, then a 1. It is
always at least one bit, so an already aligned writer gets a whole
@00000001@ byte. Every program ends with it and every byte string starts
with it.

@since 0.1.0
-}
pushFiller :: Out -> Out
pushFiller out@(Out _ _ used) = pushBits (8 - used) 1 out

-- Seven-bit blocks, least significant first, each in a byte whose top bit
-- says whether another follows. Values below 128 cost one byte.
pushNatural :: Natural -> Out -> Out
pushNatural n out =
  let low = fromIntegral (n .&. 0x7f) :: Word8
      rest = shiftR n 7
   in if rest == 0
        then pushBits 8 low out
        else pushNatural rest (pushBits 8 (0x80 .|. low) out)

{- | Write a word-sized natural, as used for versions, de Bruijn indices
and @constr@ tags.

@since 0.1.0
-}
pushWord64 :: Word64 -> Out -> Out
pushWord64 = pushNatural . fromIntegral

{- | Write an integer of any size. It is zigzagged onto the naturals
first, so numbers near zero stay short whatever their sign.

@since 0.1.0
-}
pushInteger :: Integer -> Out -> Out
pushInteger = pushNatural . zigzag

{- | Write a byte string: padding, then chunks of up to 255 bytes, each
after a one-byte length, then a zero byte.

Chunks are always as full as they can be, because that is what the
reference encoder writes, and a different split would change the script's
hash. The reader accepts any split, as the specification requires.

@since 0.1.0
-}
pushByteString :: ByteString -> Out -> Out
pushByteString bs0 out = go bs0 (pushFiller out)
  where
    -- Aligned after the filler, so chunks go to the builder whole.
    go :: ByteString -> Out -> Out
    go bs (Out emitted _ _)
      | BS.null bs = Out (emitted <> B.word8 0) 0 0
      | otherwise =
          let (chunk, rest) = BS.splitAt 255 bs
              emitted' =
                emitted
                  <> B.word8 (fromIntegral (BS.length chunk))
                  <> B.byteString chunk
           in go rest (Out emitted' 0 0)

{- | Write text: UTF-8, then as a byte string.

@since 0.1.0
-}
pushText :: Text -> Out -> Out
pushText = pushByteString . encodeUtf8

{- | Write a list: a 1 bit before each element and a 0 bit at the end.
Every list in the format uses this, from @constr@ fields to the tags of a
constant's type.

@since 0.1.0
-}
pushListWith :: (a -> Out -> Out) -> [a] -> Out -> Out
pushListWith f xs out =
  pushBit False (foldl' (\o x -> f x (pushBit True o)) out xs)

{- | A reader: given the input and a bit offset, it fails, or returns a
value and the new offset. The instances pass the offset along and stop at
the first error. Order matters, because the format has no lengths or
separators: read fields out of order and you read the wrong bits.

@since 0.1.0
-}
newtype Get a = Get (ByteString -> Int -> Either DecodeError (a, Int))

-- | @since 0.1.0
instance Functor Get where
  fmap g (Get f) = Get $ \bs off ->
    case f bs off of
      Left e -> Left e
      Right (a, off') -> Right (g a, off')

-- | @since 0.1.0
instance Applicative Get where
  pure a = Get $ \_ off -> Right (a, off)
  Get ff <*> Get fa = Get $ \bs off ->
    case ff bs off of
      Left e -> Left e
      Right (g, off') -> case fa bs off' of
        Left e -> Left e
        Right (a, off'') -> Right (g a, off'')

-- | @since 0.1.0
instance Monad Get where
  Get fa >>= k = Get $ \bs off ->
    case fa bs off of
      Left e -> Left e
      Right (a, off') -> let Get fb = k a in fb bs off'

{- | Run a reader from the start of the input. It doesn't insist that the
input is used up; 'getEnd' does that.

@since 0.1.0
-}
runGet :: Get a -> ByteString -> Either DecodeError a
runGet (Get f) bs = fst <$> f bs 0

{- | The current bit offset. Readers take it before a field that can fail,
so the error can say where that field started.

@since 0.1.0
-}
getOffset :: Get Int
getOffset = Get $ \_ off -> Right (off, off)

{- | Stop with the given error.

@since 0.1.0
-}
getFail :: DecodeError -> Get a
getFail e = Get $ \_ _ -> Left e

{- | Read one bit.

@since 0.1.0
-}
getBit :: Get Bool
getBit = Get $ \bs off ->
  let (i, b) = off `quotRem` 8
   in if i >= BS.length bs
        then Left (EndOfInput off)
        else Right (testBit (BS.index bs i) (7 - b), off + 1)

{- | Read @n@ bits, most significant first, for @n@ from 1 to 8.

@since 0.1.0
-}
getBits :: Int -> Get Word8
getBits = go 0
  where
    go :: Word8 -> Int -> Get Word8
    go !acc n
      | n <= 0 = pure acc
      | otherwise = do
          b <- getBit
          go (shiftL acc 1 .|. (if b then 1 else 0)) (n - 1)

-- Reads what pushNatural writes. There is no size limit, because integer
-- constants can be arbitrarily large.
getNatural :: Get Natural
getNatural = go 0 0
  where
    go :: Int -> Natural -> Get Natural
    go !shift !acc = do
      w <- getBits 8
      let acc' = acc .|. shiftL (fromIntegral (w .&. 0x7f) :: Natural) shift
      if testBit w 7 then go (shift + 7) acc' else pure acc'

{- | Read a natural that has to fit a 'Word64'. If it doesn't, that is
'WordOverflow'. Cutting it down instead would quietly decode a different
program.

@since 0.1.0
-}
getWord64 :: Get Word64
getWord64 = do
  start <- getOffset
  n <- getNatural
  if n > fromIntegral (maxBound :: Word64)
    then getFail (WordOverflow start)
    else pure (fromIntegral n)

{- | Read a signed integer of any size.

@since 0.1.0
-}
getInteger :: Get Integer
getInteger = unzigzag <$> getNatural

{- | Read padding: 0s and then a single 1 that ends the byte. Anything else
is 'BadFiller'.

@since 0.1.0
-}
getFiller :: Get ()
getFiller = do
  off <- getOffset
  v <- getBits (8 - (off `mod` 8))
  if v == 1 then pure () else getFail (BadFiller off)

{- | Read a byte string: padding, then length-prefixed chunks up to a zero
byte. Any chunking is accepted, so a string that wasn't split the usual way
comes out with different bytes if it is encoded again.

The result is copied, so keeping a decoded constant doesn't keep the whole
script in memory.

@since 0.1.0
-}
getByteString :: Get ByteString
getByteString = do
  getFiller
  chunks []
  where
    chunks :: [ByteString] -> Get ByteString
    chunks acc = do
      len <- getBits 8
      if len == 0
        then pure (BS.copy (BS.concat (reverse acc)))
        else do
          chunk <- bytes (fromIntegral len)
          chunks (chunk : acc)

    -- The filler landed the reader on a byte boundary.
    bytes :: Int -> Get ByteString
    bytes n = Get $ \bs off ->
      let i = off `div` 8
       in if i + n > BS.length bs
            then Left (EndOfInput (8 * BS.length bs))
            else Right (BS.take n (BS.drop i bs), off + 8 * n)

{- | Read text: a byte string decoded as UTF-8. Bad UTF-8 is an
'InvalidUtf8' error, not an exception.

@since 0.1.0
-}
getText :: Get Text
getText = do
  start <- getOffset
  bs <- getByteString
  case decodeUtf8' bs of
    Left _ -> getFail (InvalidUtf8 start)
    Right t -> pure t

{- | Read a list: an element after each 1 bit, until a 0.

Nothing on the wire says how long a list is, and a small input can hold a
very long one, so each element is forced as it arrives instead of piling up
as thunks.

@since 0.1.0
-}
getListWith :: forall a. Get a -> Get [a]
getListWith element = go []
  where
    go :: [a] -> Get [a]
    go acc = do
      more <- getBit
      if more
        then do
          !x <- element
          go (x : acc)
        else pure (reverse acc)

{- | Check that the whole input has been read; otherwise 'TrailingInput'.

@since 0.1.0
-}
getEnd :: Get ()
getEnd = Get $ \bs off ->
  if off == 8 * BS.length bs
    then Right ((), off)
    else Left (TrailingInput off)

-- Maps 0, -1, 1, -2, 2, ... to 0, 1, 2, 3, 4, ..., so numbers close to zero
-- stay short whatever their sign. In two's complement, -1 is all ones,
-- which would be written as the largest number possible.
zigzag :: Integer -> Natural
zigzag n
  | n >= 0 = fromInteger (2 * n)
  | otherwise = fromInteger (-2 * n - 1)

unzigzag :: Natural -> Integer
unzigzag n
  | even n = toInteger (n `div` 2)
  | otherwise = negate (toInteger ((n + 1) `div` 2))
