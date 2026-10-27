{- | The bitwise builtins, on plain 'ByteString'.

Arguments arrive already unlifted and range-checked for the semantics variant.
A @Left@ is the builtin's own failure. The shift and rotation amounts are
'Integer' because variants A to C accept any amount.

@since 0.1.0
-}
module Cardano.UPLC.Machine.Builtins.Bitwise (
  andByteString,
  orByteString,
  xorByteString,
  complementByteString,
  readBit,
  writeBits,
  replicateByte,
  shiftByteString,
  rotateByteString,
  countSetBits,
  findFirstSetBit,
) where

import Data.Bits (
  clearBit,
  complement,
  countTrailingZeros,
  popCount,
  setBit,
  shiftL,
  shiftR,
  testBit,
  xor,
  (.&.),
  (.|.),
 )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Word (Word8)
import Cardano.UPLC.Machine.Builtins.Error (BuiltinError (..))

-- The inputs line up at their first byte. Without padding the longer one's
-- extra bytes are dropped. With it they are kept, which is the same as padding
-- the shorter input with the operation's identity byte.
logical :: (Word8 -> Word8 -> Word8) -> Bool -> ByteString -> ByteString -> ByteString
logical op padded x y
  | padded = common <> BS.drop (BS.length common) x <> BS.drop (BS.length common) y
  | otherwise = common
  where
    common = BS.pack (BS.zipWith op x y)

{- | Bitwise and of two bytestrings.

@since 0.1.0
-}
andByteString :: Bool -> ByteString -> ByteString -> Either BuiltinError ByteString
andByteString padded x y = Right (logical (.&.) padded x y)

{- | Bitwise or of two bytestrings.

@since 0.1.0
-}
orByteString :: Bool -> ByteString -> ByteString -> Either BuiltinError ByteString
orByteString padded x y = Right (logical (.|.) padded x y)

{- | Bitwise exclusive or of two bytestrings.

@since 0.1.0
-}
xorByteString :: Bool -> ByteString -> ByteString -> Either BuiltinError ByteString
xorByteString padded x y = Right (logical xor padded x y)

{- | Flip every bit.

@since 0.1.0
-}
complementByteString :: ByteString -> Either BuiltinError ByteString
complementByteString = Right . BS.map complement

{- | The bit at an index, counted from the least significant end of the whole
bytestring.

@since 0.1.0
-}
readBit :: ByteString -> Int64 -> Either BuiltinError Bool
readBit bs ix
  | ix < 0 || toInteger ix >= 8 * toInteger n = Left Failed
  | otherwise = Right (testBit (BS.index bs (n - 1 - i `div` 8)) (i `mod` 8))
  where
    n = BS.length bs
    i = fromIntegral ix :: Int

{- | Set or clear the bits at the given indices. Input, bit indices, the value
to write at each.

@since 0.1.0
-}
writeBits :: ByteString -> [Integer] -> Bool -> Either BuiltinError ByteString
writeBits bs ixs value
  | any (\i -> i < 0 || i >= 8 * toInteger n) ixs = Left Failed
  | otherwise = Right (BS.pack (go 0 (BS.unpack bs) sorted))
  where
    n = BS.length bs
    -- Highest bit first, which is first byte first.
    sorted = sortBy (comparing Down) (map fromInteger ixs) :: [Int]
    write = if value then setBit else clearBit
    go _ [] _ = []
    go pos (b : rest) is =
      let (here, later) = span (\i -> n - 1 - i `div` 8 == pos) is
      in foldl (\acc i -> write acc (i `mod` 8)) b here : go (pos + 1) rest later

{- | A bytestring of one repeated byte. Length, byte.

@since 0.1.0
-}
replicateByte :: Integer -> Word8 -> Either BuiltinError ByteString
replicateByte len byte
  | len < 0 || len > 8192 = Left Failed
  | otherwise = Right (BS.replicate (fromInteger len) byte)

-- Shift towards byte 0 for a positive amount, the other way for a negative
-- one, filling with zero bits. The amount is less than the bit length.
shiftBits :: ByteString -> Int -> ByteString
shiftBits bs k = BS.pack (map byteAt [0 .. n - 1])
  where
    n = BS.length bs
    (q, r) = abs k `divMod` 8
    at i = if i >= 0 && i < n then BS.index bs i else 0
    byteAt j
      | k >= 0 = (at (j + q) `shiftL` r) .|. (at (j + q + 1) `shiftR` (8 - r))
      | otherwise = (at (j - q) `shiftR` r) .|. (at (j - q - 1) `shiftL` (8 - r))

{- | Move the bits towards byte 0 for a positive amount and away from it for a
negative one, keeping the length.

@since 0.1.0
-}
shiftByteString :: ByteString -> Integer -> Either BuiltinError ByteString
shiftByteString bs k
  | abs k >= bits = Right (BS.replicate (BS.length bs) 0)
  | otherwise = Right (shiftBits bs (fromInteger k))
  where
    bits = 8 * toInteger (BS.length bs)

{- | Rotate the bits towards byte 0 for a positive amount, wrapping round.

@since 0.1.0
-}
rotateByteString :: ByteString -> Integer -> Either BuiltinError ByteString
rotateByteString bs k
  | BS.null bs || r == 0 = Right bs
  | otherwise = Right (BS.pack (BS.zipWith (.|.) (shiftBits bs r') (shiftBits bs (r' - n))))
  where
    bits = 8 * toInteger (BS.length bs)
    r = k `mod` bits
    r' = fromInteger r
    n = fromInteger bits

{- | The number of bits set.

@since 0.1.0
-}
countSetBits :: ByteString -> Either BuiltinError Integer
countSetBits = Right . toInteger . BS.foldl' (\acc b -> acc + popCount b) 0

{- | The lowest index of a set bit, or -1 when none is.

@since 0.1.0
-}
findFirstSetBit :: ByteString -> Either BuiltinError Integer
findFirstSetBit bs = Right $ case BS.findIndexEnd (/= 0) bs of
  Nothing -> -1
  Just i -> toInteger ((BS.length bs - 1 - i) * 8 + countTrailingZeros (BS.index bs i))
