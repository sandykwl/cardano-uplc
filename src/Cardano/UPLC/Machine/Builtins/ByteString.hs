{- | The bytestring builtins, on plain 'ByteString'.

Arguments arrive already unlifted and range-checked for the semantics variant.
A @Left@ is the builtin's own failure.

@since 0.1.0
-}
module Cardano.UPLC.Machine.Builtins.ByteString (
  appendByteString,
  consByteString,
  sliceByteString,
  lengthOfByteString,
  indexByteString,
  equalsByteString,
  lessThanByteString,
  lessThanEqualsByteString,
  integerToByteString,
  byteStringToInteger,
) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Word (Word8)
import GHC.Num.Integer (integerLog2)
import Cardano.UPLC.Machine.Builtins.Error (BuiltinError (..))

{- | Concatenation.

@since 0.1.0
-}
appendByteString :: ByteString -> ByteString -> Either BuiltinError ByteString
appendByteString x y = Right (BS.append x y)

{- | Put a byte in front.

@since 0.1.0
-}
consByteString :: Word8 -> ByteString -> Either BuiltinError ByteString
consByteString w s = Right (BS.cons w s)

{- | Start, length, bytestring. Inside the Int64 range both numbers are
clamped: a negative start counts as 0, so slicing 5 bytes from @-3@ gives the
first five, where the specification's formula gives two; this follows
plutus-core. Values outside Int64 are refused before this function is reached.

@since 0.1.0
-}
sliceByteString :: Int64 -> Int64 -> ByteString -> Either BuiltinError ByteString
sliceByteString start len s =
  Right (BS.take (fromIntegral len) (BS.drop (fromIntegral start) s))

{- | The number of bytes.

@since 0.1.0
-}
lengthOfByteString :: ByteString -> Either BuiltinError Integer
lengthOfByteString = Right . toInteger . BS.length

{- | The byte at an index, which must be inside the bytestring.

@since 0.1.0
-}
indexByteString :: ByteString -> Int64 -> Either BuiltinError Word8
indexByteString s i
  | i < 0 || i >= fromIntegral (BS.length s) = Left Failed
  | otherwise = Right (BS.index s (fromIntegral i))

{- | Equality.

@since 0.1.0
-}
equalsByteString :: ByteString -> ByteString -> Either BuiltinError Bool
equalsByteString x y = Right (x == y)

{- | Lexicographic order on unsigned bytes; a proper prefix is smaller.

@since 0.1.0
-}
lessThanByteString :: ByteString -> ByteString -> Either BuiltinError Bool
lessThanByteString x y = Right (x < y)

{- | The same order, or equal.

@since 0.1.0
-}
lessThanEqualsByteString :: ByteString -> ByteString -> Either BuiltinError Bool
lessThanEqualsByteString x y = Right (x <= y)

{- | Big-endian flag, width, input. Width 0 gives the fewest bytes that hold
the input. Any other width gives exactly that many, padded with zero bytes,
and fails if the input does not fit. So 0 at width 12 is twelve zero bytes,
as plutus-core gives; the specification's formal definition gives an empty
string there, against its own prose. The width and the input are both
capped at 8192 bytes, tighter than CIP-0121 allows but what plutus-core
runs. A negative input fails.

@since 0.1.0
-}
integerToByteString :: Bool -> Integer -> Integer -> Either BuiltinError ByteString
integerToByteString big width n
  | width < 0 || width > maxWidth || n < 0 = Left Failed
  -- Checked before the digits are taken, so a huge input is refused cheaply.
  | n >= 1 `shiftL` (8 * fromInteger limit) = Left Failed
  | otherwise = Right (if big then BS.reverse littleEndian else littleEndian)
  where
    maxWidth = 8192
    limit = if width == 0 then maxWidth else width
    littleEndian = bytesOf (if width == 0 then needed else fromInteger width) n
    needed = if n == 0 then 0 else fromIntegral (integerLog2 n) `div` 8 + 1
    -- Exactly k bytes, least significant first. The number is split in halves
    -- like byteStringToInteger joins them, so taking one byte at a time off a
    -- large Integer does not make the work quadratic.
    bytesOf :: Int -> Integer -> ByteString
    bytesOf k m
      | k <= 8 = BS.pack (take k (map (\i -> fromInteger (m `shiftR` (8 * i))) [0 ..]))
      | otherwise =
          let h = k `div` 2
              lo = m .&. (1 `shiftL` (8 * h) - 1)
           in BS.append (bytesOf h lo) (bytesOf (k - h) (m `shiftR` (8 * h)))

{- | Big-endian flag, input. The bytes are an unsigned number, and the empty
bytestring is 0.

@since 0.1.0
-}
byteStringToInteger :: Bool -> ByteString -> Either BuiltinError Integer
byteStringToInteger big s = Right (value (if big then s else BS.reverse s))
  where
    -- Halves are joined by a shift, so a long input is not a quadratic run of
    -- big-integer additions.
    value b = case BS.length b of
      0 -> 0
      1 -> toInteger (BS.head b)
      len ->
        let (hi, lo) = BS.splitAt (len `div` 2) b
         in value hi `shiftL` (8 * BS.length lo) + value lo
