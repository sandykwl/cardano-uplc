{- | The CBOR codec for 'Data', with the one function the flat codec needs
and nothing outside the package should: reading a value that may have bytes
after it.

@since 0.1.0
-}
module Cardano.UPLC.Data.Cbor.Internal (
  DataDecodeError (..),
  encodeData,
  decodeData,
  decodeDataPrefix,
) where

import Codec.CBOR.Decoding (Decoder, TokenType (..))
import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Encoding qualified as E
import Codec.CBOR.Magic (nintegerFromBytes, uintegerFromBytes)
import Codec.CBOR.Read (DeserialiseFailure (..), deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import Control.Monad (replicateM, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (unfoldr)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Cardano.UPLC.Data (Data (..))

{- | Why a byte string is not a 'Data' value. Every one carries the byte
offset, from the start of the input, where the problem was found.

@since 0.1.0
-}
data DataDecodeError
  = {- | A definite byte string, or one chunk of an indefinite one, longer
    than 64 bytes; with its length. The chain's limit, not CBOR's.

    @since 0.1.0
    -}
    ByteStringTooLong !Int !Int
  | {- | A tag that is not a constructor tag; with the tag. This includes 2
    and 3 written with a two-byte head, which is not how a big integer starts.

    @since 0.1.0
    -}
    UnknownTag !Int !Word64
  | {- | Tag 102 without exactly two items under it.

    @since 0.1.0
    -}
    BadConstrPair !Int
  | {- | Tag 2 or 3 followed by something other than a byte string.

    @since 0.1.0
    -}
    BadBigInteger !Int
  | {- | An item 'Data' has no shape for, such as text or a float; with what
    the reader saw.

    @since 0.1.0
    -}
    NotData !Int !Text
  | {- | One whole value, then more bytes; the offset is the first one left
    over. Only 'decodeData' reports this.

    @since 0.1.0
    -}
    TrailingBytes !Int
  | {- | Anything the CBOR reader itself reports, with its message as given,
    such as running out of input.

    @since 0.1.0
    -}
    CborError !Int !Text
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )

{- | Write a value as CBOR, byte for byte as plutus-core does.

@since 0.1.0
-}
encodeData :: Data -> ByteString
encodeData = toStrictByteString . putData

putData :: Data -> Encoding
putData = \case
  Constr i ds
    | 0 <= i && i < 7 -> E.encodeTag (fromIntegral (121 + i)) <> putList ds
    | 7 <= i && i < 128 -> E.encodeTag (fromIntegral (1280 + i - 7)) <> putList ds
    | otherwise -> E.encodeTag 102 <> E.encodeListLen 2 <> putIndex i <> putList ds
  Map ps -> E.encodeMapLen (fromIntegral (length ps)) <> foldMap (\(k, v) -> putData k <> putData v) ps
  List ds -> putList ds
  I n -> putInteger n
  B b -> putBytes b
  where
    -- plutus-core writes an index outside Word64 too, though no reader
    -- accepts it back; the bytes are kept the same.
    putIndex i
      | 0 <= i && i <= word64Max = E.encodeWord64 (fromIntegral i)
      | otherwise = E.encodeInteger i

-- Empty lists are definite and the rest indefinite, as the serialise
-- library writes them for plutus-core.
putList :: [Data] -> Encoding
putList [] = E.encodeListLen 0
putList ds = E.encodeListLenIndef <> foldMap putData ds <> E.encodeBreak

putInteger :: Integer -> Encoding
putInteger n
  | -1 - word64Max <= n && n <= word64Max = E.encodeInteger n
  | n > 0 = E.encodeTag 2 <> putBytes (bytesBE n)
  | otherwise = E.encodeTag 3 <> putBytes (bytesBE (-1 - n))

-- Longer strings are cut into 64-byte chunks, so no piece breaks the limit
-- every reader enforces.
putBytes :: ByteString -> Encoding
putBytes b
  | BS.length b <= 64 = E.encodeBytes b
  | otherwise = E.encodeBytesIndef <> foldMap E.encodeBytes (chunks b) <> E.encodeBreak
  where
    chunks bs
      | BS.length bs > 64 = let (c, rest) = BS.splitAt 64 bs in c : chunks rest
      | otherwise = [bs]

-- The magnitude of a positive integer, big-endian, shortest form.
bytesBE :: Integer -> ByteString
bytesBE = BS.pack . reverse . unfoldr (\m -> if m == 0 then Nothing else Just (fromIntegral m, m `div` 256))

word64Max :: Integer
word64Max = fromIntegral (maxBound :: Word64)

{- | Read one value that must use the whole input.

@since 0.1.0
-}
decodeData :: ByteString -> Either DataDecodeError Data
decodeData bs = do
  (d, used) <- decodeDataPrefix bs
  if used == BS.length bs then Right d else Left (TrailingBytes used)

{- | Read one value from the front of the input, and say how many bytes it
took. What follows is not looked at. This is how plutus-core reads a @data@
constant, so a flat program whose constant holds @00 00@ decodes as @I 0@,
and the flat codec has to do the same.

@since 0.1.0
-}
decodeDataPrefix :: ByteString -> Either DataDecodeError (Data, Int)
decodeDataPrefix bs = case deserialiseFromBytes (runExceptT getData) (LBS.fromStrict bs) of
  Left (DeserialiseFailure off msg) -> Left (CborError (fromIntegral off) (T.pack msg))
  Right (_, Left e) -> Left e
  Right (rest, Right d) -> Right (d, BS.length bs - fromIntegral (LBS.length rest))

-- cborg's own fail takes only a string, so the reader carries its errors.
type Get s = ExceptT DataDecodeError (Decoder s)

offset :: Get s Int
offset = fromIntegral <$> lift D.peekByteOffset

-- Dispatch on the head byte alone, as plutus-core does. That is what sends
-- c2 and c3 to the big-integer reader and a tag 2 written d8 02 to the
-- constructor reader, which refuses it.
getData :: Get s Data
getData = do
  start <- offset
  lift D.peekTokenType >>= \case
    TypeUInt -> I <$> lift D.decodeInteger
    TypeUInt64 -> I <$> lift D.decodeInteger
    TypeNInt -> I <$> lift D.decodeInteger
    TypeNInt64 -> I <$> lift D.decodeInteger
    TypeInteger -> I <$> getBigInteger
    TypeBytes -> B <$> getBytes
    TypeBytesIndef -> B <$> getBytesIndef
    TypeListLen -> List <$> getList getData
    TypeListLen64 -> List <$> getList getData
    TypeListLenIndef -> List <$> getList getData
    TypeMapLen -> Map <$> getMap
    TypeMapLen64 -> Map <$> getMap
    TypeMapLenIndef -> Map <$> getMap
    TypeTag -> getConstr
    TypeTag64 -> getConstr
    t -> throwE (NotData start (T.pack (show t)))

getBigInteger :: Get s Integer
getBigInteger = do
  tag <- lift D.decodeTag
  start <- offset
  bs <-
    lift D.peekTokenType >>= \case
      TypeBytes -> getBytes
      TypeBytesIndef -> getBytesIndef
      _ -> throwE (BadBigInteger start)
  -- peekTokenType only reports a big integer for tags 2 and 3.
  pure (if tag == 2 then uintegerFromBytes bs else nintegerFromBytes bs)

getBytes :: Get s ByteString
getBytes = do
  start <- offset
  b <- lift D.decodeBytes
  when (BS.length b > 64) (throwE (ByteStringTooLong start (BS.length b)))
  pure b

getBytesIndef :: Get s ByteString
getBytesIndef = lift D.decodeBytesIndef >> go []
  where
    go acc = do
      stop <- lift D.decodeBreakOr
      if stop then pure (BS.concat (reverse acc)) else getBytes >>= \c -> go (c : acc)

getList :: Get s a -> Get s [a]
getList item = lift D.decodeListLenOrIndef >>= maybe (untilBreak item) (`replicateM` item)

getMap :: Get s [(Data, Data)]
getMap = lift D.decodeMapLenOrIndef >>= maybe (untilBreak pair) (`replicateM` pair)
  where
    pair = (,) <$> getData <*> getData

untilBreak :: Get s a -> Get s [a]
untilBreak item = go []
  where
    go acc = do
      stop <- lift D.decodeBreakOr
      if stop then pure (reverse acc) else item >>= \x -> go (x : acc)

getConstr :: Get s Data
getConstr = do
  start <- offset
  lift D.decodeTag64 >>= \case
    102 -> getConstrPair start
    t
      | 121 <= t && t < 128 -> Constr (fromIntegral t - 121) <$> getList getData
      | 1280 <= t && t < 1401 -> Constr (fromIntegral t - 1280 + 7) <$> getList getData
      | otherwise -> throwE (UnknownTag start t)

-- plutus-core reads the index and the fields before it checks the pair's
-- length, so an error in either is reported first; this follows it.
getConstrPair :: Int -> Get s Data
getConstrPair start = do
  len <- lift D.decodeListLenOrIndef
  i <- lift D.decodeWord64
  fields <- getList getData
  case len of
    Nothing -> lift D.decodeBreakOr >>= \done -> unless done (throwE (BadConstrPair start))
    Just n -> unless (n == 2) (throwE (BadConstrPair start))
  pure (Constr (fromIntegral i) fields)
