{- | The flat codec: the binary form scripts are stored in on chain.

A script is identified by a hash of its bytes, so 'encodeProgram' has to
write exactly what the reference implementation writes, and 'decodeProgram'
has to turn away the same inputs the reference does. Both work on de Bruijn
terms only, since scripts on chain carry no names.

On chain, a script also sits inside a CBOR byte string. That wrapper isn't
part of flat, so strip it before calling 'decodeProgram'.

== The format

A program is its version, then its term, then padding up to the next byte
boundary. The version is three naturals: major, minor and patch.

Every term begins with a four-bit tag, in the order the constructors of
t'Cardano.UPLC.Term.Term' are declared:

> 0  var       index, a natural
> 1  delay     term
> 2  lam       term                 the parameter writes nothing
> 3  apply     term, term           function, then argument
> 4  constant  type tags, value
> 5  force     term
> 6  error
> 7  builtin   a seven-bit tag, the position in DefaultFun
> 8  constr    tag as a natural, list of terms     version 1.1.0 and later
> 9  case      term, list of terms                 version 1.1.0 and later

Tags 10 to 15 are reserved.

A constant starts with its type, written as a list of four-bit tags. Tag 7
means application, which is how list, array and pair are given their
element types:

> integer 0  bytestring 1  string 2  unit 3  bool 4  data 8
> bls12_381_G1_element 9  bls12_381_G2_element 10  bls12_381_MlResult 11
> list t      = 7, 5, tags of t
> array t     = 7, 12, tags of t
> pair t1 t2  = 7, 7, 6, tags of t1, tags of t2

So a list of integers is @[7,5,0]@, and a pair of an integer and a bool is
@[7,7,6,0,4]@. Tag 13 is the ledger's @value@ type, and tags 14 and 15 are
unassigned.

The value comes straight after the type, with no tag of its own:

> integer      zigzagged onto the naturals, then written as a natural
> bytestring   padding, then chunks of up to 255 bytes, each after a
>              one-byte length, then a zero byte
> string       its UTF-8 bytes, as a bytestring
> unit         nothing
> bool         one bit
> list, array  a list of values
> pair         the two values in order
> data         its CBOR bytes, as a bytestring
> value        a list of currencies, each a bytestring and then a list of
>              tokens, each a bytestring and then an integer

A natural is written seven bits at a time, lowest first, each group in a
byte whose top bit says whether another follows. A list is a 1 bit before
each element and a 0 bit at the end. Padding is zero or more 0 bits and
then a 1, so it is always at least one bit long.

== What the decoder turns away

* reserved term tags, 10 to 15
* @constr@ and @case@ in programs older than version 1.1.0
* builtin tags beyond the ones this release knows
* type tags that don't form a type
* @value@ constants that break the type's rules: a key over 32 bytes, a
  quantity outside 128 bits or equal to zero, a currency with no tokens, or
  currencies or tokens not in strictly ascending order
* padding whose closing 1 doesn't end a byte
* anything after the final padding

It also turns away values of the BLS12-381 types, because flat never
carries them: a script holds a curve point as compressed bytes and
uncompresses it at run time, and an MlResult only exists while a script
runs. The reference refuses them the same way.

A @data@ constant whose bytes are not a 'Cardano.UPLC.Data.Data' value is
turned away too. Bytes after one whole value are not: the reference reads the
value and ignores the rest, so a constant holding @00 00@ is @I 0@. It doesn't
check that every variable has a binder; that's a separate pass.

@since 0.1.0
-}
module Cardano.UPLC.Flat (
  -- * Encoding
  encodeProgram,

  -- * Decoding
  decodeProgram,
  DecodeError (..),
  ValueError (..),
) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Data.Vector.Strict qualified as Strict
import Data.Word (Word8)
import Cardano.UPLC.Builtin (DefaultFun)
import Cardano.UPLC.Constant (Constant (..))
import Cardano.UPLC.Data.Cbor.Internal (decodeDataPrefix, encodeData)
import Cardano.UPLC.Flat.Bits (
  Get,
  Out,
  emptyOut,
  getBit,
  getBits,
  getByteString,
  getEnd,
  getFail,
  getFiller,
  getInteger,
  getListWith,
  getOffset,
  getText,
  getNatural,
  getWord64,
  pushBit,
  pushBits,
  pushByteString,
  pushFiller,
  pushInteger,
  pushListWith,
  pushText,
  pushNatural,
  pushWord64,
  runGet,
  runOut,
 )
import Cardano.UPLC.Flat.Error (DecodeError (..), ValueError (..))
import Cardano.UPLC.Name (DeBruijn (DeBruijn))
import Cardano.UPLC.Term (Program (Program), Term (..), Version (Version))
import Cardano.UPLC.Ty (Ty (..))
import Cardano.UPLC.Value (K, Value, Quantity, k, pack, quantity, unK, unQuantity, unpack)

{- | Encode a program. Annotations are not written: flat has no place for
them.

@since 0.1.0
-}
encodeProgram :: Program DeBruijn ann -> ByteString
encodeProgram (Program _ (Version major minor patch) term) =
  runOut (pushFiller (putTerm term (pushNatural patch . pushNatural minor . pushNatural major $ emptyOut)))

-- The tags are the format's, written out. A lambda's parameter writes
-- nothing, because a variable finds its binder by counting lambdas outward.
putTerm :: Term DeBruijn ann -> Out -> Out
putTerm t out = case t of
  Var _ (DeBruijn i) -> pushWord64 i (tag 0)
  Delay _ body -> putTerm body (tag 1)
  LamAbs _ _ body -> putTerm body (tag 2)
  Apply _ f x -> putTerm x (putTerm f (tag 3))
  Constant _ c -> putConstant c (tag 4)
  Force _ body -> putTerm body (tag 5)
  Error _ -> tag 6
  Builtin _ f -> pushBits 7 (fromIntegral (fromEnum f)) (tag 7)
  Constr _ w fields -> pushListWith putTerm fields (pushWord64 w (tag 8))
  Case _ scrutinee branches -> pushListWith putTerm (Vector.toList branches) (putTerm scrutinee (tag 9))
  where
    tag :: Word8 -> Out
    tag n = pushBits 4 n out

putConstant :: Constant -> Out -> Out
putConstant c out = putValue c (pushListWith (pushBits 4) (tyTags (tyOf c)) out)

-- Only the outermost constant carries a type header; elements land here
-- directly.
putValue :: Constant -> Out -> Out
putValue c out = case c of
  CInteger i -> pushInteger i out
  CByteString bs -> pushByteString bs out
  CString s -> pushText s out
  CUnit -> out
  CBool b -> pushBit b out
  CList _ xs -> pushListWith putValue xs out
  CArray _ xs -> pushListWith putValue (Strict.toList xs) out
  CPair _ _ x y -> putValue y (putValue x out)
  CData d -> pushByteString (encodeData d) out
  CValue v -> pushListWith putCurrency (Map.toAscList (unpack v)) out
  where
    putCurrency (symbol, tokens) = pushListWith putToken (Map.toAscList tokens) . pushByteString (unK symbol)
    putToken (t, q) = pushInteger (unQuantity q) . pushByteString (unK t)

tyOf :: Constant -> Ty
tyOf = \case
  CInteger _ -> TyInteger
  CByteString _ -> TyByteString
  CString _ -> TyString
  CUnit -> TyUnit
  CBool _ -> TyBool
  CList t _ -> TyList t
  CArray t _ -> TyArray t
  CPair a b _ _ -> TyPair a b
  CData _ -> TyData
  CValue _ -> TyValue

tyTags :: Ty -> [Word8]
tyTags = \case
  TyInteger -> [0]
  TyByteString -> [1]
  TyString -> [2]
  TyUnit -> [3]
  TyBool -> [4]
  TyList t -> [7, 5] <> tyTags t
  TyPair a b -> [7, 7, 6] <> tyTags a <> tyTags b
  TyData -> [8]
  TyBLS12_381_G1_Element -> [9]
  TyBLS12_381_G2_Element -> [10]
  TyBLS12_381_MlResult -> [11]
  TyArray t -> [7, 12] <> tyTags t
  TyValue -> [13]

{- | Decode a program from flat bytes, with the CBOR wrapper already
removed. Nothing may follow the program. The bytes carry no annotations, so
every one is @()@.

@since 0.1.0
-}
decodeProgram :: ByteString -> Either DecodeError (Program DeBruijn ())
decodeProgram = runGet $ do
  version <- Version <$> getNatural <*> getNatural <*> getNatural
  term <- getTerm version
  getFiller
  getEnd
  pure (Program () version term)

getTerm :: Version -> Get (Term DeBruijn ())
getTerm version = go
  where
    go = do
      start <- getOffset
      tag <- getBits 4
      case tag of
        0 -> Var () . DeBruijn <$> getWord64
        1 -> Delay () <$> go
        2 -> LamAbs () (DeBruijn 0) <$> go
        3 -> Apply () <$> go <*> go
        4 -> Constant () <$> getConstant
        5 -> Force () <$> go
        6 -> pure (Error ())
        7 -> Builtin () <$> getBuiltin
        8 -> gate tag start *> (Constr () <$> getWord64 <*> getListWith go)
        9 -> gate tag start *> (Case () <$> go <*> (Vector.fromList <$> getListWith go))
        _ -> getFail (BadTermTag tag start)

    gate :: Word8 -> Int -> Get ()
    gate tag start
      | version >= Version 1 1 0 = pure ()
      | otherwise = getFail (TermNotInVersion tag start)

-- Position is the tag, so toEnum is safe up to maxBound.
getBuiltin :: Get DefaultFun
getBuiltin = do
  start <- getOffset
  tag <- getBits 7
  if fromIntegral tag > fromEnum (maxBound :: DefaultFun)
    then getFail (BadBuiltinTag tag start)
    else pure (toEnum (fromIntegral tag))

getConstant :: Get Constant
getConstant = do
  start <- getOffset
  tags <- getListWith (getBits 4)
  case tyFromTags start tags of
    Left e -> getFail e
    Right ty -> getValueOf ty

-- Reads what tyTags writes. Every tag has to be used up, and only list,
-- array and pair can be applied to a type.
tyFromTags :: Int -> [Word8] -> Either DecodeError Ty
tyFromTags start tags = do
  (ty, rest) <- go tags
  case rest of
    [] -> Right ty
    _ -> Left (BadTypeTags tags start)
  where
    go :: [Word8] -> Either DecodeError (Ty, [Word8])
    go = \case
      0 : rest -> Right (TyInteger, rest)
      1 : rest -> Right (TyByteString, rest)
      2 : rest -> Right (TyString, rest)
      3 : rest -> Right (TyUnit, rest)
      4 : rest -> Right (TyBool, rest)
      8 : rest -> Right (TyData, rest)
      9 : rest -> Right (TyBLS12_381_G1_Element, rest)
      10 : rest -> Right (TyBLS12_381_G2_Element, rest)
      11 : rest -> Right (TyBLS12_381_MlResult, rest)
      13 : rest -> Right (TyValue, rest)
      7 : 5 : rest -> do
        (t, rest') <- go rest
        Right (TyList t, rest')
      7 : 12 : rest -> do
        (t, rest') <- go rest
        Right (TyArray t, rest')
      7 : 7 : 6 : rest -> do
        (a, rest') <- go rest
        (b, rest'') <- go rest'
        Right (TyPair a b, rest'')
      _ -> Left (BadTypeTags tags start)

getValueOf :: Ty -> Get Constant
getValueOf ty = case ty of
  TyInteger -> CInteger <$> getInteger
  TyByteString -> CByteString <$> getByteString
  TyString -> CString <$> getText
  TyUnit -> pure CUnit
  TyBool -> CBool <$> getBit
  TyList t -> CList t <$> getListWith (getValueOf t)
  TyArray t -> CArray t . Strict.fromList <$> getListWith (getValueOf t)
  TyPair a b -> CPair a b <$> getValueOf a <*> getValueOf b
  TyData -> do
    start <- getOffset
    bs <- getByteString
    -- Bytes after the value are ignored, as the reference ignores them.
    either (getFail . InvalidData start) (pure . CData . fst) (decodeDataPrefix bs)
  TyValue -> CValue <$> getValue
  TyBLS12_381_G1_Element -> unsupported
  TyBLS12_381_G2_Element -> unsupported
  TyBLS12_381_MlResult -> unsupported
  where
    unsupported :: Get Constant
    unsupported = getOffset >>= getFail . UnsupportedValue ty

-- The reads of plutus-core's Flat Value instance. Keys and quantities are
-- checked as they are read; order and emptiness once a list is complete,
-- which accepts the same inputs. Positions are where each item starts.
getValue :: Get Value
getValue = do
  currencies <- getListWith $ do
    start <- getOffset
    c <- getKey
    tokens <- getListWith ((,,) <$> getOffset <*> getKey <*> getQuantity)
    pure (start, c, tokens)
  inner <- traverse checkCurrency currencies
  checkAscending CurrenciesNotAscending [(s, c) | (s, c, _) <- currencies]
  pure (pack (Map.fromDistinctAscList inner))
  where
    checkCurrency (start, c, tokens) = case tokens of
      [] -> getFail (InvalidValue EmptyTokenList start)
      _ -> do
        checkAscending TokensNotAscending [(s, t) | (s, t, _) <- tokens]
        pure (c, Map.fromDistinctAscList [(t, q) | (_, t, q) <- tokens])

getKey :: Get K
getKey = do
  start <- getOffset
  bs <- getByteString
  maybe (getFail (InvalidValue KeyTooLong start)) pure (k bs)

getQuantity :: Get Quantity
getQuantity = do
  start <- getOffset
  i <- getInteger
  case quantity i of
    Nothing -> getFail (InvalidValue QuantityOutOfRange start)
    Just _ | i == 0 -> getFail (InvalidValue ZeroQuantity start)
    Just q -> pure q

-- Fails at the first key that is not greater than the one before it.
checkAscending :: ValueError -> [(Int, K)] -> Get ()
checkAscending e items = case [s | ((_, a), (s, b)) <- zip items (drop 1 items), a >= b] of
  s : _ -> getFail (InvalidValue e s)
  [] -> pure ()
