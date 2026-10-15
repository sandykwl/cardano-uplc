{- | Why the flat codec can refuse a program.

The decoder reads bytes from outside, so each way it can fail has its own
constructor, and each one records the bit where the problem was found,
counting from zero. A byte offset would not be precise enough, because the
format packs fields to the bit.

@since 0.1.0
-}
module Cardano.UPLC.Flat.Error (
  DecodeError (..),
  ValueError (..),
) where

import Data.Word (Word8)
import Cardano.UPLC.Data.Cbor.Internal (DataDecodeError)
import Cardano.UPLC.Ty (Ty)

{- | Why the decoder stopped, and at which bit.

@since 0.1.0
-}
data DecodeError
  = -- | Input ran out at this bit.
    --
    -- @since 0.1.0
    EndOfInput !Int
  | -- | A de Bruijn index or @constr@ tag too big for a
    -- 'Data.Word.Word64', or one written with more than ten
    -- seven-bit groups. Integer constants have no size limit, so they never
    -- cause this.
    --
    -- @since 0.1.0
    WordOverflow !Int
  | -- | A term tag in the reserved range 10 to 15.
    --
    -- @since 0.1.0
    BadTermTag !Word8 !Int
  | -- | A @constr@ or @case@ term in a program older than version 1.1.0,
    -- which introduced them.
    --
    -- @since 0.1.0
    TermNotInVersion !Word8 !Int
  | -- | A builtin tag beyond the ones this release knows. The script might be
    -- broken, or just newer than this package.
    --
    -- @since 0.1.0
    BadBuiltinTag !Word8 !Int
  | -- | The tags that give a constant's type don't make sense as a type, for
    -- instance a list with no element type. The tags are kept for the
    -- error message.
    --
    -- @since 0.1.0
    BadTypeTags ![Word8] !Int
  | -- | A value of a type the decoder can name but won't read. Flat never
    -- carries BLS12-381 values; a script holds a point as compressed bytes
    -- and uncompresses it at run time. The type on its own is fine, so an
    -- empty list of G1 elements decodes.
    --
    -- @since 0.1.0
    UnsupportedValue !Ty !Int
  | -- | A @data@ constant whose bytes are not a 'Cardano.UPLC.Data.Data'
    -- value: the bit where its byte string starts, and what the CBOR reader
    -- found, with its byte offset inside the constant.
    --
    -- @since 0.1.0
    InvalidData !Int !DataDecodeError
  | -- | A @value@ constant that breaks one of the ledger value's rules,
    -- and the bit where the offending key or quantity starts.
    --
    -- @since 0.1.0
    InvalidValue !ValueError !Int
  | -- | A @string@ constant whose bytes are not valid UTF-8.
    --
    -- @since 0.1.0
    InvalidUtf8 !Int
  | -- | Padding whose closing 1 doesn't end a byte. Extra whole bytes of
    -- 0s before it are fine, as they are in the reference.
    --
    -- @since 0.1.0
    BadFiller !Int
  | -- | Bytes after the program's final padding.
    --
    -- @since 0.1.0
    TrailingInput !Int
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )

{- | Which rule of the @value@ type a constant broke. They are plutus-core's
checks: the specification's conditions for a well-formed list, and its
32-byte limit on keys.

@since 0.1.0
-}
data ValueError
  = -- | A currency symbol or token name over 32 bytes.
    --
    -- @since 0.1.0
    KeyTooLong
  | -- | A quantity outside -2^127 to 2^127 - 1.
    --
    -- @since 0.1.0
    QuantityOutOfRange
  | -- | A quantity of zero.
    --
    -- @since 0.1.0
    ZeroQuantity
  | -- | A currency with no tokens.
    --
    -- @since 0.1.0
    EmptyTokenList
  | -- | A currency symbol that does not come after the one before it, so
    -- the list is descending or repeats one.
    --
    -- @since 0.1.0
    CurrenciesNotAscending
  | -- | A token name that does not come after the one before it.
    --
    -- @since 0.1.0
    TokensNotAscending
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )
