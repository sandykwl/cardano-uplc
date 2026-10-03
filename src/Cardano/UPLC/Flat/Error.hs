{- | Why the flat codec can refuse a program.

The decoder reads bytes from outside, so each way it can fail has its own
constructor, and each one records the bit where the problem was found,
counting from zero. A byte offset would not be precise enough, because the
format packs fields to the bit.

The encoder only fails on something this release doesn't support yet.

@since 0.1.0
-}
module Cardano.UPLC.Flat.Error (
  DecodeError (..),
  EncodeError (..),
) where

import Data.Word (Word8)
import Cardano.UPLC.Ty (Ty)

{- | Why the decoder stopped, and at which bit.

@since 0.1.0
-}
data DecodeError
  = -- | Input ran out at this bit.
    --
    -- @since 0.1.0
    EndOfInput !Int
  | -- | A de Bruijn index, @constr@ tag or version number too big for a
    -- 'Data.Word.Word64', or an index or tag written with more than ten
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
  | -- | Type tag 13, the ledger's @value@ type. t'Cardano.UPLC.Ty.Ty' has no
    -- way to name it yet.
    --
    -- @since 0.1.0
    UnsupportedTypeTag !Word8 !Int
  | -- | A value of a type the decoder can name but won't read. Flat never
    -- carries BLS12-381 values; a script holds a point as compressed bytes
    -- and uncompresses it at run time. @data@ values wait for a CBOR codec.
    -- The type on its own is fine, so an empty list of G1 elements decodes.
    --
    -- @since 0.1.0
    UnsupportedValue !Ty !Int
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

{- | Why the encoder produced no bytes.

@since 0.1.0
-}
data EncodeError
  = -- | The program contains a @data@ constant. Those travel as CBOR, and
    -- there is no CBOR codec yet. Once there is, this error goes away.
    --
    -- @since 0.1.0
    EncodeDataConstant
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )
