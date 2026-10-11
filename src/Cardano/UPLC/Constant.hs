{- | Literal values. Four the specification allows are missing: the three
BLS12-381 elements, which only a builtin can produce, and the ledger's
@Value@.

@since 0.1.0
-}
module Cardano.UPLC.Constant (
  Constant (..),
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector.Strict qualified as Strict
import Cardano.UPLC.Data (Data)
import Cardano.UPLC.Ty (Ty)

{- | A literal value in a program. List, array and pair carry their component
types, since the flat encoding does not describe itself.

plutus-core pairs a type tag with a Haskell value of that type, so a list
whose elements are not of the declared type cannot be built there. A plain sum
can hold one, such as @CList TyInteger [CBool True]@. The decoder never returns
such a constant; one built by hand has to keep to its declared types.

@since 0.1.0
-}
data Constant
  = CInteger !Integer
  | CByteString !ByteString
  | CString !Text
  | CUnit
  | CBool !Bool
  | CList !Ty ![Constant]
  | CArray !Ty !(Strict.Vector Constant)
  | CPair !Ty !Ty !Constant !Constant
  | CData !Data
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )
