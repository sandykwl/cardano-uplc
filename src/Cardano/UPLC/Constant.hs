{- | Literal values. Four of the constants the specification allows are not
included yet. Three of them are the BLS12-381 elements, which you can only
get by evaluating a builtin. The fourth is the ledger's @Value@, whose
builtins aren't implemented yet.

@since 0.1.0
-}
module Cardano.UPLC.Constant (
  Constant (..),
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Cardano.UPLC.Data (Data)
import Cardano.UPLC.Ty (Ty)

{- | A literal value appearing in a program. The list, array and pair constants
carry their component types, since the flat encoding does not describe itself.

@since 0.1.0
-}
data Constant
  = CInteger !Integer
  | CByteString !ByteString
  | CString !Text
  | CUnit
  | CBool !Bool
  | CList !Ty ![Constant]
  | CArray !Ty ![Constant]
  | CPair !Ty !Ty !Constant !Constant
  | CData !Data
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )
