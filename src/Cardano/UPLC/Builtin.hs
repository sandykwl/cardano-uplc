{- | The builtin functions a program may call.

Declared in flat tag order from zero, so 'fromEnum' is the tag on the wire. Do
not regroup the constructors by what they do; that changes the tags. Hashes and
signature checks interleave because a tag records when a builtin arrived, not
what it computes.

Tags 94 to 100, @InsertCoin@ through @ScaleValue@, are absent: they work on the
ledger @Value@ constant, which "Cardano.UPLC.Constant" does not carry. They sit at the
top, so 0 to 93 stay contiguous.

The tags match @plutus-core@ throughout, and 82 of them are confirmed against
conformance-corpus bytes. Where Table C.6 of the specification disagrees --
58 to 60 and 65 to 67, the BLS compress, uncompress and hashToGroup entries --
the specification is wrong.

@since 0.1.0
-}
module Cardano.UPLC.Builtin (
  DefaultFun (..),
) where

{- | A primitive operation, identified by the tag the flat encoding gives it.

@since 0.1.0
-}
data DefaultFun
  = -- | @since 0.1.0
    AddInteger
  | -- | @since 0.1.0
    SubtractInteger
  | -- | @since 0.1.0
    MultiplyInteger
  | -- | @since 0.1.0
    DivideInteger
  | -- | @since 0.1.0
    QuotientInteger
  | -- | @since 0.1.0
    RemainderInteger
  | -- | @since 0.1.0
    ModInteger
  | -- | @since 0.1.0
    EqualsInteger
  | -- | @since 0.1.0
    LessThanInteger
  | -- | @since 0.1.0
    LessThanEqualsInteger
  | -- | @since 0.1.0
    AppendByteString
  | -- | @since 0.1.0
    ConsByteString
  | -- | @since 0.1.0
    SliceByteString
  | -- | @since 0.1.0
    LengthOfByteString
  | -- | @since 0.1.0
    IndexByteString
  | -- | @since 0.1.0
    EqualsByteString
  | -- | @since 0.1.0
    LessThanByteString
  | -- | @since 0.1.0
    LessThanEqualsByteString
  | -- | @since 0.1.0
    Sha2_256
  | -- | @since 0.1.0
    Sha3_256
  | -- | @since 0.1.0
    Blake2b_256
  | -- | @since 0.1.0
    VerifyEd25519Signature
  | -- | @since 0.1.0
    AppendString
  | -- | @since 0.1.0
    EqualsString
  | -- | @since 0.1.0
    EncodeUtf8
  | -- | @since 0.1.0
    DecodeUtf8
  | -- | @since 0.1.0
    IfThenElse
  | -- | @since 0.1.0
    ChooseUnit
  | -- | @since 0.1.0
    Trace
  | -- | @since 0.1.0
    FstPair
  | -- | @since 0.1.0
    SndPair
  | -- | @since 0.1.0
    ChooseList
  | -- | @since 0.1.0
    MkCons
  | -- | @since 0.1.0
    HeadList
  | -- | @since 0.1.0
    TailList
  | -- | @since 0.1.0
    NullList
  | -- | @since 0.1.0
    ChooseData
  | -- | @since 0.1.0
    ConstrData
  | -- | @since 0.1.0
    MapData
  | -- | @since 0.1.0
    ListData
  | -- | @since 0.1.0
    IData
  | -- | @since 0.1.0
    BData
  | -- | @since 0.1.0
    UnConstrData
  | -- | @since 0.1.0
    UnMapData
  | -- | @since 0.1.0
    UnListData
  | -- | @since 0.1.0
    UnIData
  | -- | @since 0.1.0
    UnBData
  | -- | @since 0.1.0
    EqualsData
  | -- | @since 0.1.0
    MkPairData
  | -- | @since 0.1.0
    MkNilData
  | -- | @since 0.1.0
    MkNilPairData
  | -- | @since 0.1.0
    SerialiseData
  | -- | @since 0.1.0
    VerifyEcdsaSecp256k1Signature
  | -- | @since 0.1.0
    VerifySchnorrSecp256k1Signature
  | -- | @since 0.1.0
    Bls12_381_G1_add
  | -- | @since 0.1.0
    Bls12_381_G1_neg
  | -- | @since 0.1.0
    Bls12_381_G1_scalarMul
  | -- | @since 0.1.0
    Bls12_381_G1_equal
  | -- | @since 0.1.0
    Bls12_381_G1_compress
  | -- | @since 0.1.0
    Bls12_381_G1_uncompress
  | -- | @since 0.1.0
    Bls12_381_G1_hashToGroup
  | -- | @since 0.1.0
    Bls12_381_G2_add
  | -- | @since 0.1.0
    Bls12_381_G2_neg
  | -- | @since 0.1.0
    Bls12_381_G2_scalarMul
  | -- | @since 0.1.0
    Bls12_381_G2_equal
  | -- | @since 0.1.0
    Bls12_381_G2_compress
  | -- | @since 0.1.0
    Bls12_381_G2_uncompress
  | -- | @since 0.1.0
    Bls12_381_G2_hashToGroup
  | -- | @since 0.1.0
    Bls12_381_millerLoop
  | -- | @since 0.1.0
    Bls12_381_mulMlResult
  | -- | @since 0.1.0
    Bls12_381_finalVerify
  | -- | @since 0.1.0
    Keccak_256
  | -- | @since 0.1.0
    Blake2b_224
  | -- | @since 0.1.0
    IntegerToByteString
  | -- | @since 0.1.0
    ByteStringToInteger
  | -- | @since 0.1.0
    AndByteString
  | -- | @since 0.1.0
    OrByteString
  | -- | @since 0.1.0
    XorByteString
  | -- | @since 0.1.0
    ComplementByteString
  | -- | @since 0.1.0
    ReadBit
  | -- | @since 0.1.0
    WriteBits
  | -- | @since 0.1.0
    ReplicateByte
  | -- | @since 0.1.0
    ShiftByteString
  | -- | @since 0.1.0
    RotateByteString
  | -- | @since 0.1.0
    CountSetBits
  | -- | @since 0.1.0
    FindFirstSetBit
  | -- | @since 0.1.0
    Ripemd_160
  | -- | @since 0.1.0
    ExpModInteger
  | -- | @since 0.1.0
    DropList
  | -- | @since 0.1.0
    LengthOfArray
  | -- | @since 0.1.0
    ListToArray
  | -- | @since 0.1.0
    IndexArray
  | -- | @since 0.1.0
    Bls12_381_G1_multiScalarMul
  | -- | @since 0.1.0
    Bls12_381_G2_multiScalarMul
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Ord
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Enum
    , -- | @since 0.1.0
      Bounded
    )
