{- | The hash builtins, on plain 'ByteString', from @crypton@.

Arguments arrive already range-checked for the semantics variant. None of
these can fail.

@since 0.1.0
-}
module Cardano.UPLC.Machine.Builtins.Hash (
  sha2_256,
  sha3_256,
  blake2b_256,
  keccak_256,
  blake2b_224,
  ripemd_160,
) where

import Crypto.Hash (
  Blake2b_224 (..),
  Blake2b_256 (..),
  HashAlgorithm,
  Keccak_256 (..),
  RIPEMD160 (..),
  SHA256 (..),
  SHA3_256 (..),
  hashWith,
 )
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)

digest :: HashAlgorithm a => a -> ByteString -> ByteString
digest alg = BA.convert . hashWith alg

{- | SHA-256.

@since 0.1.0
-}
sha2_256 :: ByteString -> ByteString
sha2_256 = digest SHA256

{- | SHA3-256, the FIPS 202 standard.

@since 0.1.0
-}
sha3_256 :: ByteString -> ByteString
sha3_256 = digest SHA3_256

{- | BLAKE2b with a 32-byte output. It is a hash of its own, not the first
32 bytes of the 64-byte one.

@since 0.1.0
-}
blake2b_256 :: ByteString -> ByteString
blake2b_256 = digest Blake2b_256

{- | Keccak-256 with the original Keccak padding, as Ethereum uses it, so its
digests differ from 'sha3_256'.

@since 0.1.0
-}
keccak_256 :: ByteString -> ByteString
keccak_256 = digest Keccak_256

{- | BLAKE2b with a 28-byte output, the size of a Cardano key hash.

@since 0.1.0
-}
blake2b_224 :: ByteString -> ByteString
blake2b_224 = digest Blake2b_224

{- | RIPEMD-160.

@since 0.1.0
-}
ripemd_160 :: ByteString -> ByteString
ripemd_160 = digest RIPEMD160
