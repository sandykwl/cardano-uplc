{-# LANGUAGE CApiFFI #-}

#include <secp256k1.h>
#include <secp256k1_extrakeys.h>

{- | The three signature verifiers of Plutus Core, on the C libraries the
Cardano node runs.

Each verifier calls the libsodium or libsecp256k1 function that plutus-core
reaches through @cardano-crypto-class@, so the verdicts are the chain's. A key
or signature the library cannot parse gives @Nothing@, an evaluation failure,
and one it parses but rejects gives @Just False@. The Ed25519 check has no
separate parse step, so a key that is not a curve point gives @Just False@, as
on chain.

The arguments are the key, the message and the signature. An argument of the
wrong length is @Nothing@ here, which the evaluator has already ruled out;
the check keeps the C functions from reading past a short buffer.

@since 0.1.0
-}
module Cardano.UPLC.Crypto (
  libraryChecks,
) where

import Control.Exception (evaluate, mask_)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.Types (CInt (..), CSize (..), CUChar, CUInt (..), CULLong (..))
import Foreign.ForeignPtr (ForeignPtr, FinalizerPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)
import Cardano.UPLC.Machine (SignatureChecks (..))

{- | The three verifiers, ready to put in @Params@.

@since 0.1.0
-}
libraryChecks :: SignatureChecks
libraryChecks =
  SignatureChecks
    { checkEd25519 = Just ed25519
    , checkEcdsa = Just ecdsa
    , checkSchnorr = Just schnorr
    }

ed25519 :: ByteString -> ByteString -> ByteString -> Maybe Bool
ed25519 pk msg sig
  | BS.length pk /= 32 || BS.length sig /= 64 = Nothing
  | otherwise = Just . unsafeDupablePerformIO $ do
      _ <- evaluate sodiumReady
      BS.useAsCStringLen msg $ \(m, mlen) ->
        BS.useAsCStringLen pk $ \(k, _) ->
          BS.useAsCStringLen sig $ \(s, _) -> do
            r <- c_ed25519_verify_detached (castPtr s) (castPtr m) (fromIntegral mlen) (castPtr k)
            pure (r == 0)

ecdsa :: ByteString -> ByteString -> ByteString -> Maybe Bool
ecdsa pk msg sig
  | BS.length pk /= 33 || BS.length msg /= 32 || BS.length sig /= 64 = Nothing
  | otherwise = unsafeDupablePerformIO $
      withForeignPtr secpContext $ \ctx ->
        allocaBytes 64 $ \vk ->
          allocaBytes 64 $ \sg ->
            BS.useAsCStringLen pk $ \(k, klen) ->
              BS.useAsCStringLen sig $ \(s, _) ->
                BS.useAsCStringLen msg $ \(m, _) -> do
                  parsedKey <- c_ec_pubkey_parse ctx vk (castPtr k) (fromIntegral klen)
                  if parsedKey /= 1
                    then pure Nothing
                    else do
                      parsedSig <- c_ecdsa_signature_parse_compact ctx sg (castPtr s)
                      if parsedSig /= 1
                        then pure Nothing
                        else do
                          r <- c_ecdsa_verify ctx sg (castPtr m) vk
                          pure (Just (r /= 0))

schnorr :: ByteString -> ByteString -> ByteString -> Maybe Bool
schnorr pk msg sig
  | BS.length pk /= 32 || BS.length sig /= 64 = Nothing
  | otherwise = unsafeDupablePerformIO $
      withForeignPtr secpContext $ \ctx ->
        allocaBytes xonlyPubkeySize $ \vk ->
          BS.useAsCStringLen pk $ \(k, _) ->
            BS.useAsCStringLen sig $ \(s, _) ->
              BS.useAsCStringLen msg $ \(m, mlen) -> do
                parsedKey <- c_xonly_pubkey_parse ctx vk (castPtr k)
                if parsedKey /= 1
                  then pure Nothing
                  else do
                    r <- c_schnorrsig_verify ctx (castPtr s) (castPtr m) (fromIntegral mlen) vk
                    pure (Just (r /= 0))

xonlyPubkeySize :: Int
xonlyPubkeySize = #{size secp256k1_xonly_pubkey}

-- libsodium wants sodium_init before its other functions, and the result 0 or
-- 1 means it is ready.
{-# NOINLINE sodiumReady #-}
sodiumReady :: ()
sodiumReady = unsafePerformIO $ do
  r <- c_sodium_init
  if r == 0 || r == 1 then pure () else fail "sodium_init failed"

{-# NOINLINE secpContext #-}
secpContext :: ForeignPtr ()
secpContext = unsafePerformIO . mask_ $ do
  ctx <- c_context_create (contextSign .|. contextVerify)
  newForeignPtr c_context_destroy ctx

foreign import capi "sodium.h sodium_init"
  c_sodium_init :: IO CInt

foreign import capi unsafe "sodium.h crypto_sign_ed25519_verify_detached"
  c_ed25519_verify_detached ::
    Ptr CUChar -> Ptr CUChar -> CULLong -> Ptr CUChar -> IO CInt

foreign import capi "secp256k1.h value SECP256K1_CONTEXT_SIGN"
  contextSign :: CUInt

foreign import capi "secp256k1.h value SECP256K1_CONTEXT_VERIFY"
  contextVerify :: CUInt

foreign import ccall unsafe "secp256k1.h secp256k1_context_create"
  c_context_create :: CUInt -> IO (Ptr ())

foreign import ccall unsafe "secp256k1.h &secp256k1_context_destroy"
  c_context_destroy :: FinalizerPtr ()

foreign import ccall unsafe "secp256k1.h secp256k1_ec_pubkey_parse"
  c_ec_pubkey_parse :: Ptr () -> Ptr () -> Ptr CUChar -> CSize -> IO CInt

foreign import ccall unsafe "secp256k1.h secp256k1_ecdsa_signature_parse_compact"
  c_ecdsa_signature_parse_compact :: Ptr () -> Ptr () -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "secp256k1.h secp256k1_ecdsa_verify"
  c_ecdsa_verify :: Ptr () -> Ptr () -> Ptr CUChar -> Ptr () -> IO CInt

foreign import ccall unsafe "secp256k1_extrakeys.h secp256k1_xonly_pubkey_parse"
  c_xonly_pubkey_parse :: Ptr () -> Ptr () -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "secp256k1_schnorrsig.h secp256k1_schnorrsig_verify"
  c_schnorrsig_verify :: Ptr () -> Ptr CUChar -> Ptr CUChar -> CSize -> Ptr () -> IO CInt
