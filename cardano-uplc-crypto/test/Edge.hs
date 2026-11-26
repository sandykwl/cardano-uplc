{- Ed25519 cases with libsodium's verdicts, as cardano-crypto-class links it.
Ed25519 acceptance is not in the specification, and libsodium's master branch
has changed it, so a library that disagrees here is not the one the chain
runs.

A label starts with its group:

* A: valid signatures, and the same with a bit flipped, the key or signature
  all zeros or all ones, S plus the group order, R of small order, or a
  small-order or non-canonical key.
* B: keys and R built from the small-order points, keys of mixed order,
  non-canonical keys, and S at the edges of the group order. The number after
  the B says which of these.
* P: single cases, among them the first example of RFC 8032 and the
  conformance suite's own vectors.

Each line is a label, a key, a message and a signature in hex, and T or F for
the verdict, separated by tabs.
-}
module Edge (edgeTests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Cardano.UPLC.Crypto qualified as Crypto
import Cardano.UPLC.Machine (SignatureChecks (..))
import Hex (hex)

path :: FilePath
path = "test/ed25519-edge-vectors.tsv"

edgeTests :: IO TestTree
edgeTests = do
  rows <- map (BC.split '\t') . BC.lines <$> BS.readFile path
  pure . testGroup "Ed25519 edge vectors" $ zipWith one [1 :: Int ..] rows
  where
    one n = \case
      [label, k, m, s, v] ->
        testCase (show n <> " " <> BC.unpack label) $
          verify (bytes k) (bytes m) (bytes s) @?= Just (v == BC.pack "T")
      _ -> testCase (show n) (assertFailure "malformed line")
    bytes :: ByteString -> ByteString
    bytes = hex . BC.unpack
    verify = case checkEd25519 Crypto.libraryChecks of
      Just f -> f
      Nothing -> \_ _ _ -> Nothing
