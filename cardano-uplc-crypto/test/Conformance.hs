{- Plutus's conformance cases for the three signature builtins, run through
the decoder and the machine with the verifiers of this package.

Each case under test/conformance is a flat program and the flat program it
evaluates to, or the text "evaluation failure". The cases are found when the
tests run, so refreshing the directory needs no change here. Budgets are not
checked.
-}
module Conformance (conformanceTests) where

import Control.Monad (filterM)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.List (isSuffixOf, sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, makeRelative, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Cardano.UPLC.Crypto qualified as Crypto
import Cardano.UPLC.Flat (decodeProgram)
import Cardano.UPLC.Machine (EvalError (..), Params (..), SemanticsVariant (..), evaluateWith)
import Cardano.UPLC.Term (Program (..))

-- | Under @cabal test@ the working directory is the package root.
root :: FilePath
root = "test" </> "conformance"

-- | Variant E is the one the conformance suite expects.
params :: Params
params = Params {semanticsVariant = VariantE, signatureChecks = Crypto.libraryChecks}

conformanceTests :: IO TestTree
conformanceTests = do
  found <- doesDirectoryExist root
  if not found
    then pure . testCase "Conformance" $ assertFailure (root <> " not found; run the tests from the package root")
    else do
      inputs <- sort . filter (".flat" `isSuffixOf`) <$> walk root
      pure . testGroup "Conformance" $ map (runCase . dropExtension . makeRelative root) inputs

walk :: FilePath -> IO [FilePath]
walk dir = do
  entries <- map (dir </>) <$> listDirectory dir
  dirs <- filterM doesDirectoryExist entries
  rest <- traverse walk dirs
  pure (filter (`notElem` dirs) entries <> concat rest)

runCase :: FilePath -> TestTree
runCase name = testCase name $ do
  input <- BS.readFile (root </> name <> ".flat")
  expected <- BS.readFile (root </> name <> ".flat.expected")
  case decodeProgram input of
    Left e -> assertFailure ("input does not decode: " <> show e)
    Right p
      | BC.unpack expected == "evaluation failure" ->
          case evaluateWith params (programTerm p) of
            Left (NotSupported t) -> assertFailure ("reached something not supported: " <> show t)
            Left _ -> pure ()
            Right t -> assertFailure ("expected a failure, got " <> show t)
      | otherwise ->
          case decodeProgram expected of
            Left e -> assertFailure ("expected result does not decode: " <> show e)
            Right want -> evaluateWith params (programTerm p) @?= Right (programTerm want)
