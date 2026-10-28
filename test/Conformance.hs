{- Plutus's conformance cases, run through the decoder and the machine.

Each case under test/conformance is a flat program and the flat program it
evaluates to, or the text "evaluation failure" or "parse/decode error". The
cases are found when the tests run, so refreshing the directory needs no
change here. Budgets are not checked.
-}
module Conformance (conformanceTests) where

import Control.Monad (filterM)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.List (isSuffixOf, sort)
import Data.Word (Word64)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, makeRelative, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Cardano.UPLC.Flat (decodeProgram)
import Cardano.UPLC.Machine (EvalError (..), SemanticsVariant (..), evaluateWith)
import Cardano.UPLC.Name (DeBruijn (..))
import Cardano.UPLC.Term (Program (..), Term (..))

-- Under cabal test the working directory is the package root.
root :: FilePath
root = "test" </> "conformance"

conformanceTests :: IO TestTree
conformanceTests = do
  found <- doesDirectoryExist root
  if not found
    then pure . testCase "Conformance" $ assertFailure (root <> " not found; run the tests from the package root")
    else do
      inputs <- sort . filter (".flat" `isSuffixOf`) <$> walk root
      let names = map (dropExtension . makeRelative root) inputs
      pure . testGroup "Conformance" $
        if null names
          then [testCase "cases" $ assertFailure ("no cases under " <> root)]
          else map runCase names

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
  case (either (Left . show) closed (decodeProgram input), BC.unpack (BC.strip expected)) of
    (Left _, "parse/decode error") -> pure ()
    (Left e, _) -> assertFailure ("input does not decode: " <> e)
    (Right _, "parse/decode error") -> assertFailure "input decoded, but the case expects a decode error"
    (Right p, "evaluation failure") ->
      case evaluateWith VariantE (programTerm p) of
        Left (NotSupported t) -> assertFailure ("reached something not supported yet: " <> show t)
        Left _ -> pure ()
        Right t -> assertFailure ("expected a failure, got " <> show t)
    (Right p, _) ->
      case decodeProgram expected of
        Left e -> assertFailure ("expected result does not decode: " <> show e)
        Right want ->
          (programVersion p, evaluateWith VariantE (programTerm p))
            @?= (programVersion want, Right (programTerm want))

-- The conformance runner turns the program back into names before it runs it,
-- and a free variable fails there, so the case expects a decode error. This
-- decoder leaves scope to the reader, so the check is made here.
closed :: Program DeBruijn ann -> Either String (Program DeBruijn ann)
closed p
  | go 0 (programTerm p) = Right p
  | otherwise = Left "free variable"
  where
    go :: Word64 -> Term DeBruijn ann -> Bool
    go depth = \case
      Var _ (DeBruijn i) -> i >= 1 && i <= depth
      LamAbs _ _ body -> go (depth + 1) body
      Apply _ f x -> go depth f && go depth x
      Delay _ body -> go depth body
      Force _ body -> go depth body
      Constr _ _ fields -> all (go depth) fields
      Case _ s branches -> go depth s && all (go depth) branches
      Constant {} -> True
      Builtin {} -> True
      Error {} -> True
