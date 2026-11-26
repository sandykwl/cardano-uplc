module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Cells (cellTests)
import Conformance (conformanceTests)
import Edge (edgeTests)

main :: IO ()
main = do
  conformance <- conformanceTests
  edge <- edgeTests
  defaultMain (testGroup "cardano-uplc-crypto" [conformance, cellTests, edge])
