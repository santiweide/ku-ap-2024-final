module KVDB_Tests (tests) where

import KVDB
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "KVDB"
    []
