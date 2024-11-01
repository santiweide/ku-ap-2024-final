module APL.InterpConcurrent_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpConcurrent (runEval)
import APL.Monad
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

evalTest :: String -> Exp -> Val -> TestTree
evalTest desc e v = testCase desc $ do
  res <- runEval $ eval e
  res @?= Right v

evalTestFail :: String -> Exp -> TestTree
evalTestFail desc e =
  testCase desc $ do
    res <- runEval (eval e)
    case res of
      Left _ -> pure ()
      Right v ->
        assertFailure $
          "Expected error but received this value:\n" ++ show v

tests :: TestTree
tests =
  testGroup
    "Concurrent interpreter"
    []
