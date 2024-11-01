module APL.InterpPure_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpPure (runEval)
import APL.Monad
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

evalTest :: String -> Exp -> Val -> TestTree
evalTest desc e v =
  testCase desc $
    runEval (eval e) @?= Right v

evalTestFail :: String -> Exp -> TestTree
evalTestFail desc e =
  testCase desc $
    case runEval (eval e) of
      Left _ -> pure ()
      Right v ->
        assertFailure $
          "Expected error but received this value:\n" ++ show v

tests :: TestTree
tests =
  testGroup
    "Pure interpreter"
    [ evalTestFail
        "State (unknown key)"
        (KvGet (CstInt 0))
      --
      -- Should work after task A.
      , evalTest
        "(e1,e2)"
        (Tuple [CstInt 1, CstInt 2])
        (ValTuple [ValInt 1, ValInt 2])
      , evalTest
        "tuple eval order"
        (Tuple [KvPut (CstInt 1) (CstInt 2), KvGet (CstInt 1), KvPut (CstInt 1) (CstInt 3), KvGet (CstInt 1)])
        (ValTuple [ValInt 2,ValInt 2,ValInt 3,ValInt 3])
      --
      -- Should work after Task B.
      -- evalTest
      --   "For loop"
      --   (ForLoop ("x", CstInt 1) ("i", CstInt 10) (Mul (Var "x") (CstInt 2)))
      --   (ValInt 1024),
      -- --
      -- -- Should work after task C.
      -- evalTest
      --   "e1 && e2"
      --   (BothOf (CstInt 0) (CstInt 1))
      --   (ValTuple [ValInt 0, ValInt 1]),
      -- --
      -- -- Should work after task C.
      -- evalTest
      --   "e1 || e2"
      --   (OneOf (CstInt 0) (CstInt 1))
      --   (ValInt 0),
      -- --
      -- -- Should work after task C.
      -- evalTest
      --   "e1 || e2 (first fails)"
      --   (OneOf (KvGet (CstInt 0)) (CstInt 1))
      --   (ValInt 1)
    ]
