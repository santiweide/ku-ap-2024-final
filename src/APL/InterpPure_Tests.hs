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
      -- tuple-eval-order is from left to right
      , evalTest
        "tuple eval order"
        (Tuple [KvPut (CstInt 1) (CstInt 2), KvGet (CstInt 1), KvPut (CstInt 1) (CstInt 3), KvGet (CstInt 1)])
        (ValTuple [ValInt 2,ValInt 2,ValInt 3,ValInt 3])
      -- 
      , evalTest
        "Projection valid tuple and valid index"
        (Project (Tuple [CstInt 1,CstInt 2,CstInt 3]) 2)
        (ValInt 3)
      -- It is an error to try to project an element from a non-tuple
      , evalTestFail
        "Projection to a non-tuple"
        (Let "x" (CstBool True) (Project (Var "x") 0))
      -- If x has N elements, then i must be between 0 and N − 1; 
      -- using an index outside of this range is an error. 
      , evalTestFail
        "Projection out-of-range"
        (Let "x" (Tuple [CstInt 1,CstInt 2,CstInt 3,CstInt 4]) (Project (Var "x") 100))

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
