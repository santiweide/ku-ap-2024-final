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
      ---------------------
      -- Tuple Test  --
      ---------------------
      -- Should work after task A.
      , evalTest
        "(e1,e2)"
        (Tuple [CstInt 1, CstInt 2])
        (ValTuple [ValInt 1, ValInt 2])
      -- tuple-eval-order is from left to right
      -- (e1, e2, e3, e4)::e1->e2->e3->e4
      , evalTest
        "tuple eval order - success" 
        (Tuple [KvPut (CstInt 1) (CstInt 2), KvGet (CstInt 1), KvPut (CstInt 1) (CstInt 3), KvGet (CstInt 1)])
        (ValTuple [ValInt 2,ValInt 2,ValInt 3,ValInt 3])
      , evalTestFail
        "tuple eval order - failed at 1" 
        (Tuple [KvPut (CstInt 100) (CstInt 2), KvGet (CstInt 1), KvPut (CstInt 1) (CstInt 3), KvGet (CstInt 1)])
      , evalTestFail
        "tuple eval order - failed at 3" 
        (Tuple [KvPut (CstInt 1) (CstInt 2), KvGet (CstInt 1), KvPut (CstInt 1) (CstInt 3), KvGet (CstInt 100)])
      -- (e0, e1, e2).2 = e2
      , evalTest
        "Projection valid tuple and valid index"
        (Project (Tuple [CstInt 1,CstInt 2,CstInt 3]) 2)
        (ValInt 3)
      -- empty tuple projection
      -- ().0 :: error
      , evalTestFail
        "Projection invalid index on empty tuple; any index is invalid"
        (Project (Tuple []) 0)
      -- It is an error to try to project an element from a non-tuple
      -- true.0 :: error
      , evalTestFail
        "Projection to a non-tuple"
        (Let "x" (CstBool True) (Project (Var "x") 0))
      -- If x has N elements, then i must be between 0 and N − 1; 
      -- using an index outside of this range is an error. 
      -- (e0...en-1).n :: error
      , evalTestFail
        "Projection out-of-range"
        (Let "x" (Tuple [CstInt 1,CstInt 2,CstInt 3,CstInt 4]) (Project (Var "x") 4))
      ---------------------
      -- Loop Test  --
      ---------------------
      -- Should work after Task B.
     , evalTest
        "For loop"
        (ForLoop ("x", CstInt 1) ("i", CstInt 10) (Mul (Var "x") (CstInt 2)))
        (ValInt 1024)
     , evalTest
        "For loop 2^5=32, validating the p is not influcing the loop variable i"
        (ForLoop ("x",CstInt 1) ("x",CstInt 5) (Mul (Var "x") (CstInt 2)))
        (ValInt 32)
     , evalTest
        "While loop "
        (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Mul (Var "x") (CstInt 2)))
        (ValInt 2)
      , evalTest
        "For and While Loop"
        (WhileLoop ("x",Tuple [CstInt 1,CstInt 10]) (If (Eql (Project (Var "x") 1) (CstInt 0)) (CstBool False) (CstBool True)) (Tuple [Mul (Project (Var "x") 0) (CstInt 2),Sub (Project (Var "x") 1) (CstInt 1)]))
        (ValTuple [ValInt 1024,ValInt 0])
      , evalTest 
        "Double For loop"
        (ForLoop ("x",KvPut (CstInt 1) (CstInt 1)) ("i",CstInt 10) (ForLoop ("y",KvPut (CstInt 0) (CstInt 1)) ("j",CstInt 10) (KvPut (CstInt 0) (Add (KvGet (CstInt 0)) (CstInt 1)))))
        (ValInt 11)
      ---------------------------
      -- Concurrency Test Pure --
      ---------------------------
      -- Should work after task C.
      , evalTest
        "e1 && e2"
        (BothOf (CstInt 0) (CstInt 1))
        (ValTuple [ValInt 0, ValInt 1])
      , evalTestFail
        "e1 && e2 (second fails)"
        (BothOf (CstInt 1) (KvGet (CstInt 0)) )
      --
      -- Should work after task C.
      , evalTest
        "e1 || e2"
        (OneOf (CstInt 0) (CstInt 1))
        (ValInt 0)
      -- 
      -- A complicated one
      , evalTest
        "e1 || e2 -> e1 in pure"
        (OneOf (Add (CstInt 1) (CstInt 2)) (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6)))
        (ValInt 3)
      -- BothOf eval order :: e1, e2 = e1 -> e2
      , evalTest 
        "(put 0 10 && put 1 10) && get 1)).0"
        (Project (BothOf (BothOf (KvPut (CstInt 0) (CstInt 10)) (KvPut (CstInt 1) (CstInt 10))) (KvGet (CstInt 1))) 0)
        (ValTuple [ValInt 10,ValInt 10])
      , evalTest 
        "(put 0 10 && put 1 10) && get 1)).1"
        (Project (BothOf (BothOf (KvPut (CstInt 0) (CstInt 10)) (KvPut (CstInt 1) (CstInt 10))) (KvGet (CstInt 1))) 1)
        (ValInt 10)
      -- invalid key
      , evalTestFail
        "get 1"
        (KvGet (CstInt 1))
      --
      -- Should work after task C.
      , evalTest
        "e1 || e2 (first fails)"
        (OneOf (KvGet (CstInt 1)) (CstInt 1))
        (ValInt 1)
      , evalTest
        "e1 || e2 (second fails)"
        (OneOf (CstInt 1) (KvGet (CstInt 1)) )
        (ValInt 1)
      , evalTestFail
        "e1 || e2 (both fails)"
        (OneOf (KvGet (CstInt 1)) (KvGet (CstInt 0)) )
    ]
