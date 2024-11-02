module APL.InterpSim_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpSim (runEval)
import APL.Monad
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

evalTest :: String -> Exp -> Val -> TestTree
evalTest desc e v = testCase desc $ do
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
    "Simulated concurrent interpreter"
    [
      ---------------------------------
      -- Concurrency Test Simulation --
      --           BothOf            --
      ---------------------------------
      -- simple &&
      evalTest 
        "(1+2) && (3+4)"
        (BothOf (Add (CstInt 1) (CstInt 2)) (Add (CstInt 3) (CstInt 4)))
        (ValTuple [ValInt 3,ValInt 7])
      -- get wait put
      , evalTest
        "(get 0 && put 0 42)"
        (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstInt 1000)))
        ((ValTuple [ValInt 1000,ValInt 1000]))
      -- get wait put and project the get
      , evalTest
        "(get 0 && put 0 42).0"
        (Project (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstInt 42))) 0)
        (ValInt 42)
      -- get wait put and project the get
      , evalTest
        "(put 0 42 && get 0).1"
        (Project (BothOf (KvPut (CstInt 0) (CstInt 42)) (KvGet (CstInt 0))) 1)
        (ValInt 42)
      -- state eval order: s1 is used while evaluating s2
      , evalTest
        "(put 0 42 && put 0 1337).0 -> 42"
        (Project (BothOf (KvPut (CstInt 0) (CstInt 42)) (KvPut (CstInt 0) (CstInt 1337))) 0)
        (ValInt 42)
      , evalTest 
        "(get 0 && (put 0 42 && put 0 1337)).0 -> 1337"
        (Project (BothOf (KvGet (CstInt 0)) (BothOf (KvPut (CstInt 0) (CstInt 42)) (KvPut (CstInt 0) (CstInt 1337)))) 0)
        (ValInt 1337)
      ---------------------------------
      -- Concurrency Test Simulation --
      --           OneOf             --
      ---------------------------------
      -- eval order: e1 -> e2, so e1 is the "First one to finish"
      , evalTest
        "(put 0 42 || put 0 1337) -> 42"
        (OneOf (KvPut (CstInt 0) (CstInt 42)) (KvPut (CstInt 0) (CstInt 1337)))
        (ValInt 42)
      -- priority: parentheses > ||; e1 is the "First one to finish" 
      , evalTest 
        "(get 0 || (put 0 42 || put 0 1337)) -> 42"
        (OneOf (KvGet (CstInt 0)) (OneOf (KvPut (CstInt 0) (CstInt 42)) (KvPut (CstInt 0) (CstInt 1337))))
        (ValInt 42)
      -- (e1 || infinite loop) -> e1 
      , evalTest 
        "(1 || loop x = 1 while x == 1 do x) -> 1"
        (OneOf (KvGet (CstInt 0)) (OneOf (CstInt 1) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))))
        (ValInt 1)
      -- (infinite loop || e2) -> e2 
      , evalTest 
        "((loop x = 1 while x == 1 do x) || 2) -> 2"
        (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (CstInt 2))
        (ValInt 2)
      -- simple ||
      , evalTest 
        " (1+2) || (3+4+5+6)"
        (OneOf (Add (CstInt 1) (CstInt 2)) (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6)))
        (ValInt 3)
    ]
