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

-- TODO check coverage
tests :: TestTree
tests =
  testGroup
    "Simulated concurrent interpreter"
    [
      ---------------------------------
      -- Concurrency Test Simulation --
      --   BothOf with Put and Get   --
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
      --    OneOf with Put and Get   --
      ---------------------------------
      -- simple ||
      , evalTest 
        " (1+2) || (3+4+5+6)"
        (OneOf (Add (CstInt 1) (CstInt 2)) (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6)))
        (ValInt 3)
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
      ----------------------------------------------
      --       Concurrency Test Simulation        --
      --  OneOf infinite loops and finite loop    --
      ----------------------------------------------
      -- (infinite loop || e2) -> e2 
      , evalTest 
        "((loop x = 1 while x == 1 do x) || 2) -> 2"
        (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (CstInt 2))
        (ValInt 2)
      -- (finite loop || finite loop) -> finite loop 
      , evalTest 
        "((loop x = 1 while x == 1 do x) || (loop x = 1 for i < 5 do x * 2)) -> 32"
        (OneOf 
            (
              ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))
            )
            ( 
              ForLoop ("x",CstInt 1) ("i",CstInt 10) (Mul (Var "x") (CstInt 2))
            ) 
        )
        (ValInt 32)
      -- (infinite loop || finite loop) -> finite loop 
      , evalTest 
        "((loop x = 1 while x == 1 do x) || (loop x = 1 for i < 5 do x * 2)) -> 32"
        (OneOf 
            (
              ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))
            )
            ( 
              WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")
            ) 
        )
        (ValInt 32)
      -- (finite loop || infinite loop) -> finite loop 
      , evalTest 
        "( (loop x = 1 for i < 5 do x * 2) || (loop x = 1 while x == 1 do x) ) -> 32"
        (OneOf 
            ( 
              WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")
            ) 
            (
              ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))
            )
        )
        (ValInt 32)
      --------------------------------------------
      --       Concurrency Test Simulation     --
      --         with Lambda and Apply         -- 
      --------------------------------------------
      -- evaluating concurrently cannot change the fact that
      --  the scope is not shared between environment. 
      -- The variable scope is independent
      , evalTestFail
        "(\\x -> x*x*x) && x 2 "
        (BothOf (Lambda "x" (Mul (Mul (Var "x") (Var "x")) (Var "x"))) (Apply (Var "x") (CstInt 2)))
      -- we should use let to pass var in env
      , evalTest
        "(\\x -> x*x*x) && let x = (\\x -> x*x*x) in x 2 "
        (BothOf (Lambda "x" (Mul (Mul (Var "x") (Var "x")) (Var "x"))) (Let "x" (Lambda "x" (Mul (Mul (Var "x") (Var "x")) (Var "x"))) (Apply (Var "x") (CstInt 2))))
        (ValTuple [ValFun [] "x" (Mul (Mul (Var "x") (Var "x")) (Var "x")),ValInt 8])
      , evalTest
        "(\\x -> x*x*x) || let x = (\\x -> x*x*x) in x 2 "
        (OneOf (Lambda "x" (Mul (Mul (Var "x") (Var "x")) (Var "x"))) (Let "x" (Lambda "x" (Mul (Mul (Var "x") (Var "x")) (Var "x"))) (Apply (Var "x") (CstInt 2))))
        (ValFun [] "x" (Mul (Mul (Var "x") (Var "x")) (Var "x")))
      , evalTest
        "(\\x -> x*x*x) || x 2 "
        (OneOf (Lambda "x" (Mul (Mul (Var "x") (Var "x")) (Var "x"))) (Apply (Var "x") (CstInt 2)))
        (ValFun [] "x" (Mul (Mul (Var "x") (Var "x")) (Var "x")))
  ]
 
