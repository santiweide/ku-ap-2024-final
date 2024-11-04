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
    [
      ---------------------------------------
      -- Concurrency Test Real Concurrency --
      --   BothOf  --
      ---------------------------------------
      -- simple &&
      evalTest 
        "(1+2) && (3+4)"
        (BothOf (Add (CstInt 1) (CstInt 2)) (Add (CstInt 3) (CstInt 4)))
        (ValTuple [ValInt 3,ValInt 7])
      -- (Left && Left) -> Left
      , evalTestFail
        "true+false && true-false"
        (BothOf (Add (CstBool True) (CstBool False)) (Sub (CstBool True) (CstBool False)))
      -- (Right || Left) -> Right
      , evalTestFail
        "true+false && 2+1"
        (BothOf (Add (CstBool True) (CstBool False)) (Add (CstInt 2) (CstInt 1)))
      -- (Left || Right) -> Right
      , evalTestFail
        " 2+1 && true+false"
        (BothOf  (Add (CstInt 2) (CstInt 1))  (Add (CstBool True) (CstBool False)) )
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
      -- concurrency and guarantee the order
      , evalTest 
        "(put 0 10 && ((let i = (get 0) in put 1 i) && get 1)).0"
        (Project (BothOf (KvPut (CstInt 0) (CstInt 10)) (BothOf (Let "i" (KvGet (CstInt 0)) (KvPut (CstInt 1) (Var "i"))) (KvGet (CstInt 1)))) 0)
        (ValInt 10)
      , evalTest 
        "(put 0 10 && ((let i = (get 0) in put 1 i) && get 1)).1"
        (Project (BothOf (KvPut (CstInt 0) (CstInt 10)) (BothOf (Let "i" (KvGet (CstInt 0)) (KvPut (CstInt 1) (Var "i"))) (KvGet (CstInt 1)))) 1)
        (ValTuple [ValInt 10,ValInt 10])
      ---------------------------------------
      -- Concurrency Test Real Concurrency --
      --    OneOf with Put and Get   --
      ---------------------------------------
      -- simple ||
      , evalTest 
        " (1+2) || (3+4+5+6)"
        (OneOf (Add (CstInt 1) (CstInt 2)) (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6)))
        (ValInt 3)
      -- (Left || Left) -> Left
      , evalTestFail
        "true+false || true-false"
        (OneOf (Add (CstBool True) (CstBool False)) (Sub (CstBool True) (CstBool False)))
      -- (Right || Left) -> Right
      , evalTest
        "true+false || 2+1"
        (OneOf (Add (CstBool True) (CstBool False)) (Add (CstInt 2) (CstInt 1)))
        (ValInt 3)
      -- (Left || Right) -> Right
      , evalTest
        " 2+1|| true+false"
        (OneOf  (Add (CstInt 2) (CstInt 1))  (Add (CstBool True) (CstBool False)) )
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
      -- (e1 || get lock stuck ) -> e1
      , evalTest
        "((1 || loop x = 1 while x == 1 do x) || get 0)"
        (OneOf (OneOf (CstInt 1) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))) (KvGet (CstInt 0)))
        (ValInt 1)
      -- (get lock stuck || e1) -> e1 
      , evalTest
        "(get 0 || (1 || loop x = 1 while x == 1 do x)) -> 1"
        (OneOf (KvGet (CstInt 0)) (OneOf (CstInt 1) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))))
        (ValInt 1)
      ----------------------------------------------
      --       Concurrency Test Simulation        --
      --  OneOf infinite loops and finite loop    --
      ----------------------------------------------
      -- (e1 || infinite loop) -> e1 
      , evalTest
        "(1 || loop x = 1 while x == 1 do x) -> 1"
        (OneOf (CstInt 1) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")))
        (ValInt 1)
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

