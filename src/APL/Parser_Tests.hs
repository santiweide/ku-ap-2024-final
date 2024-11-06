module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ -- Example tests
      parserTest "x+y" $ Add (Var "x") (Var "y")
      , parserTestFail "x+"
      --------------------
      ---- Tuple ----
      --------------------
      -- tuple empty
      , parserTest  "()" $ (Tuple [])
      -- tuple not empty
      , parserTest  "(1,2)" $ (Tuple [CstInt 1,CstInt 2])
      -- tuple with complex elements, mixing parentheses
      , parserTest  "((x*y),2-1,let x=10 in y+z)" $ (Tuple [Mul (Var "x") (Var "y"),Sub (CstInt 2) (CstInt 1),Let "x" (CstInt 10) (Add (Var "y") (Var "z"))])
      -- tuple index
      , parserTest "(1,2,3).2" $ (Project (Tuple [CstInt 1,CstInt 2,CstInt 3]) 2)
      -- tuple with complex elements and index
      , parserTest "(x+y, (x+y+z)).0" $ (Project (Tuple [Add (Var "x") (Var "y"),Add (Add (Var "x") (Var "y")) (Var "z")]) 0)
      -- parenthese, in case we mistake for a tuple
      , parserTest "(1)" $ (CstInt 1)
      -- tuple inside a parenthese inside a tuple
      , parserTest "((1,2,(1,2,3)))" $ (Tuple [CstInt 1,CstInt 2,Tuple [CstInt 1,CstInt 2,CstInt 3]])
      -- projection
      , parserTest "let x = (1,2) in x.0" $ (Let "x" (Tuple [CstInt 1,CstInt 2]) (Project (Var "x") 0))
      --------------------
      ---- Loops ----
      --------------------
      -- normal for loop
      , parserTest "loop x = 1 for i < 5 do x * 2" $ (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))
      -- normal while loop
      , parserTest "loop x = 1 while x == 1 do x" $ (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))
      -- while mix for loop
      , parserTest "loop x = (1,10) while if (x.1 == 0) then false else true do (x.0*2,x.1-1)" $ WhileLoop ("x",Tuple [CstInt 1,CstInt 10]) (If (Eql (Project (Var "x") 1) (CstInt 0)) (CstBool False) (CstBool True)) (Tuple [Mul (Project (Var "x") 0) (CstInt 2),Sub (Project (Var "x") 1) (CstInt 1)])
      -- double for loop
      , parserTest "(loop x=(put 1 1) for i < 10 do loop y=(put 0 1) for j < 10 do put 0 ((get 0) + 1))" $  (ForLoop ("x",KvPut (CstInt 1) (CstInt 1)) ("i",CstInt 10) (ForLoop ("y",KvPut (CstInt 0) (CstInt 1)) ("j",CstInt 10) (KvPut (CstInt 0) (Add (KvGet (CstInt 0)) (CstInt 1)))))
      ------------------------
      ---- && and || ----
      ------------------------
      , parserTest "(1+2)||(3+4+5+6)" $ OneOf (Add (CstInt 1) (CstInt 2)) (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6))
      , parserTest "(1+2) && (3+4)" $ BothOf (Add (CstInt 1) (CstInt 2)) (Add (CstInt 3) (CstInt 4))
      -- priority || < && -- has parentheses, then BothOf stands
      , parserTest "(1*10 || 2-20) && (3+4)" $ BothOf (OneOf (Mul (CstInt 1) (CstInt 10)) (Sub (CstInt 2) (CstInt 20))) (Add (CstInt 3) (CstInt 4))
      -- priority || < && -- no parentheses, then OneOf
      , parserTest "1*10 || 2-20 && 3+4" $ OneOf (Mul (CstInt 1) (CstInt 10)) (BothOf (Sub (CstInt 2) (CstInt 20)) (Add (CstInt 3) (CstInt 4)))
      -- priority && < || < project
      , parserTest "(get 0 && (put 0 42 && put 0 1337)) .0" $ Project (BothOf (KvGet (CstInt 0)) (BothOf (KvPut (CstInt 0) (CstInt 42)) (KvPut (CstInt 0) (CstInt 1337)))) 0
      -- && is left associated, Porjection is higer prioritied than BothOf
      , parserTest "get 0 && put 0 42 && put 0 1337 .0" $ BothOf (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstInt 42))) (Project (KvPut (CstInt 0) (CstInt 1337)) 0)
      , parserTest "(1 || loop x = 1 while x == 1 do x)" $ OneOf (CstInt 1) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))
      -- priority && < while-loop
      , parserTest "(loop x = 1 while x == 1 do x) || 1" $ OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (CstInt 1)
      , parserTest "loop x = 1 while x == 1 do x || 1" $ WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (OneOf (Var "x") (CstInt 1))
      , parserTest "(loop x = 1 while x == 1 do x ,1,2,3)" $ Tuple [WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"),CstInt 1,CstInt 2,CstInt 3]
      -- priority && < Lambda 
      , parserTest "\\x -> x*x*x && x 2 " $ Lambda "x" (BothOf (Mul (Mul (Var "x") (Var "x")) (Var "x")) (Apply (Var "x") (CstInt 2)))
      , parserTest "(\\x -> x*x*x) && x 2 " $ BothOf (Lambda "x" (Mul (Mul (Var "x") (Var "x")) (Var "x"))) (Apply (Var "x") (CstInt 2))
      -- priority || < get 
      , parserTest "(get 0 || (1 || loop x = 1 while x == 1 do x))" $ (OneOf (KvGet (CstInt 0)) (OneOf (CstInt 1) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))))
      , parserTest "((1 || loop x = 1 while x == 1 do x) || get 0)" $ (OneOf (OneOf (CstInt 1) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))) (KvGet (CstInt 0)))
      -- priority && < let 
      , parserTest "(put 0 10 && ((let i = get 0 in put 1 i) && get 1)).0" $ (Project (BothOf (KvPut (CstInt 0) (CstInt 10)) (BothOf (Let "i" (KvGet (CstInt 0)) (KvPut (CstInt 1) (Var "i"))) (KvGet (CstInt 1)))) 0)
      -- priority && < get/put 
      , parserTest "get 0 && put 0 true" $ (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstBool True)))
      -- priority && < add < get; && < put
      , parserTest "get 0 + 1 && put 0 2" $ (BothOf (Add (KvGet (CstInt 0)) (CstInt 1)) (KvPut (CstInt 0) (CstInt 2)))
      -- priority && < put/get; && < let
      , parserTest "put (get 0) 1 && let x = put 0 2 in get 2" $ BothOf (KvPut (KvGet (CstInt 0)) (CstInt 1)) (Let "x" (KvPut (CstInt 0) (CstInt 2)) (KvGet (CstInt 2)))
    ]
