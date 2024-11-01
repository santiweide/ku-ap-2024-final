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
      , parserTest  "(1,2,3)" $ (Tuple [CstInt 1,CstInt 2,CstInt 3])
      -- tuple index
      , parserTest "(1,2,3).2" $ (Project (Tuple [CstInt 1,CstInt 2,CstInt 3]) 2)
      , parserTest "(true, false).0" $ (Project (Tuple [CstBool True,CstBool False]) 0)
      -- parenthese
      , parserTest "(1)" $ (CstInt 1)
    ]
