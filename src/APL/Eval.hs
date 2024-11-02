module APL.Eval
  ( eval,
  )
where

import APL.AST (Exp (..), VName)
import APL.Monad

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "If: non-boolean conditional"
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var pBody) = do
  env <- askEnv
  pure $ ValFun env var pBody
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var pBody, arg) ->  evalStep $ 
      localEnv (const $ envExtend var arg f_env) $ eval pBody
    (_, _) ->
      evalStep $ failure "Cannot apply non-function"

eval (KvPut key value) = do
    k <- eval key
    v <- eval value
    evalKvPut k v
    pure $ v

eval (KvGet key) = do
    k <- eval key
    evalKvGet k

eval (Tuple elements) =
  case elements of
    [] -> do
      pure (ValTuple [])
    [_] -> do
      failure $ "Evaluation of tuple should not be 1-lengthed\n"
    _ -> do
      values <- mapM eval elements
      pure (ValTuple values)

eval (Project tupleExp index) = do
  tupleVal <- eval tupleExp
  case tupleVal of
    ValTuple elements ->
      let idx = fromIntegral index
      in if idx >= 0 && idx < (length elements)
           then pure $ elements !! idx  
           else failure "index out of bounds"
    _ -> failure "invalid projection doing to a non-tuple!"

eval (ForLoop (q, initial) (j, bound) pBody) = do
  pInit <- eval initial 
  valBound <- eval bound 
  case valBound of 
    ValInt vBound -> do 
        localEnv (envExtend j (ValInt 0)) $ 
          evalStep $ loop q pInit j (ValInt vBound)
            where 
              loop :: VName -> Val -> VName -> Val -> EvalM Val
              loop p pVal i iBound = do 
                  iVal <- eval (Var i)
                  case iVal of
                    -- ValInt in Val is deriving Ord, so we can make cmp directly.
                    ValInt iCur | (ValInt iCur) < iBound -> do 
                      pVal' <- localEnv (envExtend p pVal) $ eval pBody
                      iVal' <- eval (Add (Var i) (CstInt 1))
                      localEnv (envExtend i iVal') $ evalStep $ loop p pVal' i iBound
                    _ -> pure pVal
    _ -> failure "[ForLoop]bound should be evaled to a ValInt"

-- p is in scope when evaluating cond,
eval (WhileLoop (q, initial) cond pBody) = do
  pInit <- eval initial 
  evalStep $ loop q pInit
    where 
      loop :: VName -> Val -> EvalM Val
      loop p pVal = do 
          cVal <- localEnv (envExtend p pVal) $ eval cond
          case cVal of 
            ValBool True -> do
              localEnv (envExtend p pVal) $ do
                pVal' <- eval pBody
                evalStep $ loop p pVal' 
            ValBool False -> pure pVal 
            _ -> failure "Condition must evaluate to a boolean"

eval (BothOf e1 e2) = do
  evalBothOf (eval e1) (eval e2)

eval (OneOf e1 e2) = do
  evalOneOf (eval e1) (eval e2)

