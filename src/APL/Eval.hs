module APL.Eval
  ( eval,
  )
where

import APL.AST (Exp (..))
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
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"

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
    _ -> failure "invalid tuple"

-- eval (ForLoop initial var bound body) = do
-- ForLoop (VName, Exp) (VName, Exp) Exp
eval (ForLoop (p, initial) (vloop, bound) body) = undefined
  -- do
  -- vInit <- eval initial 
  -- loop vInit bound
  -- where
  --   loop :: Val -> Exp -> EvalM Val
  --   loop currentVal boundExp = do 
  --     vBound <- eval boundExp 
  --     case (currentVal, vBound) of
  --       (ValInt n, ValInt b) | n < b -> do
  --         localEnv (envExtend var (ValInt n)) $ do
  --           v <- eval body 
  --           loop v boundExp 
  --       _ -> pure currentVal

-- WhileLoop (VName, Exp) Exp Exp
eval (WhileLoop (p, initial) cond body) = undefined

-- eval (WhileLoop initial cond body) = do
--     vInit <- eval initial 
--     env <- askEnv 
--     loop vInit env
--     where
--       loop :: Val -> Env -> EvalM Val
--       loop currentVal curEnv = do
--         vCond <- localEnv (const curEnv) $ eval cond
--         case vCond of
--           ValBool True -> do
--             localEnv (const curEnv) $ do
--                 v <- eval body
--                 newEnv <- askEnv
--                 loop v newEnv
--           ValBool False -> pure currentVal 
--           _ -> failure "Condition must evaluate to a boolean"


eval (BothOf a b) = do
  va <- eval a 
  vb <- eval b 
  pure $ ValTuple [va, vb]

eval (OneOf a b) = do
  va <- eval a 
  _ <- eval b 
  pure $ va

-- TODO just copy from a4
eval (KvPut key value) = do
    k <- eval key
    v <- eval value
    evalKvPut k v
    pure $ v

eval (KvGet key) = do
    k <- eval key
    evalKvGet k
