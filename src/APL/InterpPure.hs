module APL.InterpPure (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

runEval :: EvalM a -> Either Error a
runEval = fmap fst $ runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> (Either Error a, State)
    runEval' _ s (Pure x) = (pure x, s)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Nothing -> (Left $ "Invalid key: " ++ show key, s)
        Just val -> runEval' r s $ k val
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter ((/= key) . fst) s
       in runEval' r s' m
    runEval' _ s (Free (ErrorOp e)) = (Left e, s)
    runEval' r s (Free (StepOp m)) = runEval' r s m   

    -- Our eval order: e1 -> e1
    runEval' r s (Free (BothOfOp e1 e2 m)) = 
      let res1 = runEval' r s e1
      in case res1 of 
        (Right v1, s1) -> 
          let res2 = runEval' r s1 e2
          in case res2 of 
            (Right v2, s2) -> runEval' r s2 $ m (ValTuple [v1, v2])
            (Left err, _) -> (Left err, s)
        (Left err, _) -> (Left err, s)

    -- since it returns the calculation that finishes first, 
    -- one will not use the result of the other.
    runEval' r s (Free (OneOfOp e1 e2 m)) = 
      let res1 = runEval' r s e1
          res2 = runEval' r s e2
      in case (res1, res2) of
        ((Right v1, s1), _) -> runEval' r s1 $ m v1
        (_, (Right v2, s2)) -> runEval' r s2 $ m v2
        ((Left err, _), _) -> (Left err, s)
