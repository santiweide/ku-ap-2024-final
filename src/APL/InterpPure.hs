module APL.InterpPure (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

mergeStates :: State -> State -> State
mergeStates s1 s2 = s1 ++ filter (\(k, _) -> notElem k (map fst s1)) s2

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

    runEval' r s (Free (BothOfOp e1 e2 m)) = 
      let res1 = runEval' r s e1
          res2 = runEval' r s e2
      in case (res1, res2) of
        ((Right v1, s1), (Right v2, s2)) ->
          runEval' r (mergeStates s1 s2) $ m (ValTuple [v1, v2])
        ((Left err, _), _) -> (Left err, s)
        (_, (Left err, _)) -> (Left err, s)

    runEval' r s (Free (OneOfOp e1 e2 m)) = 
      let res1 = runEval' r s e1
          res2 = runEval' r s e2
      in case (res1, res2) of
        ((Right v1, s1), _) -> runEval' r s1 $ m v1
        (_, (Right v2, s2)) -> runEval' r s2 $ m v2
        ((Left err, _), _) -> (Left err, s)
