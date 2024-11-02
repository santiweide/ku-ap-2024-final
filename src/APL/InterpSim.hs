module APL.InterpSim (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

mergeStates :: State -> State -> State
mergeStates s1 s2 = s1 ++ filter (\(k, _) -> notElem k (map fst s1)) s2

-- | nextinue execution of the provided mutation as far as
-- possible, but executing at most one 'StepOp' effect. Any nested
-- mutations (in 'BothOp' and 'OneOfOp') must also be stepped
-- similarly. If the mutation is stuck on a 'KvGetOp' for which the
-- key is not in the state, then the mutation is merely returned
-- unchanged.
--
-- Evaluation of 'BothOp':
--
-- * If either of the nested mutations are 'Free (ErrorOp ...)',
--   then propagate that error.
--
-- * If both are 'Pure', then return a pair of the results.
--
-- * Otherwise evaluate both one step.
--
-- Evaluation of 'OneOfOp':
--
-- * If both of the nested mutations are 'Free (ErrorOp ...)', then
--   propagate one of the errors.
--
-- * If one is 'Pure', then return that result.
--
-- * Otherwise evaluate both one step.


-- | Simulates a single step in the evaluation of an expression.
-- Given an environment, current state, and mutation, it evaluates
-- one step (or as far as it can) while handling StepOp effects.
step :: Env -> State -> EvalM a -> (EvalM a, State)
step env state m = case m of
  Pure v -> (Pure v, state)
  Free (ErrorOp err) -> (Free (ErrorOp err), state)
  Free (StepOp next) -> (next, state)

  Free (BothOfOp e1 e2 next) ->
    -- result of e2 will cover e1 according to the state evaluation order
    -- , see testCase "(put 0 42 && put 0 1337).0 -> 42"
    let (e1', state1) = step env state e1 
        (e2', state2) = step env state1 e2 
    in case (e1', e2') of
         (Pure v1, Pure v2) -> (next $ ValTuple [v1,v2], state2) 
         (Free (ErrorOp err), _) -> (Free (ErrorOp err), state)
         (_, Free (ErrorOp err)) -> (Free (ErrorOp err), state)
         _ -> (Free (BothOfOp e1' e2' next), state2)

  -- result of e2 will cover e1 according to the state evaluation order
  -- , see testCase "(put 0 42 || put 0 1337) -> 42"
  Free (OneOfOp e1 e2 next) ->
    let (e1', state1) = step env state e1
        (e2', state2) = step env state1 e2
    in case (e1', e2') of
         (Pure v1, _) -> (next v1, state1)
         (_, Pure v2) -> (next v2, state2)
         (Free (ErrorOp err1), Free (ErrorOp err2)) -> (Free (ErrorOp err1), state)
         _ -> (Free (OneOfOp e1' e2' next), state2)

  Free (KvPutOp key val next) -> (next, (key, val) : state)
  
  Free (KvGetOp key next) ->
    case lookup key state of
      Just val -> (next val, state)
       -- wait until key is available, recursive style 
      Nothing -> (Free (KvGetOp key next), state)
  

-- | Fully evaluates an EvalM mutation by repeatedly applying `step`
-- until it reaches a `Pure` or `Error` state.
runEval :: EvalM a -> Either Error a
runEval m = go stateInitial m
  where
    go state c = case step [] state c of
      (Pure v, _) -> Right v
      (Free (ErrorOp err), _) -> Left err
      (c', state') -> go state' c'