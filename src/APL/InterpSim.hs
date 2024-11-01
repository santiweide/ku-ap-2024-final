module APL.InterpSim (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

-- | Continue execution of the provided computation as far as
-- possible, but executing at most one 'StepOp' effect. Any nested
-- computations (in 'BothOp' and 'OneOfOp') must also be stepped
-- similarly. If the computation is stuck on a 'KvGetOp' for which the
-- key is not in the state, then the computation is merely returned
-- unchanged.
--
-- Evaluation of 'BothOp':
--
-- * If either of the nested computations are 'Free (ErrorOp ...)',
--   then propagate that error.
--
-- * If both are 'Pure', then return a pair of the results.
--
-- * Otherwise evaluate both one step.
--
-- Evaluation of 'OneOfOp':
--
-- * If both of the nested computations are 'Free (ErrorOp ...)', then
--   propagate one of the errors.
--
-- * If one is 'Pure', then return that result.
--
-- * Otherwise evaluate both one step.
step :: Env -> State -> EvalM a -> (EvalM a, State)
step = undefined

runEval :: EvalM a -> Either Error a
runEval = undefined
