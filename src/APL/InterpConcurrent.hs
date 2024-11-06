module APL.InterpConcurrent (runEval) where

import APL.Monad
import Data.IORef
import KVDB
import SPC
import Control.Monad (void)

initialEnv :: Env
initialEnv = []  -- or some initial values for your environment

runEval :: EvalM a -> IO (Either Error a)
runEval expr = do
  spc <- startSPC
  kvdb <- startKVDB :: IO (KVDB Val Val)
  runEvalM spc kvdb initialEnv expr

-- specify the key and value type to Val
runEvalM :: SPC -> KVDB Val Val -> Env -> EvalM a -> IO (Either Error a)
runEvalM _ _ _ (Pure result) = pure $ Right result
runEvalM _ _ _ (Free (ErrorOp e)) = pure $ Left e
runEvalM spc kvdb env (Free (ReadOp cont)) = runEvalM spc kvdb env $ cont $ env 

runEvalM spc kvdb env (Free (BothOfOp op1 op2 cont)) = do
  result1Ref <- newIORef Nothing
  result2Ref <- newIORef Nothing
  jobId1 <- jobAdd spc $ Job $ void $ runEvalM spc kvdb env op1 >>= writeIORef result1Ref . Just
  jobId2 <- jobAdd spc $ Job $ void $ runEvalM spc kvdb env op2 >>= writeIORef result2Ref . Just
  j1 <- jobWaitAny spc [jobId1] 
  j2 <- jobWaitAny spc [jobId2] 
  case (j1, j2) of 
    -- Done
    ((job1,Done), (job2,Done)) -> do
      result1 <- readIORef result1Ref
      result2 <- readIORef result2Ref
      case (result1, result2) of
        (Just (Right val1), Just (Right val2)) -> runEvalM spc kvdb env (cont (ValTuple [val1, val2]))
        (Just (Left err), Just (Right _)) -> 
          pure $ Left $ "Despite JobDone Done, " ++ show job1 ++ " has error of " ++ err
        (Just (Right _), Just (Left err)) -> 
          pure $ Left $ "Despite JobDone Done, " ++ show job2 ++ " has error of " ++ err
        _ -> pure $ Left $ "Despite JobDone Done, " ++ show job1 ++ " and " ++ show job2 ++ "'s results were missing in BothOfOp"
    -- other mixed cases, keep reporting noisy-ly
    (_, _) -> pure $ Left $ "BothOf failed, with" ++ show j1 ++ ", and " ++ show j2
    

-- Cancel the other job when our job is finished.
runEvalM spc kvdb env (Free (OneOfOp op1 op2 cont)) = do
  result1Ref <- newIORef Nothing
  result2Ref <- newIORef Nothing
  jobId1 <- jobAdd spc $ Job $ void $ runEvalM spc kvdb env op1 >>= writeIORef result1Ref . Just
  jobId2 <- jobAdd spc $ Job $ void $ runEvalM spc kvdb env op2 >>= writeIORef result2Ref . Just
  jobDone <- jobWaitAny spc [jobId1, jobId2]
  result1 <- readIORef result1Ref
  result2 <- readIORef result2Ref
  case (jobDone) of
    (jobId, Done) -> do 
      if jobId == jobId1 
        then jobCancel spc $ jobId2
        else jobCancel spc $ jobId1
      case (result1, result2) of
        (Just (Right val), _) -> runEvalM spc kvdb env (cont val)
        (_, Just (Right val)) -> runEvalM spc kvdb env (cont val)
        -- If the first calculated is an Left, and the other is still in a loop, 
        -- we prefer to wait for the Right result than split out the Left result.
        (Just (Left err), _) -> pure $ Left $ "A job done " ++ show jobId ++ " encountered error: " ++ err
        (_, Just (Left err)) -> pure $ Left $ "A job done " ++ show jobId ++ " encountered error: " ++ err
        (_, _) -> pure $ Left $ "JobDone " ++ show jobId ++ ", but nothing evaled"
    -- Although we will never reach these States based on current SPC
    -- we still write secific error message handlers for these States
    (jobId, DoneTimeout) -> do 
      if jobId == jobId1 
        then jobCancel spc $ jobId2
        else jobCancel spc $ jobId1      
      case (result1, result2) of
        (Just (Right val), _) -> runEvalM spc kvdb env (cont val)
        (_, Just (Right val)) -> runEvalM spc kvdb env (cont val)
        (Just (Left err), _) -> pure $ Left $ "A job done " ++ show jobId ++ " encountered error: " ++ err
        (_, Just (Left err)) -> pure $ Left $ "A job done " ++ show jobId ++ " encountered error: " ++ err
        (_, _) -> pure $ Left $ "JobDone " ++ show jobId ++ ", but nothing evaled"
    (jobId, DoneCancelled) -> do 
      if jobId == jobId1 
        then jobCancel spc $ jobId2
        else jobCancel spc $ jobId1
      case (result1, result2) of
        (Just (Right val), _) -> runEvalM spc kvdb env (cont val)
        (_, Just (Right val)) -> runEvalM spc kvdb env (cont val)
        (Just (Left err), _) -> pure $ Left $ "A job done " ++ show jobId ++ " encountered error: " ++ err
        (_, Just (Left err)) -> pure $ Left $ "A job done " ++ show jobId ++ " encountered error: " ++ err
        (_, _) -> pure $ Left $ "JobDone " ++ show jobId ++ ", but nothing evaled"
    (jobId, DoneCrashed) -> do 
      if jobId == jobId1 
        then jobCancel spc $ jobId2
        else jobCancel spc $ jobId1
      case (result1, result2) of
        (Just (Right val), _) -> runEvalM spc kvdb env (cont val)
        (_, Just (Right val)) -> runEvalM spc kvdb env (cont val)
        (Just (Left err), _) -> pure $ Left $ "A job done " ++ show jobId ++ " encountered error: " ++ err
        (_, Just (Left err)) -> pure $ Left $ "A job done " ++ show jobId ++ " encountered error: " ++ err
        (_, _) -> pure $ Left $ "JobDone " ++ show jobId ++ ", but nothing evaled"

runEvalM spc kvdb env (Free (KvGetOp key cont)) = do
    result <- kvGet kvdb key
    runEvalM spc kvdb env (cont result)

runEvalM spc kvdb env (Free (KvPutOp key val next)) = do
    kvPut kvdb key val
    runEvalM spc kvdb env next

-- | StepOp: execute the next operation in the sequence.
runEvalM spc kvdb env (Free (StepOp next)) = runEvalM spc kvdb env next

