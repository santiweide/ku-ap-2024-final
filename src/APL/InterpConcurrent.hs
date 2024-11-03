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
runEvalM spc kvdb env (Free (ReadOp cont)) = do
  runEvalM spc kvdb env $ cont $ env 

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

    -- Timeout cases
    ((job1, DoneTimeout), (job2, Done)) ->
      pure $ Left $ show job1 ++ " timed out in BothOfOp, with " ++ show job2 ++ " completed successfully"
    ((job1, Done), (job2, DoneTimeout)) ->
      pure $ Left $ show job2 ++ " timed out in BothOfOp, with " ++ show job1 ++ " completed successfully"
    ((job1, DoneTimeout), (job2, DoneTimeout)) ->
      pure $ Left $ show job1 ++ " and " ++ show job2 ++ " both timed out in BothOfOp"

    -- Cancellation cases
    ((job1, DoneCancelled), (job2, Done)) ->
      pure $ Left $ show job1 ++ " was cancelled in BothOfOp, with " ++ show job2 ++ " completed successfully"
    ((job1, Done), (job2, DoneCancelled)) ->
      pure $ Left $ show job2 ++ " was cancelled in BothOfOp, with " ++ show job1 ++ " completed successfully"
    ((job1, DoneCancelled), (job2, DoneCancelled)) ->
      pure $ Left $ show job1 ++ " and " ++ show job2 ++ " both were cancelled in BothOfOp"

    -- Crash cases
    ((job1, DoneCrashed), (job2, Done)) ->
      pure $ Left $ show job1 ++ " crashed in BothOfOp, with " ++ show job2 ++ " completed successfully"
    ((job1, Done), (job2, DoneCrashed)) ->
      pure $ Left $ show job2 ++ " crashed in BothOfOp, with " ++ show job1 ++ " completed successfully"
    ((job1, DoneCrashed), (job2, DoneCrashed)) ->
      pure $ Left $ show job1 ++ " and " ++ show job2 ++ " both crashed in BothOfOp"

    -- Mixed cases (Timeout and Cancelled)
    ((job1, DoneTimeout), (job2, DoneCancelled)) ->
      pure $ Left $ show job1 ++ " timed out and " ++ show job2 ++ " was cancelled in BothOfOp"
    ((job1, DoneCancelled), (job2, DoneTimeout)) ->
      pure $ Left $ show job2 ++ " timed out and " ++ show job1 ++ " was cancelled in BothOfOp"

    -- Mixed cases (Timeout and Crashed)
    ((job1, DoneTimeout), (job2, DoneCrashed)) ->
      pure $ Left $ show job1 ++ " timed out and " ++ show job2 ++ " crashed in BothOfOp"
    ((job1, DoneCrashed), (job2, DoneTimeout)) ->
      pure $ Left $ show job2 ++ " timed out and " ++ show job1 ++ " crashed in BothOfOp"

    -- Mixed cases (Cancelled and Crashed)
    ((job1, DoneCancelled), (job2, DoneCrashed)) ->
      pure $ Left $ show job1 ++ " was cancelled and " ++ show job2 ++ " crashed in BothOfOp"
    ((job1, DoneCrashed), (job2, DoneCancelled)) ->
      pure $ Left $ show job2 ++ " was cancelled and " ++ show job1 ++ " crashed in BothOfOp"

runEvalM spc kvdb env (Free (OneOfOp op1 op2 cont)) = do
  result1Ref <- newIORef Nothing
  result2Ref <- newIORef Nothing
  jobId1 <- jobAdd spc $ Job $ void $ runEvalM spc kvdb env op1 >>= writeIORef result1Ref . Just
  jobId2 <- jobAdd spc $ Job $ void $ runEvalM spc kvdb env op2 >>= writeIORef result2Ref . Just
  jobDone <- jobWaitAny spc [jobId1, jobId2]
  result1 <- readIORef result1Ref
  result2 <- readIORef result2Ref
  case (jobDone, result1, result2) of
    ((_, Done), Just (Right val), _) -> runEvalM spc kvdb env (cont val)
    ((jobId, Done), Just (Left err), _) -> pure $ Left $ "First job " ++ show jobId ++ " encountered error: " ++ err
    ((jobId, Done), _, Just (Left err)) -> pure $ Left $ "Second job " ++ show jobId ++ " encountered error: " ++ err
    -- Timeout cases, should continue to find out about the other job
    ((jobId, DoneTimeout), _, _) -> pure $ Left $ show jobId ++ " timed out in OneOfOp"
    -- Cancellation cases
    ((jobId, DoneCancelled), _, _) -> pure $ Left $ show jobId ++ " was cancelled in OneOfOp"
    -- Crashes
    ((jobId, DoneCrashed), _, _) -> pure $ Left $ show jobId ++ " crashed in OneOfOp"
    -- Fallback case
    ((jobId, _), _, _ ) -> do 
      res <- jobStatus spc $ jobId
      res1 <- jobStatus spc $ jobId1
      res2 <- jobStatus spc $ jobId2
      pure $ Left $ show res ++ " jobid1 " ++ show res1 ++ " jobid2 " ++show res2
    

-- Handle the KvGetOp by converting `Val` to `k` type for lookup
runEvalM spc kvdb env (Free (KvGetOp key cont)) = do
    result <- kvGet kvdb key
    runEvalM spc kvdb env (cont result)

-- Handle the KvPutOp by converting `Val` to `k` and `v` for storing
runEvalM spc kvdb env (Free (KvPutOp key val next)) = do
    kvPut kvdb key val
    runEvalM spc kvdb env next

-- | StepOp: execute the next operation in the sequence.
runEvalM spc kvdb env (Free (StepOp next)) = runEvalM spc kvdb env next

