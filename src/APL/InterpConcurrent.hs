module APL.InterpConcurrent (runEval) where

import APL.Monad
import Data.IORef
import KVDB
import SPC
import Control.Monad (void)

-- | Run the concurrent evaluation of `EvalM`
runEval :: EvalM a -> IO (Either Error a)
runEval expr = do
  spc <- startSPC
  kvdb <- startKVDB :: IO (KVDB Val Val)
  runEvalM spc kvdb expr

runEvalM :: (Ord k) => SPC -> KVDB k v -> EvalM a -> IO (Either Error a)
runEvalM _ _ (Free (ErrorOp e)) = pure $ Left e

runEvalM spc kvdb (Free (BothOfOp op1 op2 cont)) = do
  result1Ref <- newIORef Nothing
  result2Ref <- newIORef Nothing
  -- TODO explain what is the function, why written like this and advantages in the report
  jobId1 <- jobAdd spc $ Job $ void $ runEvalM spc kvdb op1 >>= writeIORef result1Ref . Just
  jobId2 <- jobAdd spc $ Job $ void $ runEvalM spc kvdb op2 >>= writeIORef result2Ref . Just
   -- Wait for both jobs to complete
  jobWaitAny spc [jobId1] 
  jobWaitAny spc [jobId2] 
  result1 <- readIORef result1Ref
  result2 <- readIORef result2Ref
  case (result1, result2) of
    (Just (Right val1), Just (Right val2)) -> runEvalM spc kvdb (cont (ValTuple [val1, val2]))
    (Just (Left err), _) -> pure $ Left err
    (_, Just (Left err)) -> pure $ Left err
    _ -> pure $ Left "One or both results were missing in BothOfOp"


runEvalM spc kvdb (Free (OneOfOp op1 op2 cont)) = do
  result1Ref <- newIORef Nothing
  result2Ref <- newIORef Nothing
  jobId1 <- jobAdd spc $ Job $ void $ runEvalM spc kvdb op1 >>= writeIORef result1Ref . Just
  jobId2 <- jobAdd spc $ Job $ void $ runEvalM spc kvdb op2 >>= writeIORef result2Ref . Just
   -- Wait for either job to complete
  jobWaitAny spc [jobId1, jobId2] 
  result1 <- readIORef result1Ref
  result2 <- readIORef result2Ref
  case (result1, result2) of
    (Just (Right val), _) -> runEvalM spc kvdb (cont val)
    (_, Just (Right val)) -> runEvalM spc kvdb (cont val)
    (Just (Left err), _) -> pure $ Left err
    (_, Just (Left err)) -> pure $ Left err
    _ -> pure $ Left "Both operations failed in OneOfOp"

-- | KvGetOp: retrieve a value from the KVDB, block if key is not available until it's set.
-- runEvalM spc kvdb (Free (KvGetOp key cont)) = do
--   result <- kvGet kvdb key
--   case result of
--     Just val -> runEvalM spc kvdb (cont val)
--     Nothing -> do
--       mvar <- newEmptyMVar
--       kvWait kvdb key mvar
--       takeMVar mvar >>= runEvalM spc kvdb . cont

-- -- | KvPutOp: store a value in the KVDB and proceed with the next operation.
-- runEvalM spc kvdb (Free (KvPutOp key val next)) = do
--   kvPut kvdb key val
--   runEvalM spc kvdb next

-- | StepOp: execute the next operation in the sequence.
runEvalM spc kvdb (Free (StepOp next)) = runEvalM spc kvdb next

-- | Pure case: lift the pure result to an `Either` and return.
runEvalM _ _ (Pure result) = pure $ Right result