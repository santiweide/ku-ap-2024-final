module APL.InterpConcurrent (runEval) where

import APL.Monad
import Data.IORef
import KVDB
import SPC

runEval :: EvalM a -> IO (Either Error a)
runEval = undefined
