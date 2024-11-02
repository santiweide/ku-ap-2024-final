-- | Key-value database.
module KVDB
  ( KVDB,
    startKVDB,
    kvGet,
    kvPut,
  )
where

import Control.Monad (ap, forM_, liftM)
import Data.Maybe(fromMaybe)
import Data.List (partition)

import GenServer

-- TODO add jobs

-- | A reference to a KVDB instance that stores keys of type 'k' and
-- corresponding values of type 'v'.
data KVDB k v = KVDB (Server (KVDBMessage k v))

-- | The central state for the KVDB.
data KVDBState k v = KVDBState
  { kvdbEntries :: [(k, v)],                -- store of key-value pairs
    kvdbChan    :: Chan (KVDBMessage k v),   -- communication channel for requests
    kvdbPendingGets :: [(k, [ReplyChan v])] -- the pending gets
  }

-- | The monad in which the main KVDB thread runs, with support for IO.
newtype KVDBM k v a = KVDBM (KVDBState k v -> IO (a, KVDBState k v))

instance Functor (KVDBM k v) where
  fmap = liftM

instance Applicative (KVDBM k v) where
  pure x = KVDBM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad (KVDBM k v) where
  KVDBM m >>= f = KVDBM $ \state -> do
    (x, state') <- m state
    let KVDBM f' = f x
    f' state'

-- | Retrieve the current state.
get :: KVDBM k v (KVDBState k v)
get = KVDBM $ \state -> pure (state, state)

-- | Update the state.
put :: KVDBState k v -> KVDBM k v ()
put state = KVDBM $ \_ -> pure ((), state)

-- | Modify the state with a function.
modify :: (KVDBState k v -> KVDBState k v) -> KVDBM k v ()
modify f = do
  state <- get
  put $ f state

-- | Lift an IO action into KVDBM.
io :: IO a -> KVDBM k v a
io action = KVDBM $ \state -> do
  result <- action
  pure (result, state)

-- | Run the KVDBM monad with an initial state.
runKVDBM :: KVDBState k v -> KVDBM k v a -> IO a
runKVDBM state (KVDBM f) = fst <$> f state

-- | 
data KVDBMessage k v
  = Put k v                   -- Insert or update a key-value pair
  | Get k (ReplyChan v)       -- Request a value by key, with a channel for the reply

-- | Start a new KVDB instance.
startKVDB :: (Ord k) => IO (KVDB k v)
startKVDB = do
  let initial_state c =
        KVDBState
          { kvdbChan = c,
            kvdbEntries = [],
            kvdbPendingGets = []
          }
  server <- spawn $ \c -> runKVDBM (initial_state c) $ handleMsg c
  pure $ KVDB server

handleMsg :: (Ord k) => Chan (KVDBMessage k v) -> KVDBM k v ()
handleMsg chan = do
  msg <- io $ receive chan
  case msg of
    Put key value -> do
      modify $ \s -> s { kvdbEntries = (key, value) : filter ((/= key) . fst) (kvdbEntries s) }
      state <- get
      let 
        pendingGets = kvdbPendingGets state 
        (waiters, rest) = partition ((== key) . fst) pendingGets
      forM_ (concatMap snd waiters) $ \replyChan -> io $ reply replyChan value 
      modify $ \s -> s { kvdbPendingGets = rest }
      handleMsg chan

    -- Handle Get request: 
    -- reply immediately if value is present,
    --  otherwise store reply channel in pending gets
    Get key replyChan -> do
      state <- get
      let 
        entries = kvdbEntries state 
        in case lookup key entries of
          Just value -> io $ reply replyChan value  -- respond if key is found
          Nothing -> modify $ \s ->
            s { kvdbPendingGets = 
              (key, replyChan : fromMaybe [] (lookup key (kvdbPendingGets s))) : filter ((/= key) . fst) (kvdbPendingGets s) }
      handleMsg chan

-- | Retrieve the value corresponding to a given key. If that key does
-- not exist in the store, then this function blocks until another
-- thread writes the desired key with 'kvPut', after which this
-- function returns the now available value.
kvGet :: KVDB k v -> k -> IO v
kvGet (KVDB server) key = 
  requestReply server (\replyChan -> Get key replyChan)

-- | Write a key-value mapping to the database. Replaces any prior
-- mapping of the key.
kvPut :: KVDB k v -> k -> v -> IO ()
kvPut (KVDB server) key value = 
  sendTo server (Put key value)