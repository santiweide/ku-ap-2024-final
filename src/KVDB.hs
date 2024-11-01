-- | Key-value database.
module KVDB
  ( KVDB,
    startKVDB,
    kvGet,
    kvPut,
  )
where

import Control.Monad (forM_)
import GenServer

-- | A reference to a KVDB instance that stores keys of type 'k' and
-- corresponding values of type 'v'.
data KVDB k v -- TODO

-- | Start a new KVDB instance.
startKVDB :: (Ord k) => IO (KVDB k v)
startKVDB = undefined

-- | Retrieve the value corresponding to a given key. If that key does
-- not exist in the store, then this function blocks until another
-- thread writes the desired key with 'kvPut', after which this
-- function returns the now available value.
kvGet :: KVDB k v -> k -> IO v
kvGet = undefined

-- | Write a key-value mapping to the database. Replaces any prior
-- mapping of the key.
kvPut :: KVDB k v -> k -> v -> IO ()
kvPut = undefined
