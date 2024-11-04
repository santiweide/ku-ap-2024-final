module KVDB_Tests (tests) where

import KVDB
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Concurrent
  ( threadDelay,
    forkIO,
  )
tests :: TestTree
tests =
    testGroup "KVDB" $
        concat $ [ 
          replicate 100 asyncPutGetTest
          , replicate 100 asyncGetManyTimesTest 
          , replicate 100 asyncManyThreads
          , replicate 1 asyncManyThreadsGetBlocks
        ]

-- Async Put and Get fr one another thread
--  to see the block is really working
asyncPutGetTest :: TestTree
asyncPutGetTest = 
  testCase "Simple Get and Put with thread delay" $ do
    db <- startKVDB :: IO (KVDB Integer Integer)
    _ <- forkIO $ do 
      threadDelay 1000
      kvPut db 1 (200::Integer)
      v0 <- kvGet db 0
      v0 @?= 100
    kvPut db 0 100
    v1 <- kvGet db 1
    v1 @?= 200

asyncGetManyTimesTest :: TestTree
asyncGetManyTimesTest = 
  testCase "Get a key for N times" $ do
    db <- startKVDB :: IO (KVDB Integer Integer)
    _ <- forkIO $ do 
      v0 <- kvGet db 0  -- a kind of synchronize
      threadDelay 1000
      kvPut db 1 (200::Integer)
      v0 @?= 100
    kvPut db 0 100
    v1 <- kvGet db 1
    v2 <- kvGet db 1
    v3 <- kvGet db 1
    v1 @?= 200
    v2 @?= 200
    v3 @?= 200


asyncManyThreads :: TestTree
asyncManyThreads = 
  testCase "Put different keys in different threads" $ do
    db <- startKVDB:: IO (KVDB Integer Integer)
    _ <- forkIO $ do 
      v0 <- kvGet db 0  -- a kind of synchronize
      -- do some calculation
      kvPut db 1 (200::Integer)
      v0 @?= 100
    _ <- forkIO $ do 
      v0 <- kvGet db 0  -- a kind of synchronize
      -- do some calculation
      kvPut db 2 (300::Integer)
      v0 @?= 100
    _ <- forkIO $ do 
      v0 <- kvGet db 0  -- a kind of synchronize
      -- do some calculation
      kvPut db 3 (400::Integer)
      v0 @?= 100
    kvPut db 0 100
    v1 <- kvGet db 1
    v2 <- kvGet db 2
    v3 <- kvGet db 3
    v1 @?= 200
    v2 @?= 300
    v3 @?= 400

-- Dead lock simplest case
-- We use kvGetWithTimeoutKill but not kvGetWithTimeout 
-- because kvGetWithTimeoutKill is safer to recycle threads~
asyncManyThreadsGetBlocks :: TestTree
asyncManyThreadsGetBlocks = 
  testCase "DeadLock on getting no-value-keys" $ do
    db <- startKVDB:: IO (KVDB Integer Integer)
    _ <- forkIO $ do 
      v0 <- kvGetWithTimeoutKill db 1 0
      v0 @?= Nothing
    _ <- forkIO $ do 
      v1 <- kvGetWithTimeoutKill db 1 1
      v1 @?= Nothing
    v2 <- kvGetWithTimeoutKill db 1 2
    v2 @?= Nothing
