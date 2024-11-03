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
        ]

-- Async Put and Get fr one another thread
--  to see the block is really working
asyncPutGetTest :: TestTree
asyncPutGetTest = 
  testCase "add-ket-get-val" $ do
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
  testCase "add-ket-get-val" $ do
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
  testCase "add-ket-get-val" $ do
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