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
          -- , replicate 100 jobCanceledTestCase 
        ]

-- Async Put and Get fr one another thread
--  to see the block is really working
asyncPutGetTest :: TestTree
asyncPutGetTest = 
  testCase "add-ket-get-val" $ do
    db <- startKVDB
    _ <- forkIO $ do 
      threadDelay 1000
      kvPut db 1 200
      v0 <- kvGet db 0
      v0 @?= 100
    kvPut db 0 100
    v1 <- kvGet db 1
    v1 @?= 200

asyncPutGetTest :: TestTree
asyncPutGetTest = 
  testCase "add-ket-get-val" $ do
    db <- startKVDB
    _ <- forkIO $ do 
      threadDelay 1000
      kvPut db 1 200
      v0 <- kvGet db 0
      v0 @?= 100
    kvPut db 0 100
    v1 <- kvGet db 1
    v1 @?= 200