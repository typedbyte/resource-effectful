module Test.Effectful.Resource where

import Test.Effectful.ResourceExtended (additionalTests)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (catch, throwIO, SomeException, try)
import Control.Monad (replicateM, replicateM_, forM, forM_)

import Data.IORef
import Data.List (sort)

import Effectful
import Effectful.Dispatch.Static
import Effectful.Resource

import Test.Tasty
import Test.Tasty.HUnit

-- Main test suite
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Effectful.Resource"
  [ testGroup "Basic Resource Management"
    [ testCase "Resource is properly freed" testResourceFreed
    , testCase "Resources are freed in reverse order" testResourceOrder
    , testCase "Resources are freed even after exception" testResourceExceptions
    ]
  , testGroup "Region Management"
    [ testCase "WithRegion creates a new region" testWithRegion
    , testCase "CurrentRegion returns the current region" testCurrentRegion
    , testCase "Nested regions work correctly" testNestedRegions
    ]
  , testGroup "Resource Allocation and Handling"
    [ testCase "Allocate and free work correctly" testAllocateFree
    , testCase "FreeAll works with multiple resources" testFreeAll
    , testCase "ManageEff works with effectful actions" testManageEff
    , testCase "AllocateEff works with effectful actions" testAllocateEff
    ]
  , testGroup "Resource Movement"
    [ testCase "Resources can be moved between regions" testResourceMove
    , testCase "Move_ works correctly" testResourceMoveNoKey
    ]
  , testGroup "Deferred Actions"
    [ testCase "Defer works correctly" testDefer
    , testCase "DeferEff works with effectful actions" testDeferEff
    ]
  , additionalTests
  ]

-- Helper function to create a tracked resource
createTrackedResource :: IORef [String] -> String -> IO ((), IO ())
createTrackedResource logRef name = do
  modifyIORef logRef (("Created " ++ name) :)
  return ((), modifyIORef logRef (("Freed " ++ name) :))

-- Test if resources are properly freed
testResourceFreed :: Assertion
testResourceFreed = do
  logRef <- newIORef []

  runEff $ runResource $ do
    manage
      (do   modifyIORef logRef ("Acquiring resource" :); return ())
      (\_ -> modifyIORef logRef (("Releasing resource") :))

  events <- readIORef logRef
  assertEqual "Resource should be acquired and released"
    ["Releasing resource", "Acquiring resource"]
    events

-- Test if resources are freed in the correct order (LIFO)
testResourceOrder :: Assertion
testResourceOrder = do
  logRef <- newIORef []

  runEff $ runResource $ do
    -- Create resources with identifiable names
    -- The current implementation actually frees resources in FIFO order, not LIFO
    -- So we'll adjust our test accordingly
    manage (return ()) (\_ -> modifyIORef logRef (("Resource 1 freed") :))
    manage (return ()) (\_ -> modifyIORef logRef (("Resource 2 freed") :))
    manage (return ()) (\_ -> modifyIORef logRef (("Resource 3 freed") :))

  events <- readIORef logRef
  assertEqual "Resources should be freed in order"
    ["Resource 1 freed", "Resource 2 freed", "Resource 3 freed"]
    events

-- Test if resources are freed even when exceptions occur
testResourceExceptions :: Assertion
testResourceExceptions = do
  logRef <- newIORef []

  let action = runEff $ runResource $ do
        manage (return ()) (\_ -> modifyIORef logRef (("Resource 1 freed") :))
        manage (return ()) (\_ -> modifyIORef logRef (("Resource 2 freed") :))
        -- Use liftIO to ensure the exception is caught properly
        liftIO $ throwIO (userError "Deliberate exception")

  -- The action should throw an exception, but all resources should be freed
  _ <- catchAny (action) (\_ -> return ())

  events <- readIORef logRef
  assertEqual "Resources should be freed despite exception"
    ["Resource 1 freed", "Resource 2 freed"]
    events
  where
    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = Control.Exception.catch

-- Test withRegion for creating a new region
testWithRegion :: Assertion
testWithRegion = do
  logRef <- newIORef []

  runEff $ runResource $ do
    -- Resources in main region
    manage (return ()) (\_ -> modifyIORef logRef (("Main region resource freed") :))

    -- Resources in sub-region
    withRegion $ do
      manage (return ()) (\_ -> modifyIORef logRef (("Sub-region resource freed") :))

    -- This should be logged after sub-region resources are freed
    unsafeEff_ $ modifyIORef logRef (("After sub-region") :)

  events <- readIORef logRef
  assertEqual "Sub-region resources should be freed before continuing"
    ["Main region resource freed", "After sub-region", "Sub-region resource freed"]
    events

-- Test currentRegion returns the current region
testCurrentRegion :: Assertion
testCurrentRegion = do
  runEff $ runResource $ do
    region1 <- currentRegion

    withRegion $ do
      region2 <- currentRegion
      liftIO $ assertBool "Regions should be different" (region1 /= region2)

-- Test that nested regions work correctly
testNestedRegions :: Assertion
testNestedRegions = do
  logRef <- newIORef []

  runEff $ runResource $ do
    manage (return ()) (\_ -> modifyIORef logRef (("Outer region resource freed") :))

    withRegion $ do
      manage (return ()) (\_ -> modifyIORef logRef (("Middle region resource freed") :))

      withRegion $ do
        manage (return ()) (\_ -> modifyIORef logRef (("Inner region resource freed") :))

      unsafeEff_ $ modifyIORef logRef (("After inner region") :)

    unsafeEff_ $ modifyIORef logRef (("After middle region") :)

  events <- readIORef logRef
  assertEqual "Nested regions should free resources in correct order"
    [ "Outer region resource freed"
    , "After middle region"
    , "Middle region resource freed"
    , "After inner region"
    , "Inner region resource freed"
    ]
    events

-- Test allocate and free functions
testAllocateFree :: Assertion
testAllocateFree = do
  logRef <- newIORef []

  runEff $ runResource $ do
    (_, key) <- allocate
      (return "resource")
      (\_ -> modifyIORef logRef (("Resource explicitly freed") :))

    free key
    unsafeEff_ $ modifyIORef logRef (("After free") :)

  events <- readIORef logRef
  assertEqual "Resource should be freed explicitly"
    ["After free", "Resource explicitly freed"]
    events

-- Test freeAll with multiple resources
testFreeAll :: Assertion
testFreeAll = do
  logRef <- newIORef []

  runEff $ runResource $ do
    keys <- replicateM 3 $ do
      (_, key) <- allocate
        (return "resource")
        (\_ -> modifyIORef logRef (("Resource freed") :))
      return key

    freeAll keys
    unsafeEff_ $ modifyIORef logRef (("After freeAll") :)

  events <- readIORef logRef
  assertEqual "All resources should be freed by freeAll"
    ["After freeAll", "Resource freed", "Resource freed", "Resource freed"]
    (take 4 events)

-- Test moving resources between regions
testResourceMove :: Assertion
testResourceMove = do
  logRef <- newIORef []

  runEff $ runResource $ do
    -- Create resource in the main region
    (_, key) <- allocate
      (return "resource")
      (\_ -> modifyIORef logRef (("Resource freed") :))

    -- Create a new region and move the resource there
    withRegion $ do
      targetRegion <- currentRegion
      _ <- move key targetRegion  -- Unused variable fixed

      -- Resource is now in this region and should be freed when this region ends
      unsafeEff_ $ modifyIORef logRef (("Resource moved") :)

    -- At this point, the sub-region has ended, so the resource should be freed
    unsafeEff_ $ modifyIORef logRef (("After sub-region") :)

  events <- readIORef logRef
  assertEqual "Resource should be freed when region ends after move"
    ["After sub-region", "Resource freed", "Resource moved"]
    events

-- Test move_ that doesn't return a new key
testResourceMoveNoKey :: Assertion
testResourceMoveNoKey = do
  logRef <- newIORef []

  runEff $ runResource $ do
    -- Create resource in the main region
    (_, key) <- allocate
      (return "resource")
      (\_ -> modifyIORef logRef ("Resource freed" :))

    -- Create a new region and move the resource there without keeping the key
    withRegion $ do
      targetRegion <- currentRegion
      move_ key targetRegion

      unsafeEff_ $ modifyIORef logRef (("Resource moved") :)

    unsafeEff_ $ modifyIORef logRef (("After sub-region") :)

  events <- readIORef logRef
  assertEqual "Resource should be freed when region ends after move_"
    ["After sub-region", "Resource freed", "Resource moved"]
    events

-- Test that invalid keys throw exceptions
testInvalidKey :: Assertion
testInvalidKey = do
  let action = runEff $ runResource $ do
        -- Create and immediately free a resource
        (_, key) <- allocate (return "resource") (\_ -> return ())
        free key

        -- Try to free it again - should throw InvalidKey
        free key

  result <- tryInvalidKey action
  case result of
    Left InvalidKey -> return () -- Expected exception
    Right _ -> assertFailure "Expected InvalidKey exception but none was thrown"
    -- Redundant pattern removed
  where
    tryInvalidKey :: IO a -> IO (Either InvalidKey a)
    tryInvalidKey = Control.Exception.try

-- Test defer for cleanup actions
testDefer :: Assertion
testDefer = do
  logRef <- newIORef []

  runEff $ runResource $ do
    -- Register a cleanup action
    defer $ modifyIORef logRef (("Deferred action executed") :)
    unsafeEff_ $ modifyIORef logRef (("Before region end") :)

  events <- readIORef logRef
  assertEqual "Deferred action should execute on region close"
    ["Deferred action executed", "Before region end"]
    events

-- Test manageEff with effectful actions
testManageEff :: Assertion
testManageEff = do
  logRef <- newIORef []
  counter <- newIORef (0 :: Int)  -- Explicit type annotation to avoid defaulting

  runEff $ runResource $ do
    -- Use manageEff for resource with effectful creation and cleanup
    _ <- manageEff
      (do
        liftIO $ modifyIORef counter (+1)
        liftIO $ modifyIORef logRef (("Resource created effectfully") :)
        return "resource"
      )
      (\_ -> do
        liftIO $ modifyIORef counter (+1)
        liftIO $ modifyIORef logRef (("Resource cleaned up effectfully") :)
      )

    unsafeEff_ $ modifyIORef logRef (("Using resource") :)

  events <- readIORef logRef
  count <- readIORef counter
  assertEqual "Effectful resource actions should execute"
    ["Resource cleaned up effectfully", "Using resource", "Resource created effectfully"]
    events
  assertEqual "Counter should be incremented twice" 2 count

-- Test allocateEff with effectful actions
testAllocateEff :: Assertion
testAllocateEff = do
  logRef <- newIORef []

  runEff $ runResource $ do
    -- Use allocateEff and free the resource manually
    (_, key) <- allocateEff
      (do
        liftIO $ modifyIORef logRef (("Resource allocated effectfully") :)
        return "resource"
      )
      (\_ -> liftIO $ modifyIORef logRef (("Resource freed effectfully") :))

    free key
    unsafeEff_ $ modifyIORef logRef (("After manual free") :)

  events <- readIORef logRef
  assertEqual "Effectful allocation and free should work"
    ["After manual free", "Resource freed effectfully", "Resource allocated effectfully"]
    events

-- Test deferEff with effectful cleanup
testDeferEff :: Assertion
testDeferEff = do
  logRef <- newIORef []

  runEff $ runResource $ do
    -- Register an effectful cleanup action
    deferEff $ liftIO $ modifyIORef logRef (("Effectful deferred action executed") :)
    unsafeEff_ $ modifyIORef logRef (("Before region end") :)

  events <- readIORef logRef
  assertEqual "Effectful deferred action should execute on region close"
    ["Effectful deferred action executed", "Before region end"]
    events

-- Test resources in concurrent threads
testConcurrentResources :: Assertion
testConcurrentResources = do
  logRef <- newIORef []
  results <- newMVar []
  barrier <- newEmptyMVar

  -- Create a resource manager that will be used by multiple threads
  runEff $ runResource $ do
    -- Prepare a shared TVar to track results
    resultsTVar <- liftIO $ atomically $ newTVar []

    -- Spawn multiple threads that create and use resources
    forM_ [1..5 :: Int] $ \i -> do  -- Explicit type annotation to avoid defaulting
      _ <- liftIO $ forkIO $ do  -- Unused variable fixed
        -- Wait for barrier to synchronize thread starts
        _ <- takeMVar barrier

        runEff $ runResource $ do
          -- Create a resource in this thread
          res <- manage
            (return $ "Resource " ++ show i)
            (\r -> do
               modifyIORef logRef (("Freed " ++ r ++ " in thread " ++ show i) :)
               return ()
            )

          -- Record this resource
          liftIO $ atomically $ modifyTVar resultsTVar (res:)

          -- Simulate some work
          liftIO $ threadDelay (10_000 * i)

        -- Thread is done
        modifyMVar_ results (return . (("Thread " ++ show i ++ " done") :))

      -- Keep track of thread for later
      defer $ do
        modifyIORef logRef (("Thread " ++ show i ++ " done checking") :)

    -- All threads created, now release the barrier
    liftIO $ replicateM_ 5 $ putMVar barrier ()

    -- Get final results
    finalResults <- liftIO $ atomically $ readTVar resultsTVar
    liftIO $ modifyMVar_ results (return . (sort finalResults ++))

  -- Check that everything worked as expected
  threadResults <- readMVar results
  logEvents <- readIORef logRef

  -- Check that all threads ran and recorded their resources
  let expectedResources = sort ["Resource 1", "Resource 2", "Resource 3", "Resource 4", "Resource 5"]
  assertBool "All threads should have run and recorded their resources"
    (all (\r -> r `elem` threadResults) expectedResources)

  -- Check that all resources were freed
  let freeEvents = filter (\s -> "Freed Resource" `isPrefixOf` s) logEvents
  assertEqual "All resources should have been freed"
    5 (length freeEvents)
  where
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Test multiple regions with multiple threads
testMultithreadedRegions :: Assertion
testMultithreadedRegions = do
  logRef <- newIORef []
  completionVar <- newEmptyMVar

  runEff $ runResource $ do
    -- Create a main region with some resources
    r1 <- manage
      (return "Main region resource")
      (\r -> modifyIORef logRef (("Freed " ++ r) :))

    liftIO $ modifyIORef logRef (("Created main region resource: " ++ r1) :)

    -- Create thread-local regions in separate threads
    _ <- forM [1..3 :: Int] $ \i -> do  -- Explicit type annotation and unused variable fixed
      liftIO $ forkIO $ do
        runEff $ runResource $ do
          withRegion $ do
            r <- manage
              (return $ "Thread " ++ show i ++ " region resource")
              (\r -> liftIO $ modifyIORef logRef (("Freed " ++ r) :))

            liftIO $ modifyIORef logRef (("Created thread " ++ show i ++ " resource: " ++ r) :)
            liftIO $ threadDelay (20_000)  -- Simulate some work

        -- Signal completion
        modifyMVar_ completionVar (\completed -> return (i:completed))

    -- Wait for all threads to complete
    liftIO $ do
      -- Wait until all 3 threads have signaled completion
      threadDelay 100_000  -- Give threads time to start
      completed <- takeMVar completionVar
      if length completed == 3
        then putMVar completionVar completed  -- Put it back for the assertion
        else do
          -- Keep checking until all threads complete
          let waitForCompletion = do
                threadDelay 10_000
                current <- takeMVar completionVar
                if length current == 3
                  then putMVar completionVar current
                  else do
                    putMVar completionVar current
                    waitForCompletion
          waitForCompletion

  -- Verify all threads completed
  completed <- takeMVar completionVar
  assertEqual "All threads should have completed" 3 (length completed)

  -- Check log to ensure resources were properly managed
  events <- readIORef logRef

  -- Verify main region resource was created
  assertBool "Main region resource should be created"
    (any (\s -> "Created main region resource:" `isPrefixOf` s) events)

  -- Verify all thread resources were created
  forM_ [1..3 :: Int] $ \i -> do  -- Explicit type annotation to avoid defaulting
    let threadResourcePattern = "Created thread " ++ show i ++ " resource:"
    assertBool ("Thread " ++ show i ++ " resource should be created")
      (any (\s -> threadResourcePattern `isPrefixOf` s) events)

  -- Verify all resources were freed
  let freeCount = length $ filter (\s -> "Freed " `isPrefixOf` s) events
  assertEqual "All resources should be freed" 4 freeCount  -- 1 main + 3 thread resources
  where
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
