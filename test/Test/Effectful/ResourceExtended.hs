module Test.Effectful.ResourceExtended where

import Control.Exception (try)
import Data.IORef

import Effectful
import Effectful.Resource

import Test.Tasty
import Test.Tasty.HUnit

-- Additional test suite for coverage
additionalTests :: TestTree
additionalTests = testGroup "Additional Effectful.Resource Tests"
  [ testGroup "Key Tests"
    [ testCase "Keys are unique and can be moved between regions" testKeyIDs
    ]
  , testGroup "Invalid Key Tests"
    [ testCase "Moving invalid key throws InvalidKey" testMoveInvalidKey
    , testCase "Freeing invalid key throws InvalidKey" testFreeInvalidKey
    , testCase "Key not found in region throws InvalidKey" testKeyNotFound
    ]
  , testGroup "Resource Return Value Tests"
    [ testCase "ManageIO returns correct resource" testManageIOReturn
    , testCase "Manage returns correct resource" testManageReturn
    , testCase "ManageEff returns correct resource" testManageEffReturn
    ]
  , testGroup "Move Tests"
    [ testCase "Move returns new key" testMoveReturnValue
    ]
  , testGroup "Defer Tests"
    [ testCase "Defer with return value" testDeferReturn
    , testCase "DeferEff with return value" testDeferEffReturn
    ]
  ]

-- Test that keys are unique and can be operated on correctly
testKeyIDs :: Assertion
testKeyIDs = do
  logRef <- newIORef []

  runEff $ runResource $ do
    -- Create resources with unique keys
    (_, key1) <- allocate (return "resource1") (\_ -> liftIO $ modifyIORef logRef ("Resource1 freed" :))
    (_, key2) <- allocate (return "resource2") (\_ -> liftIO $ modifyIORef logRef ("Resource2 freed" :))
    (_, key3) <- allocate (return "resource3") (\_ -> liftIO $ modifyIORef logRef ("Resource3 freed" :))

    -- Test that the keys are functionally distinct
    liftIO $ assertBool "Keys should be unique" (key1 /= key2)
    liftIO $ assertBool "Keys should be unique" (key2 /= key3)
    liftIO $ assertBool "Keys should be unique" (key1 /= key3)

    -- Create a new region to test key behavior
    withRegion $ do
      targetRegion <- currentRegion

      -- Moving a key from one region to another tests the internal region tracking
      movedKey <- move key1 targetRegion

      -- The new key should be valid and can be freed
      free movedKey
      liftIO $ modifyIORef logRef ("Moved key freed" :)

  -- After test completes, check the log
  logEntries <- readIORef logRef
  -- The resource1 should still be freed, but through the moved key
  assertBool "Resource1 should be freed via moved key" ("Moved key freed" `elem` logEntries)
  assertBool "Resource2 should be freed" ("Resource2 freed" `elem` logEntries)
  assertBool "Resource3 should be freed" ("Resource3 freed" `elem` logEntries)

  -- Now test the invalid key in a separate action
  let testInvalidKey = runEff $ runResource $ do
                          (_, k1) <- allocate (return "resource") (\_ -> return ())
                          region <- currentRegion
                          _ <- move k1 region  -- Use _ to ignore the return value
                          -- Try to use the original key after move
                          free k1

  result <- try @InvalidKey testInvalidKey
  case result of
    Left _ -> return () -- Expected InvalidKey exception
    Right _ -> assertFailure "Expected InvalidKey exception but none was thrown"

-- Test that moving an invalid key throws InvalidKey
testMoveInvalidKey :: Assertion
testMoveInvalidKey = do
  let action = runEff $ runResource $ do
        -- Create and immediately free a resource
        (_, key) <- allocate (return "resource") (\_ -> return ())
        free key

        -- Get current region for the move operation
        region <- currentRegion

        -- Try to move the already-freed key - should throw InvalidKey
        -- We'll observe the exception outside the Eff monad
        move key region

  result <- try @InvalidKey action
  case result of
    Left _ -> return () -- Expected exception
    Right _ -> assertFailure "Expected InvalidKey exception but none was thrown"

-- Test that freeing an invalid key throws InvalidKey
testFreeInvalidKey :: Assertion
testFreeInvalidKey = do
  let action = runEff $ runResource $ do
        -- Create and immediately free a resource
        (_, key) <- allocate (return "resource") (\_ -> return ())
        free key

        -- Try to free it again - should throw InvalidKey
        free key

  result <- try @InvalidKey action
  case result of
    Left _ -> return () -- Expected exception
    Right _ -> assertFailure "Expected InvalidKey exception but none was thrown"

-- Test that manageIO returns the correct resource
testManageIOReturn :: Assertion
testManageIOReturn = do
  result <- runEff $ runResource $ do
    -- Since manageIO is not exported, we'll test allocate which uses it
    (resource, _) <- allocate (return "test resource") (\_ -> return ())
    return resource

  assertEqual "ManageIO should return the correct resource" "test resource" result

-- Test that manage returns the correct resource
testManageReturn :: Assertion
testManageReturn = do
  result <- runEff $ runResource $ do
    resource <- manage (return "test resource") (\_ -> return ())
    return resource

  assertEqual "Manage should return the correct resource" "test resource" result

-- Test that manageEff returns the correct resource
testManageEffReturn :: Assertion
testManageEffReturn = do
  result <- runEff $ runResource $ do
    resource <- manageEff (return "test resource") (\_ -> return ())
    return resource

  assertEqual "ManageEff should return the correct resource" "test resource" result

-- Test a key not found in a region's resources list
-- This indirectly tests the Nothing case in extract
testKeyNotFound :: Assertion
testKeyNotFound = do
  let action = runEff $ runResource $ do
        -- Get the current region
        region1 <- currentRegion

        -- Create a key in a new region
        withRegion $ do
          (_, key) <- allocate (return "resource") (\_ -> return ())

          -- We don't need this region variable for the test
          -- but we still call currentRegion to check API semantics
          _ <- currentRegion

          -- Now we've created a key in this new region
          -- But we'll try to move it to region1 after freeing it,
          -- which should fail as the key won't be found
          free key
          move key region1

  result <- try @InvalidKey action
  case result of
    Left _ -> return () -- Expected exception
    Right _ -> assertFailure "Expected InvalidKey exception but none was thrown"

-- Test move returns a new key and the resource behavior after moving
testMoveReturnValue :: Assertion
testMoveReturnValue = do
  logRef <- newIORef []

  -- Run the test in a resource context
  runEff $ runResource $ do
    -- Create a resource with a cleanup action that we can track
    (_, key1) <- allocate
      (return "resource")
      (\_ -> liftIO $ modifyIORef logRef ("Resource freed" :))

    -- Move the resource to the same region
    region <- currentRegion
    key2 <- move key1 region

    -- Make sure keys are different
    liftIO $ assertBool "Move should return a new key" (key1 /= key2)

    -- Test that the original key is invalid
    let testInvalidOriginalKey = runEff $ runResource $ free key1
    result <- liftIO $ try @InvalidKey testInvalidOriginalKey
    case result of
      Left _ -> liftIO $ putStrLn "Original key correctly invalid after move"
      Right _ -> liftIO $ assertFailure "Expected InvalidKey exception but none was thrown"

  -- After the test, check that the resource was actually freed
  events <- readIORef logRef
  assertEqual "Resource should be freed" ["Resource freed"] events

-- Test defer with return value
testDeferReturn :: Assertion
testDeferReturn = do
  resultRef <- newIORef Nothing

  runEff $ runResource $ do
    -- Use defer, which creates an empty resource and attaches cleanup
    defer $ writeIORef resultRef (Just "defer executed")

  result <- readIORef resultRef
  assertEqual "Defer should execute cleanup action" (Just "defer executed") result

-- Test deferEff with return value
testDeferEffReturn :: Assertion
testDeferEffReturn = do
  resultRef <- newIORef Nothing

  runEff $ runResource $ do
    -- Use deferEff, which creates an empty resource and attaches effectful cleanup
    deferEff $ liftIO $ writeIORef resultRef (Just "deferEff executed")

  result <- readIORef resultRef
  assertEqual "DeferEff should execute cleanup action" (Just "deferEff executed") result