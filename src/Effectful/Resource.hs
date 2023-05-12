{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Effectful.Resource
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- A region-based resource effect for the effectful ecosystem.
-----------------------------------------------------------------------------
module Effectful.Resource
  ( -- * Resource Effect
    Resource
  , runResource
  -- * Manage Regions
  , Region
  , withRegion
  , currentRegion
  -- * Manage Resources
  , Key
  , InvalidKey(..)
  , manage
  , allocate
  , free
  , freeAll
  , move
  , move_
  , defer
  ) where

-- base
import Control.Exception (Exception, bracket, finally, mask_, uninterruptibleMask_)
import Control.Monad     (join)
import Data.Functor      (void)

-- effectful-core
import Effectful                 (Dispatch(..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects(..), StaticRep, evalStaticRep,
                                  getStaticRep, localStaticRep, unsafeEff_,
                                  unsafeSeqUnliftIO)

-- stm
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar,
                               stateTVar, throwSTM, writeTVar)

-- | A region owns resources and frees them on close.
data Region = Region
  { resources :: TVar [(Key, IO ())]
  , nextID    :: TVar Int
  }
  deriving Eq

-- | Each resource is identified by a unique key.
data Key = Key
  { _keyID    :: Int
  , keyRegion :: Region
  }
  deriving Eq

open :: IO Region
open =
  Region
    <$> newTVarIO []
    <*> newTVarIO 0

close :: Region -> IO ()
close region = uninterruptibleMask_ $ do
  rs <-
    atomically $
      stateTVar
        ( resources region )
        ( \r -> (r, []) )
  freeList rs
    where
      freeList []         = pure ()
      freeList ((_,m):ms) = m `finally` freeList ms

manageIO
  :: Region
  -> IO a
  -> (a -> IO ())
  -> IO (a, Key)
manageIO region create destroy = mask_ $ do
  a <- create
  atomically $ do
    next <- readTVar idTVar
    let key = Key next region
    modifyTVar' idTVar succ
    modifyTVar' rsTVar ((key, destroy a) :)
    pure (a, key)
  where
    idTVar = nextID region
    rsTVar = resources region

-- | An error which occurs if a key is freed\/moved that has already been freed\/moved.
data InvalidKey = InvalidKey
  deriving (Eq, Ord, Show)

instance Exception InvalidKey

extract :: Eq k => k -> [(k,v)] -> Maybe (v, [(k,v)])
extract k = extract' id
  where
    extract' _ [] = Nothing
    extract' f (x@(k',v):xs)
      | k == k'   = Just (v, f xs)
      | otherwise = extract' (f . (x:)) xs

moveIO :: Key -> Region -> IO Key
moveIO key region = atomically $ do
  rs <- readTVar keyRsTVar
  case extract key rs of
    Nothing -> throwSTM InvalidKey
    Just (m, rs') -> do
      writeTVar keyRsTVar rs'
      next <- readTVar idTVar
      modifyTVar' idTVar succ
      let newKey = Key next region
      modifyTVar' rsTVar ((newKey, m) :)
      pure newKey
  where
    keyRsTVar = resources $ keyRegion key
    idTVar    = nextID region
    rsTVar    = resources region

-- | The region-based resource effect.
data Resource :: Effect

type instance DispatchOf Resource = Static WithSideEffects

newtype instance StaticRep Resource = Resource Region

-- | Runs the resource effect.
runResource :: IOE :> es => Eff (Resource : es) a -> Eff es a
runResource m =
  unsafeSeqUnliftIO $ \run ->
    bracket open close $ \emptyRegion ->
      run $
        evalStaticRep (Resource emptyRegion) m

-- | Runs a computation in a new region.
withRegion :: Resource :> es => Eff es a -> Eff es a
withRegion m =
  unsafeSeqUnliftIO $ \run ->
    bracket open close $ \emptyRegion ->
      run $
        localStaticRep (\_ -> Resource emptyRegion) m

-- | Gets the current region.
currentRegion :: Resource :> es => Eff es Region
currentRegion = do
  Resource region <- getStaticRep
  pure region

-- | Allocates a resource in the current region which can be moved and freed
-- manually using its key.
allocate
  :: Resource :> es
  => IO a            -- ^ The computation which acquires the resource.
  -> (a -> IO b)     -- ^ The computation which releases the resource.
  -> Eff es (a, Key) -- ^ The acquired resource and its corresponding key.
allocate create destroy = do
  Resource region <- getStaticRep
  unsafeEff_ $ manageIO region create (void . destroy)

-- | Allocates a resource in the current region which is automatically freed at
-- the end of the region.
manage
  :: Resource :> es
  => IO a        -- ^ The computation which acquires the resource.
  -> (a -> IO b) -- ^ The computation which releases the resource.
  -> Eff es a    -- ^ The acquired resource.
manage create destroy =
  fmap fst $ allocate create destroy

-- | Moves a resource to the specified region, yielding a new key for the resource.
-- The old key is invalid after the movement.
move :: Resource :> es => Key -> Region -> Eff es Key
move key region = 
  unsafeEff_ $ moveIO key region

-- | Moves a resource to the specified region. It is freed at the end of this region.
-- The key of the moved resource is invalid after the movement.
move_ :: Resource :> es => Key -> Region -> Eff es ()
move_ key region =
  void $ move key region

freeIO :: Key -> IO ()
freeIO key =
  join . atomically $ do
    rs <- readTVar keyRsTVar
    case extract key rs of
      Nothing -> throwSTM InvalidKey
      Just (m, rs') -> do
        writeTVar keyRsTVar rs'
        pure m
  where
    keyRsTVar = resources $ keyRegion key
{-# INLINE freeIO #-}

-- | Frees a resource manually.
free :: Key -> Eff es ()
free = unsafeEff_ . uninterruptibleMask_ . freeIO

-- | Frees a collection of resources manually.
freeAll :: Foldable t => t Key -> Eff es ()
freeAll = unsafeEff_ . uninterruptibleMask_ . mapM_ freeIO

-- | Associats a cleanup action with the current region which is executed when the
-- region is closed.
defer :: Resource :> es => IO a -> Eff es ()
defer action =
  manage (pure ()) (const action)