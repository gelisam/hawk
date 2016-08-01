{-# LANGUAGE PackageImports #-}
-- | A generic caching interface.
--
-- The intent is to support many concrete implementations,
-- and to use specify caching policies using combinators.
--
-- Note that even though we _support_ many concrete implementations,
-- for simplicity we only provide one based on an association-list.
module Data.Cache where

import Control.Monad
import "mtl" Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Maybe

-- $setup
-- >>> let verboseLength xs = liftIO (putStrLn xs) >> return (length xs)
-- >>> let cachedLength c xs = cached c xs (verboseLength xs)
-- >>> let testC c = mapM (cachedLength c) (words "one two one two testing testing")


-- Implementation notes: the `m` is a monad transformer stack, mostly StateT's,
-- holding the state of the cache. The combinators extend caches by adding more
-- state and code around the base object. This is analogous to the Decorator
-- pattern in OO, except each modification to `m` is visible in the type.
data Cache m k a = Cache
  { readCache      :: k      -> m (Maybe a)
  , writeCache     :: k -> a -> m Bool -- ^ False if full
  , clearCache     ::           m ()
  , clearFromCache :: k      -> m ()
  }

-- | Tries to avoid executing this computation in the future by storing it in
--   the cache.
cached :: Monad m => Cache m k a -> k -> m a -> m a
cached c k computeSlowly = do
    r <- readCache c k
    case r of
      Just x -> return x
      Nothing -> do
        x <- computeSlowly
        _ <- writeCache c k x
        return x


-- implementations


-- | A dummy cache which never caches anything.
--
-- Semantically equivalent to `finiteCache 0 $ assocCache`, except for the `m`.
--
-- >>> withNullCache testC
-- one
-- two
-- one
-- two
-- testing
-- testing
-- [3,3,3,3,7,7]
nullCache :: Monad m => Cache m k a
nullCache = Cache
    { readCache      = \_   -> return Nothing
    , writeCache     = \_ _ -> return False  -- always full!
    , clearCache     =         return ()
    , clearFromCache = \_   -> return ()
    }

withNullCache :: Monad m => (Cache m k v -> m a) -> m a
withNullCache body = body nullCache


-- | A very inefficient example implementation.
--
-- >>> withAssocCache testC
-- one
-- two
-- testing
-- [3,3,3,3,7,7]
assocCache :: (Monad m, Eq k) => Cache (StateT [(k,a)] m) k a
assocCache = Cache
    { readCache      = \k   -> liftM (lookup k) $ get
    , writeCache     = \k v -> modify ((k,v):)
                            >> return True  -- never full.
    , clearCache     =         put []
    , clearFromCache = \k   -> modify $ filter $ (/= k) . fst
    }

withAssocCache :: (Monad m, Eq k)
               => (Cache (StateT [(k,v)] m) k v -> StateT [(k,v)] m a)
               -> m a
withAssocCache body = evalStateT (body assocCache) []


-- decorators


-- | Only cache the first `n` requests (use n=-1 for unlimited).
--   Combine with a cache policy in order to reuse those `n` slots.
--
-- >>> withAssocCache $ withFiniteCache 2 $ testC
-- one
-- two
-- testing
-- testing
-- [3,3,3,3,7,7]
--
-- >>> withAssocCache $ withFiniteCache 1 $ testC
-- one
-- two
-- two
-- testing
-- testing
-- [3,3,3,3,7,7]
--
-- >>> withAssocCache $ withFiniteCache 0 $ testC
-- one
-- two
-- one
-- two
-- testing
-- testing
-- [3,3,3,3,7,7]
--
-- >>> withAssocCache $ withFiniteCache (-1) $ testC
-- one
-- two
-- testing
-- [3,3,3,3,7,7]
finiteCache :: Monad m => Int -> Cache m k a -> Cache (StateT Int m) k a
finiteCache n c = Cache
    { readCache      = \k   ->          (lift $ readCache      c k  )
    , writeCache     = \k v -> do
        alreadyFull <- isFull
        if alreadyFull
          then return False
          else do
            r <- lift $ writeCache c k v
            when r incr
            return r
    , clearCache     =         put 0 >> (lift $ clearCache     c    )
    , clearFromCache = \k   -> decr  >> (lift $ clearFromCache c k  )
    }
  where
    isFull = liftM (== n) $ get
    incr = modify (+1)
    decr = modify (subtract 1)

withFiniteCache :: Monad m
                => Int
                -> (Cache (StateT Int m) k v -> StateT Int m a)
                -> (Cache m k v -> m a)
withFiniteCache n body c = evalStateT (body $ finiteCache n c) 0


-- | An example cache-policy implementation: Least-Recently-Used.
--
-- >>> withAssocCache $ withFiniteCache 2 $ withLruCache testC
-- one
-- two
-- testing
-- [3,3,3,3,7,7]
--
-- >>> withAssocCache $ withFiniteCache 1 $ withLruCache testC
-- one
-- two
-- one
-- two
-- testing
-- [3,3,3,3,7,7]
--
-- >>> withAssocCache $ withFiniteCache 0 $ withLruCache testC
-- one
-- two
-- one
-- two
-- testing
-- testing
-- [3,3,3,3,7,7]
lruCache :: (Monad m, Eq k) => Cache m k a -> Cache (StateT [k] m) k a
lruCache c = Cache
    { readCache      = \k   -> do
      r <- lift $ readCache c k
      when (isJust r) $ touch k
      return r
    , writeCache     = \k v -> do
        r <- lift $ writeCache c k v
        if r
          then return True
          else do
            makeRoom
            lift $ writeCache c k v
    , clearCache     =                     (lift $ clearCache     c    )
    , clearFromCache = \k   -> remove k >> (lift $ clearFromCache c k  )
    }
  where
    touch k = write k
    makeRoom = do
        mostRecentlyUsed <- get
        case mostRecentlyUsed of
          (k:ks) -> do
            put ks
            lift $ clearFromCache c k
          [] -> do
            -- the cache is both full and empty? try to reset.
            lift $ clearCache c

    write  k = modify (k:)
    remove k = modify $ filter $ (/= k)

withLruCache :: (Monad m, Eq k)
             => (Cache (StateT [k] m) k v -> StateT [k] m a)
             -> (Cache m k v -> m a)
withLruCache body c = evalStateT (body $ lruCache c) []


-- | An extreme version of the LRU strategy.
--
-- Semantically equivalent to `lruCache . finiteCache 1`, except for the `m`.
--
-- >>> withAssocCache $ withSingletonCache testC
-- one
-- two
-- one
-- two
-- testing
-- [3,3,3,3,7,7]
singletonCache :: (Monad m) => Cache m k a -> Cache m k a
singletonCache c = c { writeCache = go }
  where
    go k mx = clearCache c >> writeCache c k mx

withSingletonCache :: (Monad m)
                   => (Cache m k v -> m a)
                   -> (Cache m k v -> m a)
withSingletonCache body c = body (singletonCache c)
