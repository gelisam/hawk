{-# LANGUAGE PackageImports #-}
-- | In which the state of a State monad is persisted to disk.
module Control.Monad.Trans.State.Persistent where

import Control.Monad
import Control.Monad.IO.Class
import "mtl" Control.Monad.Trans
import Control.Monad.Trans.State
import System.Directory


-- | Read and write the cache to a file. Not atomic.
-- 
-- >>> let f = "/tmp/doctest.txt"
-- >>> :{
-- do { exists <- doesFileExist f
--    ; when exists $ removeFile f
--    }
-- :}
-- 
-- >>> withPersistentState f 0 $ modify (+1) >> get
-- 1
-- >>> withPersistentState f 0 $ modify (+1) >> get
-- 2
-- 
-- >>> removeFile f
withPersistentState :: (Read s, Show s, Eq s)
                    => FilePath -> s -> State s a -> IO a
withPersistentState f default_s sx = do
    exists <- doesFileExist f
    s <- if exists
           then liftM read $ readFile f
           else return default_s
    let (x, s') = runState sx s
    when (s' /= s) $ do
      writeFile f $ show s'
    return x

-- | A monad-transformer version of `withPersistentState`.
-- 
-- >>> let f = "/tmp/doctest.txt"
-- >>> :{
-- do { exists <- doesFileExist f
--    ; when exists $ removeFile f
--    }
-- :}
-- 
-- >>> withPersistentStateT f 0 $ lift (putStrLn "hello") >> modify (+1) >> get
-- hello
-- 1
-- >>> withPersistentStateT f 0 $ lift (putStrLn "hello") >> modify (+1) >> get
-- hello
-- 2
-- 
-- >>> removeFile f
withPersistentStateT :: (MonadIO m, Read s, Show s, Eq s)
                     => FilePath -> s -> StateT s m a -> m a
withPersistentStateT f default_s sx = do
    exists <- liftIO $ doesFileExist f
    s <- if exists
           then liftM read $ liftIO $ readFile f
           else return default_s
    (x, s') <- runStateT sx s
    when (s' /= s) $ do
      liftIO $ writeFile f $ show s'
    return x
