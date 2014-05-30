{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
-- | In which the state of a State monad is persisted to disk.
module Control.Monad.Trans.State.Persistent where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import "mtl" Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Functor.Identity
import System.Directory
import System.FilePath
import System.IO

-- $setup
-- >>> tmp <- getTemporaryDirectory
-- >>> let f = tmp </> "doctest.txt"


-- | Read and write the cache to a file. Not atomic.
-- 
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
withPersistentState :: forall s a. (Read s, Show s, Eq s)
                    => FilePath -> s -> State s a -> IO a
withPersistentState f default_s sx = do
    withPersistentStateT f default_s sTx
  where
    sTx :: StateT s IO a
    sTx = mapStateT (return . runIdentity) sx

-- | A monad-transformer version of `withPersistentState`.
-- 
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
-- 
-- If the contents of the file has been corrupted, revert to the default value.
-- 
-- >>> withPersistentStateT f "." $ lift (putStrLn "hello") >> modify (++".") >> get
-- hello
-- ".."
-- >>> withPersistentStateT f "." $ lift (putStrLn "hello") >> modify (++".") >> get
-- hello
-- "..."
-- 
-- 
-- >>> removeFile f
withPersistentStateT :: forall m s a. (Functor m, MonadIO m, Read s, Show s, Eq s)
                     => FilePath -> s -> StateT s m a -> m a
withPersistentStateT f default_s sx = do
    Just s <- runMaybeT (get_s <|> get_default_s)
    (x, s') <- runStateT sx s
    when (s' /= s) $ do
      liftIO $ writeFile f $ show s'
    return x
  where
    get_s :: MaybeT m s
    get_s = do
        exists <- liftIO $ doesFileExist f
        guard exists
        
        -- close the file even if the parsing fails
        Just s <- liftIO $ withFile f ReadMode $ \h -> do
          file_contents <- hGetContents h
          case reads file_contents of
            [(s, "")] -> return (Just s)
            _         -> return Nothing
        return s
    
    get_default_s :: MaybeT m s
    get_default_s = return default_s


-- | Combine consecutive StateT transformers into a single StateT, so the state
--   can be persisted to a single file.
-- 
-- >>> let sx = modify (+10)       :: StateT Int (State Int) ()
-- >>> let tx = lift $ modify (*2) :: StateT Int (State Int) ()
-- >>> execState (withCombinedState $ sx >> tx) (1, 2)
-- (11,4)
withCombinedState :: Monad m => StateT s (StateT t m) a -> StateT (s, t) m a
withCombinedState ssx = do
    (s, t) <- get
    ((x, s'), t') <- lift $ runStateT (runStateT ssx s) t
    put (s', t')
    return x
