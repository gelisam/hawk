{-# LANGUAGE PackageImports, RankNTypes #-}
-- | A computation which may raise warnings or fail in error.
module Control.Monad.Trans.Uncertain where

import Control.Applicative
import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Identity
import "transformers" Control.Monad.Trans.Error hiding (Error)
import "transformers" Control.Monad.Trans.Writer
import System.Exit
import System.IO
import Text.Printf


type Warning = String
type Error = String

newtype UncertainT m a = UncertainT
  { unUncertainT :: ErrorT Error (WriterT [Warning] m) a }

type Uncertain a = UncertainT Identity a

instance Functor m => Functor (UncertainT m) where
  fmap f = UncertainT . fmap f . unUncertainT

instance (Functor m, Monad m) => Applicative (UncertainT m) where
  pure = UncertainT . pure
  UncertainT mf <*> UncertainT mx = UncertainT (mf <*> mx)

instance Monad m => Monad (UncertainT m) where
  return = UncertainT . return
  UncertainT mx >>= f = UncertainT (mx >>= f')
    where
      f' = unUncertainT . f
  fail s = UncertainT (fail s)

instance MonadTrans UncertainT where
  lift = UncertainT . lift . lift

instance MonadIO m => MonadIO (UncertainT m) where
  liftIO = lift . liftIO


warn :: Monad m => String -> UncertainT m ()
warn s = UncertainT $ lift $ tell [s]

fromRightM :: Monad m => Either String a -> UncertainT m a
fromRightM (Left e)  = fail e
fromRightM (Right x) = return x


mapUncertainT :: (forall a. m a -> m' a) -> UncertainT m b -> UncertainT m' b
mapUncertainT f = UncertainT . (mapErrorT . mapWriterT) f . unUncertainT

runUncertainT :: UncertainT m a -> m (Either Error a, [Warning])
runUncertainT = runWriterT . runErrorT . unUncertainT


-- | A version of `runWarnings` which allows you to interleave IO actions
--   with uncertain actions.
-- 
-- Note that the warnings are displayed after the IO's output.
-- 
-- >>> :{
-- runWarningsIO $ do
--   warn "before"
--   lift $ putStrLn "IO"
--   warn "after"
--   return 42
-- :}
-- IO
-- warning: before
-- warning: after
-- Right 42
-- 
-- >>> :{
-- runWarningsIO $ do
--   warn "before"
--   lift $ putStrLn "IO"
--   fail "fatal"
--   return 42
-- :}
-- IO
-- warning: before
-- Left "fatal"
runWarningsIO :: UncertainT IO a -> IO (Either String a)
runWarningsIO u = do
    (r, warnings) <- runUncertainT u
    mapM_ (hPutStrLn stderr . printf "warning: %s") warnings
    return r

-- | A version of `runUncertain` which only prints the warnings, not the
--   errors. Unlike `runUncertain`, it doesn't terminate on error.
-- 
-- >>> :{
-- runWarnings $ do
--   warn "before"
--   warn "after"
--   return 42
-- :}
-- warning: before
-- warning: after
-- Right 42
-- 
-- >>> :{
-- runWarnings $ do
--   warn "before"
--   fail "fatal"
--   return 42
-- :}
-- warning: before
-- Left "fatal"
runWarnings :: Uncertain a -> IO (Either String a)
runWarnings = runWarningsIO . mapUncertainT (return . runIdentity)


-- | A version of `runUncertain` which allows you to interleave IO actions
--   with uncertain actions.
-- 
-- Note that the warnings are displayed after the IO's output.
-- 
-- >>> :{
-- runUncertainIO $ do
--   warn "before"
--   lift $ putStrLn "IO"
--   warn "after"
--   return 42
-- :}
-- IO
-- warning: before
-- warning: after
-- 42
-- 
-- >>> :{
-- runUncertainIO $ do
--   warn "before"
--   lift $ putStrLn "IO"
--   fail "fatal"
--   return 42
-- :}
-- IO
-- warning: before
-- error: fatal
-- *** Exception: ExitFailure 1
runUncertainIO :: UncertainT IO a -> IO a
runUncertainIO u = do
    r <- runWarningsIO u
    case r of
      Left e -> do
        hPutStrLn stderr $ printf "error: %s" e
        exitFailure
      Right x -> return x

-- | Print warnings and errors, terminating on error.
-- 
-- Note that the warnings are displayed even if there is also an error.
-- 
-- >>> :{
-- runUncertainIO $ do
--   warn "first"
--   warn "second"
--   fail "fatal"
--   return 42
-- :}
-- warning: first
-- warning: second
-- error: fatal
-- *** Exception: ExitFailure 1
runUncertain :: Uncertain a -> IO a
runUncertain = runUncertainIO . mapUncertainT (return . runIdentity)
