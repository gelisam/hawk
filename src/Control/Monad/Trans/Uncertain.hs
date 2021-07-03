{-# LANGUAGE CPP, PackageImports, RankNTypes #-}
-- | A computation which may raise warnings or fail in error.
module Control.Monad.Trans.Uncertain where

import Prelude hiding (fail)

#if MIN_VERSION_base(4,12,0)
import Control.Monad.Fail (
#if !MIN_VERSION_base(4,13,0)
  MonadFail,
#endif
  fail)
#else
import Prelude (fail)
#endif
import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Identity hiding (fail)
import "transformers" Control.Monad.Trans.Except
import "transformers" Control.Monad.Trans.Writer
import System.Exit
import System.IO
import Text.Printf


type Warning = String
type Error = String

newtype UncertainT m a = UncertainT
  { unUncertainT :: ExceptT Error (WriterT [Warning] m) a }

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
#if MIN_VERSION_base(4,12,0)

instance Monad m => MonadFail (UncertainT m) where
#endif
  fail s = UncertainT (throwE s)

instance MonadTrans UncertainT where
  lift = UncertainT . lift . lift

instance MonadIO m => MonadIO (UncertainT m) where
  liftIO = lift . liftIO


warn :: Monad m => String -> UncertainT m ()
warn s = UncertainT $ lift $ tell [s]

fromRightM :: Monad m => Either String a -> UncertainT m a
fromRightM (Left e)  = fail e
fromRightM (Right x) = return x


multilineMsg :: String -> String
multilineMsg = concatMap (printf "\n  %s") . lines

-- | Indent a multiline warning message.
-- >>> :{
-- runUncertainIO $ do
--   multilineWarn "foo\nbar\n"
--   return 42
-- :}
-- warning:
--   foo
--   bar
-- 42
multilineWarn :: Monad m => String -> UncertainT m ()
multilineWarn = warn . multilineMsg

-- | Indent a multiline error message.
-- >>> :{
-- runUncertainIO $ do
--   multilineFail "foo\nbar\n"
--   return 42
-- :}
-- error:
--   foo
--   bar
-- *** Exception: ExitFailure 1
multilineFail :: Monad m => String -> UncertainT m a
multilineFail = fail . multilineMsg


mapUncertainT :: (forall a. m a -> m' a) -> UncertainT m b -> UncertainT m' b
mapUncertainT f = UncertainT . (mapExceptT . mapWriterT) f . unUncertainT

runUncertainT :: UncertainT m a -> m (Either Error a, [Warning])
runUncertainT = runWriterT . runExceptT . unUncertainT

uncertainT :: Monad m => (Either Error a, [Warning]) -> UncertainT m a
uncertainT (Left  e, warnings) = mapM_ warn warnings >> fail e
uncertainT (Right x, warnings) = mapM_ warn warnings >> return x


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


-- | Upgrade an `IO a -> IO a` wrapping function into a variant which uses
--   `UncertainT IO` instead of `IO`.
--
-- >>> :{
-- let wrap body = do { putStrLn "before"
--                    ; r <- body
--                    ; putStrLn "after"
--                    ; return r
--                    }
-- :}
--
-- >>> :{
-- wrap $ do { putStrLn "hello"
--           ; return 42
--           }
-- :}
-- before
-- hello
-- after
-- 42
--
-- >>> :{
-- runUncertainIO $ wrapUncertain wrap
--                $ do { lift $ putStrLn "hello"
--                     ; warn "be careful!"
--                     ; return 42
--                     }
-- :}
-- before
-- hello
-- after
-- warning: be careful!
-- 42
wrapUncertain :: (Monad m, Monad m')
              => (forall a. m a -> m' a)
              -> (UncertainT m b -> UncertainT m' b)
wrapUncertain wrap body = wrapUncertainArg wrap' body'
  where
    wrap' f = wrap $ f ()
    body' () = body

-- | A version of `wrapUncertain` for wrapping functions of type
--   `(Handle -> IO a) -> IO a`.
--
-- >>> :{
-- let wrap body = do { putStrLn "before"
--                    ; r <- body 42
--                    ; putStrLn "after"
--                    ; return r
--                    }
-- :}
--
-- >>> :{
-- wrap $ \x -> do { putStrLn "hello"
--                 ; return (x + 1)
--                 }
-- :}
-- before
-- hello
-- after
-- 43
--
-- >>> :{
-- runUncertainIO $ wrapUncertainArg wrap
--                $ \x -> do { lift $ putStrLn "hello"
--                           ; warn "be careful!"
--                           ; return (x + 1)
--                           }
-- :}
-- before
-- hello
-- after
-- warning: be careful!
-- 43
wrapUncertainArg :: (Monad m, Monad m')
                 => (forall a. (v -> m a) -> m' a)
                 -> ((v -> UncertainT m b) -> UncertainT m' b)
wrapUncertainArg wrap body = do
    (r, ws) <- lift $ wrap $ runUncertainT . body

    -- repackage the warnings and errors
    mapM_ warn ws
    fromRightM r
