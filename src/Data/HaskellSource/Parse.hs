{-# LANGUAGE PackageImports #-}
-- | Parser combinators for processing HaskellSource.
module Data.HaskellSource.Parse where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.ByteString.Char8 as B
import Data.Functor.Identity

import Data.HaskellSource


-- | Consume part of a HaskellSource, writing those parts along the way.
type SourceParser a = StateT HaskellSource (WriterT HaskellSource Maybe) a


-- | Print the consumed lines, the result, and the remaining lines.
-- >>> testP "foo" (return 42)
-- 42
-- "foo"
testP :: Show a => String -> SourceParser a -> IO ()
testP str = go . runWriterT . (`runStateT` source)
  where
    source = parseSource (B.pack str)
    go Nothing = putStrLn "Nothing"
    go (Just ((r, xs), ys)) = do
      mapM_ show' ys
      print r
      mapM_ show' xs
    show' (Left s) = print s
    show' (Right s) = print s

-- |
-- >>> testP "foo" eof
-- Nothing
-- 
-- >>> testP "" eof
-- ()
eof :: SourceParser ()
eof = do
    [] <- get
    return ()

-- |
-- >>> testP "foo\nbar\n" line
-- "foo"
-- "foo"
-- "bar"
-- 
-- Make sure the state is backtracked after a failed alternative.
-- 
-- >>> testP "foo\nbar\n" (line >> eof)
-- Nothing
-- 
-- >>> testP "foo\nbar\n" ((line >> eof) <|> (line >> return ()))
-- "foo"
-- ()
-- "bar"
line :: SourceParser B.ByteString
line = do
    (x:xs) <- get
    put xs
    lift $ tell [x]
    case x of
      Left s -> return s
      Right s -> return (B.pack s)
