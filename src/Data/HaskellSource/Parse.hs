{-# LANGUAGE OverloadedStrings, PackageImports #-}
-- | Parser combinators for processing HaskellSource.
module Data.HaskellSource.Parse where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Search
import Data.Char
import Data.Functor.Identity

import Data.Chunks
import Data.HaskellSource


-- | Consume part of a HaskellSource, splitting it into chunks along the way.
type SourceParser a = StateT HaskellSource               -- yet to be parsed
                    ( WriterT (StickyList HaskellSource) -- emitted chunks
                      Maybe) a                           -- backtracking


-- | Use a SourceParser to split a HaskellSource into pieces.
-- >>> let s = parseSource $ B.pack "{-# LANGUAGE OverloadedStrings,\n    PackageImports #-}\nmodule Foo where\n"
-- >>> fmap (map length) $ splitSource (comment >> next_chunk >> module_declaration) s
-- Just [2,1]
splitSource :: SourceParser () -> HaskellSource -> Maybe [HaskellSource]
splitSource p = fmap toLists . execWriterT . (p `runStateT`)

-- | Print the consumed chunks, the result, and the remaining lines.
-- >>> testP "foo" (return 42)
-- 42
-- "foo"
testP :: Show a => String -> SourceParser a -> IO ()
testP str = go . runWriterT . (`runStateT` source)
  where
    source = parseSource (B.pack str)
    
    go Nothing = putStrLn "Nothing"
    go (Just ((r, xs), chunks)) = do
      mapM_ show_chunk $ toLists chunks
      print r
      mapM_ show_line xs
    
    show_line (Left s) = print s
    show_line (Right s) = print s
    
    show_chunk xs = do
        mapM_ show_line xs
        putStrLn "==="

-- | End of file.
-- 
-- >>> testP "foo" eof
-- Nothing
-- 
-- >>> testP "" eof
-- ()
eof :: SourceParser ()
eof = do
    [] <- get
    return ()

-- | Consume a line.
-- 
-- >>> testP "foo\nbar\n" line
-- "foo"
-- ===
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
-- ===
-- ()
-- "bar"
line :: SourceParser B.ByteString
line = do
    (x:xs) <- get
    put xs
    lift $ tell $ singleton [x]
    case x of
      Left s -> return s
      Right s -> return (B.pack s)

-- | Consume a line, stripping the whitespace at both ends. Skip empty lines.
-- 
-- >>> testP "  hello  " stripped_line
-- "  hello  "
-- ===
-- "hello"
-- 
-- >>> testP "  \n  hello  \n" stripped_line
-- "  "
-- "  hello  "
-- ===
-- "hello"
stripped_line :: SourceParser B.ByteString
stripped_line = do
    s <- strip <$> line
    if B.null s
      then stripped_line
      else return s
  where
    strip = dropWhileEnd isSpace
          . B.dropWhile isSpace
    dropWhileEnd p = fst . B.spanEnd p 

-- |
-- >>> testP "foo\nbar\nbaz\n" (line >> line >> next_chunk >> line >> return ())
-- "foo"
-- "bar"
-- ===
-- "baz"
-- ===
-- ()
next_chunk :: SourceParser ()
next_chunk = lift $ tell $ divider

-- |
-- >>> testP "-- single line comment" comment
-- "-- single line comment"
-- ===
-- ()
-- 
-- >>> testP "{- multi      \n   line       \n   comment -} \n" comment
-- "{- multi      "
-- "   line       "
-- "   comment -} "
-- ===
-- ()
comment :: SourceParser ()
comment = single_line_comment <|> multi_line_comment
  where
    single_line_comment = do
        x <- stripped_line
        guard ("--" `B.isPrefixOf` x)
    
    -- limitation: "{-}" is supposed to behave like "{-", not "{--}"
    multi_line_comment = do
        x <- stripped_line
        guard ("{-" `B.isPrefixOf` x)
        x' <- nested_tags "{-" "-}" x
        guard ("-}" `B.isSuffixOf` x')

-- |
-- >>> testP "module Foo where\nmain = 42\n" module_declaration
-- "module Foo where"
-- ===
-- ()
-- "main = 42"
-- 
-- >>> testP "module Foo\n  ( main  \n  ) where \nmain = 42\n" module_declaration
-- "module Foo"
-- "  ( main  "
-- "  ) where "
-- ===
-- ()
-- "main = 42"
module_declaration :: SourceParser ()
module_declaration = do
    x <- stripped_line
    guard ("module " `B.isPrefixOf` x)
    inside_declaration x
  where
    inside_declaration :: B.ByteString -> SourceParser ()
    inside_declaration s = do
        x <- nested_tags "(" ")" s
        if " where" `B.isSuffixOf` x
          then return ()
          else do
            x' <- stripped_line
            inside_declaration x'

-- |
-- >>> testP "import Foo\nmain = 42\n" import_declaration
-- "import Foo"
-- ===
-- ()
-- "main = 42"
-- 
-- >>> testP "import Foo ( foo\n           , bar\n           )\nmain = 42" import_declaration
-- "import Foo ( foo"
-- "           , bar"
-- "           )"
-- ===
-- ()
-- "main = 42"
-- 
-- >>> testP "import Control.Applicative\n  ((<$>), (<|>), (<*>))\n" import_declaration
-- "import Control.Applicative"
-- "  ((<$>), (<|>), (<*>))"
-- ===
-- ()
-- 
-- not supported: a newline before "qualified" or "as".
import_declaration :: SourceParser ()
import_declaration = do
    x <- stripped_line
    guard ("import " `B.isPrefixOf` x)
    x' <- nested_tags "(" ")" x
    when (x == x') $ do
      -- maybe the identifier list start on the next line?
      identifier_list <|> return ()
  where
    identifier_list = do
        x <- stripped_line
        guard ("(" `B.isPrefixOf` x)
        void $ nested_tags "(" ")" x

-- | Given a line which already contains some opening tags, keep
--   consuming lines until all the tags are closed.
-- 
-- >>> testP "foo(\n...\n)\n" (line >>= nested_tags (B.pack "(") (B.pack ")"))
-- "foo("
-- "..."
-- ")"
-- ===
-- ")"
-- 
-- >>> testP "(hello (\n  bar() (\n    ...))\n);" (line >>= nested_tags (B.pack "(") (B.pack ")"))
-- "(hello ("
-- "  bar() ("
-- "    ...))"
-- ");"
-- ===
-- ");"
nested_tags :: B.ByteString              -- ^ open tag
            -> B.ByteString              -- ^ close tag
            -> B.ByteString              -- ^ first line
            -> SourceParser B.ByteString -- ^ last line
nested_tags = go 0
  where
    go x open close str = do
        let x' = open_close open close str x
        if x' == 0
          then return str
          else do
            str' <- stripped_line
            go x' open close str'
    
    -- | Count how many closing tags are still expected.
    -- 
    -- >>> open_close (B.pack "(") (B.pack ")") (B.pack "(())())") 1
    -- 0
    -- >>> open_close (B.pack "(") (B.pack ")") (B.pack "(())()") 1
    -- 1
    -- >>> open_close (B.pack "{-") (B.pack "-}") (B.pack "-}{-{--}{--}") 1
    -- 1
    open_close :: B.ByteString -- ^ open tag
               -> B.ByteString -- ^ close tag
               -> B.ByteString -- ^ string to search
               -> Int          -- ^ number of open tags occuring before the start
               -> Int          -- ^ number of closed tags expected after the end
    open_close open close str x = x + count open - count close
      where
        count tag = length (indices tag str)
