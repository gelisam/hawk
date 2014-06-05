{-# Language OverloadedStrings #-}
-- | A representation of Haskell source code.
-- 
-- Unlike haskell-src-exts, our goal is not to reconstruct detailed semantics,
-- but to preserve original line numbers (if applicable).
module Data.HaskellSource where

import Control.Monad.Trans.Class
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO
import System.Exit
import System.Process

import System.Directory.Extra
import Control.Monad.Trans.Uncertain


-- $setup
-- The code examples in this module assume the use of GHC's `OverloadedStrings`
-- extension:
--
-- >>> :set -XOverloadedStrings

-- | The Left original lines, which we never delete in order to
--   infer line numbers, while the Strings were inserted into the original.
type HaskellSource = [Either T.Text T.Text]


parseSource :: T.Text -> HaskellSource
parseSource = fmap Left . T.lines

-- | A string representation containing line pragmas so that compiler errors
--   are reported about the original file instead of the modified one.
-- 
-- >>> let (x:xs) = parseSource $ T.pack "bar = something\nmain = print 42\n"
-- >>> TextIO.putStr $ showSource "orig.hs" (x:xs)
-- bar = something
-- main = print 42
-- 
-- >>> TextIO.putStr $ showSource "orig.hs" (x:Right "foo = otherthing":xs)
-- bar = something
-- foo = otherthing
-- {-# LINE 2 "orig.hs" #-}
-- main = print 42
showSource :: FilePath -- ^ the original's filename,
                       --   used for fixing up line numbers
           -> HaskellSource -> T.Text
showSource orig = T.unlines . go True 1
  where
    go :: Bool -- ^ are line numbers already ok?
       -> Int  -- ^ the original number of the next original line
       -> HaskellSource
       -> [T.Text]
    go _     _ []           = []
    go True  i (Left x:xs)  = x
                            : go True (i + 1) xs
    go False i (Left x:xs)  = line_marker i
                            : x
                            : go True (i + 1) xs
    go _     i (Right x:xs) = x
                            : go False i xs

    line_marker :: Int -> T.Text
    line_marker i = T.unwords ["{-#", "LINE", T.pack $ show i, T.pack $ ("\"" ++ orig ++ "\""), "#-}"]


readSource :: FilePath -> IO HaskellSource
readSource = fmap parseSource . TextIO.readFile

writeSource :: FilePath -- ^ the original's filename,
                        --   used for fixing up line numbers
            -> FilePath
            -> HaskellSource
            -> IO ()
writeSource orig f = TextIO.writeFile f . showSource orig


compileSource :: FilePath -- ^ the original's filename,
                          --   used for fixing up line numbers
              -> FilePath -- ^ new filename, because ghc compiles from disk.
                          --   the compiled output will be in the same folder.
              -> HaskellSource
              -> UncertainT IO ()
compileSource = compileSourceWithArgs []

compileSourceWithArgs :: [String] -- ^ extra ghc args
                      -> FilePath -- ^ the original's filename,
                                  --   used for fixing up line numbers
                      -> FilePath -- ^ new filename, because ghc compiles from disk.
                                  --   the compiled output will be in the same folder.
                      -> HaskellSource
                      -> UncertainT IO ()
compileSourceWithArgs args orig f s = do
    lift $ writeSource orig f s
    compileFileWithArgs args f


compileFile :: FilePath -> UncertainT IO ()
compileFile = compileFileWithArgs []

compileFileWithArgs :: [String] -> FilePath -> UncertainT IO ()
compileFileWithArgs args f = do
    absFilePath <- lift $ absPath f
    let args' = absFilePath : "-v0" : args
    (exitCode, out, err) <- lift $ readProcessWithExitCode "ghc" args' ""
    case (exitCode, out ++ err) of
      (ExitSuccess, [])  -> return ()
      (ExitSuccess, msg) -> multilineWarn msg
      (_          , [])  -> fail $ "could not compile " ++ (show f)
      (_          , msg) -> multilineFail msg
