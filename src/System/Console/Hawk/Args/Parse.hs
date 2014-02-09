{-# LANGUAGE OverloadedStrings, PackageImports #-}
-- | In which Hawk's command-line arguments are structured into a `HawkSpec`.
module System.Console.Hawk.Args.Parse (parseArgs) where

import Control.Applicative
import "mtl" Control.Monad.Trans

import Control.Monad.Trans.OptionParser
import Control.Monad.Trans.Uncertain
import qualified System.Console.Hawk.Args.Option as Option
import           System.Console.Hawk.Args.Option (HawkOption, options)
import           System.Console.Hawk.Args.Spec
import           System.Console.Hawk.Context.Dir

-- $setup
-- >>> let testP parser = runUncertainIO . runOptionParserT options parser


-- | (line delimiter, word delimiter)
type CommonDelimiters = (Separator, Separator)

-- | Extract '-D' and '-d'. We perform this step separately because those two
--   delimiters are used by both the input and output specs.
-- 
-- >>> let test = testP commonDelimiters
-- 
-- >>> test []
-- ("\n"," ")
-- 
-- >>> test ["-D\\n", "-d\\t"]
-- ("\n","\t")
-- 
-- >>> test ["-D|", "-d,"]
-- ("|",",")
commonDelimiters :: (Functor m, Monad m)
                 => OptionParserT HawkOption m CommonDelimiters
commonDelimiters = do
    l <- lastDelim Option.LineDelimiter defaultLineSeparator
    w <- lastDelim Option.WordDelimiter defaultWordSeparator
    return (l, w)
  where
    lastDelim ctor def = consumeLast ctor def Option.consumeDelimiter


-- | The input delimiters have already been parsed, but we still need to
--   interpret them and to determine the input source.
-- 
-- >>> :{
-- let test = testP $ do { c <- commonDelimiters
--                       ; _ <- consumeExtra consumeString  -- skip expr
--                       ; i <- inputSpec c
--                       ; lift $ print $ inputSource i
--                       ; lift $ print $ inputFormat i
--                       }
-- :}
-- 
-- >>> test []
-- UseStdin
-- Lines "\n" (Words " ")
-- 
-- >>> test ["-d", "-a", "L.reverse"]
-- UseStdin
-- Lines "\n" RawLine
-- 
-- >>> test ["-D", "-a", "B.reverse"]
-- UseStdin
-- RawStream
-- 
-- >>> test ["-d:", "-m", "L.head", "/etc/passwd"]
-- InputFile "/etc/passwd"
-- Lines "\n" (Words ":")
inputSpec :: (Functor m, Monad m)
          => CommonDelimiters -> OptionParserT HawkOption m InputSpec
inputSpec (l, w) = InputSpec <$> source <*> format
  where
    source = do
        r <- consumeExtra consumeString
        return $ case r of
          Nothing -> UseStdin
          Just f  -> InputFile f
    format = return streamFormat
    streamFormat | l == ""   = RawStream
                 | otherwise = Lines l lineFormat
    lineFormat | w == ""   = RawLine
               | otherwise = Words w

-- | The output delimiters take priority over the input delimiters, regardless
--   of the order in which they appear.
-- 
-- >>> :{
-- let test = testP $ do { c <- commonDelimiters
--                       ; o <- outputSpec c
--                       ; let OutputFormat l w = outputFormat o
--                       ; lift $ print $ outputSink o
--                       ; lift $ print (l, w)
--                       }
-- :}
-- 
-- >>> test []
-- UseStdout
-- ("\n"," ")
-- 
-- >>> test ["-D;", "-d", "-a", "L.reverse"]
-- UseStdout
-- (";","")
-- 
-- >>> test ["-o\t", "-d,", "-O|"]
-- UseStdout
-- ("|","\t")
outputSpec :: (Functor m, Monad m)
           => CommonDelimiters -> OptionParserT HawkOption m OutputSpec
outputSpec (l, w) = OutputSpec <$> sink <*> format
  where
    sink = return UseStdout
    format = OutputFormat <$> line <*> word
    line = consumeLast Option.OutputLineDelimiter l Option.consumeDelimiter
    word = consumeLast Option.OutputWordDelimiter w Option.consumeDelimiter


-- | The information we need in order to evaluate a user expression:
--   the expression itself, and the context in which it should be evaluated.
--   In Hawk, that context is the user prelude.
-- 
-- >>> :{
-- let test = testP $ do { e <- exprSpec
--                       ; lift $ print $ userExpression e
--                       ; lift $ print $ userContextDirectory e
--                       }
-- :}
-- 
-- >>> test []
-- error: missing user expression
-- *** Exception: ExitFailure 1
-- 
-- >>> test ["-D;", "-d", "-a", "L.reverse","-c","somedir"]
-- "L.reverse"
-- "somedir"
exprSpec :: (Functor m, MonadIO m)
         => OptionParserT HawkOption m ExprSpec
exprSpec = ExprSpec <$> contextDir <*> expr
  where
    contextDir = do
      dir <- consumeLast Option.ContextDirectory "" consumeString
      if null dir
        then liftIO findContextFromCurrDirOrDefault
        else return dir
    expr = do
        r <- consumeExtra consumeString
        case r of
          Just e  -> return e
          Nothing -> fail "missing user expression"


-- | Parse command-line arguments to construct a `HawkSpec`.
-- 
-- TODO: complain if some arguments are unused (except perhaps "-d" and "-D").
-- 
-- >>> :{
-- let test args = do { spec <- runUncertainIO $ parseArgs args
--                    ; case spec of
--                        Help        -> putStrLn "Help"
--                        Version     -> putStrLn "Version"
--                        Eval  e   o -> putStrLn "Eval"  >> print (userExpression e)                                         >> print (lineDelimiter (outputFormat o), wordDelimiter (outputFormat o))
--                        Apply e i o -> putStrLn "Apply" >> print (userExpression e, inputSource i) >> print (inputFormat i) >> print (lineDelimiter (outputFormat o), wordDelimiter (outputFormat o))
--                        Map   e i o -> putStrLn "Map"   >> print (userExpression e, inputSource i) >> print (inputFormat i) >> print (lineDelimiter (outputFormat o), wordDelimiter (outputFormat o))
--                    }
-- :}
-- 
-- >>> test []
-- Help
-- 
-- >>> test ["--help"]
-- Help
-- 
-- >>> test ["--version"]
-- Version
-- 
-- >>> test ["-d\\t", "L.head"]
-- Eval
-- "L.head"
-- ("\n","\t")
-- 
-- >>> test ["-D\r\n", "-d\\t", "-m", "L.head"]
-- Map
-- ("L.head",UseStdin)
-- Lines "\r\n" (Words "\t")
-- ("\r\n","\t")
-- 
-- >>> test ["-D", "-O\n", "-m", "L.head", "file.in"]
-- Map
-- ("L.head",InputFile "file.in")
-- RawStream
-- ("\n"," ")
parseArgs :: (Functor m,MonadIO m) => [String] -> UncertainT m HawkSpec
parseArgs [] = return Help
parseArgs args = runOptionParserT options parser args
  where
    parser = do
        lift $ return ()  -- silence a warning
        cmd <- consumeExclusive assoc eval
        c <- commonDelimiters
        cmd c
    assoc = [ (Option.Help,    help)
            , (Option.Version, version)
            , (Option.Apply,   apply)
            , (Option.Map,     map')
            ]
    
    help, version, eval, apply, map' :: (Functor m,MonadIO m) => CommonDelimiters
                                     -> OptionParserT HawkOption m HawkSpec
    help    _ = return Help
    version _ = return Version
    eval    c = Eval  <$> exprSpec <*>                 outputSpec c
    apply   c = Apply <$> exprSpec <*> inputSpec c <*> outputSpec c
    map'    c = Map   <$> exprSpec <*> inputSpec c <*> outputSpec c
