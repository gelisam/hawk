{-# LANGUAGE OverloadedStrings #-}
-- | In which Hawk's command-line arguments are structured into a `HawkSpec`.
module System.Console.Hawk.Args.Parse (parseArgs) where

import Control.Applicative

import Control.Monad.Trans.OptionParser
import Control.Monad.Trans.Uncertain
import qualified System.Console.Hawk.Args.Option as Option
import           System.Console.Hawk.Args.Option (HawkOption, options)
import System.Console.Hawk.Args.Spec


-- | (line delimiter, word delimiter)
type CommonDelimiters = (Separator, Separator)

commonDelimiters :: OptionParser HawkOption CommonDelimiters
commonDelimiters = do
    l <- consumeLast Option.LineDelimiter "\n" Option.consumeDelimiter
    w <- consumeLast Option.WordDelimiter " " Option.consumeDelimiter
    return (l, w)


inputSpec :: CommonDelimiters -> OptionParser HawkOption InputSpec
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

outputSpec :: CommonDelimiters -> OptionParser HawkOption OutputSpec
outputSpec (l, w) = OutputSpec <$> sink <*> format
  where
    sink = return UseStdout
    format = OutputFormat <$> line <*> word
    line = consumeLast Option.OutputLineDelimiter l Option.consumeDelimiter
    word = consumeLast Option.OutputWordDelimiter w Option.consumeDelimiter


exprSpec :: OptionParser HawkOption ExprSpec
exprSpec = ExprSpec <$> prelude <*> expr
  where
    prelude = do
        r <- consumeLast Option.Recompile False consumeFlag
        return $ if r then UseUserPrelude else DetectPrelude
    expr = do
        r <- consumeExtra consumeString
        case r of
          Just e  -> return e
          Nothing -> fail "missing user expression"


-- |
-- >>> runUncertain $ parseArgs ["--help"]
-- Help
-- 
-- >>> runUncertain $ parseArgs ["--version"]
-- Version
-- 
-- >>> runUncertain $ parseArgs ["-d\\t", "L.head"]
-- Eval (ExprSpec {userPrelude = DetectPrelude, userExpression = "L.head"}) (OutputSpec {outputSink = UseStdout, outputFormat = OutputFormat {lineDelimiter = "\n", wordDelimiter = "\t"}})
-- 
-- >>> runUncertain $ parseArgs ["-d\\t", "-m", "L.head"]
-- Map (ExprSpec {userPrelude = DetectPrelude, userExpression = "L.head"}) (InputSpec {inputSource = UseStdin, inputFormat = Lines "\n" (Words "\t")}) (OutputSpec {outputSink = UseStdout, outputFormat = OutputFormat {lineDelimiter = "\n", wordDelimiter = "\t"}})
parseArgs :: [String] -> Uncertain HawkSpec
parseArgs = runOptionParserT options $ do
    cmd <- consumeExclusive assoc eval
    c <- commonDelimiters
    cmd c
  where
    assoc = [ (Option.Help,    help)
            , (Option.Version, version)
            , (Option.Apply,   apply)
            , (Option.Map,     map')
            ]
    
    help, version, eval, apply, map' :: CommonDelimiters
                                     -> OptionParser HawkOption HawkSpec
    help    _ = return Help
    version _ = return Version
    eval    c = Eval  <$> exprSpec <*>                 outputSpec c
    apply   c = Apply <$> exprSpec <*> inputSpec c <*> outputSpec c
    map'    c = Map   <$> exprSpec <*> inputSpec c <*> outputSpec c
