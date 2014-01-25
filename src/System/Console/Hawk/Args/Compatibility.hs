{-# LANGUAGE OverloadedStrings #-}
-- | Convert new-style Specs to old-style Options.
module System.Console.Hawk.Args.Compatibility
  ( optionsFromSpec
  , notOptionsFromSpec
  , fileFromInputSource
  ) where

import Control.Applicative
import Data.Maybe

import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Options


optionsFromSpec :: HawkSpec -> Options
optionsFromSpec Help = defaultOptions { optHelp = True }
optionsFromSpec Version = defaultOptions { optVersion = True }
optionsFromSpec (Eval  x   o) = updateExprOptions x
                              $ updateOutputOptions o
                              $ defaultOptions { optMode = EvalMode }
optionsFromSpec (Apply x i o) = updateExprOptions x
                              $ updateInputOptions i
                              $ updateOutputOptions o
                              $ defaultOptions { optMode = ApplyMode }
optionsFromSpec (Map   x i o) = updateExprOptions x
                              $ updateInputOptions i
                              $ updateOutputOptions o
                              $ defaultOptions { optMode = MapMode }

updateInputOptions :: InputSpec -> Options -> Options
updateInputOptions (InputSpec _ format) = go format
  where
    go RawStream            o = o { optLinesDelim = Just "" }
    go (Lines s lineFormat) o = goLine lineFormat
                              $ o { optLinesDelim = Just s }
    goLine RawLine   o = o { optWordsDelim = Just "" }
    goLine (Words s) o = o { optWordsDelim = Just s }

updateOutputOptions :: OutputSpec -> Options -> Options
updateOutputOptions (OutputSpec _ format) = go format
  where
    go (OutputFormat lineD wordD) o = o { optOutLinesDelim = Just lineD
                                        , optOutWordsDelim = Just wordD
                                        }

updateExprOptions :: ExprSpec -> Options -> Options
updateExprOptions (ExprSpec p _) = updatePreludeOptions p

updatePreludeOptions :: PreludeSpec -> Options -> Options
updatePreludeOptions UseUserPrelude     o = o { optRecompile = True }
updatePreludeOptions DetectPrelude      o = o


-- | The "not option"s are the extra string arguments after all the flags.
--   In the case of Hawk, this is the user expression and optionally an input
--   file.
notOptionsFromSpec :: HawkSpec -> [String]
notOptionsFromSpec s = maybeToList expr ++ maybeToList file
  where
    expr = userExpression <$> exprSpecFromSpec s
    file = do
        i <- inputSpecFromSpec s
        case inputSource i of
          InputFile f -> return f
          _           -> Nothing

exprSpecFromSpec :: HawkSpec -> Maybe ExprSpec
exprSpecFromSpec (Eval  e   _) = Just e
exprSpecFromSpec (Apply e _ _) = Just e
exprSpecFromSpec (Map   e _ _) = Just e
exprSpecFromSpec _ = Nothing

inputSpecFromSpec :: HawkSpec -> Maybe InputSpec
inputSpecFromSpec (Apply _ i _) = Just i
inputSpecFromSpec (Map   _ i _) = Just i
inputSpecFromSpec _ = Nothing


fileFromInputSource :: InputSource -> Maybe FilePath
fileFromInputSource (InputFile f) = Just f
fileFromInputSource _ = Nothing
