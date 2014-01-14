{-# LANGUAGE OverloadedStrings #-}
-- | Convert new-style Specs to old-style Options.
module System.Console.Hawk.Args.Compatibility where

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
updatePreludeOptions UseCachedPrelude   _ = error "cannot be represented with Options"
updatePreludeOptions (UsePreludeFile f) o = o { optModuleFile = Just f }
updatePreludeOptions DetectPrelude      o = o
