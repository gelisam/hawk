{-# LANGUAGE OverloadedStrings #-}
-- | The precisely-typed version of Hawk's command-line arguments.
module System.Console.Hawk.Args.Spec where

import Data.ByteString (ByteString)


data HawkSpec
    = Help
    | Version
    | Eval  ExprSpec           OutputSpec
    | Apply ExprSpec InputSpec OutputSpec
    | Map   ExprSpec InputSpec OutputSpec
  deriving (Show, Eq)


data InputSpec = InputSpec
    { inputSource :: InputSource
    , inputFormat :: InputFormat
    }
  deriving (Show, Eq)

data OutputSpec = OutputSpec
    { outputSink :: OutputSink
    , outputFormat :: OutputFormat
    }
  deriving (Show, Eq)


data InputSource
    = NoInput
    | UseStdin
    | InputFile FilePath
  deriving (Show, Eq)

data OutputSink
    = UseStdout
    -- OutputFile FilePath  -- we might want to implement --in-place
                            -- in the future
  deriving (Show, Eq)

data InputFormat
    = RawStream
    | Lines Separator LineFormat
  deriving (Show, Eq)

data LineFormat
    = RawLine
    | Fields Separator
  deriving (Show, Eq)

-- We can't know ahead of time whether it's going to be a raw stream
-- or raw lines or fields, it depends on the type of the user expression.
data OutputFormat = OutputFormat
    { lineDelimiter :: Delimiter
    , fieldDelimiter :: Delimiter
    }
  deriving (Show, Eq)


-- A separator is a strategy for separating a string into substrings.
-- One such strategy is to split the string on every occurrence of a
-- particular delimiter.
type Delimiter = ByteString
data Separator = Delimiter Delimiter
  deriving (Show, Eq)

fromSeparator :: Separator -> Delimiter
fromSeparator (Delimiter d) = d


data ExprSpec = ExprSpec
    { userContextDirectory :: FilePath
    , userExpression :: String
    }
  deriving (Show, Eq)

defaultInputSpec, noInput :: InputSpec
defaultInputSpec = InputSpec UseStdin defaultInputFormat
noInput          = InputSpec NoInput  defaultInputFormat

defaultOutputSpec :: OutputSpec
defaultOutputSpec = OutputSpec UseStdout defaultOutputFormat


defaultInputFormat :: InputFormat
defaultInputFormat = Lines defaultLineSeparator
                   $ Fields defaultFieldSeparator

defaultOutputFormat :: OutputFormat
defaultOutputFormat = OutputFormat defaultLineDelimiter defaultFieldDelimiter


defaultLineSeparator, defaultFieldSeparator :: Separator
defaultLineSeparator = Delimiter defaultLineDelimiter
defaultFieldSeparator = Delimiter defaultFieldDelimiter

defaultLineDelimiter, defaultFieldDelimiter :: Delimiter
defaultLineDelimiter = "\n"
defaultFieldDelimiter = " "
