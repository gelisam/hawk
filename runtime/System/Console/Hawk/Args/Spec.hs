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
    { lineDelimiter :: Separator
    , fieldDelimiter :: Separator
    }
  deriving (Show, Eq)

type Separator = ByteString


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
defaultInputFormat = Lines defaultLineSeparator (Fields defaultFieldSeparator)

defaultOutputFormat :: OutputFormat
defaultOutputFormat = OutputFormat defaultLineSeparator defaultFieldSeparator


defaultLineSeparator, defaultFieldSeparator :: Separator
defaultLineSeparator = "\n"
defaultFieldSeparator = " "
