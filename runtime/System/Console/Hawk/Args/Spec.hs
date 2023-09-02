{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- | The precisely-typed version of Hawk's command-line arguments.
module System.Console.Hawk.Args.Spec where

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
    | Records Separator RecordFormat
  deriving (Show, Eq)

data RecordFormat
    = RawRecord
    | Fields Separator
  deriving (Show, Eq)

-- We can't know ahead of time whether it's going to be a raw stream
-- or raw records or fields, it depends on the type of the user expression.
data OutputFormat = OutputFormat
    { recordDelimiter :: Delimiter
    , fieldDelimiter :: Delimiter
    }
  deriving (Show, Eq)


-- A 'Processor' describes how to process a string; either by separating it
-- into chunks or by leaving it as-is. When separating it into chunks, we can
-- use whitespace as a delimiter (meaning one or more consecutive whitespace
-- characters), or we can use a specific delimiter.
type Delimiter = String
data Separator = Whitespace | Delimiter Delimiter
  deriving (Show, Eq)
data Processor = DoNotSeparate | SeparateOn Separator
  deriving (Show, Eq)

fromSeparator :: Delimiter -> Separator -> Delimiter
fromSeparator def = \case
  Whitespace  -> def
  Delimiter d -> d

fromProcessor :: Delimiter -> Processor -> Delimiter
fromProcessor def = \case
  DoNotSeparate -> def
  SeparateOn s  -> fromSeparator def s


newtype ContextSpec = ContextSpec
    { userContextDirectory :: FilePath
    }
  deriving (Show, Eq)

type UntypedExpr = String

data ExprSpec = ExprSpec
    { contextSpec :: ContextSpec
    , untypedExpr :: UntypedExpr
    }
  deriving (Show, Eq)

defaultInputSpec, noInput :: InputSpec
defaultInputSpec = InputSpec UseStdin defaultInputFormat
noInput          = InputSpec NoInput  defaultInputFormat

defaultOutputSpec :: OutputSpec
defaultOutputSpec = OutputSpec UseStdout defaultOutputFormat


defaultInputFormat :: InputFormat
defaultInputFormat = Records defaultRecordSeparator
                   $ Fields defaultFieldSeparator

defaultOutputFormat :: OutputFormat
defaultOutputFormat = OutputFormat defaultRecordDelimiter defaultFieldDelimiter


defaultRecordSeparator, defaultFieldSeparator :: Separator
defaultRecordSeparator = Delimiter defaultRecordDelimiter
defaultFieldSeparator = Whitespace

defaultRecordDelimiter, defaultFieldDelimiter :: Delimiter
defaultRecordDelimiter = "\n"
defaultFieldDelimiter = " "
