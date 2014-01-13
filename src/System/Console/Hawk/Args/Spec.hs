-- | The precisely-typed version of Hawk's command-line arguments.
module System.Console.Hawk.Args.Spec where

import Data.ByteString (ByteString)


data HawkSpec
    = Help
    | Version
    | Eval            ExprSpec OutputSpec
    | Apply InputSpec ExprSpec OutputSpec
    | Map   InputSpec ExprSpec OutputSpec
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
    = UseStdin
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
    | Words Separator
  deriving (Show, Eq)

-- We can't know ahead of time whether it's going to be a raw stream
-- or raw lines or words, it depends on the type of the user expression.
data OutputFormat = OutputFormat
    { lineDelimiter :: Separator
    , wordDelimiter :: Separator
    }
  deriving (Show, Eq)

type Separator = ByteString


data ExprSpec = ExprSpec
    { userPrelude :: PreludeSpec
    , userExpression :: String
    }
  deriving (Show, Eq)

data PreludeSpec
    = UseUserPrelude
    | UseCachedPrelude
    | UsePreludeFile FilePath
    -- | UsePreludeText String  -- we might want to accept prelude literals
                                -- in the future, to simplify testing
    | DetectPrelude  -- cached if possible, else user
  deriving (Show, Eq)
