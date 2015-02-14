-- | Re-export the most important functions from Args.*
module System.Console.Hawk.Args
  ( HawkSpec(..)
  , InputSpec(..), OutputSpec(..)
  , InputSource(..), OutputSink(..)
  , InputFormat(..), RecordFormat(..)
  , OutputFormat(..)
  , Separator(..), Delimiter
  , ContextSpec(..), UntypedUserExpression, ExprSpec(..)
  , parseArgs
  ) where

import System.Console.Hawk.Args.Parse
import System.Console.Hawk.Args.Spec
