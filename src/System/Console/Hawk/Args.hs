-- | Re-export the most important functions from Args.*
module System.Console.Hawk.Args
  ( HawkSpec(..)
  , InputSpec(..), OutputSpec(..)
  , InputSource(..), OutputSink(..)
  , InputFormat(..), LineFormat(..)
  , OutputFormat(..), Separator
  , ExprSpec(..) 
  , parseArgs
  , optionsFromSpec
  , notOptionsFromSpec
  ) where

import System.Console.Hawk.Args.Compatibility
import System.Console.Hawk.Args.Parse
import System.Console.Hawk.Args.Spec
