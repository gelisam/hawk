{-# LANGUAGE DeriveDataTypeable #-}
-- | The part of a HawkSpec used at Runtime. The API may change at any time.
-- 
-- Due to a `hint` limitation, this module is imported unqualified when
-- interpreting the user expression. This allows `hint` to read and write the
-- type of the expression which it interprets without falling prey to module
-- scoping issues.
module System.Console.Hawk.Runtime.Base where

import Data.Typeable

import System.Console.Hawk.Args.Spec


data HawkRuntime = HawkRuntime
    { inputSpec :: InputSpec
    , outputSpec :: OutputSpec
    }
  deriving (Show, Eq, Typeable)

-- reexport IO under a unique name
newtype HawkIO a = HawkIO { runHawkIO :: IO a }
  deriving Typeable
