{-# LANGUAGE DeriveDataTypeable #-}
-- | The part of a HawkSpec used at Runtime. The API may change at any time.
module System.Console.Hawk.Runtime.Base where

import Data.Typeable

import System.Console.Hawk.Args.Spec


data HawkRuntime = HawkRuntime
    { inputSpec :: InputSpec
    , outputSpec :: OutputSpec
    }
  deriving (Show, Eq, Typeable)
