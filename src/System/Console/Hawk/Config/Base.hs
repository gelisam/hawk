module System.Console.Hawk.Config.Base where

import Data.ByteString.Char8


type ExtensionName = String
type QualifiedModule = (String, Maybe String)
type Source = ByteString

defaultModuleName :: String
defaultModuleName = "System.Console.Hawk.CachedPrelude"
