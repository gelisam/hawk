module System.Console.Hawk.Config.Base where


type ExtensionName = String
type QualifiedModule = (String, Maybe String)

defaultModuleName :: String
defaultModuleName = "System.Console.Hawk.CachedPrelude"
