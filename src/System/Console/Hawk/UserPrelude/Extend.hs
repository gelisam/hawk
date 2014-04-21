-- | In which the implicit defaults are explicitly added.
module System.Console.Hawk.UserPrelude.Extend
  ( extendModuleName
  , extendImports
  ) where

import Control.Applicative
import Data.Maybe

import Data.HaskellModule


-- | We cannot import a module unless it has a name.
extendModuleName :: HaskellModule -> HaskellModule
extendModuleName = until hasModuleName
                       $ addDefaultModuleName defaultModuleName
  where
    defaultModuleName = "System.Console.Hawk.CachedPrelude"
    
    hasModuleName = isJust . moduleName


moduleNames :: HaskellModule -> [String]
moduleNames = map fst . importedModules

-- | GHC imports the Haskell Prelude by default, but hint doesn't.
-- 
-- >>> let m name = (name, Nothing)
-- >>> :{
--   let testM exts modules = moduleNames m'
--     where
--       m0  = emptyModule
--       m1  = foldr addExtension m0 exts
--       m2  = foldr addImport m1 modules
--       m' = extendImports m2
-- :}
-- 
-- >>> testM [] []
-- ["Prelude"]
-- 
-- >>> testM [] [m "Data.Maybe"]
-- ["Prelude","Data.Maybe"]
-- 
-- >>> testM [] [m "Data.Maybe", m "Prelude", m "Data.Either"]
-- ["Data.Maybe","Prelude","Data.Either"]
-- 
-- >>> :{
-- testM [] [ ("Data.Maybe", Just "M")
--          , ("Prelude", Just "P")
--          , ("Data.Either", Just "E")
--          ]
-- :}
-- ["Data.Maybe","Prelude","Data.Either"]
-- 
-- >>> :{
-- testM ["OverloadedStrings","NoImplicitPrelude"]
--       [m "Data.Maybe"]
-- :}
-- ["Data.Maybe"]
extendImports :: HaskellModule -> HaskellModule
extendImports = until preludeOk
                    $ addImport unqualified_prelude
  where
    prelude = "Prelude"
    noPrelude = "NoImplicitPrelude"
    unqualified_prelude = (prelude, Nothing)
    
    preludeOk = liftA2 (||) hasPrelude noImplicitPrelude
    hasPrelude        m =   prelude `elem` moduleNames m
    noImplicitPrelude m = noPrelude `elem` languageExtensions m
