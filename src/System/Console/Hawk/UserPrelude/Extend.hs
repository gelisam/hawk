-- | In which the implicit defaults are explicitly added.
module System.Console.Hawk.UserPrelude.Extend
  ( extendModules
  ) where

import Control.Applicative
import Data.HaskellModule


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
--       m' = extendModules m2
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
extendModules :: HaskellModule -> HaskellModule
extendModules = until preludeOk
                    $ addImport unqualified_prelude
  where
    prelude = "Prelude"
    noPrelude = "NoImplicitPrelude"
    unqualified_prelude = (prelude, Nothing)
    
    preludeOk = liftA2 (||) hasPrelude noImplicitPrelude
    hasPrelude        m =   prelude `elem` moduleNames m
    noImplicitPrelude m = noPrelude `elem` languageExtensions m
