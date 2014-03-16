{-# LANGUAGE PackageImports #-}
-- | In which a Haskell module is deconstructed into extensions and imports.
module Data.HaskellModule.Parse (readModule) where

import Control.Arrow
import "mtl" Control.Monad.Trans
import Control.Monad.Trans.Writer
import qualified Data.ByteString.Char8 as B
import Language.Haskell.Exts
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax

import Control.Monad.Trans.Uncertain
import Data.HaskellModule.Base
import Data.HaskellSource
import Data.Monoid.Ord
import Language.Haskell.Exts.Location


testM :: FilePath -> IO ()
testM f = do
    m <- runUncertainIO $ readModule f
    printSource (pragmaSource m)
    print (languageExtensions m)
    putStrLn "==="
    printSource (moduleSource m)
    print (moduleName m)
    putStrLn "==="
    printSource (importSource m)
    print (importedModules m)
    putStrLn "==="
    printSource (codeSource m)
  where
    printSource = mapM_ (print . either id B.pack)


-- Due to a limitation of haskell-parse-exts, there is no `parseModule`
-- variant of `readModule` which would parse from a String instead of a file.
-- 
-- According to the documentation [1], only `parseFile` honors language
-- pragmas, without which PackageImport-style imports will fail to parse.
-- 
-- [1] http://hackage.haskell.org/package/haskell-src-exts-1.14.0.1/docs/Language-Haskell-Exts-Parser.html#t:ParseMode

-- |
-- >>> testM "tests/preludes/default/prelude.hs"
-- "{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}"
-- ["ExtendedDefaultRules","OverloadedStrings"]
-- ===
-- Nothing
-- ===
-- "import Prelude"
-- "import qualified Data.ByteString.Lazy.Char8 as B"
-- "import qualified Data.List as L"
-- [("Prelude",Nothing),("Data.ByteString.Lazy.Char8",Just "B"),("Data.List",Just "L")]
-- ===
readModule :: FilePath -> UncertainT IO HaskellModule
readModule f = do
    s <- lift $ readSource f
    r <- lift $ parseFile f
    case r of
      ParseOk (Module _ moduleDecl pragmas _ exports imports decls)
        -> return $ go s pragmas moduleDecl imports decls
      ParseFailed loc err -> fail err
  where
    go source pragmas moduleDecl imports decls
      = HaskellModule languageExtensions' pragmaSource'
                      moduleName'         moduleSource'
                      importedModules'    importSource'
                                          codeSource'
      where
        languageExtensions' = map prettyPrint
                            $ concatMap languagePragma
                            $ pragmas
        languagePragma (LanguagePragma _ exts) = exts
        languagePragma (OptionsPragma _ _ _) = []  -- TODO: accept "-XExtName"
        languagePragma _ = []
        
        moduleName' = case moduleDecl of
            ModuleName "Main" -> Nothing  -- TODO: distinguish between
                                          -- "module Main where" and default.
            ModuleName name -> Just name
        
        importedModules' = fmap qualifiedImport imports
        qualifiedImport decl = (fullName decl, qualifiedName decl)
          where
            fullName = prettyPrint . importModule
            qualifiedName = fmap prettyPrint . importAs
        
        pragmaSource' = take 1 source
        moduleSource' = []
        importSource' = drop 1 source
        codeSource' = []
