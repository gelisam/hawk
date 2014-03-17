{-# LANGUAGE PackageImports, RecordWildCards #-}
-- | In which a Haskell module is deconstructed into extensions and imports.
module Data.HaskellModule.Parse (readModule) where

import "mtl" Control.Monad.Trans
import Language.Haskell.Exts

import Control.Monad.Trans.Uncertain
import Data.HaskellModule.Base
import Data.HaskellSource
import Language.Haskell.Exts.Location

-- $setup
-- >>> import qualified Data.ByteString.Char8 as B
-- >>> let putSource = mapM_ (print . either id B.pack)
-- >>> :{
-- let testM f = do
--     m <- runUncertainIO $ readModule f
--     putSource (pragmaSource m)
--     print (languageExtensions m)
--     putStrLn "==="
--     putSource (moduleSource m)
--     print (moduleName m)
--     putStrLn "==="
--     putSource (importSource m)
--     print (importedModules m)
--     putStrLn "==="
--     putSource (codeSource m)
-- :}


locatedExtensions :: [ModulePragma] -> Located [ExtensionName]
locatedExtensions = fmap go . located
  where
    go :: [ModulePragma] -> [ExtensionName]
    go = concatMap extNames
    
    extNames :: ModulePragma -> [ExtensionName]
    extNames (LanguagePragma _ exts) = map prettyPrint exts
    extNames (OptionsPragma _ _ _) = []  -- TODO: accept "-XExtName"
    extNames _ = []

locatedModuleName :: ModuleName -> Located (Maybe String)
locatedModuleName = fmap go . not_located
  where
    go :: ModuleName -> Maybe String
    go (ModuleName "Main") = Nothing  -- TODO: distinguish between
                                      -- "module Main where" and default.
    go (ModuleName moduleName) = Just moduleName
    
    -- Unfortunately, haskell-src-exts does not give a location
    -- for the module declaration.
    not_located :: a -> Located a
    not_located = return

locatedImports :: [ImportDecl] -> Located [QualifiedModule]
locatedImports = fmap go . located
  where
    go :: [ImportDecl] -> [QualifiedModule]
    go = map qualify
    
    qualify :: ImportDecl -> QualifiedModule
    qualify decl = (fullName decl, qualifiedName decl)
    
    fullName :: ImportDecl -> String
    fullName = prettyPrint . importModule
    
    qualifiedName :: ImportDecl -> Maybe String
    qualifiedName = fmap prettyPrint . importAs


-- | A variant of `splitAt` which makes it easy to make `snd` empty.
-- 
-- >>> maybeSplitAt Nothing "abc"
-- ("abc","")
-- 
-- >>> maybeSplitAt (Just 0) "abc"
-- ("","abc")
maybeSplitAt :: Maybe Int -> [a] -> ([a], [a])
maybeSplitAt Nothing  ys = (ys, [])
maybeSplitAt (Just i) ys = splitAt i ys

-- | Given n ordered indices before which to split, split the list into n+1 pieces.
--   Omitted indices will produce empty pieces.
-- 
-- >>> multiSplit [] "foo"
-- ["foo"]
-- 
-- >>> multiSplit [Just 0, Just 1, Just 2] "foo"
-- ["","f","o","o"]
-- 
-- >>> multiSplit [Just 0, Just 1, Nothing] "foo"
-- ["","f","oo",""]
-- 
-- >>> multiSplit [Just 0, Nothing, Just 2] "foo"
-- ["","fo","","o"]
-- 
-- >>> multiSplit [Just 0, Nothing, Nothing] "foo"
-- ["","foo","",""]
-- 
-- >>> multiSplit [Nothing, Just 1, Just 2] "foo"
-- ["f","","o","o"]
-- 
-- >>> multiSplit [Nothing, Just 1, Nothing] "foo"
-- ["f","","oo",""]
-- 
-- >>> multiSplit [Nothing, Nothing, Just 2] "foo"
-- ["fo","","","o"]
-- 
-- >>> multiSplit [Nothing, Nothing, Nothing] "foo"
-- ["foo","","",""]
multiSplit :: [Maybe Int] -> [a] -> [[a]]
multiSplit []           xs = [xs]
multiSplit (j:js) xs = ys1 : ys2 : yss
  where
    (ys:yss) = multiSplit js xs
    (ys1, ys2) = maybeSplitAt j ys

-- | Given n ordered source locations, split the source into n+1 pieces.
--   Omitted source locations will produce empty pieces.
splitSource :: [Maybe SrcLoc] -> HaskellSource -> [HaskellSource]
splitSource = multiSplit . (fmap . fmap) (line2index . srcLine)
  where
    -- line numbers start at 1, list indices start at 0.
    line2index :: Int -> Int
    line2index = subtract 1


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
-- 
-- >>> testM "tests/preludes/readme/prelude.hs"
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
-- "takeLast n = reverse . take n . reverse"
-- 
-- >>> testM "tests/preludes/modulename/prelude.hs"
-- []
-- ===
-- "module MyPrelude where"
-- Just "MyPrelude"
-- ===
-- []
-- ===
-- "t = take"
readModule :: FilePath -> UncertainT IO HaskellModule
readModule f = do
    s <- lift $ readSource f
    r <- lift $ parseFile f
    case r of
      ParseOk (Module _ moduleDecl pragmas _ _ imports decls)
        -> return $ go s pragmas moduleDecl imports decls
      ParseFailed _ err -> fail err
  where
    go source pragmas moduleDecl imports decls = HaskellModule {..}
      where
        (languageExtensions,      _) = runLocated (locatedExtensions pragmas)
        (moduleName,      moduleLoc) = runLocated (locatedModuleName moduleDecl)
        (importedModules, importLoc) = runLocated (locatedImports imports)
        (_,                 declLoc) = runLocated (located decls)
        
        sourceParts = splitSource [moduleLoc, importLoc, declLoc] source
        [pragmaSource, moduleSource, importSource, codeSource] = sourceParts
