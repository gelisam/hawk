{-# LANGUAGE PackageImports, RecordWildCards #-}
-- | In which a Haskell module is deconstructed into extensions and imports.
module Data.HaskellModule.Parse (readModule) where

import "mtl" Control.Monad.Trans
import Data.List (findIndex)
import qualified Data.Text.Lazy as T
import Language.Haskell.Exts (ImportDecl(..), Module(..), ModuleName(..), ModulePragma(..),
                              ParseResult(..), SrcLoc(..),
                              parseFile, prettyPrint)
import Text.Printf

import Control.Monad.Trans.Uncertain
import Data.HaskellModule.Base
import Data.HaskellSource
import Language.Haskell.Exts.Location


locatedExtensions :: [ModulePragma] -> Located [ExtensionName]
locatedExtensions = fmap go . located
  where
    go :: [ModulePragma] -> [ExtensionName]
    go = concatMap extNames
    
    extNames :: ModulePragma -> [ExtensionName]
    extNames (LanguagePragma _ exts) = map (T.pack . prettyPrint) exts
    extNames (OptionsPragma _ _ _) = []  -- TODO: accept "-XExtName"
    extNames _ = []

locatedImports :: [ImportDecl] -> Located [QualifiedModule]
locatedImports = fmap go . located
  where
    go :: [ImportDecl] -> [QualifiedModule]
    go = map qualify
    
    qualify :: ImportDecl -> QualifiedModule
    qualify decl = (fullName decl, qualifiedName decl)
    
    fullName :: ImportDecl -> T.Text
    fullName = T.pack . prettyPrint . importModule
    
    qualifiedName :: ImportDecl -> Maybe T.Text
    qualifiedName = fmap (T.pack . prettyPrint) . importAs

locatedModule :: SrcLoc -> HaskellSource -> ModuleName -> Located (Maybe T.Text)
locatedModule srcLoc source (ModuleName mName) = case moduleLine of
    Nothing -> return Nothing
    Just line -> located (srcLoc {srcLine = line}) >> return (Just . T.pack $ mName)
  where
    isModuleDecl (Left xs) = isModuleDecl' xs
    isModuleDecl (Right xs) = isModuleDecl' xs

    isModuleDecl' = T.isPrefixOf (T.pack "module ")

    moduleLine :: Maybe Int
    moduleLine = fmap index2line $ findIndex isModuleDecl source


-- line numbers start at 1, list indices start at 0.
line2index, index2line :: Int -> Int
line2index = subtract 1
index2line = (+ 1)


-- | A variant of `splitAt` which makes it easy to make `snd` empty.
--
-- >>> -- be explicit about String type for clarity
-- >>> maybeSplitAt Nothing ("abc" :: String)
-- ("abc","")
-- 
-- >>> maybeSplitAt (Just 0) ("abc" :: String)
-- ("","abc")
maybeSplitAt :: Maybe Int -> [a] -> ([a], [a])
maybeSplitAt Nothing  ys = (ys, [])
maybeSplitAt (Just i) ys = splitAt i ys

-- | Given n ordered indices before which to split, split the list into n+1 pieces.
--   Omitted indices will produce empty pieces.
-- 
-- >>> multiSplit [] ("foo" :: String)
-- ["foo"]
-- 
-- >>> multiSplit [Just 0, Just 1, Just 2] ("foo" :: String)
-- ["","f","o","o"]
-- 
-- >>> multiSplit [Just 0, Just 1, Nothing] ("foo" :: String)
-- ["","f","oo",""]
-- 
-- >>> multiSplit [Just 0, Nothing, Just 2] ("foo" :: String)
-- ["","fo","","o"]
-- 
-- >>> multiSplit [Just 0, Nothing, Nothing] ("foo" :: String)
-- ["","foo","",""]
-- 
-- >>> multiSplit [Nothing, Just 1, Just 2] ("foo" :: String)
-- ["f","","o","o"]
-- 
-- >>> multiSplit [Nothing, Just 1, Nothing] ("foo" :: String)
-- ["f","","oo",""]
-- 
-- >>> multiSplit [Nothing, Nothing, Just 2] ("foo" :: String)
-- ["fo","","","o"]
-- 
-- >>> multiSplit [Nothing, Nothing, Nothing] ("foo" :: String)
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


-- Due to a limitation of haskell-parse-exts, there is no `parseModule`
-- variant of `readModule` which would parse from a String instead of a file.
-- 
-- According to the documentation [1], only `parseFile` honors language
-- pragmas, without which PackageImport-style imports will fail to parse.
-- 
-- [1] http://hackage.haskell.org/package/haskell-src-exts-1.14.0.1/docs/Language-Haskell-Exts-Parser.html#t:ParseMode

readModule :: FilePath -> UncertainT IO HaskellModule
readModule f = do
    s <- lift $ readSource f
    r <- lift $ parseFile f
    case r of
      ParseOk (Module srcLoc moduleDecl pragmas _ _ imports decls)
        -> return $ go s srcLoc pragmas moduleDecl imports decls
      ParseFailed loc err -> multilineFail msg
        where
          -- we start with a newline to match ghc's errors
          msg = printf "\n%s:%d:%d: %s" (srcFilename loc) (srcLine loc) (srcColumn loc) err
  where
    go source srcLoc pragmas moduleDecl imports decls = HaskellModule {..}
      where
        (languageExtensions,      _) = runLocated (locatedExtensions pragmas)
        (moduleName,      moduleLoc) = runLocated (locatedModule srcLoc source moduleDecl)
        (importedModules, importLoc) = runLocated (locatedImports imports)
        (_,                 declLoc) = runLocated (located decls)
        
        sourceParts = splitSource [moduleLoc, importLoc, declLoc] source
        [pragmaSource, moduleSource, importSource, codeSource] = sourceParts
