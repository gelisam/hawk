{-# LANGUAGE OverloadedStrings #-}
module System.Console.Hawk.Config.Extend
    ( extendSource
    , forceModuleName
    )
  where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Search as BSS
import Data.Char
import Data.Maybe
import Data.Monoid ((<>))
import Text.Printf

import System.Console.Hawk.Config.Base


-- adjust the prelude to make it loadable from hint.
extendSource :: FilePath
             -> [ExtensionName]
             -> [QualifiedModule]
             -> Source
             -> Source
extendSource configFile extensions modules = addPreludeIfMissing . addModuleIfMissing
  where
    addModuleIfMissing :: Source -> Source
    addModuleIfMissing s | getModuleName s == Nothing = addModule configFile s
    addModuleIfMissing s | otherwise                  = s
    
    addPreludeIfMissing :: Source -> Source
    addPreludeIfMissing s | "NoImplicitPrelude" `elem` extensions = s
    addPreludeIfMissing s | "Prelude" `elem` map fst modules      = s
    addPreludeIfMissing s | otherwise = addImport "Prelude" configFile s

-- add a module to a string representing a Haskell source file
addModule :: FilePath -> Source -> Source
addModule configFile source =
    let strippedCode = C8.dropWhile isSpace source
        maybePragma = if "{-#" `C8.isPrefixOf` strippedCode
                        then let (pragma,afterPragma) = BSS.breakAfter "#-}" strippedCode
                             in (Just pragma, afterPragma)
                        else (Nothing,strippedCode)
        line :: Int -> ByteString
        line n = C8.pack $ printf "{-# LINE %d %s #-}" n $ show configFile
        moduleLine = C8.pack $ unwords ["module", defaultModuleName, "where"]
    in case maybePragma of
        (Nothing,c) -> C8.unlines [moduleLine,c]
        (Just pragma,c) -> let n = 1 + C8.length (C8.filter (=='\n') pragma)
                            in C8.unlines [line 1,pragma,moduleLine,line n,c]

-- add an import statement to a string representing a Haskell source file
addImport :: String -> FilePath -> Source -> Source
addImport moduleName configFile source =
    let (premodule,postmodule)   = BSS.breakAfter "module " source
        (prewhere,postwhere)     = BSS.breakAfter " where" postmodule
        (prenewline,postnewline) = BSS.breakAfter "\n" postwhere
        preimports = premodule <> prewhere <> prenewline
        postimports = postnewline
        line :: Int -> ByteString
        line n = C8.pack $ printf "{-# LINE %d %s #-}" n $ show configFile
        importLine = C8.pack $ unwords ["import", moduleName]
        m = 1 + C8.length (C8.filter (=='\n') preimports)
        extraLines = C8.unlines [importLine, line m]
    in preimports <> extraLines <> postimports


-- get the module name from a file if it exists
getModuleName :: Source -> Maybe ByteString
getModuleName bs = case BSS.indices (C8.pack "module") bs of
                    [] -> Nothing
                    (i:_) -> Just
                           . C8.takeWhile (\c -> isAlphaNum c || c == '.')
                           . C8.dropWhile isSpace
                           . C8.drop (i + 6) $ bs

-- same, but crash if there is no module
forceModuleName :: Source -> String
forceModuleName = C8.unpack . fromJust . getModuleName
