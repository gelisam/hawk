{-# LANGUAGE OverloadedStrings #-}
module System.Console.Hawk.Config.Parse
    ( ExtensionName
    , QualifiedModule
    , parseExtensions
    , parseModules
    , parseSource
    , forceModuleName
    )
  where

import Control.Applicative ((<$>))

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Search as BSS
import Data.Char
import Data.Maybe
import Data.Monoid ((<>))
import Language.Haskell.Exts ( parseFileWithExts )
import Language.Haskell.Exts.Extension ( Extension (..) )
import Language.Haskell.Exts.Parser
    ( getTopPragmas
    , ParseResult (..)
    )
import Language.Haskell.Exts.Syntax
import System.Exit
import Text.Printf

import System.Console.Hawk.Config.Base


getResult :: FilePath -> ParseResult a -> IO a
getResult _ (ParseOk x) = return x
getResult sourceFile (ParseFailed srcLoc err) = do
    putStrLn $ printf "error parsing file %s:%d: %s" sourceFile (show srcLoc) err
    exitFailure


parseExtensions :: FilePath -> IO [ExtensionName]
parseExtensions sourceFile = do
    result <- getTopPragmas <$> readFile sourceFile 
    listExtensions <$> getResult sourceFile result
  where
    listExtensions :: [ModulePragma] -> [ExtensionName]
    listExtensions = map getName . concat . mapMaybe extensionNames
    
    extensionNames :: ModulePragma -> Maybe [Name]
    extensionNames (LanguagePragma _ names) = Just names
    extensionNames _                        = Nothing
    
    getName :: Name -> ExtensionName
    getName (Ident  s) = s
    getName (Symbol s) = s


parseModules :: FilePath -> [ExtensionName] -> IO [QualifiedModule]
parseModules sourceFile extensions = do
    result <- parseFileWithExts extensions' sourceFile
    Module _ _ _ _ _ importDeclarations _ <- getResult sourceFile result
    return $ concatMap toHintModules importDeclarations
  where
    extensions' :: [Extension]
    extensions' = map read extensions
    
    toHintModules :: ImportDecl -> [QualifiedModule]
    toHintModules importDecl =
      case importDecl of
        ImportDecl _ (ModuleName mn) False _ _ Nothing _ -> [(mn,Nothing)]
        ImportDecl _ (ModuleName mn) False _ _ (Just (ModuleName s)) _ ->
                              [(mn,Nothing),(mn,Just s)]
        ImportDecl _ (ModuleName mn) True _ _ Nothing _ -> [(mn,Just mn)]
        ImportDecl _ (ModuleName mn) True _ _ (Just (ModuleName s)) _ ->
                              [(mn,Just s)]


-- adjust the prelude to make it loadable from hint.
-- return the generated source.
parseSource :: FilePath
            -> [ExtensionName]
            -> [QualifiedModule]
            -> IO ByteString
parseSource configFile extensions modules = do
    adjustSource <$> C8.readFile configFile
  where
    adjustSource :: ByteString -> ByteString
    adjustSource = addPreludeIfMissing . addModuleIfMissing

    addModuleIfMissing :: ByteString -> ByteString
    addModuleIfMissing s | getModuleName s == Nothing = addModule configFile s
    addModuleIfMissing s | otherwise                  = s
    
    addPreludeIfMissing :: ByteString -> ByteString
    addPreludeIfMissing s | "NoImplicitPrelude" `elem` extensions = s
    addPreludeIfMissing s | "Prelude" `elem` map fst modules      = s
    addPreludeIfMissing s | otherwise = addImport "Prelude" configFile s

-- add a module to a string representing a Haskell source file
addModule :: FilePath -> ByteString -> ByteString
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
addImport :: String -> FilePath -> ByteString -> ByteString
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
getModuleName :: ByteString -> Maybe ByteString
getModuleName bs = case BSS.indices (C8.pack "module") bs of
                    [] -> Nothing
                    (i:_) -> Just
                           . C8.takeWhile (\c -> isAlphaNum c || c == '.')
                           . C8.dropWhile isSpace
                           . C8.drop (i + 6) $ bs

-- same, but crash if there is no module
forceModuleName :: ByteString -> String
forceModuleName = C8.unpack . fromJust . getModuleName
