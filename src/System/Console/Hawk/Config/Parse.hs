{-# LANGUAGE OverloadedStrings #-}
module System.Console.Hawk.Config.Parse
    ( ExtensionName
    , QualifiedModule
    , parseExtensions
    , parseModules
    , parseSource
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


-- the configuration format is designed to look like a Haskell module,
-- so we just return the whole file.
parseSource :: FilePath -> IO ByteString
parseSource = C8.readFile
