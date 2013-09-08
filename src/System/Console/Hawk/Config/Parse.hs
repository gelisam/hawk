--   Copyright 2013 Mario Pastorelli (pastorelli.mario@gmail.com) Samuel Gélineau (gelisam@gmail.com)
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

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

import qualified Data.ByteString.Char8 as C8
import Data.Maybe
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
    putStrLn $ printf "error parsing file %s:%s: %s" sourceFile (show srcLoc) err
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
parseSource :: FilePath -> IO Source
parseSource = C8.readFile
