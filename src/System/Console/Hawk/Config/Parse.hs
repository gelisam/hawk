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
-- | In which the user prelude is deconstructed into the parts we care about.
module System.Console.Hawk.Config.Parse
    ( ExtensionName
    , QualifiedModule
    , parseExtensions
    , parseModules
    , readExtensions
    , readModules
    , readSource
    )
  where

import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as C8
import Data.Maybe
import Language.Haskell.Exts ( parseFileContentsWithExts )
import Language.Haskell.Exts.Extension ( parseExtension, Extension (..) )
import Language.Haskell.Exts.Parser
    ( getTopPragmas
    , ParseResult (..)
    )
import Language.Haskell.Exts.Syntax
import System.Exit
import Text.Printf

import System.Console.Hawk.Config.Base


-- | Our parse methods terminate the program upon failure,
--   but those from Haskell.Exts don't.
getResult :: FilePath -> ParseResult a -> IO a
getResult _ (ParseOk x) = return x
getResult sourceFile (ParseFailed srcLoc err) = do
    putStrLn $ printf "error parsing file %s:%s: %s" sourceFile (show srcLoc) err
    exitFailure


-- | The LANGUAGE pragma at the top of the user prelude, if any.
-- 
-- >>> parseExtensions "{-# LANGUAGE OverloadedStrings, GADTs #-}\nmain = print 42\n"
-- ParseOk ["OverloadedStrings","GADTs"]
-- 
-- >>> parseExtensions "main = print 42\n"
-- ParseOk []
parseExtensions :: String -> ParseResult [ExtensionName]
parseExtensions = fmap listExtensions . getTopPragmas
  where
    listExtensions :: [ModulePragma] -> [ExtensionName]
    listExtensions = map getName . concat . mapMaybe extensionNames
    
    extensionNames :: ModulePragma -> Maybe [Name]
    extensionNames (LanguagePragma _ names) = Just names
    extensionNames _                        = Nothing
    
    getName :: Name -> ExtensionName
    getName (Ident  s) = s
    getName (Symbol s) = s


-- | The modules imported by the user prelude.
-- 
-- >>> parseModules [] "import Data.Maybe\nmain = print 42\n"
-- ParseOk [("Data.Maybe",Nothing)]
-- 
-- >>> parseModules [] "import qualified Data.Maybe as M"
-- ParseOk [("Data.Maybe",Just "M")]
-- 
-- >>> parseModules ["PackageImports"] "import \"network\" Network.Socket"
-- ParseOk [("Network.Socket",Nothing)]
parseModules :: [ExtensionName] -> String -> ParseResult [QualifiedModule]
parseModules extensions = fmap getQualifiedModules
                        . parseFileContentsWithExts extensions'
  where
    extensions' :: [Extension]
    extensions' = map parseExtension extensions
    
    getImportDecls :: Module -> [ImportDecl]
    getImportDecls (Module _ _ _ _ _ ds _) = ds
    
    getQualifiedModules :: Module -> [QualifiedModule]
    getQualifiedModules = concatMap toHintModules
                        . getImportDecls
    
    toHintModules :: ImportDecl -> [QualifiedModule]
    toHintModules importDecl =
      case importDecl of
        ImportDecl _ (ModuleName mn) False _ _ Nothing _ -> [(mn,Nothing)]
        ImportDecl _ (ModuleName mn) False _ _ (Just (ModuleName s)) _ ->
                              [(mn,Nothing),(mn,Just s)]
        ImportDecl _ (ModuleName mn) True _ _ Nothing _ -> [(mn,Just mn)]
        ImportDecl _ (ModuleName mn) True _ _ (Just (ModuleName s)) _ ->
                              [(mn,Just s)]


readExtensions :: FilePath -> IO [ExtensionName]
readExtensions sourceFile = do
    result <- parseExtensions <$> readFile sourceFile
    getResult sourceFile result

readModules :: [ExtensionName] -> FilePath -> IO [QualifiedModule]
readModules extensions sourceFile = do
    result <- parseModules extensions <$> readFile sourceFile
    getResult sourceFile result

-- the configuration format is designed to look like a Haskell module,
-- so we just return the whole file.
readSource :: FilePath -> IO Source
readSource = C8.readFile
