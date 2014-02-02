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
-- | In which the implicit defaults are explicitly added.
module System.Console.Hawk.UserPrelude.Extend
    ( extendModules
    , extendSource
    , getModuleName
    )
  where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Search as BSS
import Data.Char
import Data.Maybe
import Data.Monoid ((<>))
import Text.Printf

import System.Console.Hawk.UserPrelude.Base

-- $setup
-- >>> let prependBar = unlines . map ('|':) . lines
-- >>> let stripLastNewline = reverse . tail . reverse
-- >>> let applyBS f = C8.unpack . f . C8.pack
-- >>> let testBS f = putStrLn . stripLastNewline . prependBar . applyBS f
-- 
-- >>> testBS id "main = print 42\n"
-- |main = print 42


-- | GHC imports the Haskell Prelude by default, but hint doesn't.
-- 
-- >>> let m x = (x, Nothing)
-- 
-- >>> map fst $ extendModules [] [m "Data.Maybe"]
-- ["Prelude","Data.Maybe"]
-- 
-- >>> map fst $ extendModules [] [m "Data.Maybe", m "Prelude", m "Data.Either"]
-- ["Data.Maybe","Prelude","Data.Either"]
-- 
-- >>> :{
-- map fst $ extendModules [] [ ("Data.Maybe", Just "M")
--                            , ("Prelude", Just "P")
--                            , ("Data.Either", Just "E")
--                            ]
-- :}
-- ["Data.Maybe","Prelude","Data.Either"]
-- 
-- >>> :{
-- map fst $ extendModules ["OverloadedStrings","NoImplicitPrelude"]
--                         [m "Data.Maybe"]
-- :}
-- ["Data.Maybe"]
extendModules :: [ExtensionName]
              -> [QualifiedModule]
              -> [QualifiedModule]
extendModules extensions modules = addIfNecessary (importsPrelude extensions modules)
                                                  (unqualified_prelude:)
                                                  modules
  where
    unqualified_prelude = ("Prelude", Nothing)


-- | Adjust the prelude to make it loadable from hint.
-- 
-- If there is no "module ModuleName where" declaration, we need to add one in
-- order to be able to reference the file later on.
-- 
-- We also need to add the Prelude import if its missing. `extendModules` above
-- was making the Prelude available to the user expression, while we are making
-- it available to the user prelude.
extendSource :: FilePath
             -> [ExtensionName]
             -> [QualifiedModule]
             -> Source
             -> Source
extendSource preludeFile extensions modules = addPreludeIfMissing . addModuleIfMissing
  where
    addModuleIfMissing s = addIfNecessary (hasModuleDecl s)
                                          (addModuleDecl preludeFile)
                                          s
    addPreludeIfMissing = addIfNecessary (importsPrelude extensions modules)
                                         (addImport "Prelude" preludeFile)


-- | A helper function for conditionally applying a function.
-- 
-- >>> addIfNecessary (even 11) (+1) 11
-- 12
-- 
-- >>> addIfNecessary (even 42) (+1) 42
-- 42
addIfNecessary :: Bool -> (a -> a) -> a -> a
addIfNecessary False  f x = f x
addIfNecessary True _ x = x

-- |
-- >>> hasModuleDecl $ C8.pack "module Foo where\nfoo = 42\n"
-- True
-- 
-- >>> hasModuleDecl $ C8.pack "foo = 42\n"
-- False
hasModuleDecl :: Source -> Bool
hasModuleDecl = isJust . parseModuleName

importsPrelude :: [ExtensionName] -> [QualifiedModule] -> Bool
importsPrelude extensions _ | "NoImplicitPrelude" `elem` extensions = True
importsPrelude _ modules    | "Prelude" `elem` map fst modules      = True
importsPrelude _ _          | otherwise                             = False


-- | Add a module declaration to a string representing a Haskell source file.
-- 
-- >>> testBS (addModuleDecl "myfile.hs") "main = print 42"
-- |module System.Console.Hawk.CachedPrelude where
-- |{-# LINE 1 "myfile.hs" #-}
-- |main = print 42
-- 
-- >>> testBS (addModuleDecl "myfile.hs") "{-# LANGUAGE NoImplicitPrelude #-}\nimport Prelude (print)\nmain = print 42"
-- |{-# LINE 1 "myfile.hs" #-}
-- |{-# LANGUAGE NoImplicitPrelude #-}
-- |module System.Console.Hawk.CachedPrelude where
-- |{-# LINE 1 "myfile.hs" #-}
-- |
-- |import Prelude (print)
-- |main = print 42
addModuleDecl :: FilePath -> Source -> Source
addModuleDecl preludeFile source =
    let strippedCode = C8.dropWhile isSpace source
        maybePragma = if "{-#" `C8.isPrefixOf` strippedCode
                        then let (pragma,afterPragma) = BSS.breakAfter "#-}" strippedCode
                             in (Just pragma, afterPragma)
                        else (Nothing,strippedCode)
        line :: Int -> ByteString
        line n = C8.pack $ printf "{-# LINE %d %s #-}" n $ show preludeFile
        moduleLine = C8.pack $ unwords ["module", defaultModuleName, "where"]
    in case maybePragma of
        (Nothing,c) -> C8.unlines [moduleLine,line 1,c]
        (Just pragma,c) -> let n = 1 + C8.length (C8.filter (=='\n') pragma)
                            in C8.unlines [line 1,pragma,moduleLine,line n,c]

-- | Add an import statement to a string representing a Haskell source file.
-- 
-- >>> testBS (addImport "Data.Maybe" "myfile.hs") "module Main where\nmain = print 42"
-- |module Main where
-- |import Data.Maybe
-- |{-# LINE 2 "myfile.hs" #-}
-- |main = print 42
-- 
-- >>> testBS (addImport "Data.Maybe" "myfile.hs") "{-# LANGUAGE NoImplicitPrelude #-}\nmodule Main where\nimport Prelude (print)\nmain = print 42"
-- |{-# LANGUAGE NoImplicitPrelude #-}
-- |module Main where
-- |import Data.Maybe
-- |{-# LINE 3 "myfile.hs" #-}
-- |import Prelude (print)
-- |main = print 42
addImport :: String -> FilePath -> Source -> Source
addImport moduleName preludeFile source =
    let (premodule,postmodule)   = BSS.breakAfter "module " source
        (prewhere,postwhere)     = BSS.breakAfter " where" postmodule
        (prenewline,postnewline) = BSS.breakAfter "\n" postwhere
        preimports = premodule <> prewhere <> prenewline
        postimports = postnewline
        line :: Int -> ByteString
        line n = C8.pack $ printf "{-# LINE %d %s #-}" n $ show preludeFile
        importLine = C8.pack $ unwords ["import", moduleName]
        m = 1 + C8.length (C8.filter (=='\n') preimports)
        extraLines = C8.unlines [importLine, line m]
    in preimports <> extraLines <> postimports


-- TODO: not sure what those two are doing in this file.


-- | Get the module name from the top of a source file.
-- 
-- >>> parseModuleName $ C8.pack "module Foo where\nfoo = 42\n"
-- Just "Foo"
parseModuleName :: Source -> Maybe ByteString
parseModuleName bs = case BSS.indices (C8.pack "module") bs of
                       [] -> Nothing
                       (i:_) -> Just
                              . C8.takeWhile (\c -> isAlphaNum c || c == '.')
                              . C8.dropWhile isSpace
                              . C8.drop (i + 6) $ bs

-- | Gets the module name from the top of a source file. Crashes if missing.
-- 
-- >>> getModuleName $ C8.pack "{-# LANGUAGE NoImplicitPrelude #-}\nmodule Main where\nimport Prelude (print)\nmain = print 42"
-- "Main"
getModuleName :: Source -> String
getModuleName = C8.unpack . fromJust . parseModuleName
