module Data.HaskellModule.Parse.Test where

import qualified Data.Text.Lazy as T

import Control.Monad.Trans.Uncertain
import Data.HaskellModule.Base
import Data.HaskellModule.Parse

-- | Test that `readModule` splits prelude files into correct sections.
-- 
-- >>> testM "tests/preludes/default/prelude.hs"
-- "{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}"
-- ["ExtendedDefaultRules","OverloadedStrings"]
-- ===
-- Nothing
-- ===
-- "import Prelude"
-- "import qualified Data.List as L"
-- [("Prelude",Nothing),("Data.List",Just "L")]
-- ===
-- 
-- >>> testM "tests/preludes/readme/prelude.hs"
-- "{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}"
-- ["ExtendedDefaultRules","OverloadedStrings"]
-- ===
-- Nothing
-- ===
-- "import Prelude"
-- "import qualified Data.List as L"
-- [("Prelude",Nothing),("Data.List",Just "L")]
-- ===
-- "takeLast n = reverse . take n . reverse"
-- 
-- >>> testM "tests/preludes/moduleName/prelude.hs"
-- []
-- ===
-- "module MyPrelude where"
-- Just "MyPrelude"
-- ===
-- []
-- ===
-- "t = take"
-- 
-- >>> testM "tests/preludes/moduleNamedMain/prelude.hs"
-- []
-- ===
-- "module Main where"
-- Just "Main"
-- ===
-- []
-- ===
-- "t = take"
testM :: FilePath -> IO ()
testM f = do
    m <- runUncertainIO $ readModule f
    putSource (pragmaSource m)
    print (languageExtensions m)
    putStrLn "==="
    putSource (moduleSource m)
    print (moduleName m)
    putStrLn "==="
    putSource (importSource m)
    print (importedModules m)
    putStrLn "==="
    putSource (codeSource m)
  where
    putSource = mapM_ (print . either id T.pack)
