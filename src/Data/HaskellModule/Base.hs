{-# LANGUAGE OverloadedStrings #-}
-- | A version of HaskellSource with slightly more semantics.
module Data.HaskellModule.Base where

import Control.Applicative
import qualified Data.Text.Lazy as T

import Data.HaskellSource


-- | hint has `Interpreter.Extension`, but strings are simpler.
type ExtensionName = T.Text

-- | import [qualified] <module-name> [as <qualified-name>]
type QualifiedModule = (T.Text, Maybe T.Text)

-- | Three key pieces of information about a Haskell Module:
--   the language extensions it enables,
--   the module's name (if any), and
--   the modules it imports.
data HaskellModule = HaskellModule
  { languageExtensions :: [ExtensionName]   , pragmaSource :: HaskellSource
  , moduleName         :: Maybe T.Text      , moduleSource :: HaskellSource
  , importedModules    :: [QualifiedModule] , importSource :: HaskellSource
                                            , codeSource   :: HaskellSource
  } deriving (Show, Eq)


emptyModule :: HaskellModule
emptyModule = HaskellModule [] [] Nothing [] [] [] []


addExtension :: ExtensionName -> HaskellModule -> HaskellModule
addExtension e m = m
    { languageExtensions = extraExtensions e ++ languageExtensions m
    , pragmaSource       = extraSource e ++ pragmaSource m
    }
  where
    extraExtensions = return
    extraSource = return . return . languagePragma

    languagePragma p = T.unwords ["{-#", "LANGUAGE", p, "#-}"]

addDefaultModuleName :: T.Text -> HaskellModule -> HaskellModule
addDefaultModuleName s m = m
    { moduleName   = moduleName m <|> defaultName s
    , moduleSource = extraSource s ++ moduleSource m
    }
  where
    defaultName = return
    extraSource = return . return . moduleDeclaration

    moduleDeclaration m' = T.unwords ["module", m', "where"]

addImport :: QualifiedModule -> HaskellModule -> HaskellModule
addImport qm m = m
    { importedModules = extraModules qm ++ importedModules m
    , importSource    = extraSource qm ++ importSource m
    }
  where
    extraModules = return
    extraSource = return . return . importStatement

    importStatement (fullName, Nothing) = T.unwords ["import", fullName]
    importStatement (fullName, Just qualifiedName) =
        T.unwords ["import", "qualified", fullName, "as", qualifiedName]
