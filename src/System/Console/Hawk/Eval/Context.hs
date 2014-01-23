module System.Console.Hawk.Eval.Context where

import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Config
import System.Console.Hawk.Config.Base
import System.Console.Hawk.Config.Cache
import System.Console.Hawk.Config.Parse


data EvalContext = EvalContext
  { originalPreludePath :: FilePath
  , canonicalPrelude :: FilePath
  , compiledPrelude :: FilePath
  , moduleName :: String
  , extensions :: [ExtensionName]
  , modules :: [QualifiedModule]
  } deriving (Show, Eq)

newEvalContext :: PreludeSpec -> IO EvalContext
newEvalContext spec = do
    -- currently, only ~/.hawk/prelude.hs is supported.
    originalPreludePath' <- getConfigFile
    
    (canonicalPrelude', moduleName') <- makeCanonical spec
    extensions' <- readExtensions originalPreludePath'
    modules' <- readModules extensions' originalPreludePath'
    
    -- I think it hint will automatically use the version we have just
    -- compiled if we give it the path to the .hs file.
    -- 
    -- TODO: check whether using .o or .hi instead works
    -- and whether it makes any difference.
    let compiledPrelude' = canonicalPrelude'
    
    return $ EvalContext
           { originalPreludePath = originalPreludePath'
           , canonicalPrelude = canonicalPrelude'
           , compiledPrelude = compiledPrelude'
           , moduleName = moduleName'
           , extensions = extensions'
           , modules = modules'
           }
  where
    makeCanonical UseUserPrelude = recompileConfig
    makeCanonical DetectPrelude  = recompileConfigIfNeeded
