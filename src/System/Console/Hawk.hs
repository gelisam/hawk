{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , ScopedTypeVariables
           , TupleSections #-}

module System.Console.Hawk (

    hawk
  , main

) where


import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.List as L
import Data.List ((++),(!!))
import Data.Bool
import Data.Either
import Data.Function
import Data.Ord
import Data.Maybe
import Data.String
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Search as S
import Language.Haskell.Interpreter
import qualified Prelude as P
import System.Console.GetOpt (usageInfo)
import System.Directory (getTemporaryDirectory)
import System.Environment (getArgs,getProgName)
import System.EasyFile ((</>),doesFileExist)
import System.Exit (exitFailure)
import qualified System.IO as IO
import System.IO (FilePath,IO,hFlush,print,putStr,stdout)

import System.Console.Hawk.CabalDev
import System.Console.Hawk.Config
import System.Console.Hawk.Lock
import System.Console.Hawk.Options


getInterpreterLockFile :: IO FilePath
getInterpreterLockFile = do
    lockFile <- (</> "hawk.lock") <$> getTemporaryDirectory
    exists <- doesFileExist lockFile
    unless exists $ IO.writeFile lockFile ""
    return lockFile

-- missing error handling!!
readImportsFromFile :: FilePath -> IO [(String,Maybe String)]
readImportsFromFile fp = P.read <$> IO.readFile fp

initInterpreter :: Maybe (String, String)
                -> Maybe FilePath
                -> InterpreterT IO ()
initInterpreter toolkit moduleFile = do
        set [languageExtensions := [ExtendedDefaultRules
                                   ,NoImplicitPrelude
                                   ,NoMonomorphismRestriction
                                   ,OverloadedStrings]]

        -- load the toolkit
        maybe (return ()) (loadModules . (:[]) . P.fst) toolkit

        -- load imports
        -- TODO: add option to avoit loading default modules
--        setImportsQ $ defaultModules ++ maybe [] ((:[]) . (,Nothing) . P.snd) toolkit
        let modules = defaultModules 
                   ++ maybe [] ((:[]) . (,Nothing) . P.snd) toolkit

        maybe (setImportsQ modules)
              (setImportsQFromFile modules)
              moduleFile 
    where
          setImportsQFromFile :: [(String,Maybe String)]
                              -> FilePath
                              -> InterpreterT IO ()
          setImportsQFromFile requiredImports confFile = do
                imports <- lift (readImportsFromFile confFile)
                setImportsQ $ imports ++ requiredImports

printErrors :: InterpreterError -> IO ()
printErrors e = case e of
                  WontCompile es' -> do
                    IO.hPutStrLn IO.stderr "\nWon't compile:"
                    forM_ es' $ \e' ->
                      case e' of
                        GhcError e'' -> IO.hPutStrLn IO.stderr $ '\t':e'' ++ "\n"
                  _ -> print e

runLockedHawkInterpreter :: forall a . InterpreterT IO a
                            -> IO (Either InterpreterError a)
runLockedHawkInterpreter i = do
    lockFile <- getInterpreterLockFile
    withLock lockFile $ runHawkInterpreter i

hawkeval :: Maybe (String,String) -- ^ The toolkit file and module name
         -> Options               -- ^ Program options
         -> String                -- ^ The user expression to evaluate
         -> IO ()
hawkeval toolkit opts expr_str = do
    maybe_f <- runLockedHawkInterpreter $ do
        initInterpreter toolkit (optModuleFile opts)
        let ignoreErrors = P.show $ optIgnoreErrors opts
        interpret ("printRows " ++ ignoreErrors ++ "(" ++ expr_str++ ")")
                  (as :: IO ())
    case maybe_f of
        Left ie -> printErrors ie
        Right f -> f

-- TODO missing error handling!
hawk :: Maybe (String,String) -- ^ The toolkit file and module name
     -> Options               -- ^ Program options
     -> String                -- ^ The user expression to evaluate
     -> Maybe FilePath        -- ^ The input file
     -> IO ()
hawk toolkit opts expr_str file = do
    maybe_f <- runLockedHawkInterpreter $ do

        initInterpreter toolkit (optModuleFile opts)
        
        let ignoreErrors = P.show $ optIgnoreErrors opts

        -- eval program based on the existence of a delimiter
        case (optDelimiter opts,optMap opts) of
            (Nothing,_) -> interpret (mkF "printRows" ignoreErrors expr_str)
                                     (as :: LB.ByteString -> IO ())
            (Just d,False) -> do
                f <- interpret (mkF "printRows" ignoreErrors expr_str)
                               (as :: [LB.ByteString] -> IO ())
                -- TODO: avoid keep everything in buffer, repr' should output
                -- as soon as possible (for example each line)
                return $ f . dropLastIfEmpty . S.split d
            (Just d,True) -> do
                f <- interpret (mkF "printRow" ignoreErrors expr_str)
                               (as :: LB.ByteString -> IO ())
                return $ mapM_ f . dropLastIfEmpty . S.split d
    case maybe_f of
        Left ie -> printErrors ie -- error hanling!
        Right f -> maybe LB.getContents LB.readFile file >>= f
    where 
          dropLastIfEmpty :: [LB.ByteString]
                          -> [LB.ByteString]
          dropLastIfEmpty [] = []
          dropLastIfEmpty (x:[]) = if LB.null x then [] else [x]
          dropLastIfEmpty (x:xs) = x:dropLastIfEmpty xs
          mkF pf ie exp = unlines ["((",pf,ie,") . (",exp,"))"]

getUsage :: IO String
getUsage = do
    pn <- getProgName
    return $ usageInfo ("Usage: " ++ pn ++ " [<options>] <expr> [<file>]") 
                       options

main :: IO ()
main = do
    maybeCfgFile <- getModulesFileIfExists
    optsArgs <- processArgs maybeCfgFile <$> getArgs
    
    -- checkToolkitOrRecompileIt
    either printErrorAndExit go optsArgs
    where getModulesFileIfExists :: IO (Maybe FilePath)
          getModulesFileIfExists = do
                cfgFile <- getModulesFile
                cfgFileExists <- doesFileExist cfgFile
                return $ if cfgFileExists then Just cfgFile else Nothing
          processArgs cfgFile args = do
                compiledOpts <- compileOpts args
                postOptsProcessing cfgFile compiledOpts
          printErrorAndExit errors = errorMessage errors >> exitFailure
          errorMessage errs = do
                usage <- getUsage
                IO.hPutStr IO.stderr $ L.intercalate "\n" (errs ++ ['\n':usage])
          go (opts,notOpts) = do
                toolkit <- if optRecompile opts
                              then recompileConfig
                              else getConfigFileAndModuleName
                if L.null notOpts || optHelp opts
                  then getUsage >>= putStr
                  else runHawk toolkit opts notOpts
          runHawk t os nos = do
                if optEval os
                  then hawkeval t os (L.head nos)
                  else do
                    let file = if L.length nos > 1
                                 then Just $ nos !! 1
                                 else Nothing
                    hawk t os (L.head nos) file
                hFlush stdout 
