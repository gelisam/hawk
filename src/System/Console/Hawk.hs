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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Search as S
import Language.Haskell.Interpreter
import qualified Prelude as P
import System.Console.GetOpt (usageInfo)
import System.Environment (getArgs,getProgName)
import System.EasyFile (doesFileExist)
import System.Exit (exitFailure)
import qualified System.IO as IO
import System.IO (FilePath,IO,hFlush,print,putStr,stdout)
import Text.Printf (printf)

import System.Console.Hawk.CabalDev
import System.Console.Hawk.Config
import System.Console.Hawk.Options


initInterpreter :: (String, String) -- ^ config file and module nme
                -> Maybe FilePath
                -> InterpreterT IO ()
initInterpreter config moduleFile = do
        set [languageExtensions := [ExtendedDefaultRules
                                   ,NoImplicitPrelude
                                   ,NoMonomorphismRestriction
                                   ,OverloadedStrings]]

        -- load the config file
        loadModules [P.fst config]

        -- load the config module plus representable
        let requiredModules = (P.snd config,Nothing):defaultModules
        userModules <- lift $ maybe (return []) loadFromFile moduleFile

        let modules = requiredModules ++ userModules

        let modulesWithPrelude = if "Prelude" `L.notElem` fmap P.fst modules
                                    then ("Prelude",Nothing):modules
                                    else modules

        setImportsQ modulesWithPrelude
    where loadFromFile :: FilePath -> IO [(String,Maybe String)]
          loadFromFile fp = P.read <$> IO.readFile fp

printErrors :: InterpreterError -> IO ()
printErrors e = case e of
                  WontCompile es' -> do
                    IO.hPutStrLn IO.stderr "\nWon't compile:"
                    forM_ es' $ \e' ->
                      case e' of
                        GhcError e'' -> IO.hPutStrLn IO.stderr $ '\t':e'' ++ "\n"
                  _ -> print e

runHawk :: (String,String)
        -> Options
        -> [String]
        -> IO ()
runHawk config os nos = do
      if optEval os
        then hawkeval config os (L.head nos)
        else do
          let file = if L.length nos > 1
                       then Just $ nos !! 1
                       else Nothing
          hawk config os (L.head nos) file
      hFlush stdout 

hawkeval :: (String,String) -- ^ The config file and module name
         -> Options          -- ^ Program options
         -> String           -- ^ The user expression to evaluate
         -> IO ()
hawkeval config opts expr_str = do
    maybe_f <- runHawkInterpreter $ do
        initInterpreter config (optModuleFile opts)
        let ignoreErrors = P.show $ optIgnoreErrors opts
        interpret (printf "System.Console.Hawk.Representable.printRows %s (%s)"
                   ignoreErrors expr_str)
                  (as :: IO ())
    case maybe_f of
        Left ie -> printErrors ie
        Right f -> f

-- TODO missing error handling!
hawk :: (String,String) -- ^ The config file and module name
     -> Options               -- ^ Program options
     -> String                -- ^ The user expression to evaluate
     -> Maybe FilePath        -- ^ The input file
     -> IO ()
hawk config opts expr_str file = do
    maybe_f <- runHawkInterpreter $ do

        initInterpreter config (optModuleFile opts)
        
        let ignoreErrors = optIgnoreErrors opts

        -- eval program based on the existence of a delimiter
        case (optDelimiter opts,optMap opts) of
            (Nothing,_) ->
                interpret (runExpr file [printRows ignoreErrors, expr_str])
                          (as :: IO ())
            (Just d,False) ->
                interpret (runExpr file [printRows ignoreErrors, expr_str, parseRows d])
                          (as :: IO ())
                -- TODO: avoid keep everything in buffer, repr' should output
                -- as soon as possible (for example each line)
            (Just d,True) ->
                interpret (runExprs file [runMap [printRow ignoreErrors, expr_str]
                                    ,parseRows d])
                          (as :: IO ())
    case maybe_f of
        Left ie -> printErrors ie -- error hanling!
        Right f -> f
    where 
          compose :: [String] -> String
          compose = L.intercalate "." . P.map (printf "(%s)")
          
          runExprs :: Maybe FilePath -> [String] -> String
          runExprs file = printf "(System.Console.Hawk.Representable.runExprs (%s) (%s))"
                     (P.show file) . compose
          
          runExpr :: Maybe FilePath -> [String] -> String
          runExpr file = printf "(System.Console.Hawk.Representable.runExpr (%s) (%s))"
                    (P.show file) . compose
          
          runMap :: [String] -> String
          runMap = printf "(System.Console.Hawk.Representable.listMap (%s))"
                   . compose
          
          printRow :: Bool -> String
          printRow b = printf "(System.Console.Hawk.Representable.printRow %s)"
                       (P.show b)
          
          printRows :: Bool -> String
          printRows b = printf "(System.Console.Hawk.Representable.printRows %s)"
                        (P.show b)
          
          parseRows :: B.ByteString -> String
          parseRows = printf "System.Console.Hawk.Representable.parseRows (%s)"
                         . P.show

getUsage :: IO String
getUsage = do
    pn <- getProgName
    return $ usageInfo ("Usage: " ++ pn ++ " [<options>] <expr> [<file>]") 
                       options

main :: IO ()
main = do
    moduleFile <- getModulesFile
    optsArgs <- processArgs moduleFile <$> getArgs
    either printErrorAndExit go optsArgs
    where processArgs cfgFile args = do
                compiledOpts <- compileOpts args
                postOptsProcessing cfgFile compiledOpts
          printErrorAndExit errors = errorMessage errors >> exitFailure
          errorMessage errs = do
                usage <- getUsage
                IO.hPutStr IO.stderr $ L.intercalate "\n" (errs ++ ['\n':usage])
          go (opts,notOpts) = do
                config <- if optRecompile opts
                              then recompileConfig
                              else recompileConfigIfNeeded
                if L.null notOpts || optHelp opts
                  then getUsage >>= putStr
                  else runHawk config opts notOpts
