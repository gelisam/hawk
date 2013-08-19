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
import Language.Haskell.Interpreter
import qualified Prelude as P
import System.Console.GetOpt (usageInfo)
import System.Environment (getArgs,getProgName)
import System.Exit (exitFailure)
import qualified System.IO as IO
import System.IO (FilePath,IO)
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
                  _ -> IO.print e

runHawk :: (String,String)
        -> Options
        -> [String]
        -> IO ()
runHawk config os nos = let file = if L.length nos > 1
                                    then Just (nos !! 1)
                                    else Nothing
                        in hawk config os (L.head nos) file


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
        case optMode opts of
            EvalMode -> 
                interpret (printf "System.Console.Hawk.Representable.printRows %s (%s)"
                           (P.show ignoreErrors) expr_str)
                  (as :: IO ())
            StreamMode ->
                interpret (runExpr [printRows ignoreErrors, expr_str])
                          (as :: IO ())
            LinesMode -> do
                interpret (runExpr [ printRows ignoreErrors
                                        , expr_str
                                        , parseRows linesDelim])
                          (as :: IO ())
            MapMode -> do
                interpret (runExprs [runMap [ printRow ignoreErrors
                                                 , expr_str]
                                    ,parseRows linesDelim])
                          (as :: IO ())
            WordsMode -> do
                let expr = (runExprs [runWords [ printRow ignoreErrors
                                                   , expr_str]
                                    ,parseWords linesDelim wordsDelim])
                interpret expr (as :: IO ())
                
    case maybe_f of
        Left ie -> printErrors ie -- error hanling!
        Right f -> f
    where 
          linesDelim = optLinesDelim opts
          wordsDelim = optWordsDelim opts
          compose :: [String] -> String
          compose = L.intercalate "." . P.map (printf "(%s)")
          
          runExprs :: [String] -> String
          runExprs = printf "(System.Console.Hawk.Representable.runExprs (%s) (%s))"
                     (P.show file) . compose
          
          runExpr :: [String] -> String
          runExpr = printf "(System.Console.Hawk.Representable.runExpr (%s) (%s))"
                    (P.show file) . compose
          
          runMap :: [String] -> String
          runMap = printf "(System.Console.Hawk.Representable.listMap (%s))"
                   . compose

          runWords :: [String] -> String
          runWords = printf "(System.Console.Hawk.Representable.listMapWords (%s))"
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

          parseWords :: B.ByteString -> B.ByteString -> String
          parseWords ld wd = compose
            [ printf "System.Console.Hawk.Representable.parseWords (%s)" (P.show wd)
            , parseRows ld
            ]

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
    where processArgs moduleFile args =
                case compileOpts args of
                    Left err -> Left err
                    Right (opts,notOpts) -> 
                        Right (opts{ optModuleFile = Just moduleFile},notOpts)
          printErrorAndExit errors = errorMessage errors >> exitFailure
          errorMessage errs = do
                usage <- getUsage
                IO.hPutStr IO.stderr $ L.intercalate "\n" (errs ++ ['\n':usage])
          go (opts,notOpts) = do
                config <- if optRecompile opts
                              then recompileConfig
                              else recompileConfigIfNeeded
                if L.null notOpts || optHelp opts
                  then getUsage >>= IO.putStr
                  else runHawk config opts notOpts
