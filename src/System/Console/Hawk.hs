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
import System.Console.Hawk.Lock
import System.Console.Hawk.Options


initInterpreter :: (String, String) -- ^ config file and module name
                -> Maybe FilePath
                -> FilePath
                -> InterpreterT IO ()
initInterpreter config moduleFile extensionsFile = do
        extensions <- lift $ P.read <$> IO.readFile extensionsFile
        
        set [languageExtensions := (extensions::[Extension])]

        -- load the config file
        loadModules [P.fst config]

        -- load the config module plus representable
        let requiredModules = (P.snd config,Nothing):defaultModules
        userModules <- lift $ maybe (return []) loadFromFile moduleFile

        let modules = requiredModules ++ userModules

        setImportsQ modules
    where loadFromFile :: FilePath -> IO [(String,Maybe String)]
          loadFromFile fp = P.read <$> IO.readFile fp

printErrors :: InterpreterError -> IO ()
printErrors e = case e of
                  WontCompile es' -> do
                    IO.hPutStrLn IO.stderr "\nWon't typecheck:"
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
                        in do
                            extFile <- getExtensionsFile
                            hawk config os extFile (L.head nos) file

runLockedHawkInterpreter :: forall a . InterpreterT IO a
                            -> IO (Either InterpreterError a)
runLockedHawkInterpreter i = do
    withLock $ runHawkInterpreter i

data StreamFormat = StreamFormat | LinesFormat | WordsFormat
    deriving (P.Eq,P.Show,P.Read)

streamFormat :: B.ByteString
             -> B.ByteString
             -> StreamFormat
streamFormat ld wd = if B.null ld
                        then StreamFormat
                        else if B.null wd
                                then LinesFormat
                                else WordsFormat

-- TODO missing error handling!
hawk :: (String,String)      -- ^ The config file and module name
     -> Options               -- ^ Program options
     -> FilePath              -- ^ The file containing the extensions
     -> String                -- ^ The user expression to evaluate
     -> Maybe FilePath        -- ^ The input file
     -> IO ()
hawk config opts extFile expr_str file = do
    maybe_f <- runLockedHawkInterpreter $ do

        initInterpreter config (optModuleFile opts) extFile
        
        -- eval program based on the existence of a delimiter
        case (optMode opts,streamFormat linesDelim wordsDelim) of
            (EvalMode,_)             -> interpret' evalExpr
            (ApplyMode,StreamFormat) -> interpret' streamExpr
            (ApplyMode,LinesFormat)  -> interpret' linesExpr
            (ApplyMode,WordsFormat)  -> interpret' wordsExpr
            (MapMode,StreamFormat)   -> interpret' mapStreamExpr
            (MapMode,LinesFormat)    -> interpret' mapLinesExpr
            (MapMode,WordsFormat)    -> interpret' mapWordsExpr

    case maybe_f of
        Left ie -> printErrors ie -- error hanling!
        Right () -> IO.putStrLn ""
    where 
          interpret' expr = do
            -- print the user expression
            --lift $ IO.hPutStrLn IO.stderr expr 
            interpret (unsafe expr) (as :: ())
          
          unsafe :: String -> String
          unsafe = printf "System.IO.Unsafe.unsafePerformIO (%s)"
          
          evalExpr :: String
          evalExpr = printf "%s (%s)" printRows expr_str
          mapStreamExpr = runExpr [printRows, listMap expr_str]
          mapLinesExpr = runExpr [printRows,listMap expr_str,parseRows]
          mapWordsExpr = runExpr [printRows,listMap expr_str,parseWords]
          listMap = printf (repr "listMap (%s)")
          c8pack :: P.String -> P.String
          c8pack = printf (repr "c8pack (%s)")
          sc8pack :: P.String -> P.String
          sc8pack = printf (repr "sc8pack (%s)")
          streamExpr = runExpr [printRows, expr_str]
          linesExpr = runExpr [printRows, expr_str, parseRows]
          wordsExpr = runExpr [printRows, expr_str, parseWords]
          ignoreErrors = P.show $ optIgnoreErrors opts
          linesDelim = optLinesDelim opts
          wordsDelim = optWordsDelim opts
          compose :: [String] -> String
          compose = L.intercalate (prel ".") . P.map (printf "(%s)")
          runExpr :: [String] -> String
          runExpr = printf (repr "runExpr (%s) (%s)") (prel $ P.show file) . compose
          printRows :: String
          printRows = printf (repr "printRows (%s) (%s) (%s)")
                             (prel ignoreErrors)
                             (c8pack $ P.show linesDelim)
                             (c8pack $ P.show wordsDelim)
          
          parseRows :: String
          parseRows = printf (repr "parseRows (%s)")
                             (sc8pack $ P.show linesDelim)

          parseWords :: String
          parseWords = printf
                       (repr "parseWords (%s) (%s)")
                       (sc8pack $ P.show linesDelim)
                       (sc8pack $ P.show wordsDelim)
          
          qualify :: String -> String -> String
          qualify moduleName = printf "%s.%s" moduleName
          
          prel = qualify "Prelude"
          repr = qualify "System.Console.Hawk.Representable"

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
