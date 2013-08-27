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
import Language.Haskell.Exts.Parser (ParseResult (..), parseType)
import Language.Haskell.Exts.Syntax (Type (..))
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

        let modulesWithPrelude = if "Prelude" `L.notElem` fmap P.fst modules
                                    then ("Prelude",Nothing):modules
                                    else modules

        setImportsQ modulesWithPrelude
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
            (EvalMode,_) -> do
              type_string <- typeOf expr_str
              let ParseOk t = parseType type_string
              interpret (magicExpr t) (as :: IO ())
            (ApplyMode,StreamFormat) -> interpret streamExpr (as :: IO ())
            (ApplyMode,LinesFormat)  -> interpret linesExpr  (as :: IO ())
            (ApplyMode,WordsFormat)  -> interpret wordsExpr  (as :: IO ())
            (MapMode,StreamFormat) -> interpret mapStreamExpr (as :: IO ())
            (MapMode,LinesFormat)  -> interpret mapLinesExpr  (as :: IO ())
            (MapMode,WordsFormat) -> interpret mapWordsExpr (as :: IO ())

    case maybe_f of
        Left ie -> printErrors ie -- error hanling!
        Right f -> f
    where 
          magicExpr :: Type -> String
          magicExpr (TyForall _ _ t)              = magicExpr t
          magicExpr (TyParen t)                   = magicExpr t
          magicExpr (TyFun (TyList (TyList _)) _) = wordsExpr     -- [[a]] -> b, parse as words
          magicExpr (TyFun (TyList _) _)          = linesExpr     -- [a] -> b, parse as lines
          magicExpr (TyFun _ _)                   = mapLinesExpr  -- a -> b, map lines
          magicExpr _                             = evalExpr      -- a, eval
          
          evalExpr :: String
          evalExpr = printf "%s (%s)" printRows expr_str
          mapStreamExpr = runExpr [printRows, listMap expr_str]
          mapLinesExpr = runExpr [printRows,listMap expr_str,parseRows]
          mapWordsExpr = runExpr [printRows,listMap expr_str,parseWords]
          listMap = printf (qualify "listMap (%s)")
          streamExpr = runExpr [printRows, expr_str]
          linesExpr = runExpr [printRows, expr_str, parseRows]
          wordsExpr = runExpr [printRows, expr_str, parseWords]
          ignoreErrors = P.show $ optIgnoreErrors opts
          linesDelim = optLinesDelim opts
          wordsDelim = optWordsDelim opts
          compose :: [String] -> String
          compose = L.intercalate "." . P.map (printf "(%s)")
          runExpr :: [String] -> String
          runExpr = printf (qualify "runExpr (%s) (%s)") (P.show file) . compose
          printRows :: String
          printRows = printf (qualify "printRows (%s)") ignoreErrors
          
          parseRows :: String
          parseRows = printf (qualify "parseRows (%s)") (P.show linesDelim)

          parseWords :: String
          parseWords = printf
                       (qualify "parseWords (%s) (%s)")
                       (P.show linesDelim)
                       (P.show wordsDelim)
          qualify :: String -> String
          qualify s = "(System.Console.Hawk.Representable." ++ s ++ ")"

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
