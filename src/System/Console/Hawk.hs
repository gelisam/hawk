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
import qualified Data.Typeable.Internal as Typeable
import Data.Typeable.Internal
  (Typeable
  ,TypeRep(..)
  ,tyConName)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy
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
import System.Console.Hawk.IO
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
runHawk config os nos = do
  let file = if L.length nos > 1 then Just (nos !! 1) else Nothing
  extFile <- getExtensionsFile
  maybe_f <- hawk config os extFile (L.head nos)
  case maybe_f of
    Left ie -> printErrors ie
    Right f -> getInput file >>= printOutput . f

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

-- | 'ByteString' wrapper used to override the 'ByteString' @typeOf@ into
-- a qualified version
-- @typeOf (ByteString.pack "test") == "ByteString"@
-- @typeof (QualifiedByteString $ ByteString.pack "test") == "Data.ByteString.Lazy.Char8.Bytestring"@
newtype QualifiedByteString = QB { unQB :: LB.ByteString }

instance Typeable.Typeable QualifiedByteString where
  typeOf (QB bs) = let TypeRep fp tc trs = Typeable.typeOf bs
                   in TypeRep fp
                              tc{ tyConName = "Data.ByteString.Lazy.Char8."
                                          ++ tyConName tc }
                              trs

-- TODO missing error handling!
hawk :: (String,String)      -- ^ The config file and module name
     -> Options               -- ^ Program options
     -> FilePath              -- ^ The file containing the extensions
     -> String                -- ^ The user expression to evaluate
     -> IO (Either InterpreterError (LB.ByteString -> LB.ByteString))
hawk config opts extFile expr_str = do
    eitherErrorF <- runLockedHawkInterpreter $ do

        initInterpreter config (optModuleFile opts) extFile
        
        -- eval program based on the existence of a delimiter
        case (optMode opts,streamFormat linesDelim wordsDelim) of
            (EvalMode,_)             -> interpret' $ evalExpr      expr_str
            (ApplyMode,StreamFormat) -> interpret' $ streamExpr    expr_str
            (ApplyMode,LinesFormat)  -> interpret' $ linesExpr     expr_str
            (ApplyMode,WordsFormat)  -> interpret' $ wordsExpr     expr_str
            (MapMode,StreamFormat)   -> interpret' $ mapStreamExpr expr_str
            (MapMode,LinesFormat)    -> interpret' $ mapLinesExpr  expr_str
            (MapMode,WordsFormat)    -> interpret' $ mapWordsExpr  expr_str
    
    return ((\f -> unQB . f . QB) <$> eitherErrorF)
    where 
          interpret' expr = do
            -- print the user expression
            -- lift $ IO.hPutStrLn IO.stderr expr 
            interpret expr (as :: QualifiedByteString -> QualifiedByteString)
          evalExpr = printf "const (%s (%s))" showRows
          mapStreamExpr = streamExpr . listMap
          mapLinesExpr = linesExpr . listMap
          mapWordsExpr = wordsExpr . listMap
          streamExpr expr = compose [showRows, expr]
          linesExpr expr = compose [showRows, expr, parseRows]
          wordsExpr expr = compose [showRows, expr, parseWords]
          linesDelim = optLinesDelim opts
          wordsDelim = optWordsDelim opts
          compose :: [String] -> String
          compose = L.intercalate (prel ".") . P.map (printf "(%s)")
          listMap :: String -> String
          listMap = printf (repr "listMap (%s)")
          c8pack :: String -> String
          c8pack = printf (repr "c8pack (%s)")
          sc8pack :: String -> String
          sc8pack = printf (repr "sc8pack (%s)")
          showRows :: String
          showRows = printf (repr "showRows (%s) (%s)")
                             (c8pack $ P.show linesDelim)
                             (c8pack $ P.show wordsDelim)
          parseRows :: String
          parseRows = printf (repr "parseRows (%s)")
                             (sc8pack $ P.show linesDelim)

          parseWords :: String
          parseWords = printf (repr "parseWords (%s) (%s)")
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
