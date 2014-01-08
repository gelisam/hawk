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
import Data.Either
import Data.Function
import Data.Ord
import Data.Maybe
import Data.String
import qualified Data.Typeable.Internal as Typeable
import Data.Typeable.Internal
  (TypeRep(..)
  ,tyConName)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Version (versionBranch)
import Language.Haskell.Interpreter
import qualified Prelude as P
import System.Console.GetOpt (usageInfo)
import System.Environment (getArgs,getProgName)
import System.Exit (exitFailure,exitSuccess)
import qualified System.IO as IO
import System.IO (IO)
import Text.Printf (printf)

import System.Console.Hawk.Sandbox
import System.Console.Hawk.Config
import System.Console.Hawk.Lock
import System.Console.Hawk.IO
import System.Console.Hawk.Options

-- magic self-referential module created by cabal
import Paths_haskell_awk (version)


initInterpreter :: (String, String) -- ^ config file and module name
                -> [(String,Maybe String)] -- ^ the modules maybe qualified
                -> [Extension]
                -> InterpreterT IO ()
initInterpreter (preludeFile,preludeModule) userModules extensions = do
        
        set [languageExtensions := extensions]

        -- load the config file
        loadModules [preludeFile]

        -- load the config module plus representable
        setImportsQ $ (preludeModule,Nothing):defaultModules
                                           ++ userModules

printErrors :: InterpreterError -> IO ()
printErrors e = case e of
                  WontCompile es' -> do
                    IO.hPutStrLn IO.stderr "\nWon't compile:"
                    forM_ es' $ \e' ->
                      case e' of
                        GhcError e'' -> IO.hPutStrLn IO.stderr $ '\t':e'' ++ "\n"
                  _ -> IO.print e

runHawk :: Options
        ->  (String,String)
        -> [String]
        -> IO ()
runHawk os prelude nos = do
  let file = if L.length nos > 1 then Just (nos !! 1) else Nothing
  extensions <- P.read <$> (getExtensionsFile >>= IO.readFile)
  modules <- maybe (return []) (\f -> P.read <$> IO.readFile f) (optModuleFile os)

  maybe_f <- hawk os prelude modules extensions (L.head nos)
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
streamFormat ld wd 
    | B.null ld = StreamFormat
    | B.null wd = LinesFormat
    | otherwise = WordsFormat

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

hawk :: Options                -- ^ Program options
     -> (String,String)         -- ^ The prelude file and module name
     -> [(String,Maybe String)] -- ^ The modules maybe qualified
     -> [Extension]             -- ^ The extensions to enable
     -> String                  -- ^ The user expression to evaluate
     -> IO (Either InterpreterError (LB.ByteString -> LB.ByteString))
hawk opts prelude modules extensions userExpr = do
    eitherErrorF <- runLockedHawkInterpreter $ do

        initInterpreter prelude modules extensions
        
        -- eval program based on the existence of a delimiter
        case (optMode opts,streamFormat linesDelim wordsDelim) of
            (EvalMode,_)             -> interpret' $ evalExpr      userExpr
            (ApplyMode,StreamFormat) -> interpret' $ streamExpr    userExpr
            (ApplyMode,LinesFormat)  -> interpret' $ linesExpr     userExpr
            (ApplyMode,WordsFormat)  -> interpret' $ wordsExpr     userExpr
            (MapMode,StreamFormat)   -> interpret' $ mapStreamExpr userExpr
            (MapMode,LinesFormat)    -> interpret' $ mapLinesExpr  userExpr
            (MapMode,WordsFormat)    -> interpret' $ mapWordsExpr  userExpr
    
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
          outLinesDelim = case optOutLinesDelim opts of
                            Nothing -> linesDelim
                            Just delim -> delim
          outWordsDelim = case optOutWordsDelim opts of
                            Nothing -> wordsDelim
                            Just delim -> delim
          compose :: [String] -> String
          compose = L.intercalate (prel ".") . P.map (printf "(%s)")
          listMap :: String -> String
          listMap = printf (runtime "listMap (%s)")
          c8pack :: String -> String
          c8pack = printf (runtime "c8pack (%s)")
          sc8pack :: String -> String
          sc8pack = printf (runtime "sc8pack (%s)")
          showRows :: String
          showRows = printf (runtime "showRows (%s) (%s)")
                             (c8pack $ P.show outLinesDelim)
                             (c8pack $ P.show outWordsDelim)
          parseRows :: String
          parseRows = printf (runtime "parseRows (%s)")
                             (sc8pack $ P.show linesDelim)

          parseWords :: String
          parseWords = printf (runtime "parseWords (%s) (%s)")
                              (sc8pack $ P.show linesDelim)
                              (sc8pack $ P.show wordsDelim)
          
          qualify :: String -> String -> String
          qualify moduleName = printf "%s.%s" moduleName
          
          prel = qualify "Prelude"
          runtime = qualify "System.Console.Hawk.Runtime"

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
                
                when (optVersion opts) $ do
                  let versionString = L.intercalate "."
                                    $ P.map P.show
                                    $ versionBranch version
                  IO.putStrLn versionString
                  exitSuccess
                
                when (optHelp opts) $ do
                  getUsage >>= IO.putStr
                  exitSuccess
                
                runHawk opts config notOpts
