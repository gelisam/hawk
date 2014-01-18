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
-- | Hawk as seen from the outside world: parsing command-line arguments,
--   evaluating user expressions.
module System.Console.Hawk (

    hawk
  , main

) where


import Control.Applicative ((<$>))
import Control.Monad
import Data.Bool (not,(&&))
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
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import Language.Haskell.Interpreter
import qualified Prelude as P
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import qualified System.IO as IO
import System.IO (IO)
import Text.Printf (printf)

import Control.Monad.Trans.Uncertain
import System.Console.Hawk.Args
import System.Console.Hawk.Sandbox
import System.Console.Hawk.Config
import System.Console.Hawk.Help
import System.Console.Hawk.Lock
import System.Console.Hawk.IO
import System.Console.Hawk.Options
import System.Console.Hawk.Version


-- | Tell hint to load the user prelude, the modules it imports, and the
--   language extensions it specifies.
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

runHawk :: Options          -- ^ built from the flags
        ->  (String,String) -- ^ (cleaned-up prelude file, module name)
        -> [String]         -- ^ optional input filename
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
runLockedHawkInterpreter i = withLock $ runHawkInterpreter i

data StreamFormat = StreamFormat | LinesFormat | WordsFormat
    deriving (P.Eq,P.Show,P.Read)

-- | Omitting the argument to the `-D` and `-d` flags prevents the input from
--   being split into lines or words. Each combination is implemented via a
--   dedicated stream format.
streamFormat :: B.ByteString
             -> B.ByteString
             -> StreamFormat
streamFormat ld wd
    | B.null ld = StreamFormat
    | B.null wd = LinesFormat
    | P.otherwise = WordsFormat

-- | 'ByteString' wrapper used to force `typeOf` to fully-qualify the type
--   `ByteString`. Otherwise hint may try to use a type which we haven't
--   explicitly imported.
-- 
-- >>> Typeable.typeOf (fromString "test" :: LB.ByteString)
-- ByteString
-- 
-- >>> Typeable.typeOf $ QB (fromString "test" :: LB.ByteString)
-- Data.ByteString.Lazy.Char8.ByteString
newtype QualifiedByteString = QB { unQB :: LB.ByteString }

instance Typeable.Typeable QualifiedByteString where
  typeOf (QB bs) = let TypeRep fp tc trs = Typeable.typeOf bs
                   in TypeRep fp
                              tc{ tyConName = "Data.ByteString.Lazy.Char8."
                                          ++ tyConName tc }
                              trs

hawk :: Options                 -- ^ Program options
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
          linesDelim = case optLinesDelim opts of
                         Nothing -> C8.singleton '\n'
                         Just d -> d
          wordsDelim = case optWordsDelim opts of
                         Nothing -> C8.singleton ' '
                         Just d -> d
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


main :: IO ()
main = do
    args <- getArgs
    moduleFile <- getModulesFile
    optsArgs <- runWarnings $ processArgs args moduleFile
    either printErrorAndExit go optsArgs
    where processArgs args moduleFile = do
                spec <- parseArgs args
                let opts = optionsFromSpec spec
                let notOpts = notOptionsFromSpec spec
                if not (optVersion opts) && not (optHelp opts) && P.null notOpts
                  then fail "the expression <expr> is required"
                  else return (opts{ optModuleFile = Just moduleFile},notOpts)
          printUsageAndExit = help >> exitSuccess
          printErrorAndExit = failHelp
          go (opts,notOpts) = do
                config <- if optRecompile opts
                              then recompileConfig
                              else recompileConfigIfNeeded
                
                when (optVersion opts) $ do
                  IO.putStrLn versionString
                  exitSuccess
                
                when (optHelp opts) printUsageAndExit
                
                runHawk opts config notOpts
