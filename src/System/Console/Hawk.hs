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
  let mode = optMode os
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
                  else runHawk opts config notOpts
