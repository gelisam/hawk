{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , ScopedTypeVariables
           , TupleSections #-}

module HSProcess (

    hscmd
  , main

) where


import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.List as L
import Data.List ((++),(!!))
import Data.Bool
import Data.Either
import Data.Eq
import Data.Function
import Data.Ord
import Data.Maybe
import Data.String
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Search as S
import Language.Haskell.Interpreter
import qualified Prelude as P
import System.Console.GetOpt (usageInfo)
import System.Environment (getArgs,getProgName)
import System.Exit (exitFailure)
import System.IO (FilePath,IO,print,putStr)

import HSProcess.Config
import HSProcess.Options


-- missing error handling!!
readImportsFromFile :: FilePath -> IO [(String,Maybe String)]
readImportsFromFile fp = (P.map parseImport . P.filter notImport . P.lines) 
                         `fmap` P.readFile fp
    where parseImport :: String -> (String,Maybe String)
          parseImport s = case words s of
                            w:[] -> (w,Nothing)
                            w:q:[] -> (w,Just q)
                            _ -> P.undefined -- error!
          notImport s = (not $ L.null s) && not (L.isPrefixOf "--" s)


-- TODO missing error handling!
hscmd :: Maybe (String,String) -- ^ The toolkit file and module name
      -> Options               -- ^ Program options
      -> String                -- ^ The user expression to evaluate
      -> Maybe FilePath        -- ^ The input file
      -> IO ()
hscmd toolkit opts expr_str file = do
    maybe_f <- runInterpreter $ do
        set [languageExtensions := [ExtendedDefaultRules
                                   ,NoImplicitPrelude
                                   ,NoMonomorphismRestriction]]

        -- load the toolkit
        maybe (return ()) (loadModules . (:[]) . P.fst) toolkit

        -- load imports
        -- TODO: add option to avoit loading default modules
--        setImportsQ $ defaultModules ++ maybe [] ((:[]) . (,Nothing) . P.snd) toolkit
        let modules = defaultModules 
                   ++ maybe [] ((:[]) . (,Nothing) . P.snd) toolkit

        maybe (setImportsQ modules)
              (setImportsQFromFile modules)
              (optModuleFile opts)


        let ignoreErrors = P.show $ optIgnoreErrors opts

        -- eval program based on the existence of a delimiter
        case (optDelimiter opts,optMap opts) of
            (Nothing,_) -> do
                f <- interpret (mkF "printRows" ignoreErrors expr_str)
                               (as :: LB.ByteString -> IO ())
                return $ f
            (Just d,False) -> do
                f <- interpret (mkF "printRows" ignoreErrors expr_str)
                               (as :: [LB.ByteString] -> IO ())
                -- TODO: avoid keep everything in buffer, rshow should output
                -- as soon as possible (for example each line)
                return $ f . dropLastIfEmpty . S.split d
            (Just d,True) -> do
                f <- interpret (mkF "printRow" ignoreErrors expr_str)
                               (as :: LB.ByteString -> IO ())
                return $ mapM_ f . dropLastIfEmpty . S.split d
    case maybe_f of
        Left ie -> print ie -- error hanling!
        Right f -> maybe LB.getContents LB.readFile file >>= f
    where setImportsQFromFile :: [(String,Maybe String)]
                              -> FilePath
                              -> InterpreterT IO ()
          setImportsQFromFile requiredImports confFile = do
                imports <- lift (readImportsFromFile confFile)
                setImportsQ $ imports ++ requiredImports
          dropLastIfEmpty :: [LB.ByteString]
                          -> [LB.ByteString]
          dropLastIfEmpty [] = []
          dropLastIfEmpty (x:[]) = if LB.null x then [] else (x:[])
          dropLastIfEmpty (x:xs) = x:dropLastIfEmpty xs
          mkF pf ie exp = unlines ["((",pf,ie,") . (",exp,"))"]

getUsage :: IO String
getUsage = do
    pn <- getProgName
    return $ usageInfo ("Usage: " ++ pn ++ " [<options>] <cmd> [<file>]") 
                       options

main :: IO ()
main = do
    defaultConfigFile <- getDefaultConfigFile
    optsArgs <- processArgs defaultConfigFile <$> getArgs
    
    -- checkToolkitOrRecompileIt
    either printErrorAndExit go optsArgs
    where processArgs cfgFile args = compileOpts args >>=
                                     postOptsProcessing cfgFile
          printErrorAndExit errors = errorMessage errors >> exitFailure
          errorMessage errs = do
                        usage <- getUsage
                        P.putStrLn $ L.unlines (errs ++ ['\n':usage])
          go (opts,notOpts) = do
                        toolkit <- if optRecompile opts
                                      then recompile
                                      else getToolkitFileAndModuleName
                        if L.null notOpts || optHelp opts == True
                                then getUsage >>= putStr
                                else hscmd toolkit
                                           opts
                                           (L.head notOpts)
                                           (if L.length notOpts > 1
                                                then Just $ notOpts !! 1
                                                else Nothing)
