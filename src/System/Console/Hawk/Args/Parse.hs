{-# LANGUAGE OverloadedStrings, PackageImports #-}
-- | In which Hawk's command-line arguments are structured into a `HawkSpec`.
module System.Console.Hawk.Args.Parse (parseArgs) where

import Control.Applicative
import Data.Char                                 (isSpace)
import Data.Maybe                                (isNothing, fromJust)
import "mtl" Control.Monad.Trans

import Control.Monad.Trans.OptionParser
import Control.Monad.Trans.Uncertain
import qualified System.Console.Hawk.Args.Option as Option
import           System.Console.Hawk.Args.Option (HawkOption, options)
import           System.Console.Hawk.Args.Spec
import           System.Console.Hawk.Context.Dir

-- $setup
-- >>> let testP parser = runUncertainIO . runOptionParserT options parser


-- | Record separator, field separator, and input file
--   is common to both output and input option parsing
data CommonOptions = CommonOptions
    { record :: Separator
    , field :: Separator
    , inputFile :: Maybe String
    }

-- | Extract '-D', '-d', and input file.
--   We perform this step separately because those three
--   options are used by both the input and output specs.
-- 
-- TODO: UPDATE TEST CASES
-- >>> let test = testP commonSeparators
-- 
-- >>> test []
-- (Delimiter "\n",Whitespace)
-- 
-- >>> test ["-D\\n", "-d\\t"]
-- (Delimiter "\n",Delimiter "\t")
-- 
-- >>> test ["-D|", "-d,"]
-- (Delimiter "|",Delimiter ",")
commonOptions :: (Functor m, Monad m)
                 => OptionParserT HawkOption m CommonOptions
commonOptions = do
    r <- lastSep Option.RecordDelimiter defaultRecordSeparator
    f <- lastSep Option.FieldDelimiter defaultFieldSeparator
    file <- consumeLast Option.InputFile "" $ consumeNullable "" consumeString
    return $ CommonOptions r f $ if null file then Nothing else Just file
  where
    lastSep opt def = consumeLast opt def consumeSep
    consumeSep = fmap Delimiter . Option.consumeDelimiter

-- | The input delimiters have already been parsed, but we still need to
--   interpret them and to determine the input source.
-- 
-- >>> :{
-- let test = testP $ do { c <- commonSeparators
--                       ; _ <- consumeExtra consumeString  -- skip expr
--                       ; i <- inputSpec c
--                       ; lift $ print $ inputSource i
--                       ; lift $ print $ inputFormat i
--                       }
-- :}
-- 
-- >>> test []
-- UseStdin
-- Records (Delimiter "\n") (Fields Whitespace)
-- 
-- >>> test ["-d", "-a", "L.reverse"]
-- UseStdin
-- Records (Delimiter "\n") RawRecord
-- 
-- >>> test ["-D", "-a", "B.reverse"]
-- UseStdin
-- RawStream
-- 
-- >>> test ["-d:", "-m", "L.head", "/etc/passwd"]
-- InputFile "/etc/passwd"
-- Records (Delimiter "\n") (Fields (Delimiter ":"))
inputSpec :: (Functor m, Monad m)
          => CommonOptions -> OptionParserT HawkOption m InputSpec
inputSpec options = InputSpec <$> source <*> format
  where
    source = return $ maybe UseStdin InputFile file
    format = return streamFormat
    streamFormat | rSep == Delimiter "" = RawStream
                 | otherwise           = Records rSep recordFormat
    recordFormat | fSep == Delimiter "" = RawRecord
                 | otherwise           = Fields fSep
    rSep = record options
    fSep = field options
    file = inputFile options

-- | The output delimiters take priority over the input delimiters, regardless
--   of the order in which they appear.
-- 
-- >>> :{
-- let test = testP $ do { c <- commonSeparators
--                       ; o <- outputSpec c
--                       ; let OutputFormat r f = outputFormat o
--                       ; lift $ print $ outputSink o
--                       ; lift $ print (r, f)
--                       }
-- :}
-- 
-- >>> test []
-- UseStdout
-- ("\n"," ")
-- 
-- >>> test ["-D;", "-d", "-a", "L.reverse"]
-- UseStdout
-- (";"," ")
-- 
-- >>> test ["-o\t", "-d,", "-O|"]
-- UseStdout
-- ("|","\t")
--
-- TODO: write test cases for in-place edit
outputSpec :: (Functor m, Monad m)
           => CommonOptions -> OptionParserT HawkOption m OutputSpec
outputSpec options = OutputSpec <$> sink <*> format
  where
    sink = do
        -- TODO: refactor with upcoming consumeNullable changes
        backupSuffix <- consumeLast Option.InPlaceEdit "" $
                            consumeNullable "<none>" consumeString
        if isNothing file
            then if null backupSuffix
                then return UseStdout
                else fail "in-place edit requires input file"
            else if null backupSuffix
                then return UseStdout
                else if backupSuffix == "<none>"
                    then return $ OutputFile file' Nothing
                    else return $ OutputFile file' $ Just $ file' ++ backupSuffix

    format = OutputFormat <$> record' <*> field'
    record' = consumeLast Option.OutputRecordDelimiter r' Option.consumeDelimiter
    field' = consumeLast Option.OutputFieldDelimiter f' Option.consumeDelimiter
    r' = fromSeparator $ record options
    f' = fromSeparator $ field options
    file = inputFile options
    file' = (fromJust file)


-- | The information we need in order to evaluate a user expression:
--   the expression itself, and the context in which it should be evaluated.
--   In Hawk, that context is the user prelude.
-- 
-- >>> :{
-- let test = testP $ do { e <- exprSpec
--                       ; lift $ print $ untypedExpr e
--                       ; lift $ print $ userContextDirectory (contextSpec e)
--                       }
-- :}
-- 
-- >>> test []
-- error: missing user expression
-- *** Exception: ExitFailure 1
-- 
-- >>> test [""]
-- error: user expression cannot be empty
-- *** Exception: ExitFailure 1
--
-- >>> test ["-D;", "-d", "-a", "L.reverse","-c","somedir"]
-- "L.reverse"
-- "somedir"
exprSpec :: (Functor m, MonadIO m)
         => OptionParserT HawkOption m ExprSpec
exprSpec = ExprSpec <$> (ContextSpec <$> contextDir)
                    <*> expr
  where
    contextDir = do
      dir <- consumeLast Option.ContextDirectory "" consumeString
      if null dir
        then liftIO findContextFromCurrDirOrDefault
        else return dir
    expr = do
        r <- consumeExtra consumeString
        case r of
          Just e  -> if all isSpace e
                      then fail "user expression cannot be empty"
                      else return e
          Nothing -> fail "missing user expression"


-- | Parse command-line arguments to construct a `HawkSpec`.
-- 
-- TODO: complain if some arguments are unused (except perhaps "-d" and "-D").
-- 
-- >>> :{
-- let test args = do { spec <- runUncertainIO $ parseArgs args
--                    ; case spec of
--                        Help        -> putStrLn "Help"
--                        Version     -> putStrLn "Version"
--                        Eval  e   o -> putStrLn "Eval"  >> print (untypedExpr e)                                         >> print (recordDelimiter (outputFormat o), fieldDelimiter (outputFormat o))
--                        Apply e i o -> putStrLn "Apply" >> print (untypedExpr e, inputSource i) >> print (inputFormat i) >> print (recordDelimiter (outputFormat o), fieldDelimiter (outputFormat o))
--                        Map   e i o -> putStrLn "Map"   >> print (untypedExpr e, inputSource i) >> print (inputFormat i) >> print (recordDelimiter (outputFormat o), fieldDelimiter (outputFormat o))
--                    }
-- :}
-- 
-- >>> test []
-- Help
-- 
-- >>> test ["--help"]
-- Help
-- 
-- >>> test ["--version"]
-- Version
-- 
-- >>> test ["-d\\t", "L.head"]
-- Eval
-- "L.head"
-- ("\n","\t")
-- 
-- >>> test ["-D\r\n", "-d\\t", "-m", "L.head"]
-- Map
-- ("L.head",UseStdin)
-- Records (Delimiter "\r\n") (Fields (Delimiter "\t"))
-- ("\r\n","\t")
-- 
-- >>> test ["-D", "-O\n", "-m", "L.head", "file.in"]
-- Map
-- ("L.head",InputFile "file.in")
-- RawStream
-- ("\n"," ")
parseArgs :: (Functor m,MonadIO m) => [String] -> UncertainT m HawkSpec
parseArgs [] = return Help
parseArgs args = runOptionParserT options parser args
  where
    parser = do
        lift $ return ()  -- silence a warning
        cmd <- consumeExclusive assoc eval
        c <- commonOptions
        cmd c
    assoc = [ (Option.Help,    help)
            , (Option.Version, version)
            , (Option.Apply,   apply)
            , (Option.Map,     map')
            ]

    help, version, eval, apply, map' :: (Functor m,MonadIO m) => CommonOptions
                                     -> OptionParserT HawkOption m HawkSpec
    help    _ = return Help
    version _ = return Version
    eval    c = Eval  <$> exprSpec <*>                 outputSpec c
    apply   c = Apply <$> exprSpec <*> inputSpec c <*> outputSpec c
    map'    c = Map   <$> exprSpec <*> inputSpec c <*> outputSpec c
