{-# LANGUAGE DeriveFunctor, LambdaCase, PackageImports, RankNTypes #-}
-- | A typeclass- and monad-based interface for GetOpt,
--   designed to look as if the options had more precise types than String.
module Control.Monad.Trans.OptionParser where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Data.Maybe
import qualified System.Console.GetOpt as GetOpt
import Text.Printf

import Control.Monad.Trans.Uncertain

-- $setup
--
-- >>> :{
-- let testH tp = do { putStrLn "Usage: more [option]... <song.mp3>"
--                   ; putStr $ optionsHelpWith head
--                                              id
--                                              (return . printf "adds more %s.")
--                                              tp
--                                              ["cowbell","guitar","saxophone"]
--                   }
-- :}
--
-- >>> let testP args tp p = runUncertain $ runOptionParserWith head id (const [""]) tp ["cowbell","guitar","saxophone"] p args


-- | List your options as a datatype, so you can consume specific values later.
--   Then make your datatype an Option instance so we know how to parse them.
class Eq a => Option a where
  shortName :: a -> Char
  longName :: a -> String
  helpMsg :: a -> [String]
  optionType :: a -> OptionType

-- | The type of the argument set by the option. Since Haskell doesn't support
--   dependent types, this is just a string description of the type, plus extra
--   support for boolean flags and optional arguments.
--
-- To maintain the illusion of precise types, please use combining functions
-- such as `optional int` instead.
data OptionType
    = Flag                   -- Bool, no argument
    | Setting String         -- mandatory String argument
    | OptionalSetting String -- optional String argument
  deriving (Show, Eq)


-- | The monad in which you can consume options.
type OptionParser o a = OptionParserT o Identity a

-- | A monad-transformer version of `OptionParser`.
data OptionParserT o m a = OptionParserT
    { unOptionParserT :: StateT [(o, Maybe String)] -- flags and settings
                       ( StateT [String]            -- extra arguments
                       ( UncertainT m
                       )) a
    }

instance Functor m => Functor (OptionParserT o m) where
  fmap f = OptionParserT . fmap f . unOptionParserT

instance (Functor m, Monad m) => Applicative (OptionParserT o m) where
  pure = OptionParserT . pure
  OptionParserT mf <*> OptionParserT mx = OptionParserT (mf <*> mx)

instance Monad m => Monad (OptionParserT o m) where
  return = OptionParserT . return
  OptionParserT mx >>= f = OptionParserT (mx >>= f')
    where
      f' = unOptionParserT . f
  fail s = OptionParserT (fail s)

instance MonadTrans (OptionParserT o) where
  lift = OptionParserT . lift . lift . lift

instance MonadIO m => MonadIO (OptionParserT o m) where
  liftIO = lift . liftIO

mapOptionParserT :: (forall a. m a -> m' a)
                 -> OptionParserT o m b -> OptionParserT o m' b
mapOptionParserT f = OptionParserT
                   . (mapStateT $ mapStateT $ mapUncertainT f)
                   . unOptionParserT

liftUncertain :: (Monad m) => UncertainT m a -> OptionParserT o m a
liftUncertain = OptionParserT . lift . lift

-- | Convert an option into the structure `getOpt` expects.
optDescr :: forall o. Option o => o -> GetOpt.OptDescr (o, Maybe String)
optDescr = optDescrWith shortName longName helpMsg optionType

-- | A version of `optDescr` which doesn't use the Option typeclass.
optDescrWith :: (o -> Char)
             -> (o -> String)
             -> (o -> [String])
             -> (o -> OptionType)
             -> o -> GetOpt.OptDescr (o, Maybe String)
optDescrWith shortName' longName' helpMsg' optionType'
             o = GetOpt.Option [shortName' o]
                               [longName' o]
                               argDescr
                               (intercalate "\n" $ helpMsg' o)
  where
    argDescr = case optionType' o of
        Flag               -> GetOpt.NoArg (o, Just "")
        Setting tp         -> GetOpt.ReqArg (\s -> (o, Just s)) tp
        OptionalSetting tp -> GetOpt.OptArg (\ms -> (o, ms)) tp


-- | The part of your --help which describes each possible option.
optionsHelp :: Option o => [o] -> String
optionsHelp = optionsHelpWith shortName longName helpMsg optionType

-- | A version of `optionsHelp` which doesn't use the Option typeclass.
--
-- >>> :{
-- let { tp "cowbell"   = flag
--     ; tp "guitar"    = string
--     ; tp "saxophone" = optional int
--     }
-- :}
--
-- >>> testH tp
-- Usage: more [option]... <song.mp3>
-- Options:
--   -c       --cowbell          adds more cowbell.
--   -g str   --guitar=str       adds more guitar.
--   -s[int]  --saxophone[=int]  adds more saxophone.
--
optionsHelpWith :: (o -> Char)
                -> (o -> String)
                -> (o -> [String])
                -> (o -> OptionType)
                -> [o] -> String
optionsHelpWith shortName' longName' helpMsg' optionType'
  = GetOpt.usageInfo "Options:" . map optDescrWith'
  where
    optDescrWith' = optDescrWith shortName' longName' helpMsg' optionType'


-- | Use this instead of `getOpt`. It's not a drop-in replacement, it's a
--   different interface which allows you to consume arguments if and when you
--   need them. Ideal when different commands need a different subset of all
--   available arguments.
runOptionParserT :: (Option o, Monad m)
                 => [o]                  -- ^ every possible option
                 -> OptionParserT o m a  -- ^ an action which consumes options
                 -> [String]             -- ^ the command-line arguments
                 -> UncertainT m a
runOptionParserT = runOptionParserWith shortName longName helpMsg optionType

-- | A version of `runOptionParserT` which doesn't use the Option typeclass.
--
-- >>> :{
-- testP ["--cowbell","-s"] (const flag) $ do
--   { c <- fromMaybe False <$> consumeLast "cowbell"   flagConsumer
--   ; g <- fromMaybe False <$> consumeLast "guitar"    flagConsumer
--   ; s <- fromMaybe False <$> consumeLast "saxophone" flagConsumer
--   ; return (c, g, s)
--   }
-- :}
-- (True,False,True)
runOptionParserWith :: (Eq o, Monad m)
                    => (o -> Char)
                    -> (o -> String)
                    -> (o -> [String])
                    -> (o -> OptionType)
                    -> [o] -> OptionParserT o m a
                    -> [String] -> UncertainT m a
runOptionParserWith shortName' longName' helpMsg' optionType'
                    available_options parser args
  = case GetOpt.getOpt GetOpt.Permute optDescrs args of
      (given_options, extra_options, [])
        -> flip evalStateT extra_options
         $ flip evalStateT given_options
         $ unOptionParserT parser
      (_, _, error_mesage:_) -> fail msg
        where
          n = length error_mesage
          msg | last error_mesage == '\n' = take (n - 1) error_mesage
              | otherwise                 = error_mesage
  where
    optDescrWith' = optDescrWith shortName'
                                 longName'
                                 helpMsg'
                                 optionType'
    optDescrs = map optDescrWith'
                    available_options


-- | Try to parse a setting of a particular type.
--
-- The input will never be Nothing unless the optionType is optional, and even
-- then optionalConsumer will get rid of it for you. Yet we still need the type
-- of the input to be `Maybe String` in order for optionalConsumer itself to be
-- a valid OptionConsumerT.
newtype OptionConsumerT m a = OptionConsumerT
  { runOptionConsumerT :: Maybe String -> UncertainT m a
  } deriving Functor


-- | Specifies that the option cannot be assigned a value.
--
-- >>> let tp = const flag
-- >>> testH tp
-- Usage: more [option]... <song.mp3>
-- Options:
--   -c  --cowbell    adds more cowbell.
--   -g  --guitar     adds more guitar.
--   -s  --saxophone  adds more saxophone.
flag :: OptionType
flag = Flag

-- | True if the given flag appears.
--
-- >>> let tp = const flag
-- >>> let consumeCowbell = fromMaybe False <$> consumeLast "cowbell" flagConsumer :: OptionParser String Bool
--
-- >>> testP ["-cs"] tp consumeCowbell
-- True
--
-- >>> testP ["--saxophone"] tp consumeCowbell
-- False
flagConsumer :: Monad m => OptionConsumerT m Bool
flagConsumer = OptionConsumerT $ \_ -> return True


-- | Specifies that the option must be assigned a String value.
--
-- >>> let tp = const string
-- >>> testH tp
-- Usage: more [option]... <song.mp3>
-- Options:
--   -c str  --cowbell=str    adds more cowbell.
--   -g str  --guitar=str     adds more guitar.
--   -s str  --saxophone=str  adds more saxophone.
string :: OptionType
string = Setting "str"

-- | The value assigned to the option, interpreted as a string.
--
-- >>> let tp = const string
-- >>> let consumeCowbell = fromMaybe "<none>" <$> consumeLast "cowbell" stringConsumer :: OptionParser String String
--
-- >>> testP ["--cowbell", "extra"] tp consumeCowbell
-- "extra"
--
-- >>> testP ["-cs"] tp consumeCowbell
-- "s"
--
-- >>> testP [] tp consumeCowbell
-- "<none>"
--
-- >>> testP ["-c"] tp consumeCowbell
-- error: option `-c' requires an argument str
-- *** Exception: ExitFailure 1
stringConsumer :: Monad m => OptionConsumerT m String
stringConsumer = OptionConsumerT $ \case
  Just s -> return s
  Nothing -> error "please use optionalConsumer to consume optional options"


-- | Specifies that the value of the option may be omitted.
--
-- >>> let tp = const (optional string)
-- >>> testH tp
-- Usage: more [option]... <song.mp3>
-- Options:
--   -c[str]  --cowbell[=str]    adds more cowbell.
--   -g[str]  --guitar[=str]     adds more guitar.
--   -s[str]  --saxophone[=str]  adds more saxophone.
optional :: OptionType -> OptionType
optional (Setting tp) = OptionalSetting tp
optional (OptionalSetting _) = error "double optional"
optional Flag = error "optional flag doesn't make sense"

-- | The value assigned to an option, or a default value if no value was
--   assigned. Must be used to consume `optional` options.
--
-- >>> let tp = const (optional string)
-- >>> let consumeCowbell = fromMaybe "<none>" <$> consumeLast "cowbell" $ fromMaybe "<default>" <$> optionalConsumer stringConsumer :: OptionParser String String
--
-- >>> testP ["-cs"] tp consumeCowbell
-- "s"
--
-- >>> testP ["-c", "-s"] tp consumeCowbell
-- "<default>"
--
-- >>> testP ["-s"] tp consumeCowbell
-- "<none>"
--
-- >>> testP ["-c"] tp $ fromMaybe "<none>" <$> consumeLast "cowbell" stringConsumer
-- *** Exception: please use optionalConsumer to consume optional options
optionalConsumer :: Monad m => a -> OptionConsumerT m a -> OptionConsumerT m a
optionalConsumer defaultValue optionConsumer = OptionConsumerT $ \case
  Nothing -> return defaultValue
  o -> runOptionConsumerT optionConsumer o


-- | A helper for defining custom options types.
--
-- >>> :{
-- let { tp "cowbell"   = readable "amount"
--     ; tp "guitar"    = readable "file"
--     ; tp "saxophone" = readable "weight"
--     }
-- :}
--
-- >>> testH tp
-- Usage: more [option]... <song.mp3>
-- Options:
--   -c amount  --cowbell=amount    adds more cowbell.
--   -g file    --guitar=file       adds more guitar.
--   -s weight  --saxophone=weight  adds more saxophone.
readable :: String -> OptionType
readable = Setting

-- | The value assigned to the option, interpreted by `read`.
--
-- >>> let tp = const (readable "unit")
-- >>> let consumeCowbell = fromMaybe () <$> consumeLast "cowbell" readConsumer :: OptionParser String ()
--
-- >>> testP ["--cowbell=()"] tp consumeCowbell >>= print
-- ()
--
-- >>> testP ["--cowbell=foo"] tp consumeCowbell >>= print
-- error: "foo" is not a valid value for this option.
-- *** Exception: ExitFailure 1
readConsumer :: (Read a, Monad m) => OptionConsumerT m a
readConsumer = OptionConsumerT $ \o -> do
    s <- runOptionConsumerT stringConsumer o
    case reads s of
      [(x, "")] -> return x
      _ -> fail $ printf "%s is not a valid value for this option." $ show s


-- | Users are encouraged to create custom option types, like this.
--
-- (see the source)
int :: OptionType
int = readable "int"

-- | The value assigned to the option, interpreted as an int.
--
-- This is a good example of how to consume custom option types.
-- (see the source)
--
-- >>> let tp = const int
-- >>> let consumeCowbell = fromMaybe (-1) <$> consumeLast "cowbell" intConsumer :: OptionParser String Int
--
-- >>> testP ["--cowbell=42"] tp consumeCowbell
-- 42
intConsumer :: Monad m => OptionConsumerT m Int
intConsumer = readConsumer


-- | The value assigned to the option, interpreted as a path (String)
filePath :: OptionType
filePath = Setting "path"

-- | The value assigned to the option if the check function doesn't fail with
-- an error. The check functions must return a file path.
--
-- >>> import Control.Monad
-- >>> import System.EasyFile (doesDirectoryExist)
-- >>> let testIO args tp p = runUncertainIO $ runOptionParserWith head id (const [""]) tp ["input-dir"] p args
-- >>> let inputDir = const filePath
-- >>> :{
--   let checkDir f e d = do
--         c <- lift (f d)
--         if c then return d  :: UncertainT IO FilePath
--              else fail (e d)
-- :}
--
-- >>> let dirExists      = checkDir doesDirectoryExist                          (++ " doesn't exist")
-- >>> let dirDoesntExist = checkDir (\d -> doesDirectoryExist d >>= return . not) (++ " exists")
-- >>> let consumeLastInputDir = fromMaybe "error" <$> consumeLast "input-dir" :: OptionConsumerT IO String -> OptionParserT String IO String
-- >>> let consumeExistingDir    = consumeLastInputDir (consumeFilePath dirExists)
-- >>> let consumeNotExistingDir = consumeLastInputDir (consumeFilePath dirDoesntExist)
-- >>> testIO ["--input-dir=."] inputDir consumeExistingDir
-- "."
-- >>> testIO ["--input-dir=."] inputDir consumeNotExistingDir
-- error: . exists
-- *** Exception: ExitFailure 1
filePathConsumer :: MonadIO m
                 => (FilePath -> UncertainT m FilePath) -> OptionConsumerT m String
filePathConsumer check = OptionConsumerT $ \o -> do
  filePath_ <- runOptionConsumerT stringConsumer o
  check filePath_


-- | All the occurences of a given option.
--
-- It is an error to consume the same value twice (we currently return an
-- empty list).
--
-- >>> let tp = const string
-- >>> let consumeCowbell = consumeAll "cowbell" stringConsumer :: OptionParser String [String]
--
-- >>> :{
-- testP ["--cowbell=foo", "--cowbell", "bar"] tp $ do
--   { xs <- consumeCowbell
--   ; xs' <- consumeCowbell
--   ; return (xs, xs')
--   }
-- :}
-- (["foo","bar"],[])
consumeAll :: (Eq o, Monad m)
           => o -> OptionConsumerT m a -> OptionParserT o m [a]
consumeAll o optionConsumer = OptionParserT $ do
    matching_options <- state $ partition $ (== o) . fst
    lift . lift $ mapM (runOptionConsumerT optionConsumer . snd) matching_options

-- | The last occurence of a given option, or Nothing if the option isn't
--   specified.
--
-- If 'consumeAll' is called twice on the same option, the second call returns
-- Nothing.
--
-- >>> let tp = const string
-- >>> let consumeCowbell = fromMaybe "<none>" <$> consumeLast "cowbell" stringConsumer :: OptionParser String String
--
-- >>> :{
-- testP ["--cowbell=foo", "--cowbell", "bar"] tp $ do
--   { xs <- consumeCowbell
--   ; xs' <- consumeCowbell
--   ; return (xs, xs')
--   }
-- :}
-- ("bar","<none>")
consumeLast :: (Eq o, Monad m)
            => o -> OptionConsumerT m a -> OptionParserT o m (Maybe a)
consumeLast o optionConsumer = do
    xs <- consumeAll o optionConsumer
    if null xs
      then return Nothing
      else return $ Just $ last xs


-- | For use with mutually-exclusive flags.
consumeExclusive :: (Option o, Functor m, Monad m)
                 => [(o, a)] -> a -> OptionParserT o m a
consumeExclusive = consumeExclusiveWith longName

-- | A version of `consumeExclusive` which doesn't use the Option typeclass.
--
-- >>> let tp = const flag
-- >>> let consume = consumeExclusiveWith id [("cowbell",0),("guitar",1),("saxophone",2)] (-1) :: OptionParser String Int
--
-- >>> testP ["-s"] tp consume
-- 2
--
-- >>> testP [] tp consume
-- -1
--
-- >>> testP ["-cs"] tp consume
-- error: cowbell and saxophone are incompatible
-- *** Exception: ExitFailure 1
consumeExclusiveWith :: (Eq o, Functor m, Monad m)
                     => (o -> String)
                     -> [(o, a)] -> a -> OptionParserT o m a
consumeExclusiveWith longName' assoc defaultValue = do
    oss <- forM (map fst assoc) $ \o ->
      map (const o) <$> consumeAll o flagConsumer
    case concat oss of
      []  -> return defaultValue
      [o] -> return $ fromMaybe defaultValue $ lookup o assoc
      os  -> fail msg
        where
          n = length os
          (ss, [s]) = splitAt (n - 1) (map longName' os)
          msg = printf "%s and %s are incompatible" (intercalate ", " ss) s


-- | The next non-option argument.
--
-- >>> let tp = const flag
-- >>> let consume = consumeExtra stringConsumer :: OptionParser String (Maybe String)
--
-- >>> testP ["-cs", "song.mp3", "jazz.mp3"] tp consume
-- Just "song.mp3"
--
-- >>> testP ["-cs", "song.mp3", "jazz.mp3"] tp (consume >> consume)
-- Just "jazz.mp3"
--
-- >>> testP ["-cs", "song.mp3", "jazz.mp3"] tp (consume >> consume >> consume)
-- Nothing
consumeExtra :: (Functor m, Monad m)
             => OptionConsumerT m a -> OptionParserT o m (Maybe a)
consumeExtra optionConsumer = OptionParserT $ do
    extra_options <- lift get
    case extra_options of
      [] -> return Nothing
      (x:xs) -> do
        lift $ put xs
        fmap Just $ lift . lift $ runOptionConsumerT optionConsumer $ Just x

-- | All remaining non-option arguments.
--
-- >>> let tp = const flag
-- >>> let consume = consumeExtras stringConsumer :: OptionParser String [String]
--
-- >>> testP ["-cs", "song.mp3", "jazz.mp3"] tp consume
-- ["song.mp3","jazz.mp3"]
--
-- >>> testP ["-cs", "song.mp3", "jazz.mp3"] tp (consumeExtra stringConsumer >> consume)
-- ["jazz.mp3"]
--
-- >>> testP ["-cs", "song.mp3", "jazz.mp3"] tp (consume >> consume)
-- []
consumeExtras :: (Functor m, Monad m)
              => OptionConsumerT m a -> OptionParserT o m [a]
consumeExtras optionConsumer = fmap reverse $ go []
  where
    go xs = do
        r <- consumeExtra optionConsumer
        case r of
          Nothing -> return xs
          Just x  -> go (x:xs)
