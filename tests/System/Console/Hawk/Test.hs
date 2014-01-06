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

{-# LANGUAGE OverloadedStrings #-}
module System.Console.Hawk.Test where

import Control.Applicative
  ((<$>))
import Data.ByteString.Lazy.Char8
  (ByteString)
import Language.Haskell.Interpreter
  (Extension)
import System.FilePath
import Test.Hspec
import Test.HUnit

import System.Console.Hawk
  (hawk)
import System.Console.Hawk.Config
import System.Console.Hawk.Options
import System.Console.Hawk.TestUtils
  (withTempDir')


--hawk :: Options                -- ^ Program options
--     -> (String,String)         -- ^ The prelude file and module name
--     -> [(String,Maybe String)] -- ^ The modules maybe qualified
--     -> [Extension]             -- ^ The extensions to enable
--     -> String                  -- ^ The user expression to evaluate
--     -> IO (Either InterpreterError (LB.ByteString -> LB.ByteString))

mode :: Modes
     -> Options
mode m = defaultOptions{ optMode = m }

run :: IO ()
run = withContextHSpec $ \itEval itApply itMap ->
        describe "Hawk" $ do
          itEval "" `into` ""
          itEval "1" `into` "1"
          itEval "1+1" `into` "2"
          itEval "[]" `into` ""
          itEval "[1]" `into` "1"
          itEval "[1,2]" `into` "1\n2"
          itEval "(1,2)" `into` "1\n2"
          itEval "[[1]]" `into` "1"
          itEval "[[1,2]]" `into` "1 2"
          itEval "[[1,2],[3,4]]" `into` "1 2\n3 4"
          
          itApply "id" `onInput` "foo" `into` "foo"
          itApply "L.transpose" `onInput` "1 2\n3 4" `into` "1 3\n2 4"
          itApply "L.map (!! 1)" `onInput` "1 2\n3 4" `into` "2\n4"

          itMap "(!! 1)" `onInput` "1 2\n3 4" `into` "2\n4"
  where onInput f x = f x
        into f x = f x

withContextHSpec :: ((String -> ByteString -> Spec)
                    -> (String -> ByteString -> ByteString -> Spec)
                    -> (String -> ByteString -> ByteString -> Spec)
                    -> Spec)
                 -> IO ()
withContextHSpec body = withDefaultConfiguration $ \prelude modules extensions -> do
  let dhawk m expr = hawk (mode m) prelude modules extensions expr
  let it' m expr input expected =
        let descr = "evals " ++ show expr ++
                    " on input " ++ show input ++
                    " equals to " ++ show expected
        in it descr $ do eitherErrorF <- dhawk m expr
                         case eitherErrorF of
                           Left e -> assertFailure (show e)
                           Right f -> assertEqual descr expected (f input)
  let [itApply,itMap] = map it' [ApplyMode,MapMode]
  let itEval expr expected = it' EvalMode expr "" expected
  hspec $ body itEval itApply itMap


-- itEval <str> `withInput` <input> `equalsTo` <expected>

withDefaultConfiguration :: ((String,String) 
                             -> [(String,Maybe String)]
                             -> [Extension]
                             -> IO ())
                         -> IO ()
withDefaultConfiguration f =
  withTempDir' "HawkTest" $ \dir -> do
    let prelude = dir </> "prelude.hs"
    writeFile prelude defaultPrelude
    let cacheDir = dir </> "cache"
    let source = dir </> "post_process_prelude.hs"
    let extensionsFile = dir </> "extensions"
    let modulesFile = dir </> "modules"
    let compiled = dir </> "prelude"
    let configInfoPath = dir </> "configInfo"

    (outputFile,outputModule) <- recompileConfig' prelude
                                                 cacheDir
                                                 source
                                                 extensionsFile
                                                 modulesFile
                                                 compiled
                                                 configInfoPath

    modules <- read <$> readFile modulesFile
    extensions <- read <$> readFile extensionsFile

    f (outputFile,outputModule) modules extensions

--recompileConfig' :: FilePath -- ^ config file
--                 -> FilePath -- ^ cache dir
--                 -> FilePath -- ^ source file
--                 -> FilePath -- ^ output extensions cache file
--                 -> FilePath -- ^ output modules cache file
--                 -> FilePath -- ^ output compiled file
--                 -> FilePath -- ^ output config info path
--                 -> IO (String,String)
