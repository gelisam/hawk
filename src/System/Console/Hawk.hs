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

-- | Hawk as seen from the outside world: parsing command-line arguments,
--   evaluating user expressions.
module System.Console.Hawk (

    hawk

) where


import Language.Haskell.Interpreter

import System.Console.Hawk.UserPrelude.Extend

import System.Console.Hawk.Sandbox

runLockedHawkInterpreter :: InterpreterT IO a -> IO (Either InterpreterError a)
runLockedHawkInterpreter i = runHawkInterpreter i

-- |
-- >>> hawk "42"
-- 33

-- |
-- >>> hawk "number `seq` 42"
-- 33
hawk :: String -> IO Int
hawk expr = do
    Right x <- runLockedHawkInterpreter $ do
        setImportsQ [("Prelude", Nothing), ("System.Console.Hawk.Runtime", Nothing)]
        interpret expr (as :: Int)
    return x
