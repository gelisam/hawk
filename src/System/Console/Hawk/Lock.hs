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

-- | Two concurrent Hawk processes (when the output of a Hawk command is piped
--   into another, for example) are in a race condition to compile and cache
--   the user prelude. The global application lock prevents this.
module System.Console.Hawk.Lock
    ( withLock
    , withTestLock
    ) where

import System.Directory ( doesFileExist, removeFile )
import Control.Monad ( when )
import System.FileLock ( SharedExclusive( Exclusive ), withFileLock )

withLock :: IO a -> IO a
withLock = lock False

withTestLock :: IO a -> IO a
withTestLock = lock True

lockfile :: FilePath
lockfile = "lock"

-- | Create a temporary file to indicate to other hawk process
--  not to run until this process is unblocked
lock :: Bool -> IO a -> IO a
lock testing action = do
    when testing $ putStrLn "** LOCKED **"
    res <- withFileLock lockfile Exclusive $ const action
    -- check in-case another process deletes filelock
    fileExists <- doesFileExist lockfile
    when fileExists $ removeFile lockfile
    when testing $ putStrLn "** UNLOCKED **"
    return res

