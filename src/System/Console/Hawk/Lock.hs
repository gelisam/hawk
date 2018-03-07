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

-- | Create a temporary file to indicate to other hawk process
--  not to run until this process is unblocked
withLock :: IO a -> IO a
withLock action = withFileLock lockfile Exclusive (const action) >>= cleanup

-- | withLock but with additional messages
withTestLock :: IO a -> IO a
withTestLock action = do
    result <- withFileLock lockfile Exclusive $ \_ -> do
        putStrLn "** LOCKED **"
        res <- action
        putStrLn "** UNLOCKED **"
        return res
    cleanup result

-- A temporary file - to be deleted after unlock
lockfile :: FilePath
lockfile = "lock"

-- check in-case another process deletes filelock
cleanup :: a -> IO a
cleanup result = do
    fileExists <- doesFileExist lockfile
    when fileExists $ removeFile lockfile
    return result
