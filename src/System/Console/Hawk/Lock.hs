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

import System.FileLock ( SharedExclusive( Exclusive ), withFileLock )
import System.FilePath ( (</>) )

-- | Create a temporary file to indicate to other hawk process
--  not to run until this process is unblocked
withLock :: FilePath -> IO a -> IO a
withLock ctxDir action = withFileLock tempFile Exclusive (const action)
    where tempFile = ctxDir </> "lock.tmp"

-- | withLock but with additional messages
withTestLock :: FilePath -> IO a -> IO a
withTestLock ctxDir action = withFileLock tempFile Exclusive $ \_ -> do
    putStrLn "** LOCKED **"
    res <- action
    putStrLn "** UNLOCKED **"
    return res
    where tempFile = ctxDir </> "lock.tmp"
