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

module System.Console.Hawk.Lock.Test where

import Control.Concurrent
import System.Console.Hawk.Lock

-- $setup
-- >>> :{
--   let par body1 body2 = do
--         done1 <- newEmptyMVar
--         done2 <- newEmptyMVar
--         forkFinally body1 (\_ -> putMVar done1 ())
--         forkFinally body2 (\_ -> putMVar done2 ())
--         takeMVar done1
--         takeMVar done2
-- :}


printDelayed :: [Int] -> IO ()
printDelayed [] = return ()
printDelayed (x:xs) = do threadDelay 10000
                         print x
                         printDelayed xs


-- | Use `withLock` around critical sections which cannot run in parallel with
--   other instances of hawk.
-- 
-- In the ideal case, nobody else is trying to use `withLock`.
-- 
-- >>> withLock print3
-- 1
-- 2
-- 3
-- 
-- If two instances of hawk are trying to use same resource (here stdout) at
-- the same time, problems can occur.
-- (test disabled because the unlocked behaviour isn't deterministic)
-- 
-- -- >>> print3 `par` print3
-- -- 1
-- -- 1
-- -- 2
-- -- 2
-- -- 3
-- -- 3
-- 
-- By using `withLock`, we serialize the execution of the two critical sections.
-- 
-- >>> withLock print3 `par` withLock print3
-- 1
-- 2
-- 3
-- 1
-- 2
-- 3
-- 
-- There is a verbose version of `withLock` which makes it easy to see when
-- two instances are trying to enter `withLock` at the same time.
-- 
-- >>> withTestLock print3
-- ** LOCKED **
-- 1
-- 2
-- 3
-- ** UNLOCKED **
-- 
-- >>> withTestLock print3 `par` withTestLock print3
-- ** LOCKED **
-- 1
-- 2
-- 3
-- ** UNLOCKED **
-- ** LOCKED **
-- 1
-- 2
-- 3
-- ** UNLOCKED **
-- 
-- Here is a variant of the previous test which tests a race condition in which
-- instance 1 releases the lock immediately after instance 2 notices that the
-- lock is busy, but before instance 2 begins waiting for the lock to be released.
-- 
-- This test case has been covered with filelock, the second process will be blocked
-- at any point if the first process is running.
-- 
-- >>> withTestLock print3 `par` (threadDelay 15000 >> withTestLock print3)
-- ** LOCKED **
-- 1
-- 2
-- 3
-- ** UNLOCKED **
-- ** LOCKED **
-- 1
-- 2
-- 3
-- ** UNLOCKED **
print3 :: IO ()
print3 = printDelayed [1..3]
