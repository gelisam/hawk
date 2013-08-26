module System.Console.Hawk.Lock.Test where

import Control.Concurrent
import System.Console.Hawk.Lock


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
-- In two instances of hawk are trying to use same resource (here stdout) at
-- the same time, problems can occur.
-- 
-- >>> forkIO print3 >> print3
-- 1
-- 1
-- 2
-- 2
-- 3
-- 3
-- 
-- By using `withLock`, we serialize the execution of the two critical sections.
-- 
-- >>> forkIO (withLock print3) >> (withLock print3)
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
-- 1
-- 2
-- 3
-- 
-- >>> forkIO (withTestLock print3) >> (withTestLock print3)
-- ** LOCKED **
-- 1
-- 2
-- 3
-- ** UNLOCKED (hGetContents) **
-- 1
-- 2
-- 3
-- 
-- This verbosity allows us to test a special case: a race condition in which
-- instance 1 releases the lock immediately after instance 2 notices that the
-- lock is busy, but before instance 2 begins waiting for the lock to be released.
-- 
-- We can trigger this special case with a bit of collaboration from `withTestLock`.
-- It inserts an artificial delay at the point in which we want the lock to be
-- released, and we time our instances so that instance 1 unlocks just at the right
-- moment. If we timed the experiment right, the error message should be "connect"
-- instead of "hGetContents".
-- 
-- >>> forkIO (withTestLock print3) >> threadDelay 15000 >> (withTestLock print3)
-- 1
-- ** LOCKED **
-- 2
-- 3
-- ** UNLOCKED (connect) **
-- 1
-- 2
-- 3
print3 :: IO ()
print3 = printDelayed [1..3]
