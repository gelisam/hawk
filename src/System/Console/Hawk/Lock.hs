module System.Console.Hawk.Lock ( withLock ) where

import Control.Concurrent ( threadDelay )
import Control.Exception
import Control.Monad ( guard )
import GHC.IO.Exception
import Network.BSD ( getProtocolNumber ) -- still cross-platform, don't let the name fool you
import Network.Socket


-- use a socket number as a lock indicator.
withLock :: IO a -> IO a
withLock body = withSocketsDo $ do
    bracket lock unlock $ const body
  where
    lock = do
        catchJust isADDRINUSE open $ \() -> do
            -- open failed, the lock must be in use.
            
            -- uncomment this and race two instances in order to verify that locking works
            --putStrLn "** LOCKED **"
            
            -- wait for the other instance to finish, then try again.
            threadDelay 250000  -- 0.25 seconds
            lock
    unlock = close
    
    
    isADDRINUSE :: IOError -> Maybe ()
    isADDRINUSE = guard . (== "bind") . ioe_location
    
    open = listenOn portNumber
    
    -- the first few [0-9] characters of the sha1 of "hawk"
    portNumber :: PortNumber
    portNumber = 62243
    
    -- from the source of Network.listenTo
    listenOn port = do
        proto <- getProtocolNumber "tcp"
        bracketOnError
            (socket AF_INET Stream proto)
            (sClose)
            (\sock -> do
                -- unlike the original listenOn, we do NOT set ReuseAddr.
                -- this way the call will fail if another instance holds the lock.
                --setSocketOption sock ReuseAddr 1
                bindSocket sock (SockAddrInet port iNADDR_ANY)
                listen sock maxListenQueue
                return sock
            )
