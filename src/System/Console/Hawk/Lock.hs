module System.Console.Hawk.Lock ( withLock ) where

--import Control.Concurrent ( threadDelay )
import Control.Exception
import Control.Monad ( guard )
import GHC.IO.Exception
import GHC.IO.Handle ( hGetContents, hClose )
import Network.BSD ( getProtocolNumber ) -- still cross-platform, don't let the name fool you
import Network ( PortID (..), connectTo )
import Network.Socket
import Text.Printf


-- use a socket number as a lock indicator.
withLock :: IO a -> IO a
withLock body = withSocketsDo $ do
    bracket lock unlock $ const body
  where
    lock = do
        catchJust isADDRINUSE openSocket $ \() -> do
            -- open failed, the lock must be in use.
            
            -- uncomment this and race two instances in order to verify that locking works.
            --putStrLn "** LOCKED **"
            
            -- uncomment and run less than 5 seconds before another process unlocks,
            -- to verify that we correctly handle the case where the socket is closed
            -- between openSocket and waitForException.
            --threadDelay 5000000
            
            -- wait for the other instance to signal that it is done with the lock.
            catchJust isDisconnected waitForException $ \() -> do
              -- we were disconnected, the server must have released the lock!
              
              -- uncomment this and race with a long-running instance to verify
              -- that we unlock as early as possible.
              --putStrLn "** UNLOCKED **"
              
              -- try again.
              lock
    unlock = closeSocket
    
    openHandle = connectTo "localhost" $ PortNumber portNumber
    closeHandle = hClose
    
    waitForException :: IO a
    waitForException = bracket openHandle closeHandle $ \h -> do
      s <- hGetContents h
      length s `seq` return ()  -- blocks until EOF, which never comes
                                -- because the server never accepted the connection
      error $ printf "port %s in use by a program other than hawk" $ show portNumber
    
    isADDRINUSE :: IOError -> Maybe ()
    isADDRINUSE = guard . (== "bind") . ioe_location
    
    isDisconnected :: IOError -> Maybe ()
    isDisconnected = guard . (`elem` ["connect", "hGetContents"]) . ioe_location
    
    openSocket = listenOn portNumber
    closeSocket = sClose
    
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
