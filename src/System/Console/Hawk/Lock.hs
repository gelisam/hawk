module System.Console.Hawk.Lock
    ( withLock
    , withTestLock
    ) where

import Control.Concurrent ( threadDelay )
import Control.Exception
import Control.Monad ( when, guard )
import GHC.IO.Exception
import GHC.IO.Handle ( Handle, hGetContents, hClose )
import Network.BSD ( getProtocolNumber ) -- still cross-platform, don't let the name fool you
import Network ( PortID (..), connectTo )
import Network.Socket
import Text.Printf


-- use a socket number as a lock indicator.
withSocketLock :: Bool -> IO a -> IO a
withSocketLock testing = withSocketsDo . bracket (lock testing) unlock . const

-- make sure GHC inlines withSocketLock in the following two functions
-- and optimizes the testing and non-testing versions separately.
{-# INLINE withSocketLock #-}

withLock :: IO a -> IO a
withLock = withSocketLock False

withTestLock :: IO a -> IO a
withTestLock = withSocketLock True


lock :: Bool -> IO Socket
lock testing = catchJust isADDRINUSE openSocket $ \() -> do
    -- open failed, the lock must be in use.
    
    when testing $ putStrLn "** LOCKED **"
    
    -- used to test an interleaving in which the socket is closed here,
    -- between openSocket and waitForException.
    when testing $ threadDelay 5000000
    
    -- wait for the other instance to signal that it is done with the lock.
    catchJust isDisconnected waitForException $ \() -> do
      -- we were disconnected, the server must have released the lock!
      
      when testing $ putStrLn "** UNLOCKED **"
      
      -- try again.
      lock testing

unlock :: Socket -> IO ()
unlock = closeSocket


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


openHandle :: IO Handle
openHandle = connectTo "localhost" $ PortNumber portNumber

closeHandle :: Handle -> IO ()
closeHandle = hClose


openSocket :: IO Socket
openSocket = listenOn portNumber

closeSocket :: Socket -> IO ()
closeSocket = sClose


-- the first few [0-9] characters of the sha1 of "hawk"
portNumber :: PortNumber
portNumber = 62243

-- from the source of Network.listenTo
listenOn :: PortNumber -> IO Socket
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
