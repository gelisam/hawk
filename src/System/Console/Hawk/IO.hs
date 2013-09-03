{-# LANGUAGE NoImplicitPrelude #-}
module System.Console.Hawk.IO 
  (getInput
  ,printOutput)
where

import Control.Exception
  (handle)
import Data.ByteString.Lazy.Char8
import Data.Maybe
  (Maybe
  ,maybe)
import GHC.IO.Exception
  (IOErrorType(ResourceVanished)
  ,IOException(ioe_type))
import System.IO
  (IO
  ,hFlush
  ,hPrint
  ,stderr
  ,stdout)
import Prelude hiding (getContents,putStrLn,readFile)

getInput :: Maybe FilePath
         -> IO ByteString
getInput = maybe getContents readFile

printOutput :: ByteString
            -> IO ()
printOutput s = handle ioHandler (putStrLn s >> hFlush stdout)
  where ioHandler e = case ioe_type e of
                        ResourceVanished -> return ()
                        _ -> hPrint stderr e
