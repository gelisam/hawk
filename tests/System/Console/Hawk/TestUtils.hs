module System.Console.Hawk.TestUtils where

import Control.Applicative
  ( (<$>) )
import Data.List
  ( foldl'
  , isSuffixOf
  , isPrefixOf )
import System.Directory
  ( getDirectoryContents
  , getTemporaryDirectory
  , removeFile)
import System.FilePath
  ( FilePath
  , (</>)
  , dropExtension
  , takeExtension)


nextFilePath :: FilePath -- ^ directory
             -> String -- ^ prefix
             -> String -- ^ suffix
             -> IO FilePath -- ^ next file path available
nextFilePath dir pre post = do
    contents <- getDirectoryContents dir
    let max = foldl maybeTakeNum 0 contents
    return $ pre ++ show max ++ post
    where maybeTakeNum :: Int -> String -> Int
          maybeTakeNum acc str =
            if pre `isPrefixOf` str && post `isSuffixOf` str
                then let num = read $ take (lnum str) $ drop lpre str
                     in max acc num
                else acc
          lpre = length pre
          lnum str = length str - lpre - length post

withTempFile :: FilePath -- ^ directory
             -> String -- ^ file template
             -> (FilePath -> IO a) -- ^ action to be run with the temp file
             -> IO a
withTempFile dir template action = do
    let pre = dropExtension template
    let post = takeExtension template
    tempFileName <- ((</>) dir) <$> nextFilePath dir pre post
    writeFile tempFileName ""
    res <- action tempFileName
    removeFile tempFileName
    return res

withTempFile' :: String -- ^ file template
              -> (FilePath -> IO a) -- ^ action to be run with the temp file
              -> IO a
withTempFile' template action = do
    tempDir <- getTemporaryDirectory
    withTempFile tempDir template action
