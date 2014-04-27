-- | For functions which should have been System.Directory
module System.Directory.Extra where

import System.Directory
import System.FilePath


-- A version of `canonicalizePath` which works even if the file
-- doesn't exist.
absPath :: FilePath -> IO FilePath
absPath f = do
    pwd <- getCurrentDirectory
    return (pwd </> f)
