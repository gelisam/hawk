-- | For functions which should have been System.Directory
module System.Directory.Extra where

import Data.List
import System.FilePath


-- | Only works with absolute paths.
-- 
-- >>> ancestors "/bin"
-- ["/","/bin"]
-- 
-- >>> ancestors "/bin/foo/bar"
-- ["/","/bin","/bin/foo","/bin/foo/bar"]
ancestors :: FilePath -> [FilePath]
ancestors absPath = absPaths
  where
    (drive, fullRelPath) = splitDrive absPath 
    reconstruct relPath = joinDrive drive relPath
    
    components = splitDirectories fullRelPath
    relPaths = map joinPath (inits components)
    absPaths = map reconstruct relPaths
