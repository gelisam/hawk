-- | For functions which should have been System.Directory
module System.Directory.Extra where

import System.Directory
import System.FilePath


-- | A version of `canonicalizePath` which works even if the file
--   doesn't exist.
-- 
-- >>> setCurrentDirectory "/bin"
-- >>> absPath "foo"
-- "/bin/foo"
-- 
-- Bug: unlike `canonicalizePath`, the resulting path isn't absolute.
-- 
-- -->>> absPath "foo/../bar"
-- --"/bin/bar"
absPath :: FilePath -> IO FilePath
absPath f = do
    pwd <- getCurrentDirectory
    return (pwd </> f)

-- | Only works with relative paths.
-- 
-- >>> parentPath "foo/bar/baz"
-- "foo/bar"
-- >>> parentPath "foo/bar"
-- "foo"
-- >>> parentPath "foo"
-- "."
parentPath :: FilePath -> FilePath
parentPath = init . dropFileName
