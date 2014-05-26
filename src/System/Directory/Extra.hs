-- | For functions which should have been System.Directory
module System.Directory.Extra where

import System.Directory
import System.FilePath


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
