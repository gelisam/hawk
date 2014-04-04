-- | In which the user prelude is massaged into the form hint needs.
module System.Console.Hawk.UserPrelude where

import Data.HaskellModule


type UserPrelude = HaskellModule


canonicalUserPrelude :: HaskellModule -> UserPrelude
canonicalUserPrelude = undefined

compileUserPrelude :: FilePath -> IO ()
compileUserPrelude = undefined
