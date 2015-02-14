module System.Console.Hawk.UserExpression.OriginalExpression where

import qualified Data.ByteString.Lazy.Char8 as B

import Data.HaskellExpr


-- | The user expression could have many different types, and we need to track
--   how that type changes under our various manipulations. Use `Nothing` to
--   indicate that a particular type is not compatible with the modifications
--   mandated by the command-line flags.
data UserExpression a b c d = UserExpression (Maybe (HaskellExpr a))
                                             (Maybe (HaskellExpr b))
                                             (Maybe (HaskellExpr c))
                                             (Maybe (HaskellExpr d))

-- | To avoid an ambiguous type variable, we instantiate the output type of the
--   user expression to unit. The code will still work with any instance of Rows.
type OriginalUserExpression = UserExpression ()
                                             (B.ByteString -> ())
                                             ([B.ByteString] -> ())
                                             ([[B.ByteString]] -> ())

-- | The user expression is expected to have one of the above four types.
--   For now we just pretend like it has all four types, and as the flags
--   are processed, some of those cases will be eliminated, until only
--   one case remains.
-- 
-- If the actual user expression doesn't have the type we use it at, hint
-- will give a type error, and that's fine.
originalUserExpression :: String -> OriginalUserExpression
originalUserExpression s = UserExpression (Just $ HaskellExpr s)
                                          (Just $ HaskellExpr s)
                                          (Just $ HaskellExpr s)
                                          (Just $ HaskellExpr s)
