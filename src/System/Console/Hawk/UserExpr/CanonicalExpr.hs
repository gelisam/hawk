module System.Console.Hawk.UserExpr.CanonicalExpr where


import qualified Data.ByteString.Lazy.Char8 as B

import           Data.HaskellExpr
import           Data.HaskellExpr.Base
import           System.Console.Hawk.Args
import           System.Console.Hawk.Runtime
import           System.Console.Hawk.Runtime.Base
import           System.Console.Hawk.Runtime.HaskellExpr
import           System.Console.Hawk.UserExpr.InputReadyExpr
import           System.Console.Hawk.UserExpr.OriginalExpr


-- | Regardless of the requested input format, we currently convert all user expressions
--   so that they expect a `[[ByteString]]`. The runtime will also look at the input format,
--   in order to encode it as a `[[ByteString]]` as well. For example, if the input format is
--   supposed to be a single `ByteString`, the runtime will pack that string `s` into a nested
--   singleton list `[[s]]`, and the canonical user expression will expect this format.
type CanonicalExpr = HaskellExpr ([[B.ByteString]] -> SomeRows)

-- | Could fail if the required case of the user expression is `Nothing`, meaning that the
--   other flags were not compatible with the requested input format.
canonicalizeExpr :: InputSpec -> InputReadyExpr -> Maybe CanonicalExpr
canonicalizeExpr i (UserExpr _ e1 e2 e3) = case inputFormat i of
  RawStream            -> (`eComp` (eHead `eComp` eHead)) <$> e1
  Records _ RawRecord  -> (`eComp` (eMap `eAp` eHead)) <$> e2
  Records _ (Fields _) -> e3


runCanonicalExpr :: CanonicalExpr -> HaskellExpr (HawkRuntime -> HawkIO ())
runCanonicalExpr eExpr = eFlip `eAp` eProcessTable `eAp` eExpr
