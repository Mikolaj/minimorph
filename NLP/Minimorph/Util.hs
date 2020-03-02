-- | Text utility functions.
module NLP.Minimorph.Util
 ( (<+>), tshow )
 where

#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
import Data.Semigroup
#endif

import           Data.Text (Text)
import qualified Data.Text as T

infixr 6 <+>  -- matches Monoid.<>
-- | Separated by space unless one of them is empty (in which case just
--   the non-empty one).
(<+>) :: Text -> Text -> Text
t1 <+> t2 | T.null t1 = t2
          | T.null t2 = t1
          | otherwise = t1 <> " " <> t2

-- | Show a value in `Text` format.
tshow :: Show a => a -> Text
tshow = T.pack . show
