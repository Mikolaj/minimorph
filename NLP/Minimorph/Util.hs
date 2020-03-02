-- | Text utility functions.
module NLP.Minimorph.Util
 ( (<+>), tshow )
 where

-- Only needed for older GHC, but let's avoid CPP for this instance.
import Data.Monoid ((<>))

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
