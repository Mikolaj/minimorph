{-# LANGUAGE OverloadedStrings #-}
-- | Text utility functions.
module NLP.Minimorph.Util
 ( tTakeEnd, tDropEnd, (<>), (<+>), tshow )
 where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

-- | @tTakeEnd n t@ returns the last @n@ letters of @t@.
tTakeEnd :: Int -> Text -> Text
tTakeEnd n t = T.drop (T.length t - n) t

-- | @tDropEnd n t@ drops the last @n@ letters of @t@.
tDropEnd :: Int -> Text -> Text
tDropEnd n x = T.take (T.length x - n) x

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
