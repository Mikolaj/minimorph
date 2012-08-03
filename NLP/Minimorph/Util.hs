{-# LANGUAGE OverloadedStrings #-}
-- | Module    : NLP.Minimorph.Util
-- Copyright   : 2012 Eric Kow (Computational Linguistics Ltd.)
-- License     : BSD3
-- Maintainer  : eric.kow@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Utility functions probably internal to minimorph
module NLP.Minimorph.Util where

import Data.Text ( Text )
import qualified Data.Text as T

-- | @tTakeEnd n t@ returns the last @n@ letters of @t@
tTakeEnd :: Int -> Text -> Text
tTakeEnd n t = T.drop (T.length t - n) t

-- | @tDropEnd n t@ drops the last @n@ letters of @t@
tDropEnd :: Int -> Text -> Text
tDropEnd n x = T.take (T.length x - n) x

-- | Identical to 'T.append'
(<>) :: Text -> Text -> Text
t1 <> t2 = t1 `T.append` t2

-- | Separated by space unless one of them is empty (in which case just
--   the non-empty one)
(<+>) :: Text -> Text -> Text
t1 <+> t2 | T.null t1 = t2
          | T.null t2 = t1
          | otherwise = t1 `T.append` " " `T.append` t2
