{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
-- TODO : learn how to use Functional Morphology instead

-- | Module    : NLP.Minimorph.English
-- Copyright   : 2012 Eric Kow (Computational Linguistics Ltd.)
-- License     : BSD3
-- Maintainer  : eric.kow@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Simple default rules for English morphology
module NLP.Minimorph.English where

import Data.Text ( Text )
import qualified Data.Text as T

import NLP.Minimorph.Util

-- | No Oxford commas, alas.
--
-- > commas "and" "foo bar"       == "foo and bar"
-- > commas "and" "foo, bar, baz" == "foo, bar and baz"
commas :: Text -> [Text] -> Text
commas _ []  = ""
commas _ [x] = x
commas et xs = T.intercalate ", " (init xs) <+> et <+> last xs

-- | > cardinal 1 == "one"
--   > cardinal 2 == "two"
--   > cardinal 3 == "three"
--   > cardinal 11 == "11"
cardinal :: Int -> Text
cardinal n = case n of
    1  -> "one"
    2  -> "two"
    3  -> "three"
    4  -> "four"
    5  -> "five"
    6  -> "six"
    7  -> "seven"
    8  -> "eight"
    9  -> "nine"
    10 -> "ten"
    _ -> T.pack (show n)

-- | > cardinal 1 == "first"
--   > cardinal 2 == "second"
--   > cardinal 3 == "third"
--   > cardinal 4 == "4th"
ordinal :: Int -> Text
ordinal n = case n of
    1  -> "first"
    2  -> "second"
    3  -> "third"
    4  -> "fourth"
    5  -> "fifth"
    6  -> "sixth"
    7  -> "seventh"
    8  -> "eighth"
    9  -> "ninth"
    10 -> "tenth"
    n | n < 21          -> n `suf` "th"
      | n `rem` 10 == 2 -> n `suf` "nd"
      | n `rem` 10 == 3 -> n `suf` "rd"
      | otherwise       -> n `suf` "th"
  where
    n `suf` s = T.pack (show n) <> s

-- | Heuristics for English plural for an unknown noun
--
-- > defaultNounPlural "egg"    == "eggs"
-- > defaultNounPlural "patch"  == "patches"
-- > defaultNounPlural "boy"    == "boys"
-- > defaultNounPlural "spy"    == "spies"
-- > defaultNounPlural "thesis" == "theses"
--
-- http://www.paulnoll.com/Books/Clear-English/English-plurals-1.html
defaultNounPlural :: Text -> Text
defaultNounPlural x
    | "is" `T.isSuffixOf` x = thesis
    | hasSibilantSuffix x   = es
    | hasCySuffix x         = y_ies
    | "f"  `T.isSuffixOf` x = f_ves
    | otherwise             = plain
  where
    plain  = x            <> "s"
    es     = x            <> "es"
    y_ies  = T.init x     <> "ies"
    f_ves  = T.init x     <> "ves"
    thesis = tDropEnd 2 x <> "es"

-- | Heuristics for 3rd person singular and past participle
--   for an unknown regular verb
--
-- > defaultVerbStuff "walk"  == ("walks",  "walked")
-- > defaultVerbStuff "push"  == ("pushes", "pushed")
-- > defaultVerbStuff "play"  == ("plays",  "played")
-- > defaultVerbStuff "cry"   == ("cries",  "cried")
defaultVerbStuff :: Text -> (Text, Text)
defaultVerbStuff v
    | hasSibilantSuffix v   = sibilant_o v
    | "o" `T.isSuffixOf` v  = sibilant_o v
    | "e" `T.isSuffixOf` v  = e_final v
    | hasCySuffix v         = y_final v
    | otherwise             = plain v
  where
    plain x      = (x <> "s"         , x <> "ed")
    sibilant_o x = (x <> "es"        , x <> "ed")
    e_final    x = (x <> "s"         , x <> "d")
    y_final    x = (T.init x <> "ies", T.init x <> "ied")

-- | > indefiniteDet "dog"  == "a"
--   > indefiniteDet "egg"  == "an"
--   > indefiniteDet "ewe"  == "a"
--   > indefiniteDet "ewok" == "an"
--   > indefiniteDet "8th"  == "an"
indefiniteDet :: Text -> Text
indefiniteDet (T.toLower -> t) =
    if useAn then "an" else "a"
  where
    useAn  = t `elem` [ "11", "11th" ] || useAn'
    useAn' = case T.uncons t of
                Just ('8',_) -> True
                Just (h,_)   -> isVowel h `butNot` hasSemivowelPrefix t
                Nothing      -> False
    x `butNot` y = x && not y

-- | Ends with a sh sound
hasSibilantSuffix :: Text -> Bool
hasSibilantSuffix x = any (`T.isSuffixOf` x) ["x","s","ch","sh"]

-- | Starts with a semivowel
hasSemivowelPrefix :: Text -> Bool
hasSemivowelPrefix ls = any (`T.isPrefixOf` ls) ["y","w","eu","ewe"]

-- | Last two letters are a consonant and 'y'
hasCySuffix :: Text -> Bool
hasCySuffix (T.unpack . tTakeEnd 2 -> [x, 'y']) = isConsonant x
hasCySuffix _ = False

-- | Is a vowel
isVowel :: Char -> Bool
isVowel = (`elem` "aeiouAEIOU")

-- | Is a consonant
isConsonant :: Char -> Bool
isConsonant = not . isVowel
