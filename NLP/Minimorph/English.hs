{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
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

import           Data.Char          (toLower, isSpace, isUpper)
import           Data.Text          (Text)
import qualified Data.Text          as T

import           NLP.Minimorph.Util

-- ---------------------------------------------------------------------
-- ** Punctuation
-- ---------------------------------------------------------------------

-- | No Oxford commas, alas.
--
-- > commas "and" "foo bar"       == "foo and bar"
-- > commas "and" "foo, bar, baz" == "foo, bar and baz"
commas :: Text -> [Text] -> Text
commas _ []  = ""
commas _ [x] = x
commas et xs = T.intercalate ", " (init xs) <+> et <+> last xs

-- ---------------------------------------------------------------------
-- ** Numbers
-- ---------------------------------------------------------------------

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
    _ -> showT n

-- | > ordinalNotSpelled 1 == "1st"
--   > ordinalNotSpelled 2 == "2nd"
--   > ordinalNotSpelled 11 == "11th"
ordinalNotSpelled :: Int -> Text
ordinalNotSpelled k = case abs k `rem` 100 of
  n | n > 3 && n < 21 -> k `suf` "th"
    | n `rem` 10 == 1 -> k `suf` "st"
    | n `rem` 10 == 2 -> k `suf` "nd"
    | n `rem` 10 == 3 -> k `suf` "rd"
    | otherwise       -> k `suf` "th"
 where
  num `suf` s = showT num <> s

-- | > ordinal 1 == "first"
--   > ordinal 2 == "second"
--   > ordinal 3 == "third"
--   > ordinal 11 == "11th"
--   > ordinal 42 == "42nd"
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
    k  -> ordinalNotSpelled k

-- ---------------------------------------------------------------------
-- ** Nouns and verbs
-- ---------------------------------------------------------------------

-- | Heuristics for English plural for an unknown noun
--
-- > defaultNounPlural "egg"    == "eggs"
-- > defaultNounPlural "patch"  == "patches"
-- > defaultNounPlural "boy"    == "boys"
-- > defaultNounPlural "spy"    == "spies"
-- > defaultNounPlural "thesis" == "theses"
--
-- http://www.paulnoll.com/Books/Clear-English/English-plurals-1.html
-- http://en.wikipedia.org/wiki/English_plural
defaultNounPlural :: Text -> Text
defaultNounPlural x
    | "is" `T.isSuffixOf` x = thesis
    | hasSibilantSuffix x   = sibilant_o
    | hasCoSuffix x         = sibilant_o
    | hasCySuffix x         = y_final
    | "f"  `T.isSuffixOf` x = f_final
    | otherwise             = plain
  where
    plain      = x            <> "s"
    sibilant_o = x            <> "es"
    y_final    = T.init x     <> "ies"
    f_final    = T.init x     <> "ves"
    thesis     = tDropEnd 2 x <> "es"

-- | Heuristics for 3rd person singular and past participle
--   for an unknown regular verb. Doubling of final consonants
--   can be handled via a table of (partially) irrefular verbs.
--
-- > defaultVerbStuff "walk"  == ("walks",  "walked")
-- > defaultVerbStuff "push"  == ("pushes", "pushed")
-- > defaultVerbStuff "play"  == ("plays",  "played")
-- > defaultVerbStuff "cry"   == ("cries",  "cried")
defaultVerbStuff :: Text -> (Text, Text)
defaultVerbStuff x
    | hasSibilantSuffix x   = sibilant_o
    | "o" `T.isSuffixOf` x  = sibilant_o
    | hasCySuffix x         = y_final
    | "e" `T.isSuffixOf` x  = e_final
    | otherwise             = plain
  where
    plain      = (x <> "s"         , x <> "ed")
    sibilant_o = (x <> "es"        , x <> "ed")
    e_final    = (x <> "s"         , x <> "d")
    y_final    = (T.init x <> "ies", T.init x <> "ied")

-- | Heuristics for a possesive form for an unknown noun.
--
-- > defaultPossesive "pass"        == "pass'"
-- > defaultPossesive "SOS"         == "SOS'"
-- > defaultPossesive "Mr Blinkin'" == "Mr Blinkin's"
-- > defaultPossesive "cry"         == "cry's"
defaultPossesive :: Text -> Text
defaultPossesive t =
  case T.last t of
    's'  -> t <> "'"
    'S'  -> t <> "'"
    '\'' -> t <> "s"
    _    -> t <> "'s"

-- ---------------------------------------------------------------------
-- ** Determiners
-- ---------------------------------------------------------------------

-- | > indefiniteDet "dog"  == "a"
--   > indefiniteDet "egg"  == "an"
--   > indefiniteDet "ewe"  == "a"
--   > indefiniteDet "ewok" == "an"
--   > indefiniteDet "8th"  == "an"
indefiniteDet :: Text -> Text
indefiniteDet t = if wantsAn t then "an" else "a"

-- | True if the indefinite determiner for a word would normally be
--   'an' as opposed to 'a'
wantsAn :: Text -> Bool
wantsAn t_ =
    if startsWithAcronym t_
       then acronymWantsAn t_
       else useAn0 || useAn1 || useAn2
  where
    t      = T.toLower t_
    useAn0 = t `elem` [ "11", "11th" ]
    useAn1 = case T.uncons t of
                Just ('8',_) -> True
                Just (h,_)   -> isVowel h `butNot` hasSemivowelPrefix t
                Nothing      -> False
    useAn2 = case T.break isSep t of
                (T.unpack -> [c], _) -> isLetterWithInitialVowelSound c
                _ -> False
    x `butNot` y = x && not y
    isSep c = isSpace c || c `elem` "-"

-- | Variant of 'wantsAn' that assumes the input string is pronounced
--   one letter at a time.
--
--   > wantsAn        "x-ray" == False
--   > acronymWantsAn "x-ray" == True
--
--   Note that this won't do the right thing for words like @"SCUBA"@
--   You really have to reserve it for those separate-letter acronyms
acronymWantsAn :: Text -> Bool
acronymWantsAn (T.toLower -> t) =
    useAn0 || useAn1
  where
    useAn0 = t `elem` [ "11", "11th" ]
    useAn1 = case T.uncons t of
                Just ('8',_) -> True
                Just (h,_)   -> isLetterWithInitialVowelSound h
                Nothing      -> False

-- ---------------------------------------------------------------------
-- ** Acronyms
-- ---------------------------------------------------------------------

-- | True if all upper case from second letter and up
--
--   > looksLikeAcronym "DNA"  == True
--   > looksLikeAcronym "tRNA" == True
--   > looksLikeAcronym "DnA"  == False
looksLikeAcronym :: Text -> Bool
looksLikeAcronym x = T.all isUpper (T.drop 1 x)

-- | True if the first word (separating on either - or space)
--   looks like an acronym
startsWithAcronym :: Text -> Bool
startsWithAcronym =
    looksLikeAcronym . firstWord
  where
    firstWord = fst . T.break isSep
    isSep c   = isSpace c || c `elem` "-"

-- ---------------------------------------------------------------------
-- ** Sounds
-- ---------------------------------------------------------------------

-- | Ends with a sh sound
hasSibilantSuffix :: Text -> Bool
hasSibilantSuffix x = any (`T.isSuffixOf` x) ["x","s","ch","sh","z","j"]

-- | Starts with a semivowel
hasSemivowelPrefix :: Text -> Bool
hasSemivowelPrefix ls = any (`T.isPrefixOf` ls) ["y","w","eu","ewe"]

-- | Last two letters are a consonant and 'y'
hasCySuffix :: Text -> Bool
hasCySuffix (T.unpack . tTakeEnd 2 -> [x, 'y']) = isConsonant x
hasCySuffix _ = False

-- | Last two letters are a consonant and 'o'
hasCoSuffix :: Text -> Bool
hasCoSuffix (T.unpack . tTakeEnd 2 -> [x, 'o']) = isConsonant x
hasCoSuffix _ = False

-- | Is a vowel
isVowel :: Char -> Bool
isVowel = (`elem` "aeiou") . toLower

-- | Letters that when pronounced independently in English sound like they
--   begin with vowels
--
--   > isLetterWithInitialVowelSound 'r' == True
--   > isLetterWithInitialVowelSound 'k' == False
--
--   (In the above, @'r'@ is pronounced @"are"@, but @'k'@ is pronounced
--   @"kay"@)
isLetterWithInitialVowelSound :: Char -> Bool
isLetterWithInitialVowelSound = (`elem` "aeioufhlmnrsx") . toLower

-- | Is a consonant
isConsonant :: Char -> Bool
isConsonant = not . isVowel
