import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework

import qualified NLP.Minimorph.EnglishTest

main :: IO ()
main = defaultMain 
        [ NLP.Minimorph.EnglishTest.suite
        ]
