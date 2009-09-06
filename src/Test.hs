module Test where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import TreeBank


main = defaultMain tests

tests = [
        testGroup "Parsing" [
                testCase "sort1" test_parse
                      ]
        ]

test_parse = 
    assertEqual "Word Info Parse" 
                (parseWordInfo  "6   of           IN     5   NP+NP+PP     *+PP    0  a")
                (Right $ WordInfo { 
                            ind = 6,
                            word = Word "of",
                            pos = POS "IN",
                            adjoinInd = 5,
                            spine = Spine [NTStar, NTNamed "PP"],
                            sister = False
                          })
             