module Main (
      main
       ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import AnalyzerTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "Analyzer"
    [
      testProperty "All pages generated" prop_buildGraph_allPagesIn
    ]
  ]
