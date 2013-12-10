module Main (
      main
       ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import AnalyzerTest
import WebGraphTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "Analyzer"
    [
      testProperty "All pages generated" prop_buildGraph_allPagesIn,
      testProperty "Keywords" prop_keywords,
      testProperty "prop_links_domain" prop_links_domain
    ],
    testGroup "WebGraph"
    [
      testProperty "After insert is in" prop_insert_exists,
      testProperty "" prop_insert_uris,
      testProperty "" prop_insert_links,
      testProperty "" prop_insert_kws,
      testProperty "" prop_unions
      ]
  ]
