module AnalyzerTest where

-------------------------------------------------------------------------------

import Analyzer
import Test.QuickCheck
import qualified WebGraph as WG
import Util.Lorem
import Util.HTMLMock
import HTMLScrapper
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

-------------------------------------------------------------------------------

prop_buildGraph_allPagesIn htmldomain = all (`WG.exists` graph) allURL
  where
    graph = buildWebGraph (htmlDomToPool htmldomain) 
    allURL = getAllURLs htmldomain

prop_links_domain dom = all (`elem` listFromURIs) listFromLinks
  where
    listFromURIs =  map _uriP (_domain dom)
    listFromLinks = concatMap _links $ _domain dom

prop_keywords :: HTMLDomain -> Bool
prop_keywords htmldomain = all property $ zip keywords_results keywords_expected
    where
      keywords_results = map (\(_,htmldoc,_) -> M.keys $ fetchKeywords htmldoc [("title",10)]) pagePool
      keywords_expected = map (fromJust . M.lookup "title" . _keywords) (tail $ _domain htmldomain)
      pagePool = tail $ htmlDomToPool htmldomain
      property (kw_r,kw_e) = all (`elem` kw_r) (L.nub kw_e)


