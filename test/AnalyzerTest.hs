module AnalyzerTest where


import Analyzer
import qualified        WebGraph as WG
import Util.Lorem
import Util.HTMLMock
import HTMLScrapper
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe


prop_buildGraph_allPagesIn htmldomain = all (`WG.exists` graph) allURL
  where
    graph = buildWebGraph (htmlDomToPool htmldomain) 
    allURL = getAllURLs htmldomain

prop_links_domain dom = all (`elem` listFromURIs) listFromLinks
  where
    listFromURIs =  map _uriP (_domain dom)
    listFromLinks = concatMap _links $ _domain dom

prop_keywords :: HTMLDomain -> Bool
prop_keywords htmldomain = all (\(x,y) -> x L.\\ y == []) results_expected
    where
        keywords_results = map (M.keys . (flip fetchKeywords [("title",10)]) . parsePage . _page) pages
        keywords_expected = map (fromJust . M.lookup "title" . _keywords) pages
        results_expected = zip keywords_results keywords_expected
        pages = tail $ _domain htmldomain
