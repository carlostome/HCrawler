module AnalyzerTest where


import Analyzer
import WebGraph as WG
import Util.Lorem
import Util.HTMLMock


prop_buildGraph_allPagesIn htmldomain = all (`WG.exists` graph) allURL
  where
    graph = buildWebGraph (htmlDomToPool htmldomain) 
    allURL = getAllURLs htmldomain
    
