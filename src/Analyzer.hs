module Analyzer (
  buildWebGraph,
  makeQueryTable,
  queryTable,
  fetchKeywords,
  ResultTable
  ) where

-------------------------------------------------------------------------------

import Data.Maybe 
import Data.Char (toLower)
import qualified Data.List as L
import qualified Data.Map as M
import qualified WebGraph as WG
import HTMLScrapper
import Network.URI (URI)

-------------------------------------------------------------------------------

type ResultTable = M.Map String [(URI,Integer)]
type PageRank = M.Map URI Integer

ranks :: [(String, Integer)]
ranks =  [("title",10) ,("h1", 8), ("h2", 6), ("p",4), ("a", 3), ("div", 4)]

-- | Builds the webGraph of a given pool of pages
buildWebGraph :: [(URI,HTMLDoc,[URI])] -> WG.WebG
buildWebGraph  = g0 WG.empty . map (\(uri,doc,links) -> (uri,fetchKeywords doc ranks,links))
  where
    g0 webG [] = webG
    g0 webG ((uri,kw,links):rest) = g0 (WG.insert uri kw links webG) rest
   

-- | Fetches all the keywords of a page and assigns them a value 
fetchKeywords :: HTMLDoc -> [(String, Integer)] -> WG.Keywords
fetchKeywords doc = foldl (\m (tag, val) -> foldl (\mp (word, points) -> M.insertWith (+) word points mp) m $ getAndClassify tag val) M.empty
  where
    getAndClassify :: String -> Integer -> [(String, Integer)]
    getAndClassify tag val = zip (map (map toLower) $ concatMap words $ fetchTag tag doc) (repeat val)
          

-- | Assigns a rank to every page in a WebGraph according to the number
-- | of times they are referenced
rankPages :: WG.WebG -> PageRank
rankPages  = foldl (\m u -> M.insertWith (+) u 1 m) M.empty . allLinks
  where
    allLinks webg = concat $ mapMaybe (`WG.links` webg) (WG.getURIs webg)


-- | Makes a search table which maps Words to web pages
makeTable :: WG.WebG -> PageRank -> ResultTable
makeTable webGraph pageRank = M.unionsWith (++) $ map processURI  graphURIs
  where
    calculateRank valWord uri = getRank uri + valWord
    graphURIs = WG.getURIs webGraph
    processURI uri = M.fromList $  map (\(w,val) -> (w,[(uri,calculateRank val uri)]))
                     $ M.toList $ fromJust $ WG.keywords uri webGraph
    getRank uri = case M.lookup uri pageRank of
      Nothing -> 0
      Just r  -> r


-- | Search for a word in the ResultTable and return the list of pages
-- | ordered by rank
queryTable :: String -> ResultTable -> [(URI,Integer)]
queryTable s  = L.sortBy (\(_,v1) (_,v2) -> compare v2 v1) . concat . maybeToList . M.lookup s


-- | makeQueryTable gets a pool of htmlpages and return the table with ranks
makeQueryTable :: [(URI,HTMLDoc,[URI])] -> ResultTable
makeQueryTable pagePool = makeTable  webGraph pagesRank
  where
    webGraph  = buildWebGraph pagePool
    pagesRank = rankPages webGraph

