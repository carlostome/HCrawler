module HTMLScrapper(
  HTMLDoc,
  parsePage,
  getPage,
  fetchTag,
  fetchAllLinks
  ) where

-------------------------------------------------------------------------------

import Text.HandsomeSoup
import Text.XML.HXT.Core
import Network.URI
import Data.Maybe (mapMaybe)
import Data.List (isSuffixOf, nub)

import HTTPClient (downloadURL)

-------------------------------------------------------------------------------

type HTMLDoc = XmlTree

getPage :: URI -> IO HTMLDoc
getPage url = do 
    html <- downloadURL (show url)
    fmap head $ runX $ parseHtml html 

fetchTag :: String -> HTMLDoc -> [String]
fetchTag tag = runLA (css tag >>> getChildren >>> getText)

fetchAllLinks :: HTMLDoc -> [URI]
fetchAllLinks =  nub . mapMaybe (parseURI . removeSlash) . runLA (css "a" ! "href") 
    where
        removeSlash str
            | "/" `isSuffixOf` str = take (length str - 1) str
            | otherwise = str

parsePage :: String -> HTMLDoc
parsePage  = head . runLA hread 
