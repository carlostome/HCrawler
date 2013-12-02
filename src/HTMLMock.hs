module HTMLMock (
  HTMLDomain,
  getAllURLs,
  htmlDomToPool
  ) where

-------------------------------------------------------------------------------

import Data.Maybe
import Data.List (nub)
import HTMLScrapper (parsePage, HTMLDoc)
import Network.URI
import Test.QuickCheck
import qualified Lorem 

import qualified Data.Map as M
import Control.Monad (sequence)

-------------------------------------------------------------------------------
boundMax = 3367900313

newtype HTMLURI = HTMLURI { _uri :: String } deriving (Show, Eq)

instance Arbitrary HTMLURI where
  arbitrary = fmap f (arbitrary :: Gen Integer)
    where
      f = HTMLURI . ("http://www.testing.com/"++) . (++".html") . show . (`mod` boundMax) . abs

data HTMLPage = HTMLPage {
  _uriP :: HTMLURI,
  _page :: String,
  _keywords :: M.Map String [String],
  _links :: [HTMLURI]
  } deriving Show

newtype HTMLDomain = HTMLDomain {
  _domain :: [HTMLPage]
  } deriving Show

instance Arbitrary HTMLDomain where
  arbitrary = do
    urls <- fmap nub $ listOf arbitrary
    randomPages <- sequence $ map (randomPage urls) urls
    return $ HTMLDomain $ (principal urls) : randomPages
    where
      principal urls = HTMLPage (HTMLURI "http://www.testing.com/") ("<html><body>" ++ makeLinks urls ++ "</body></html>")
                       M.empty urls
      arbitraryWords n = fmap unwords $ vectorOf n $ elements $ Lorem.generate
      makeLinks = concatMap ((\l -> "<a href=\"" ++ l ++ "\"</a>") . _uri)
      randomPage domainUris url = do
        title <- sized arbitraryWords
        h1 <- sized arbitraryWords
        h2 <- sized arbitraryWords 
        p1 <- sized arbitraryWords
        p2 <- sized arbitraryWords
        links <- fmap nub $ listOf $ elements domainUris
        let body =  "<html> <head> <title>" ++ title ++ "</title> </head>"
                    ++ "<body> <h1>" ++ h1 ++ "</h1> <h2>" ++ h2 ++ "</h2><p>"
                    ++ p1 ++ "</p><p>" ++ p2 ++ "</p>" ++ makeLinks links ++ "</body> </html>"
            kw   = M.fromList $ zip ["title","h1","h2","p","p"] $ map words [title,h1,h2,p1,p2]
        return $ HTMLPage url body kw links

-- Functions to export
htmlDomToPool :: HTMLDomain -> [(URI,HTMLDoc,[URI])]
htmlDomToPool dom = zip3 uris pages links
  where
    uris  = map (fromJust . parseURI . _uri . _uriP) (_domain dom)
    pages = map (parsePage . _page) (_domain dom)
    links = map (map (fromJust . parseURI . _uri ) . _links) (_domain dom)

getAllURLs :: HTMLDomain -> [URI]
getAllURLs = map (fromJust . parseURI . _uri . _uriP) . _domain

-- Properties

-- All links from pages is a subset of all the urls
prop_links_domain dom = all (`elem` listFromURIs) listFromLinks
  where
    listFromURIs =  map _uriP (_domain dom)
    listFromLinks = concatMap _links $ _domain dom

