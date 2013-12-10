module WebGraphTest where

import Test.QuickCheck
import WebGraph
import qualified Data.List as L
import Data.Maybe


-- Tests
prop_insert_exists uri kw lnks webg =  uri `exists` insert uri kw lnks webg

prop_insert_uris uri kw links webg = uri `elem` getURIs (insert uri kw links webg)

prop_insert_links uri kw lnks webg = lnks == fromJust (links uri (insert uri kw lnks webg))

prop_insert_kws uri kw lnks webg = kw == fromJust (keywords uri (insert uri kw lnks webg))

prop_unions g1 g2 = null $ getURIs (unions [g1,g2]) L.\\ (getURIs g1 ++ getURIs g2)
