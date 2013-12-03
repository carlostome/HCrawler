{-# LANGUAGE TemplateHaskell #-}
module WebGraph
       (
        WebG,
        Keywords,
        empty,
        insert,
        isLeaf,
        exists,
        links,
        getURIs,
        keywords,
        unions
       ) where

-------------------------------------------------------------------------------

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Network.URI
import Data.DeriveTH
import Data.Derive.Arbitrary
import Test.QuickCheck
-------------------------------------------------------------------------------

$(derive makeArbitrary ''URIAuth)
$(derive makeArbitrary ''URI)

type Keywords = M.Map String Integer

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (M.Map a b) where
    arbitrary = sized rMap
        where
            rMap n = do
                ws <- vectorOf n arbitrary
                vals <- vectorOf n arbitrary
                return $ M.fromList $ zip ws vals

newtype WebG = WebG { webG :: M.Map URI (Keywords, [URI]) }

instance Show WebG where
    show = show . M.toList . webG

subListOf :: [a] -> Gen [a]
subListOf xs = do
    let l = length xs
    n1 <- fmap ((`mod` l) . abs) arbitrary
    n2 <- fmap ((`mod` l) . abs) arbitrary
    return $ take n1 $ drop n2 xs

instance Arbitrary WebG where
    arbitrary = sized rGraph
        where
            rGraph 0 = return $ WebG M.empty
            rGraph n = do
                headNode <- arbitrary
                headKw <- arbitrary
                uris <- vectorOf (n-1) arbitrary
                kw <- vectorOf (n-1) arbitrary
                links <- vectorOf (n-1) (subListOf uris)
                let g = (headNode,headKw,uris) : zip3 uris kw links
                return $ WebG $ foldl (\m (u,k,l) -> M.insert u (k,filter (/=u) l) m) M.empty g



-- | Empty graph
empty :: WebG
empty = WebG M.empty

-- | Insert a Node into WebG
insert :: URI -> Keywords -> [URI] -> WebG -> WebG
insert uri kw links = WebG . M.insert uri (kw,filter (/= uri) links) . webG

-- | Get all links of a given URI
links :: URI -> WebG -> Maybe [URI]
links uri = fmap snd . M.lookup uri . webG 

-- | Get Keywords of a given URI
keywords :: URI -> WebG -> Maybe Keywords
keywords uri = fmap fst . M.lookup uri . webG

-- | Get all URI's of a graph
getURIs :: WebG -> [URI]
getURIs = fst . unzip . M.toList . webG

-- | Checks if a URI have links
isLeaf :: URI -> WebG -> Bool
isLeaf uri webg = 
    let l = links uri webg 
    in case l of 
        Nothing -> False
        Just links -> null links                

-- | Checks if a URI exists in the graph
exists :: URI -> WebG -> Bool
exists uri = M.member uri . webG

-- | Union of a list of WebGraph
unions :: [WebG] -> WebG
unions = WebG . M.unions . map webG

