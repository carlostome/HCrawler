{-# LANGUAGE TemplateHaskell #-}
module Main where

-------------------------------------------------------------------------------

import System.IO
import System.Environment
import Control.DeepSeq (deepseq, NFData, rnf)
import Control.Monad
import Control.Exception
import qualified Data.Set as S
import Network.URI
import HTMLScrapper
import qualified HTTPClient as H
import qualified UI.Server as Server
import Analyzer

import Data.DeriveTH

-------------------------------------------------------------------------------

$(derive makeNFData ''URI)
$(derive makeNFData ''URIAuth)


-- | Given a starting URL builds a graph until max depth          
crawlPages :: Integer -> URI -> IO [(URI,HTMLDoc,[URI])]
crawlPages = g0 S.empty 
  where
    g0 _ 0 _ = return []
    g0 visited n url 
      | url `S.member` visited = return []                                 
      | otherwise = do
        putStrLn $ "Fetching url: " ++ show url
        result <- try (getPage url) :: IO (Either H.HttpException HTMLDoc)
        case result of
          Left _ -> return []
          Right doc -> do
            let links = fetchAllLinks doc
                newVisited = url `S.insert` visited
            nextPages <- fmap concat $ forM links (g0 newVisited (n-1))
            return $ (url,doc,links) : nextPages

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  [pageURL,n] <- getArgs
  case parseURI pageURL of
    Nothing -> putStrLn "Invalid URI"
    Just u -> do
      putStrLn "\nBuilding the WebGraph. This will take a while.\n"
      pages <- crawlPages (read n) u
      putStrLn "\nBuilding the query table\n"
      let resultTable = makeQueryTable pages
      resultTable `deepseq` putStrLn "\nTable evaluated\n"
      Server.start resultTable queryTable
