{-# LANGUAGE OverloadedStrings #-}
module UI.Server(
  start
  ) where

import Network.URI
import Web.Scotty
import qualified Data.Map as M
import qualified Data.Text.Lazy as LazyT

start :: M.Map String [(URI,Integer)] -> (String -> M.Map String [(URI,Integer)] -> [(URI,Integer)]) -> IO ()
start table searchTable = scotty 3000 $ do
  get "/:word" $ do
    word <- param "word"
    let result = searchTable word table
    html $ LazyT.pack $ generateHTMLpage word result
  

generateHTMLpage :: String -> [(URI, Integer)] -> String
generateHTMLpage word results =
  "<html><link rel=\"stylesheet\" href=\"//netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css\">" ++
  "<link rel=\"stylesheet\" href=\"//netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css\">" ++
  "<script src=\"//netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js\"></script><title>Search " ++ 
  word ++ "</title></head><body><h1>"++ "Results for " ++ word ++ "</h1><div class=\"panel panel-default\">" ++
  "<table class=\"table\"><tr><th>Points</th><th>URL</th><tr>" ++
   list results ++
  "</table></div></body></html>"
    where
      list :: [(URI, Integer)] -> String
      list results = unlines $ map (\(url, value) -> "<tr><td>" ++ show value ++
       "</td><td><a href=\"" ++ show url ++ "\">" ++ show url ++ "</a></td></tr>") results

