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
  get "/" $ do
    html $ LazyT.pack $ generateIndex
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
      list res = unlines $ map (\(url, value) -> "<tr><td>" ++ show value ++
       "</td><td><a href=\"" ++ show url ++ "\">" ++ show url ++ "</a></td></tr>") res

generateIndex :: String
generateIndex = "<html><head><script src=\"http://code.jquery.com/jquery-latest.min.js\" type=\"text/javascript\"></script></head><body><input type=\"text\" id=\"querytext\"/><input id=\"searchbtn\" type=\"submit\" value=\"Search\"/><script type=\"text/javascript\">$(document).ready(function() { $(\"#searchbtn\").click(function() {  window.location.href = \"http://localhost:3000/\"+$(\"#querytext\").val(); }); });</script><body></html>"
