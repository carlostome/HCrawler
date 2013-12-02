{-# LANGUAGE DeriveDataTypeable #-}
module HTTPClient (
  HttpException,
  downloadURL
  ) where

-------------------------------------------------------------------------------

import Network.HTTP
import Network.URI
import Network.HTTP.Headers
import Control.Exception
import Data.Typeable
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

-------------------------------------------------------------------------------

data HttpException = HttpException deriving (Show, Typeable)

instance Exception HttpException

downloadURL :: String -> IO String
downloadURL url
    | "http" `isPrefixOf`url && not ("https" `isPrefixOf` url) = do
       resp <- simpleHTTP request
       case resp of
         Left x -> 
            putStrLn ("Error in downloadUrl:" ++ show x) >> throwIO HttpException
         Right r -> 
             case rspCode r of
               (2,_,_) -> return (rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return (show r)
                   Just url -> downloadURL url
               _ -> return (show r)
    | otherwise = throwIO HttpException
        where 
        request = Request {rqURI = uri,
                           rqMethod = GET,
                           rqHeaders = [],
                           rqBody = ""}
        uri = fromJust $ parseURI url
