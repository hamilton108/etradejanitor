{-# LANGUAGE OverloadedStrings #-}

module Demo.Demo1 where

import Control.Monad.IO.Class (liftIO)

import Data.Maybe (fromJust)
import Data.Text as Text
-- import Network.HTTP.Req ((/:),(=:))
-- import qualified Network.HTTP.Req as Req
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as Char8

 {-
main :: IO ()
main = runReq defaultHttpConfig $ do
    -- This is an example of what to do when URL is given dynamically. Of
    -- course in a real application you may not want to use 'fromJust'.
    let (url, options) = fromJust (parseUrlHttps "https://httpbin.org/get?foo=bar")
    response <- req GET url NoReqBody jsonResponse $
        "from" =: (15 :: Int)           <>
        "to"   =: (67 :: Int)           <>
        basicAuth "username" "password" <>
        options                         <> -- contains the ?foo=bar part
        port 443 -- here you can put any port of course
    liftIO $ putStrLn "YES" -- print (responseBody response :: Value)

yax = 
    let 
        myurl = Req.https  "www.abc.com" 
        prm = Req.queryParam "foo" (Just 3 :: Maybe Int)
    in 
    9
-}

main :: IO ()
main = runReq defaultHttpConfig $ do
    let n, seed :: Int
        n    = 5
        seed = 100
    bs <- req GET (https "httpbin.org" /: "bytes" /~ n) NoReqBody bsResponse $
        "seed" =: seed
    liftIO $ Char8.putStrLn "Hey ho" -- (responseBody bs)