
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req as R -- (runReq,GET(GET),req,https,NoReqBody,bsResponse,responseBody)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = 
    R.runReq def 
    $ do
        let n :: Int
            n = 5
        bs <- R.req R.GET (R.https "netfonds.no") R.NoReqBody R.bsResponse mempty
        -- bs <- R.req R.GET (R.https "httpbin.org" /: "bytes" /~ n) R.NoReqBody R.bsResponse mempty
        liftIO $ B.putStrLn (R.responseBody bs)
{-
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.UTF8 as UTF

import qualified Network.Http.Client as C
import qualified Text.HTML.TagSoup as TS

responseHandler :: C.Response -> Streams.InputStream S.ByteString -> IO ()
responseHandler res s = 
    Streams.read s >>= \sx ->
        case sx of 
            Just result -> S.putStr result
            Nothing ->  return ()
                

doStuff :: C.Connection -> IO () -- IO S.ByteString
doStuff conn = C.receiveResponse conn responseHandler 
-- doStuff c = return (UTF.fromString "sdfsdåøåø" )

foo :: IO () -- IO S.ByteString
foo = C.withConnection (C.openConnection "www.netfonds.no" 443) doStuff

xmain :: IO ()
xmain = 
    putStrLn "yep"


ymain :: IO ()
ymain = do
    c <- C.openConnection "www.example.com" 80

    let q = C.buildRequest1 $ do
                C.http C.GET "/"
                C.setAccept "text/html"

    C.sendRequest c q C.emptyBody

    C.receiveResponse c (\p i -> do
        xm <- Streams.read i
        case xm of
            Just x    -> S.putStr x
            Nothing   -> return ())

    C.closeConnection c

builder :: C.RequestBuilder ()  
builder = 
    C.http C.GET "/" >>
    C.setAccept "text/html"

responseHandler2 :: C.Response -> Streams.InputStream S.ByteString -> IO ()
responseHandler2 p i = 
    Streams.read i >>= \xm -> 
        case xm of
            Just x    -> S.putStr x
            Nothing   -> return ()

main :: IO ()
main = 
    C.openConnection "www.netfonds.no" 443 >>= \c ->
    let q = C.buildRequest1 builder in 
        C.sendRequest c q C.emptyBody >>
        C.receiveResponse c responseHandler2 >> 
        C.closeConnection c 

-}
