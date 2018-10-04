
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-top-binds #-}

module Main (main) where

{- import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
 -}

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Network.HTTP.Req as R 
import qualified Data.ByteString.Char8 as B

{- xmain :: IO ()
xmain = 
    R.runReq def 
    $ do
        let n :: Int
            n = 5
        bs <- R.req R.GET (R.https "netfonds.no") R.NoReqBody R.bsResponse mempty
        liftIO $ B.putStrLn (R.responseBody bs)
 -}
 
_main :: IO ()
_main = 
    R.runReq def $ 
    R.req R.GET (R.https "netfonds.no") R.NoReqBody R.bsResponse mempty >>= \bs ->
    liftIO $ B.putStrLn (R.responseBody bs)

main :: IO ()
main =  
    R.runReq def $
    rx >>= \bs -> 
    liftIO $ B.writeFile "netfonds.html" (R.responseBody bs)

rx :: R.Req R.BsResponse
rx = 
    R.req R.GET (R.https "netfonds.no") R.NoReqBody R.bsResponse mempty --  >>= \bs ->