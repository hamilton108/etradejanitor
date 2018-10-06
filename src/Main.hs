
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS -Wno-unused-imports #-}

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
import qualified Network.HTTP.Req as R 
import Network.HTTP.Req ((=:), (/:))
import Data.Text (Text,pack)
import Data.Aeson (Value)
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

-- newtype MyUrl = MyUrl Text
-- http://www.netfonds.no/quotes/paperhistory.php?paper=NHY.OSE&csv_format=csv

-- nhy = R.http "www.netfonds.no" /: "quotes" /: "paperhistory.php" /: "paper=NHY.OSE" /: "csv_format=csv"
nhy = R.http "www.netfonds.no" 

-- params = "paper" =: ("NHY.OSE" :: Text)

-- param = R.queryParam "paper" Nothing :: R.QueryParam

main :: IO ()
main =  
    R.runReq def $
    rx >>= \bs -> 
    liftIO $ B.writeFile "netfonds.html" (R.responseBody bs)


rx :: R.Req R.BsResponse
rx = 
    R.req R.GET nhy R.NoReqBody R.bsResponse mempty --  >>= \bs ->
    -- R.req R.GET (R.https "netfonds.no") R.NoReqBody R.bsResponse mempty --  >>= \bs ->

main2 :: IO ()
main2 = R.runReq def $ do
    let params =
            "paper" =: ("NHY.OSE" :: Text) <> "csv_format" =: ("csv" :: Text)
    response <- R.req R.GET (R.http "netfonds.no" /: "quotes" /: "paperhistory.php") R.NoReqBody R.bsResponse params 
    -- response <- R.req R.GET (R.http "netfonds.no") (R.ReqBodyUrlEnc params) R.bsResponse mempty
    liftIO $ B.putStrLn (R.responseBody response)
    -- liftIO $ print (R.responseBody response :: Value)