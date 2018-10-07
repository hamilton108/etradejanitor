
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS -Wno-unused-imports #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import qualified Network.HTTP.Req as R 
import Network.HTTP.Req ((=:), (/:))
import Data.Text (Text,pack)
import Text.Printf (printf)
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as B

import qualified Data.List as L
-- import qualified System.IO.Encoding as IO
import Control.Monad (forM_)
import Data.List.Split (splitOn)


import System.IO

main2 = do
    -- text <- getLine
    inputHandle <- openFile "NHY.csv" ReadMode 
    hSetEncoding inputHandle latin1 -- utf8
    theInput <- hGetContents inputHandle
    let lx = L.lines theInput
    forM_ lx $ \s -> do
        let lxx = splitOn "," s
        putStrLn (head lxx) 
    putStrLn "OK"


{-|
    downloadPaperHistory returns a response BsResponse from the url like (f.ex NHY):
    
        http://www.netfonds.no/quotes/paperhistory.php?paper=NHY.OSE&csv_format=csv
-}
downloadPaperHistory :: String -> R.Req R.BsResponse
downloadPaperHistory ticker = 
    let 
        tickerParam = printf "%s.OSE" ticker 
        params = "paper" =: (pack tickerParam) <> "csv_format" =: ("csv" :: Text)
    in
    R.req R.GET (R.http "netfonds.no" /: "quotes" /: "paperhistory.php") R.NoReqBody R.bsResponse params 

{-|
    savePaperHistory gets a http response from downloadPaperHistory 
    and writes it to a csv file (f.ex NHY):

        NHY.csv
-}
savePaperHistory :: String -> IO ()
savePaperHistory ticker = 
    R.runReq def $
    downloadPaperHistory ticker >>= \bs -> 
    liftIO $ B.writeFile (printf "%s.csv" ticker) (R.responseBody bs)

main :: IO ()
main =  
    savePaperHistory "NHY"