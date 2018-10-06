
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

{-|
    downLoadPaperHistory returns a response BsResponse from the url like (f.ex NHY):
    
        http://www.netfonds.no/quotes/paperhistory.php?paper=NHY.OSE&csv_format=csv

    and writes it to a csv file (f.ex NHY):

        NHY.csv
-}
downLoadPaperHistory :: String -> R.Req R.BsResponse
downLoadPaperHistory ticker = 
    let 
        tickerParam = printf "%s.OSE" ticker 
        params = "paper" =: (pack tickerParam) <> "csv_format" =: ("csv" :: Text)
    in
    R.req R.GET (R.http "netfonds.no" /: "quotes" /: "paperhistory.php") R.NoReqBody R.bsResponse params 

savePaperHistory :: String -> IO ()
savePaperHistory ticker = 
    R.runReq def $
    downLoadPaperHistory ticker >>= \bs -> 
    liftIO $ B.writeFile (printf "%s.csv" ticker) (R.responseBody bs)

main :: IO ()
main =  
    savePaperHistory "NHY"