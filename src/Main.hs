
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
import Control.Monad (forM,forM_)
import Data.List.Split (splitOn)
import EtradeJanitor.Netfonds as NP
import EtradeJanitor.Common.Types (Ticker(..))

import System.IO (openFile,hSetEncoding,hGetContents,latin1,IOMode(..))

asDateString :: String -> String
asDateString v =
  let
    year :: String
    year = take 4 v

    month :: String
    month = take 2 $ drop 4 v

    day :: String
    day = take 2 $ drop 6 v
  in
    printf "%s-%s-%s" year month day

asSql :: StockPrice -> String -- B.ByteString
asSql sp =
    -- B.pack $
    printf
      "insert into stockmarket.stockprice (ticker_id,dx,opn,hi,lo,cls,vol) values (3,'%s',%s,%s,%s,%s,%s)"
      (dx sp)
      (opn sp)
      (hi sp)
      (lo sp)
      (cls sp)
      (vol sp)



processLine :: String -> IO StockPrice
processLine line =
        let
          [dx',_,_,opn',hi',lo',cls',vol',_] = splitOn "," line
          dxx = asDateString dx'
        in
          -- forM_ lxx putStrLn >>
          return (StockPrice dxx opn' hi' lo' cls' vol')


main2 =
    -- text <- getLine
    openFile "NHY.csv" ReadMode >>= \inputHandle ->
    hSetEncoding inputHandle latin1 >> -- utf8
    hGetContents inputHandle >>= \theInput ->
    let
      lx = L.lines theInput
    in
      forM lx processLine >>= \stockPrices ->
      --forM_ stockPrices (putStrLn . asSql) >>
      putStrLn "Done!"

main :: IO ()
main =
    NP.savePaperHistory $ Ticker "NHY"
