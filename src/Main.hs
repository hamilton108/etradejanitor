
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
import Control.Monad (forM_)
import Data.List.Split (splitOn)
import EtradeJanitor.Netfonds as NP


import System.IO

processLine :: String -> IO ()
processLine line =
        let lxx = splitOn "," line
        in
        putStrLn (head lxx)

main3 =
    -- text <- getLine
    openFile "NHY.csv" ReadMode >>= \inputHandle ->
    hSetEncoding inputHandle latin1 >> -- utf8
    hGetContents inputHandle >>= \theInput ->
    let lx = L.lines theInput
    in
    forM_ lx processLine >>
    putStrLn "OK"

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


main :: IO ()
main =
    NP.savePaperHistory "NHY"
