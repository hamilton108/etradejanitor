{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Common.Misc where

import           Data.List                      ( intercalate )
import           Data.List.Split                ( splitOn )

feedRoot :: String
feedRoot = "/home/rcs/opt/haskell/etradejanitor"

decimalStrToAscii :: String -> String
decimalStrToAscii s = let txtSplit = splitOn "," s in intercalate "." txtSplit

decimalStrToFloat :: String -> Float
decimalStrToFloat = read . decimalStrToAscii
{-
    let 
        txtSplit = splitOn "," s
        txtNum = intercalate "." txtSplit
    in 
    read txtNum
-}

