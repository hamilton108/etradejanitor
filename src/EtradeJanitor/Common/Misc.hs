{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Common.Misc where
    
import Data.List (intercalate)
import Data.List.Split (splitOn)

feedRoot :: String
feedRoot = "/home/rcs/opt/haskell/etradejanitor"

decimalStrToFloat :: String -> Float
decimalStrToFloat s =
    let 
        txtSplit = splitOn "," s
        txtNum = intercalate "." txtSplit
    in 
    read txtNum