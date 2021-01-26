{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Common.Misc
  ( feedRoot
  , decimalStrToAscii
  , decimalStrToFloat
  , showHttpException
  )
where

import           Data.List                      ( intercalate )
import           Data.List.Split                ( splitOn )
import           Data.Text                      ( Text )
import qualified Data.Text                     as TS
import qualified Network.HTTP.Client           as Client
import qualified Network.HTTP.Req              as Req
import qualified Text.Printf                   as Printf

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
showHttpException'' :: Client.HttpExceptionContent -> Text
showHttpException'' Client.ConnectionTimeout      = "ConnectionTimeout"
showHttpException'' (Client.ConnectionFailure ex) = TS.pack $ show ex
showHttpException'' x                             = TS.pack $ show x -- "HttpExceptionContent"

showHttpException' :: Client.HttpException -> Text
showHttpException' (Client.HttpExceptionRequest _ content) =
  showHttpException'' content
showHttpException' (Client.InvalidUrlException url reason) =
  TS.pack $ Printf.printf "%s - %s" url reason

showHttpException :: Req.HttpException -> Text
showHttpException (Req.VanillaHttpException httpEx) = showHttpException' httpEx
showHttpException (Req.JsonHttpException    json  ) = TS.pack json
