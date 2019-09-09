{-# LANGUAGE OverloadedStrings #-}

module Demo.Demo1 where

import Data.Text as Text
import Network.HTTP.Req ((/:),(=:))
import qualified Network.HTTP.Req as Req
import qualified Data.ByteString.Char8 as Char8

p :: Text.Text -> Char8.ByteString -> Req.QueryParam
p = "a" =: "b" 