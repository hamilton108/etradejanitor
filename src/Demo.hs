{-# LANGUAGE OverloadedStrings #-}

module Demo where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Text as T
import Text.Printf (printf)
import Control.Monad.IO.Class (liftIO)

import Data.Either (fromRight)

import Database.Redis

ci = defaultConnectInfo { connectHost = "172.20.1.2", connectDatabase = 5 }

c = checkedConnect ci

expiryKey :: String -> B.ByteString
expiryKey ticker = 
    BU.fromString $ printf "expiry-%s" (ticker :: String)

exp1 :: Redis (Either Reply [(B.ByteString, B.ByteString)])
exp1 = 
    hgetall (BU.fromString "expiry-1")

exp2 :: Redis (Either Reply [(B.ByteString, B.ByteString)])
exp2 = 
    hgetall (BU.fromString "expiry-2")

-- x :: String -> Connection -> IO (Maybe (Either Reply [(B.ByteString, B.ByteString)]))
x ticker conn = 
    runRedis conn $
        hget "expiry" (BU.fromString ticker) >>= \ex ->
        case ex of 
            Left err ->
                pure [] 
            Right result -> 
                case result of 
                    Nothing ->
                        pure []
                    Just result1 ->
                        case result1 of
                            "1" ->  
                                liftIO (putStrLn "1") >>
                                exp1 >>= \z -> 
                                    pure [] -- $ Just z
                            "2" -> 
                                liftIO (putStrLn "2") >>
                                exp1 >>= \z -> 
                                    exp2 >>= \z2-> 
                                        pure [] -- $ Just ((++) <$> z <*> z2)

--x2 :: String -> Connection -> IO (Maybe (Either Reply [(B.ByteString, B.ByteString)]))
x2 ticker conn = 
    runRedis conn $
        hget "expiry" (BU.fromString ticker) >>= \ex ->
            pure ex
{-
        case ex of 
            Left err ->
                pure [] 
            Right result -> 
                pure [] 
                case result of 
                    Nothing ->
                        pure []
                    Just result1 ->
                        case result1 of
                            "1" ->  
                                liftIO (putStrLn "1") >>
                                exp1 >>= \z -> 
                                    pure [] -- $ Just z
                            "2" -> 
                                liftIO (putStrLn "2") >>
                                exp1 >>= \z -> 
                                    exp2 >>= \z2-> 
                                        pure [] -- $ Just ((++) <$> z <*> z2)

-}



xx = 
    c >>= \c1 ->
        runRedis c1 $
            hget "expiry" (BU.fromString "NHY") >>= \ex ->
                let 
                    ex1 = fromRight Nothing ex
                in
                case ex1 of 
                    Nothing -> pure []
                    Just ex1 ->
                        --liftIO (putStrLn (BU.toString "ex1")) >>
                        case ex1 of
                            "1" ->  
                                exp1 >>= \z -> 
                                    pure [] -- $ Just z
                            "2" -> 
                                exp1 >>= \z -> 
                                    exp2 >>= \z2-> 
                                        pure []

{-
        x "NHYx" c1 -- >>= \result -> 
            case result of 
                Nothing -> putStrLn "OOP"
                Just r1 -> 
                    case r1 of 
                        Left err -> putStrLn "JADDA ERR"
                        Right _ -> putStrLn "JADDA OK"

import Data.ByteString.Lazy as BL
import Data.ByteString as BS
import Data.Text as TS
import Data.Text.Lazy as TL
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import Data.ByteString.UTF8 as BSU      -- from utf8-string
import Data.Text.Encoding as TSE
import Data.Text.Lazy.Encoding as TLE

-- String <-> ByteString

BLU.toString   :: BL.ByteString -> String
BLU.fromString :: String -> BL.ByteString
BSU.toString   :: BS.ByteString -> String
BSU.fromString :: String -> BS.ByteString

-- String <-> Text

TL.unpack :: TL.Text -> String
TL.pack   :: String -> TL.Text
TS.unpack :: TS.Text -> String
TS.pack   :: String -> TS.Text

-- ByteString <-> Text

TLE.encodeUtf8 :: TL.Text -> BL.ByteString
TLE.decodeUtf8 :: BL.ByteString -> TL.Text
TSE.encodeUtf8 :: TS.Text -> BS.ByteString
TSE.decodeUtf8 :: BS.ByteString -> TS.Text

-- Lazy <-> Strict

BL.fromStrict :: BS.ByteString -> BL.ByteString
BL.toStrict   :: BL.ByteString -> BS.ByteString
TL.fromStrict :: TS.Text -> TL.Text
TL.toStrict   :: TL.Text -> TS.Text


import Data.List.Split (splitOn)

import qualified EtradeJanitor.Repos.Stocks as RS

x = "a:b:c:x"

lx = splitOn ":" x

y = case lx of
    [v1,v2,v3] -> "Yep"
    _ -> "Nope"

tix = RS.tickers "172.20.1.3"
-}