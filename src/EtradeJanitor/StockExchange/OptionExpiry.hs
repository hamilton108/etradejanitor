{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module EtradeJanitor.StockExchange.OptionExpiry (expiryTimes) where 
    
import qualified Control.Monad.Reader as Reader
import Control.Monad.IO.Class (liftIO)
import System.IO (openFile,hSetEncoding,hGetContents,latin1,IOMode(..))

import Data.Either (fromRight)
import qualified Data.Maybe as Maybe
import qualified Data.List.Split as Split
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Text.Encoding as TE

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Database.Redis as Redis

import EtradeJanitor.Common.Types (REIO,NordnetExpiry,getParams,Ticker(..))
import qualified EtradeJanitor.Params as Params
import qualified EtradeJanitor.Common.CalendarUtil as CalendarUtil

ci ::String -> Redis.ConnectInfo 
ci host = 
    Redis.defaultConnectInfo { Redis.connectHost = host, Redis.connectDatabase = 5 }

conn :: String -> IO Redis.Connection
conn host = 
    Redis.checkedConnect $ ci host

exp1 :: Redis.Redis (Either Redis.Reply [(B.ByteString, B.ByteString)])
exp1 = 
    Redis.hgetall (BU.fromString "expiry-1")

exp2 :: Redis.Redis (Either Redis.Reply [(B.ByteString, B.ByteString)])
exp2 = 
    Redis.hgetall (BU.fromString "expiry-2")

fetchExpiryFromRedis :: String -> Ticker -> IO [(B.ByteString,B.ByteString)]
fetchExpiryFromRedis host (Ticker { ticker }) = 
    conn host >>= \c1 ->
        Redis.runRedis c1 $
            Redis.hget "expiry" (TE.encodeUtf8 ticker) >>= \ex ->
                let 
                    ex1 = fromRight Nothing ex
                in
                case ex1 of 
                    Nothing -> 
                        pure []
                    Just ex1 ->
                        case ex1 of
                            "1" ->  
                                exp1 >>= \z -> 
                                    pure (fromRight [] z) 
                            "2" -> 
                                exp1 >>= \z -> 
                                    exp2 >>= \z2-> 
                                        let
                                            merged = (++) <$> z <*> z2
                                        in
                                        pure (fromRight [] merged)
                            _ -> 
                                pure []

parseRedisItem :: Calendar.Day -> (B.ByteString,B.ByteString) -> Maybe NordnetExpiry  
parseRedisItem curDay (datePart,timePart) = 
    let
        [ys,ms,ds] = Split.splitOn "-" (BU.toString datePart)
        year = read ys :: Integer
        month = read ms :: Int
        day = read ds :: Int
        expDate = Calendar.fromGregorian year month day
    in
    if expDate < curDay then
        Nothing
    else
        let 
            result :: Int
            result = read (BU.toString timePart)
        in
        Just result

expiryTimes :: Ticker -> Calendar.Day -> REIO [NordnetExpiry] 
expiryTimes ticker curDay = 
    Reader.ask >>= \env ->
        let
            redisHost = (Params.redisHost . getParams) env
            parseFn = parseRedisItem curDay 
        in
        liftIO (fetchExpiryFromRedis redisHost ticker) >>= \items ->
            let 
                result = map parseFn items  
            in
            pure $ map (\y -> Maybe.fromJust y) $ filter (\x -> x /= Nothing) result
