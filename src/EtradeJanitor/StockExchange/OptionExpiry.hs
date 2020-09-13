{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.StockExchange.OptionExpiry where 
    
import qualified Control.Monad.Reader as Reader
import Control.Monad.IO.Class (liftIO)
import System.IO (openFile,hSetEncoding,hGetContents,latin1,IOMode(..))

import Data.Either (fromRight)
import qualified Data.Maybe as Maybe
import qualified Data.List.Split as Split
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as POSIX

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Database.Redis as Redis

import EtradeJanitor.Common.Types (REIO,NordnetExpiry,getParams)
import qualified EtradeJanitor.Params as Params
import qualified EtradeJanitor.Common.CalendarUtil as CalendarUtil


{-
parseStringDate :: Calendar.Day -> String -> Maybe NordnetExpiry  
parseStringDate curDay sd = 
    let
        -- [datePart,timePart] = Split.splitOn ":" sd
        -- [ys,ms,ds] = Split.splitOn "-" datePart -- sd
        [ys,ms,ds] = Split.splitOn "-" sd
        year = read ys :: Integer
        month = read ms :: Int
        day = read ds :: Int
        expDate = Calendar.fromGregorian year month day
    in
    if expDate < curDay then
        Nothing
    else
        -- Just $ CalendarUtil.strToUnixTime timePart -- CalendarUtil.dayToUnixTime expDate 
        Just $ CalendarUtil.dayToUnixTime expDate 
-}

parseStringDate :: Calendar.Day -> String -> Maybe NordnetExpiry  
parseStringDate curDay sd = 
    let
        [datePart,timePart] = Split.splitOn ":" sd
        [ys,ms,ds] = Split.splitOn "-" datePart 
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
            result = read timePart
        in
        Just result
    


parseStringDates :: Calendar.Day -> [String] -> [NordnetExpiry]
parseStringDates curDay lx = 
    let
        parseFn = parseStringDate curDay 
        result = map parseFn lx
    in
    map (\y -> Maybe.fromJust y) $ filter (\x -> x /= Nothing) result
    
expiryFileName :: REIO String
expiryFileName =
    Reader.ask >>= \env ->
    let
        feed = (Params.feed . getParams) env
        fpath :: String
        fpath = feed ++ "/expiry_dates"
    in
    pure fpath

readExpiryFile :: String -> IO [String]
readExpiryFile fname =  
    openFile fname ReadMode >>= \inputHandle ->
    hSetEncoding inputHandle latin1 >> -- utf8
    hGetContents inputHandle >>= \theInput ->
    pure $ lines theInput

{-
expiryTimes :: Calendar.Day -> REIO [NordnetExpiry] 
expiryTimes curDay = |
    expiryFileName >>= \fname ->
    liftIO $ readExpiryFile fname >>= \lx -> 
    pure (parseStringDates curDay lx)
-}

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

fetchExpiryFromRedis :: String -> String -> IO [a]
fetchExpiryFromRedis host ticker = 
    conn host >>= \c1 ->
        Redis.runRedis c1 $
            Redis.hget "expiry" (BU.fromString "NHY") >>= \ex ->
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

expiryTimes :: Calendar.Day -> REIO [NordnetExpiry] 
expiryTimes curDay = 
    Reader.ask >>= \env ->
    let
        redisHost = (Params.redisHost . getParams) env
    in
    expiryFileName >>= \fname ->
    liftIO $ readExpiryFile fname >>= \lx -> 
    pure (parseStringDates curDay lx)