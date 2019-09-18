{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.StockExchange.OptionExpiry where 
    
import qualified Control.Monad.Reader as Reader
import Control.Monad.IO.Class (liftIO)
import System.IO (openFile,hSetEncoding,hGetContents,latin1,IOMode(..))

import qualified Data.Maybe as Maybe
import qualified Data.List.Split as Split
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as POSIX

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

expiryTimes :: Calendar.Day -> REIO [NordnetExpiry] 
expiryTimes curDay = 
    expiryFileName >>= \fname ->
    liftIO $ readExpiryFile fname >>= \lx -> 
    pure (parseStringDates curDay lx)