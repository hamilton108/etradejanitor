{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistoryEuroInvestor where

import Text.Printf (printf)
import System.IO (openFile,hSetEncoding,hGetContents,latin1,IOMode(..))
import Data.List.Split (splitOn,chunksOf)
import Text.Read (readMaybe)
import qualified Data.Int as DI

import qualified Data.Time.Calendar as Cal
import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup ((~/=),(~==),sections)

import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Common.Types as T


type StringSoup = [TS.Tag String]

eiDateFormat :: Cal.Day -> String
eiDateFormat = concat . splitOn "/" . Cal.showGregorian

fetchHtml :: T.Ticker -> IO String
fetchHtml (T.Ticker _ s _ dx) =
    let
        tickerHtml :: String
        tickerHtml = printf "%s/%s.html" T.feed s

        netfondsDx :: String
        netfondsDx = eiDateFormat dx
    in
    openFile tickerHtml ReadMode >>= \inputHandle ->
    hSetEncoding inputHandle latin1 >> -- utf8
    hGetContents inputHandle >>= \theInput ->
    return theInput

soup :: T.Ticker -> IO StringSoup
soup t =
    fetchHtml t >>= pure . TS.parseTags


dataLineTag :: TS.Tag String
dataLineTag = TS.TagOpen "div" [("class", "data_line")]

collect_ :: StringSoup -> [StringSoup] -> [StringSoup]
collect_ [] result = result
collect_ curSoup@(x:xs) result = 
    case x ~== dataLineTag of
        True -> collect_ xs (take 28 curSoup : result)
        False ->  collect_ xs result

collect :: StringSoup -> [StringSoup]
collect curSoup = 
    collect_ curSoup []

asDay :: TS.Tag String -> Cal.Day
asDay tag = 
  let 
    [ds,ms,ys] = splitOn "/" $ TS.fromTagText tag 
    year = read ys :: Integer
    month = read ms :: Int
    day = read ds :: Int
  in 
  Cal.fromGregorian year month day

--readTag :: Read a => TS.Tag String -> a
--readTag x = read (TS.fromTagText x) 

readFloat :: TS.Tag String -> Float
readFloat x = read (TS.fromTagText x) :: Float 

{-
readInt64 :: TS.Tag String -> DI.Int64
readInt64 x = read (TS.fromTagText x) :: DI.Int64
-}

readVol :: TS.Tag String -> DI.Int64
readVol x = 
    let 
        tagStr = TS.fromTagText x
    in
    read (concat $ splitOn "," tagStr) :: DI.Int64





createStockPrice :: T.Ticker -> StringSoup -> T.StockPrice
createStockPrice tik stockSoup =
    let 
        day = (asDay . head . drop 2) stockSoup
        opn = (readFloat . head . drop 5) stockSoup
        hi = (readFloat . head . drop 8) stockSoup
        lo = (readFloat . head . drop 11) stockSoup
        cls = (readFloat . head . drop 14) stockSoup
        vol = (readVol . head . drop 17) stockSoup
    in 
    T.StockPrice tik day opn hi lo cls vol

{-
startDataLines :: StringSoup -> StringSoup 
startDataLines curSoup = 
    let 
        dropSoup = dropWhile (~/= dataLineTag) curSoup
        dataLines = (take 28 . drop 56) dropSoup
    in 
    dataLines
-}

fetchStockPrices :: T.Ticker -> IO [T.StockPrice]
fetchStockPrices tik = 
    soup tik >>= \curSoup ->
    let
        stockSoups = collect curSoup
        stox = map (createStockPrice tik) stockSoups
    in
    pure $ reverse stox
