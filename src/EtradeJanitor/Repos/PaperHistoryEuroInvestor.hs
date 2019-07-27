{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistoryEuroInvestor where

import Text.Printf (printf)
import System.IO (openFile,hSetEncoding,hGetContents,latin1,IOMode(..))
import Data.List.Split (splitOn)
--import Text.Read (readMaybe)
import qualified Data.Int as DI
import Data.Maybe (fromJust)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.Time.Calendar as Cal
import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup ((~==))

import qualified EtradeJanitor.Params as PA
import qualified EtradeJanitor.Common.Types as T
--import qualified EtradeJanitor.Common.Types as T


type StringSoup = [TS.Tag String]

fetchHtml :: T.Ticker -> T.REIO String
fetchHtml (T.Ticker _ s _ _) =
    ask >>= \env ->
    let
        feed = (PA.feed . T.getParams) env
        tickerHtml :: String
        tickerHtml = printf "%s/%s.html" feed s
    in
    liftIO $ openFile tickerHtml ReadMode >>= \inputHandle ->
    hSetEncoding inputHandle latin1 >> -- utf8
    hGetContents inputHandle >>= \theInput ->
    return theInput

soup :: T.Ticker -> T.REIO StringSoup
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





createStockPrice :: T.Ticker -> StringSoup -> Maybe T.StockPrice
createStockPrice tik stockSoup =
    let 
        day = (asDay . head . drop 2) stockSoup
    in 
    case ((T.date tik) >= day) of
        True -> 
            Nothing
        False -> 
            let 
                opn = (readFloat . head . drop 5) stockSoup
                hi = (readFloat . head . drop 8) stockSoup
                lo = (readFloat . head . drop 11) stockSoup
                cls = (readFloat . head . drop 14) stockSoup
                vol = (readVol . head . drop 17) stockSoup
            in 
            Just $ T.StockPrice tik day opn hi lo cls vol

{-
startDataLines :: StringSoup -> StringSoup 
startDataLines curSoup = 
    let 
        dropSoup = dropWhile (~/= dataLineTag) curSoup
        dataLines = (take 28 . drop 56) dropSoup
    in 
    dataLines
-}

fetchStockPrices :: T.Ticker -> T.REIO [T.StockPrice]
fetchStockPrices tik = 
    soup tik >>= \curSoup ->
    let
        stockSoups = collect curSoup
        stox = map (createStockPrice tik) stockSoups
        result = map (\y -> fromJust y) $ filter (\x -> x /= Nothing) stox
    in
    pure $ reverse result
