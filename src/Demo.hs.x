{-# LANGUAGE OverloadedStrings #-}

module Demo where

import Control.Monad.Reader (runReaderT)
import qualified Data.Time.Calendar as Calendar
import qualified Data.Vector as Vector

import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup (Tag(..),(~==),(~/=))

import qualified EtradeJanitor.Repos.Nordnet.RedisRepos as RedisRepos 
import qualified EtradeJanitor.Params as Params
import EtradeJanitor.Common.Types (Env(..),Ticker(..),Tickers)
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Repos.Nordnet as Nordnet
--import qualified EtradeJanitor.Repos.Nordnet (Prices(..)) 
import qualified EtradeJanitor.Common.Html as Html
import EtradeJanitor.Common.Html (StringSoup)

prms = Params.Params 
        { Params.databaseIp = "172.20.1.3"
        , Params.redisHost = "172.20.1.2"
        , Params.feed = "/home/rcs/opt/haskell/etradejanitor/feedtmp"
        , Params.downloadDerivatives = False
        , Params.dbUpdateStocks = False
        , Params.skipIfDownloadFileExists = False
        , Params.showStockTickers = False
        , Params.openingPricesToRedis = True
        }

dx1 :: Calendar.Day
dx1 = Calendar.fromGregorian 2020 10 26

env = Env prms dx1

nhy :: Ticker
nhy = Ticker 
        { T.oid = 1
        , T.ticker = "NHY2"
        , T.category = 1
        , T.date = dx1
        }

tix :: Tickers
tix =
    Vector.fromList [nhy] 

work :: IO ()
work = 
    runReaderT (RedisRepos.expiryTimes nhy) env >>= \expiryTimes ->
        mapM_ (\x -> putStrLn $ show x) expiryTimes
        

work2 :: IO ()
work2 = 
    runReaderT (Nordnet.downloadOpeningPrices tix) env >> 
    runReaderT (Nordnet.downloadDerivativePrices tix) env 


work3 :: IO (Tag String)
work3 = 
    runReaderT (Html.close nhy) env 

{-
stock :: IO StringSoup
stock =
    work3 >>= \soup ->
        pure $ dropWhile (~/= ("<table>" :: String)) soup
        -- pure $ TS.sections (~== ("<table>" :: String)) soup

tbody = 
    stock >>= \soup ->
        pure $ dropWhile (~/= ("<tbody>" :: String)) soup
        -- pure $ dropWhile (~/= ("<tbody>" :: String)) (take 1 soup)

trx = 
    tbody >>= \soup ->
        pure $ dropWhile (~/= ("<tr>" :: String)) soup

tr :: IO [Tag String]
tr = 
    work3 >>= \soup ->
        let
            table = dropWhile (~/= ("<table>" :: String)) soup
            tbody = dropWhile (~/= ("<tbody>" :: String)) table
        in
        pure $ dropWhile (~/= ("<tr>" :: String)) tbody

cls = 
    tr >>= \soup ->
        let 
            td = dropWhile (~/= TagOpen ("td" :: String) [("data-title","Siste")]) soup 
        in
        pure $ (head . drop 1) $ dropWhile (~/= TagOpen ("span" :: String) [("aria-hidden","true")]) td

buy = 
    tr >>= \soup ->
        pure $ (head . drop 1) $ dropWhile (~/= TagOpen ("td" :: String) [("data-title","Lav")]) soup 

spjPapers :: IO ()
spjPapers = do
        tags <- TS.parseTags <$> openItem "http://research.microsoft.com/en-us/people/simonpj/"
        let links = map f $ TS.sections (~== "<A>") $
                    takeWhile (~/= "<a name=haskell>") $
                    drop 5 $ dropWhile (~/= "<a name=current>") tags
        putStr $ unlines links
    where
        f :: [Tag String] -> String
        f = dequote . unwords . words . TS.fromTagText . head . filter TS.isTagText

        dequote ('\"':xs) | last xs == '\"' = init xs
        dequote x = x
-}


