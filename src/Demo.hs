{-# LANGUAGE OverloadedStrings #-}

module Demo where

--import           Control.Monad.State            ( runState )
import           Control.Monad.Reader           ( runReaderT )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Time.Calendar            as Calendar
import qualified Data.Vector                   as Vector


import qualified EtradeJanitor.Repos.Nordnet.RedisRepos
                                               as RedisRepos
import qualified EtradeJanitor.Params          as Params
import           EtradeJanitor.Common.Types     ( Env(..)
                                                , Ticker(..)
                                                , Tickers
                                                , REIO
                                                )
import qualified EtradeJanitor.Common.Types    as T
import qualified EtradeJanitor.Repos.Nordnet   as Nordnet
--import qualified EtradeJanitor.Repos.Nordnet (Prices(..)) 

import qualified EtradeJanitor.Repos.Nordnet.RedisRepos
                                               as RedisRepos -- (expiryTimes,saveOpeningPricesToRedis) where 

prms = Params.Params
  { Params.databaseIp               = "172.20.1.3"
  , Params.redisHost                = "172.20.1.2"
  , Params.redisDatabase            = "5"
  , Params.feed = "/home/rcs/opt/haskell/etradejanitor/feedtmp"
  , Params.downloadDerivatives      = False
  , Params.dbUpdateStocks           = False
  , Params.skipIfDownloadFileExists = False
  , Params.showStockTickers         = False
  , Params.openingPricesToRedis     = True
  }

dx1 :: Calendar.Day
dx1 = Calendar.fromGregorian 2020 10 26

env = Env prms dx1

nhy :: Ticker
nhy = Ticker { T.oid = 1, T.ticker = "NHY", T.category = 1, T.date = dx1 }

sdrl :: Ticker
sdrl = Ticker { T.oid = 4, T.ticker = "SDRL", T.category = 1, T.date = dx1 }

tix :: Tickers
tix = Vector.fromList [sdrl]

work2 :: IO ()
work2 = runReaderT (T.runApp $ Nordnet.downloadOpeningPrices tix) env

data State s a = State { runState :: s -> (s, a) }

--fmap :: (a -> b) -> State s a -> State s b

instance Functor (State s) where
  fmap f (State stateFn) = State (\s ->
    let (s', result) = stateFn s
    in (s', f result))

instance Applicative (State s) where
  pure x = State (\s -> (s, x))
  (<*>) (State stateFx) (State stateX) = State (\s ->
    let (s', fx) = stateFx s
        (s'', x) = stateX s'
    in (s'', fx x))

instance Monad (State s) where
  return = pure
  (>>=) (State stateX) nextFn = State (\s ->
    let (s', x) = stateX s
    in runState (nextFn x) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> (s, ()))

modify :: (s -> s) -> State s ()
modify f = get >>= (\s -> put (f s))

reverseWithCount :: [a] -> State Int [a]
reverseWithCount list = 
  modify (+1) >>
  pure (reverse list)


append3ReversedWithCount :: [a] -> [a] -> [a] -> State Int [a]
append3ReversedWithCount list1 list2 list3 = 
  reverseWithCount list1 >>= \revList1 ->
  reverseWithCount list2 >>= \revList2 ->
  reverseWithCount list3 >>= \revList3 ->
  modify (+1) >>
  pure (revList1 ++ revList2 ++ revList3)

runState1 :: State Int Int
runState1 =
    get >>= \x ->
    put (10 + x) >> pure 123

sx :: State Int Int
sx = State (\s -> (s,3))

sx2 :: State Int (Int -> Int)
sx2 = 
    let fx v = 2 * v
    in
    State (\s -> (s,fx))

demo1 = 
    runState (append3ReversedWithCount [1..5] [6..10] [11..15]) 0

demo2 = 
    runState runState1 100
{-
demos = 
    let 
        f v = v * 100
        sx2 = fmap f sx
        sx3 = fmap f sx2
    in
    runState sx 34
    

work :: IO ()
work = 
    runReaderT (RedisRepos.expiryTimes nhy) env >>= \expiryTimes ->
        mapM_ (\x -> putStrLn $ show x) expiryTimes
        

work2 :: IO ()
work2 = 
    runReaderT (Nordnet.downloadOpeningPrices tix) env >>= \t ->
    runReaderT (Nordnet.downloadDerivativePrices tix) env 


ioInt :: IO Int
ioInt = pure 12

reioInt :: REIO Int
reioInt =
    (liftIO $ ioInt) >>= \t ->
        pure t

work3 = 
    runReaderT (reioInt) env

work4 = 
    runReaderT (Nordnet.openingPrice nhy) env >>= \p ->
    runReaderT (RedisRepos.saveOpeningPricesToRedis [p]) env

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


