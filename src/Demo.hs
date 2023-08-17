{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Demo where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (execStateT)
import qualified Data.Time.Calendar as Calendar
import Data.UUID
  ( UUID
  , nil
  )
import qualified Data.Vector as Vector
import qualified EtradeJanitor.Common.Misc as Misc
import EtradeJanitor.Common.Types
  ( Env (..)
  , NordnetExpiry
  , OpeningPrice (..)
  , Ticker (..)
  , Tickers
  )
import qualified EtradeJanitor.Params as Params
import EtradeJanitor.Repos.Nordnet (Prices (..))
import qualified EtradeJanitor.Repos.Nordnet as Nordnet
import qualified EtradeJanitor.Repos.Nordnet.RedisRepos as RedisRepos

import EtradeJanitor.AMQP.RabbitMQ
  ( Payload (..)
  , RoutingKey
  )
import qualified EtradeJanitor.AMQP.RabbitMQ as Rabbit
import qualified EtradeJanitor.Common.Types as T

import Data.UUID.V4 (nextRandom)
import EtradeJanitor.Repos.Nordnet (openingPrice)
import Network.AMQP (Connection)
import qualified Network.AMQP as AMQP

testDay :: Calendar.Day
testDay =
  let
    year = 2021 :: Integer
    month = 6 :: Int
    day = 18 :: Int
  in
    Calendar.fromGregorian year month day

testParams :: Params.Params
testParams =
  Params.Params
    { Params.databaseIp = "172.20.1.3"
    , Params.redisHost = "172.20.1.2"
    , Params.redisPort = 6379
    , Params.redisDatabase = 5
    , Params.rabbitHost = "172.20.1.4"
    , Params.rabbitPort = 5672
    , Params.nordnetPort = 8082
    , Params.feed = "/home/rcs/opt/haskell/etradejanitor/test/testfeed"
    , Params.downloadDerivatives = True
    , Params.dbUpdateStocks = True
    , Params.skipIfDownloadFileExists = True
    , Params.showStockTickers = False
    , Params.openingPricesToRedis = True
    }

testEnv :: Maybe Connection -> UUID -> Env
testEnv conn uuid = Env testParams testDay conn uuid

eqnr :: Ticker
eqnr = Ticker 2 "EQNR" 1 testDay

nhy :: Ticker
nhy = Ticker 1 "NHY" 1 testDay

demo :: IO ()
demo =
  let
    host = T.getRabbitHost testParams
    port = T.getRabbitPort testParams
  in
    nextRandom >>= \uuid ->
      Rabbit.myConnection' host port >>= \conn ->
        let
          env = testEnv (Just conn) nil
          payload = NordnetPayload uuid 0 "iso8601" "EQNR" "demo" 10 "demo run"
        in
          runReaderT (Rabbit.publish payload Rabbit.rkInfo) env
            >> AMQP.closeConnection conn

-- demo2 :: IO [NordnetExpiry]
demo2 =
  let
    host = T.getRabbitHost testParams
    port = T.getRabbitPort testParams
  in
    nextRandom >>= \uuid ->
      Rabbit.myConnection' host port >>= \conn ->
        let
          env = testEnv (Just conn) uuid
        in
          runReaderT
            -- RedisRepos.expiryTimes2
            -- (Nordnet.openingPricesToRedis [nhy])
            -- (Nordnet.openingPrice nhy)
            -- (Nordnet.tryDownloadOpeningPrice eqnr)
            RedisRepos.expiryTimes2
            env
            >>= \result ->
              -- putStrLn (show result) >>
              AMQP.closeConnection conn
                >> pure result

demo3 =
  let
    host = T.getRabbitHost testParams
    port = T.getRabbitPort testParams
  in
    nextRandom >>= \uuid ->
      Rabbit.myConnection' host port >>= \conn ->
        let
          env = testEnv (Just conn) uuid
          tix = Vector.singleton eqnr
        in
          execStateT
            ( runReaderT
                (T.runApp2 $ Nordnet.downloadOpeningPrices tix)
                env
            )
            []
            >>= \result ->
              putStrLn (show result)

{--
demo =
    runReaderT (runApp $ Nordnet.openingPrice testTicker) testEnv

import EtradeJanitor.Repos.Nordnet.RedisRepos

import           Control.Monad.State            ( MonadState
                                                , runStateT
                                                , execStateT
                                                , put
                                                , get
                                                , modify
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , MonadIO
                                                , ask
                                                , runReaderT
                                                )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , MonadMask
                                                , catch
                                                )
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
                                                , REIO2
                                                , OpeningPrice(..)
                                                , AppState
                                                )
import qualified EtradeJanitor.Common.Types    as T
import qualified EtradeJanitor.Repos.Nordnet   as Nordnet

import qualified EtradeJanitor.Repos.Nordnet.RedisRepos
                                               as RedisRepos -- (expiryTimes,saveOpeningPricesToRedis) where

import           Control.Exception              ( try
                                                , SomeException
                                                , IOException
                                                )
import           System.IO                      ( openFile
                                                , hSetEncoding
                                                , hGetContents
                                                , latin1
                                                , IOMode(..)
                                                )

import           Data.UUID.V4                   ( nextRandom )
import           Data.UUID                      ( nil )
import           EtradeJanitor.AMQP.RabbitMQ    ( myConnection )
import qualified Data.Text                     as Text

prms = Params.Params
  { Params.databaseIp               = "172.20.1.3"
  , Params.redisHost                = "172.20.1.2"
  , Params.redisDatabase            = "5"
  , Params.feed = "/home/rcs/opt/haskell/etradejanitor/feedtmp"
  , Params.downloadDerivatives      = True
  , Params.dbUpdateStocks           = False
  , Params.skipIfDownloadFileExists = False
  , Params.showStockTickers         = False
  , Params.openingPricesToRedis     = False
  }

dx1 :: Calendar.Day
dx1 = Calendar.fromGregorian 2021 6 17

env = Env prms dx1

nhy :: Ticker
nhy = Ticker { T.oid = 1, T.ticker = "NHY", T.category = 1, T.date = dx1 }

sdrl :: Ticker
sdrl = Ticker { T.oid = 4, T.ticker = "SDRL", T.category = 1, T.date = dx1 }

tix :: Tickers
tix = Vector.fromList [nhy]

work2 :: IO ()
work2 = undefined -- runReaderT (T.runApp $ Nordnet.downloadOpeningPrices tix) env

riox2 :: (MonadReader Env m) => m ()
riox2 = ask >>= \x -> pure ()

riox1 :: (MonadState AppState m) => m ()
riox1 = modify (nhy :) >> modify (sdrl :) >> pure ()

rio :: REIO2 Int
rio = riox1 >> riox2 >> pure 3

what :: T.Ticker -> IO ()
what (T.Ticker {ticker}) = (putStrLn . Text.unpack) ticker

runD =
  let envc = env Nothing nil
  -- in  runReaderT (T.runApp $ RedisRepos.fetchExpiryFromRedis2) envc
  in  runReaderT (T.runApp $ RedisRepos.expiryTimes2) envc

runX = myConnection >>= \c ->
    nextRandom >>= \uuid ->
  let envc = env (Just c) uuid
  in
  runReaderT (T.runApp $ Nordnet.downloadDerivativePrices tix) envc >>
  execStateT
    (runReaderT
        (T.runApp2 $ Nordnet.downloadOpeningPrices tix)
        envc
    )
    []

--}

{-
runRio = myConnection >>= \c ->
    nextRandom >>= \uuid ->
        let envc = env c uuid in runStateT (runReaderT (T.runApp2 rio) envc) []

runOp = myConnection >>= \c ->
    nextRandom >>= \uuid ->
  let envc = env c uuid
  in  runStateT
        (runReaderT (T.runApp2 $ Nordnet.downloadOpeningPrices tix) envc)
        []

runRedis = myConnection >>= \c ->
  let envc = env c
  in  runReaderT (T.runApp $ Nordnet.openingPricesToRedis [nhy]) envc
-}

{-
exdemo =
    liftIO (
    (try (openFile "xstack.yaml" ReadMode >>= \handle ->
        putStrLn "Hi") :: IO (Either IOException ())) >>= \result ->
    case result of
        Left ex ->
            print (ex :: IOException) >>
            pure False
        Right ok ->
            putStrLn "Ok" >>
            pure True)

exdemo2 :: (MonadIO m, MonadState AppState m) => m String
exdemo2 =
    exdemo >>= \b ->
        if b == True then
            modify (nhy :) >>
            pure "YESSSSSS"
        else
            modify (sdrl :) >>
            pure "NOPE!!!!!"

runExdemo :: IO (String, AppState)
runExdemo =
    runStateT exdemo2 []
 -}

{-

data State s a = State { runState :: s -> (s, a) }

fmap :: (a -> b) -> State s a -> State s b

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
