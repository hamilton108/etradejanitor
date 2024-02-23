{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Nordnet.RedisRepos
  ( expiryTimes
  , expiryTimes2
  , saveOpeningPricesToRedis
  , fetchExpiryFromRedis2
  )
where

-- import qualified Data.Vector as Vector
-- import Data.Typeable

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
  ( MonadIO
  , MonadReader
  )
import qualified Control.Monad.Reader as Reader

import Data.Either (fromRight)
import qualified Data.List.Split as Split
import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Calendar as Calendar

import qualified Data.ByteString as B
import qualified Data.ByteString.Conversion as BC
import qualified Data.ByteString.UTF8 as BU
import qualified Database.Redis as Redis
--import qualified Data.Vector as DV 
--import Data.Vector (Vector(..))

import EtradeJanitor.Common.Types
  ( Env
  , NordnetExpiry
  , Ticker (..)
  , getDownloadDate
  , getParams
  )

import EtradeJanitor.Domain.OpeningPrice 
  ( OpeningPrice(..)
  )

import EtradeJanitor.Common.CalendarUtil
  ( dayToUnixTime
  , unixTimeToInt
  )
import qualified EtradeJanitor.Params as Params

ci :: String -> Integer -> Redis.ConnectInfo
ci host redisDatabase =
  Redis.defaultConnectInfo
    { Redis.connectHost = host
    , Redis.connectDatabase = redisDatabase
    }

conn :: (MonadIO m, MonadReader Env m) => m Redis.Connection
conn =
  Reader.ask >>= \env ->
    let
      redisHost = (Params.redisHost . getParams) env
      redisDatabase = (Params.redisDatabase . getParams) env
    in
      liftIO $ Redis.checkedConnect $ ci redisHost redisDatabase

exp1 :: Redis.Redis (Either Redis.Reply [(B.ByteString, B.ByteString)])
exp1 = Redis.hgetall (BU.fromString "expiry-1")

exp2 :: Redis.Redis (Either Redis.Reply [(B.ByteString, B.ByteString)])
exp2 = Redis.hgetall (BU.fromString "expiry-2")

fetchExpiryFromRedis ::
  (MonadIO m, MonadReader Env m) =>
  Ticker ->
  m [(B.ByteString, B.ByteString)]
fetchExpiryFromRedis (Ticker{ticker}) =
  conn >>= \c1 ->
    liftIO $
      Redis.runRedis c1 $
        Redis.hget "expiry" (TE.encodeUtf8 ticker)
          >>= \ex ->
            let
              ex1 = fromRight Nothing ex
            in
              case ex1 of
                Nothing -> pure []
                Just ex2 -> case ex2 of
                  "1" -> exp1 >>= \z -> pure (fromRight [] z)
                  "2" ->
                    exp1 >>= \z ->
                      exp2
                        >>= \z2 ->
                          let
                            merged = (++) <$> z <*> z2
                          in
                            pure (fromRight [] merged)
                  _ -> pure []

parseRedisItem ::
  Calendar.Day -> (B.ByteString, B.ByteString) -> Maybe NordnetExpiry
parseRedisItem curDay (datePart, timePart) =
  let
    [ys, ms, ds] = Split.splitOn "-" (BU.toString datePart)
    year = read ys :: Integer
    month = read ms :: Int
    day = read ds :: Int
    expDate = Calendar.fromGregorian year month day
  in
    if expDate < curDay
      then Nothing
      else
        let
          result :: Int
          result = read (BU.toString timePart)
        in
          Just result

expiryTimes :: (MonadIO m, MonadReader Env m) => Ticker -> m [NordnetExpiry]
expiryTimes ticker =
  Reader.ask >>= \env ->
    let
      parseFn = parseRedisItem (getDownloadDate env)
    in
      fetchExpiryFromRedis ticker >>= \items ->
        let
          result = map parseFn items
        in
          pure $
            map (\y -> Maybe.fromJust y) $
              filter
                (\x -> x /= Nothing)
                result

fetchExpiryFromRedis2 :: (MonadIO m, MonadReader Env m) => m [B.ByteString]
fetchExpiryFromRedis2 =
  conn >>= \c1 ->
    liftIO $
      Redis.runRedis c1 $
        Redis.smembers "expiry" >>= \ex ->
          let ex1 = fromRight [] ex in pure ex1

expiryTimes2 :: (MonadIO m, MonadReader Env m) => m [NordnetExpiry]
expiryTimes2 =
  Reader.ask >>= \env ->
    fetchExpiryFromRedis2 >>= \byteStrings ->
      let
        ints = map (read . BU.toString) byteStrings
        curUnixTime =
          1000 * ((unixTimeToInt . dayToUnixTime) (getDownloadDate env))
      in
        liftIO ((putStrLn . show) curUnixTime)
          >> pure (filter (\x -> x > curUnixTime) ints)

saveOpeningPricesToRedis' ::
  (MonadIO m, MonadReader Env m) =>
  [(B.ByteString, B.ByteString)] ->
  m (Either Redis.Reply Redis.Status)
saveOpeningPricesToRedis' prices =
  conn
    >>= \c1 -> liftIO $ Redis.runRedis c1 $ Redis.hmset "openingprices" prices

openingPriceToRedisFormat :: OpeningPrice -> (B.ByteString, B.ByteString)
openingPriceToRedisFormat (OpeningPrice ticker price) =
  (BC.toByteString' ticker, BC.toByteString' price)

-- (TE.encodeUtf8 ticker, BC.toByteString' price)

saveOpeningPricesToRedis ::
  (MonadIO m, MonadReader Env m) => [OpeningPrice] -> m ()
saveOpeningPricesToRedis prices =
  let
    redisPrices = map openingPriceToRedisFormat prices
  in
    saveOpeningPricesToRedis' redisPrices >> pure ()
