{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module EtradeJanitor.Repos.Nordnet.RedisRepos
  ( expiryTimes
  , saveOpeningPricesToRedis
  )
where

--import qualified Data.Vector as Vector
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.IO.Class         ( liftIO )

import           Data.Either                    ( fromRight )
import qualified Data.Maybe                    as Maybe
import qualified Data.List.Split               as Split
import qualified Data.Time.Calendar            as Calendar
import qualified Data.Text.Encoding            as TE

import qualified Data.ByteString               as B
import qualified Data.ByteString.Conversion    as BC
import qualified Data.ByteString.UTF8          as BU
import qualified Database.Redis                as Redis

import           EtradeJanitor.Common.Types     ( REIO
                                                , NordnetExpiry
                                                , getParams
                                                , getDownloadDate
                                                , Ticker(..)
                                                , OpeningPrice(..)
                                                )

import qualified EtradeJanitor.Params          as Params

ci :: String -> Integer -> Redis.ConnectInfo
ci host redisDatabase = Redis.defaultConnectInfo
  { Redis.connectHost     = host
  , Redis.connectDatabase = redisDatabase
  }

conn :: REIO Redis.Connection
conn = Reader.ask >>= \env ->
  let redisHost     = (Params.redisHost . getParams) env
      redisDatabase = (read . Params.redisDatabase . getParams) env
  in  liftIO $ Redis.checkedConnect $ ci redisHost redisDatabase

exp1 :: Redis.Redis (Either Redis.Reply [(B.ByteString, B.ByteString)])
exp1 = Redis.hgetall (BU.fromString "expiry-1")

exp2 :: Redis.Redis (Either Redis.Reply [(B.ByteString, B.ByteString)])
exp2 = Redis.hgetall (BU.fromString "expiry-2")

fetchExpiryFromRedis :: Ticker -> REIO [(B.ByteString, B.ByteString)]
fetchExpiryFromRedis (Ticker { ticker }) = conn >>= \c1 ->
  liftIO
    $   Redis.runRedis c1
    $   Redis.hget "expiry" (TE.encodeUtf8 ticker)
    >>= \ex ->
          let ex1 = fromRight Nothing ex
          in
            case ex1 of
              Nothing  -> pure []
              Just ex2 -> case ex2 of
                "1" -> exp1 >>= \z -> pure (fromRight [] z)
                "2" -> exp1 >>= \z ->
                  exp2
                    >>= \z2 ->
                          let merged = (++) <$> z <*> z2
                          in  pure (fromRight [] merged)
                _ -> pure []

parseRedisItem
  :: Calendar.Day -> (B.ByteString, B.ByteString) -> Maybe NordnetExpiry
parseRedisItem curDay (datePart, timePart) =
  let [ys, ms, ds] = Split.splitOn "-" (BU.toString datePart)
      year         = read ys :: Integer
      month        = read ms :: Int
      day          = read ds :: Int
      expDate      = Calendar.fromGregorian year month day
  in  if expDate < curDay
        then Nothing
        else
          let result :: Int
              result = read (BU.toString timePart)
          in  Just result

expiryTimes :: Ticker -> REIO [NordnetExpiry]
expiryTimes ticker = Reader.ask >>= \env ->
  let parseFn = parseRedisItem (getDownloadDate env)
  in  fetchExpiryFromRedis ticker >>= \items ->
        let result = map parseFn items
        in  pure $ map (\y -> Maybe.fromJust y) $ filter (\x -> x /= Nothing)
                                                         result


saveOpeningPricesToRedis'
  :: [(B.ByteString, B.ByteString)] -> REIO (Either Redis.Reply Redis.Status)
saveOpeningPricesToRedis' prices = conn
  >>= \c1 -> liftIO $ Redis.runRedis c1 $ Redis.hmset "openingprices" prices

openingPriceToRedisFormat :: OpeningPrice -> (B.ByteString, B.ByteString)
openingPriceToRedisFormat (OpeningPrice ticker price) =
  (TE.encodeUtf8 ticker, BC.toByteString' price)

saveOpeningPricesToRedis :: [OpeningPrice] -> REIO ()
saveOpeningPricesToRedis prices =
  let redisPrices = map openingPriceToRedisFormat prices
  in  saveOpeningPricesToRedis' redisPrices >> pure ()
