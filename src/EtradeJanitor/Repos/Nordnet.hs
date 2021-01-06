{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module EtradeJanitor.Repos.Nordnet where

import           Data.List                      ( sortBy )
import           Data.Ord                       ( Down(..)
                                                , comparing
                                                )
import           Control.Monad.State            ( MonadState
                                                , modify
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , MonadIO
                                                )
import           Control.Exception              ( try )
import qualified Control.Monad.Reader          as Reader
import qualified Data.Vector                   as Vector
import qualified Text.Printf                   as Printf
import qualified Data.Text                     as Text
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Time.Calendar            as Calendar
import qualified System.Directory              as Directory

import           Network.HTTP.Req               ( (/:)
                                                , (=:)
                                                , HttpException
                                                )
import qualified Network.HTTP.Req              as R
import qualified Data.ByteString.Char8         as Char8

import qualified Text.HTML.TagSoup             as TS
import           Text.HTML.TagSoup              ( Tag(..)
                                                , (~/=)
                                                )

import           Data.Text                      ( Text )
import qualified EtradeJanitor.Repos.Nordnet.RedisRepos
                                               as RedisRepos
import qualified EtradeJanitor.Common.Types    as T
import qualified EtradeJanitor.Params          as Params

import           EtradeJanitor.Common.Html      ( soup )
import           EtradeJanitor.Common.Misc      ( decimalStrToAscii )

import           EtradeJanitor.Common.Types     ( Ticker
                                                , Tickers
                                                , NordnetExpiry
                                                , OpeningPrice(..)
                                                , Env
                                                , AppState
                                                )
import           EtradeJanitor.AMQP.RabbitMQ    ( Payload(..)
                                                , RoutingKey
                                                , rkInfo
                                                , rkError
                                                , publish
                                                )
import           Data.Time.Clock.POSIX          ( getPOSIXTime )

-- import           EtradeJanitor.Params           ( Params )

data Prices =
      DerivativePrices Ticker
    | OpeningPrices Ticker


{-
responseGETx :: R.Req R.BsResponse
responseGETx =
  let
      --myUrl = R.https "www.bullcrap.no" 
      myUrl = R.https "www.nordnet.no" /: "idonotexists"
  in  R.req R.GET myUrl R.NoReqBody R.bsResponse mempty

demoReq :: IO ()
demoReq =
          R.runReq R.defaultHttpConfig (responseGETx)
  >>= \bs -> Char8.writeFile "demoreq" (R.responseBody bs)
-}

responseGET :: Ticker -> NordnetExpiry -> R.Req R.BsResponse
responseGET t unixTime =
  let myUrl      = R.https "www.nordnet.nox" /: "market" /: "options"
      optionName = T.ticker t
  in  R.req R.GET myUrl R.NoReqBody R.bsResponse
        $  "currency"
        =: ("NOK" :: Text.Text)
        <> "underlyingSymbol"
        =: (optionName :: Text.Text)
        <> "expireDate"
        =: (unixTime :: Int)

downloadAbleTickers :: Tickers -> Tickers
downloadAbleTickers allTix = Vector.filter (\x -> T.category x == 1) allTix

pathName :: (MonadReader Env m) => Prices -> m FilePath
pathName (OpeningPrices _) = Reader.ask >>= \env ->
  let feed = (Params.feed . T.getParams) env
  in  pure $ Printf.printf "%s/openingprices" feed
pathName (DerivativePrices t) = Reader.ask >>= \env ->
  let feed      = (Params.feed . T.getParams) env
      curDay    = T.getDownloadDate env
      ticker    = T.ticker t
      (y, m, d) = Calendar.toGregorian curDay
  in  pure $ Printf.printf "%s/%d/%d/%d/%s" feed y m d ticker

mkDir :: (MonadIO m) => FilePath -> m ()
mkDir fp = liftIO (Directory.createDirectoryIfMissing True fp)

currentPosixTime :: IO Int
currentPosixTime = getPOSIXTime >>= \t -> pure (round t :: Int)

payloadStd :: Ticker -> NordnetExpiry -> IO Payload
payloadStd ticker unixTime =
  currentPosixTime >>= \t -> pure $ PayloadStd t unixTime

payloadErr :: Ticker -> NordnetExpiry -> HttpException -> IO Payload
payloadErr ticker unixTime httpEx =
  currentPosixTime >>= \t -> pure $ PayloadErr t unixTime

unixTimesDesc :: [NordnetExpiry] -> [NordnetExpiry]
unixTimesDesc unixTimes = sortBy (comparing Down) unixTimes

tryDownloadOpeningPrice :: (MonadIO m, MonadReader Env m) => Ticker -> m Bool
tryDownloadOpeningPrice t = pathName (OpeningPrices t) >>= \pn ->
  RedisRepos.expiryTimes t >>= \unixTimes ->
    mkDir pn
      >> let unixTime = head $ unixTimesDesc unixTimes
             fileName = Printf.printf "%s/%s.html" pn (T.ticker t)
             reqFn =
               putStrLn (Printf.printf "Downloading %s" fileName)
                 >>  R.runReq R.defaultHttpConfig (responseGET t unixTime)
                 >>= \bs -> Char8.writeFile fileName (R.responseBody bs)
         in  liftIO
             $   (try reqFn :: IO (Either HttpException ()))
             >>= \result -> case result of
                   Left  e -> putStrLn (show e) >> pure False
                   Right _ -> pure True

downloadOpeningPrice
  :: (MonadIO m, MonadReader Env m, MonadState AppState m) => Ticker -> m ()
downloadOpeningPrice t = tryDownloadOpeningPrice t
  >>= \result -> if result == True then modify (t :) else pure ()

downloadOpeningPrices
  :: (MonadIO m, MonadReader Env m, MonadState AppState m) => Tickers -> m ()
downloadOpeningPrices tix = Reader.ask >>= \env ->
  let doDownload = (Params.openingPricesToRedis . T.getParams) env
  in  case doDownload of
        True ->
          let dt = downloadAbleTickers tix in mapM_ downloadOpeningPrice dt
        False -> pure ()

openingPricesToRedis :: (MonadIO m, MonadReader Env m) => [Ticker] -> m ()
openingPricesToRedis tix = Reader.ask >>= \env ->
  let opr = (Params.openingPricesToRedis . T.getParams) env
  in  if opr == True
        then mapM openingPrice tix
          >>= \tixx -> RedisRepos.saveOpeningPricesToRedis tixx
        else pure ()

download
  :: (MonadIO m, MonadReader Env m)
  => Ticker
  -> FilePath
  -> Bool
  -> NordnetExpiry
  -> m ()
download t filePath skipIfExists unixTime =
  let fileName = Printf.printf "%s/%d.html" filePath unixTime
      doDownloadIO :: IO Bool
      doDownloadIO =
          (   Directory.doesFileExist fileName
          >>= \fileExist -> pure $ not $ skipIfExists && fileExist
          )
      downloadWithPayload :: IO (Payload, RoutingKey)
      downloadWithPayload = doDownloadIO >>= \doDownload -> case doDownload of
        False ->
          --msg = (Printf.printf "Skipping download of %s" fileName) :: Text
          payloadStd t unixTime >>= \p -> pure (p, rkError)
        True ->
          let reqFn =
                  putStrLn (Printf.printf "Downloading %s" fileName)
                    >>  R.runReq R.defaultHttpConfig (responseGET t unixTime)
                    >>= \bs -> Char8.writeFile fileName (R.responseBody bs)
          in  (try reqFn :: IO (Either HttpException ())) >>= \result ->
                case result of
                  Left  e -> payloadErr t unixTime e >>= \p -> pure (p, rkError)
                  Right _ -> payloadStd t unixTime >>= \p -> pure (p, rkInfo)
  in  (liftIO $ downloadWithPayload) >>= \(x, y) -> publish x y

downloadDerivativePrices' :: (MonadIO m, MonadReader Env m) => Ticker -> m ()
downloadDerivativePrices' t = Reader.ask >>= \env ->
  let skipIfExists = (Params.skipIfDownloadFileExists . T.getParams) env
  in  pathName (DerivativePrices t) >>= \pn ->
        RedisRepos.expiryTimes t >>= \unixTimes ->
          mkDir pn
            >> let dlfn = download t pn skipIfExists in mapM_ dlfn unixTimes

downloadDerivativePrices :: (MonadIO m, MonadReader Env m) => Tickers -> m ()
downloadDerivativePrices tix = Reader.ask >>= \env ->
  let doDownload = (Params.downloadDerivatives . T.getParams) env
  in
    case doDownload of
      True ->
        let dt = downloadAbleTickers tix in mapM_ downloadDerivativePrices' dt
      False -> pure ()

tr :: (MonadIO m, MonadReader Env m) => Ticker -> m [Tag String]
tr t = pathName (OpeningPrices t) >>= \pn ->
  let fname = Printf.printf "%s/%s.html" pn (T.ticker t)
  in  (liftIO $ soup fname) >>= \soupx ->
        let table = dropWhile (~/= ("<table>" :: String)) soupx
            tbody = dropWhile (~/= ("<tbody>" :: String)) table
        in  pure $ dropWhile (~/= ("<tr>" :: String)) tbody

openingPrice :: (MonadIO m, MonadReader Env m) => Ticker -> m OpeningPrice
openingPrice t = tr t >>= \trx ->
  let td =
          dropWhile (~/= TagOpen ("td" :: String) [("data-title", "Siste")]) trx
      txt = (TS.fromTagText . head . drop 1) $ dropWhile
        (~/= TagOpen ("span" :: String) [("aria-hidden", "true")])
        td
  in  pure $ OpeningPrice (T.ticker t) (decimalStrToAscii txt)
