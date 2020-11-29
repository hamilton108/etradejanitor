{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Nordnet where

import           Control.Monad.State            ( modify )
import qualified Control.Monad.Reader          as Reader
import qualified Data.Vector                   as Vector
import qualified Text.Printf                   as Printf
import qualified Data.Text                     as Text
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Time.Calendar            as Calendar
import qualified System.Directory              as Directory

import           Network.HTTP.Req               ( (/:)
                                                , (=:)
                                                )
import qualified Network.HTTP.Req              as R
import qualified Data.ByteString.Char8         as Char8

import qualified Text.HTML.TagSoup             as TS
import           Text.HTML.TagSoup              ( Tag(..)
                                                , (~/=)
                                                )

import qualified EtradeJanitor.Repos.Nordnet.RedisRepos
                                               as RedisRepos
import qualified EtradeJanitor.Common.Types    as T
import qualified EtradeJanitor.Params          as Params

import           EtradeJanitor.Common.Html      ( soup )
import           EtradeJanitor.Common.Misc      ( decimalStrToAscii )

import           EtradeJanitor.Common.Types     ( Ticker
                                                , Tickers
                                                , NordnetExpiry
                                                , REIO
                                                , OpeningPrice(..)
                                                )

import           EtradeJanitor.Params           ( Params )

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
  let myUrl      = R.https "www.nordnet.no" /: "market" /: "options"
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

pathName :: Prices -> REIO FilePath
pathName (OpeningPrices _) = Reader.ask >>= \env ->
  let feed = (Params.feed . T.getParams) env
  in  pure $ Printf.printf "%s/openingprices" feed
pathName (DerivativePrices t) = Reader.ask >>= \env ->
  let feed      = (Params.feed . T.getParams) env
      curDay    = T.getDownloadDate env
      ticker    = T.ticker t
      (y, m, d) = Calendar.toGregorian curDay
  in  pure $ Printf.printf "%s/%d/%d/%d/%s" feed y m d ticker

{-
fileName :: Prices -> REIO String
fileName p@(OpeningPrices t) = 
    pathName p >>= \pn -> 
        pure (Printf.printf "%s/%s.html" pn (T.ticker t))
fileName _ = 
    pure "N/A"
 -}


mkDir :: FilePath -> REIO ()
mkDir fp = liftIO (Directory.createDirectoryIfMissing True fp)

download' :: Ticker -> FilePath -> Bool -> NordnetExpiry -> REIO ()
download' t filePath skipIfExists unixTime =
  let fileName = Printf.printf "%s/%d.html" filePath unixTime -- expiryAsUnixTime
      doDownloadIO =
          (   Directory.doesFileExist fileName
          >>= \fileExist -> pure $ not $ skipIfExists && fileExist
          ) :: IO Bool
  in  liftIO $ doDownloadIO >>= \doDownload -> case doDownload of
        False ->
          putStrLn (Printf.printf "Skipping download of %s" fileName) >> pure ()
        True ->
          putStrLn (Printf.printf "Downloading %s" fileName)
            >>  R.runReq R.defaultHttpConfig (responseGET t unixTime)
            >>= \bs -> Char8.writeFile fileName (R.responseBody bs)

download :: Prices -> REIO ()
download p@(OpeningPrices t) = pathName p >>= \pn ->
  RedisRepos.expiryTimes t >>= \unixTimes ->
    mkDir pn
      >> let unixTime = head unixTimes
             fileName = Printf.printf "%s/%s.html" pn (T.ticker t)
         in  (liftIO
             $   putStrLn (Printf.printf "Downloading %s" fileName)
             >>  R.runReq R.defaultHttpConfig (responseGET t unixTime)
             >>= \bs -> Char8.writeFile fileName (R.responseBody bs))
             >> modify (t:)
download p@(DerivativePrices t) = Reader.ask >>= \env ->
  let skipIfExists = (Params.skipIfDownloadFileExists . T.getParams) env
  in  pathName p >>= \pn -> RedisRepos.expiryTimes t >>= \unixTimes ->
        mkDir pn
          >> let dlfn = download' t pn skipIfExists in mapM_ dlfn unixTimes


downloadOpeningPrices :: Tickers -> REIO ()
downloadOpeningPrices tix =
  dl Params.openingPricesToRedis (\t -> download (OpeningPrices t)) tix

downloadDerivativePrices :: Tickers -> REIO ()
downloadDerivativePrices tix =
  dl Params.downloadDerivatives (\t -> download (DerivativePrices t)) tix

openingPricesToRedis :: Tickers -> REIO ()
openingPricesToRedis tix = Reader.ask >>= \env ->
  let opr = (Params.openingPricesToRedis . T.getParams) env
  in  if opr == True
        then mapM openingPrice tix >>= \tixx ->
          RedisRepos.saveOpeningPricesToRedis $ Vector.toList tixx
        else pure ()

dl :: (Params -> Bool) -> (Ticker -> REIO ()) -> Tickers -> REIO ()
dl fn tixFn tix = Reader.ask >>= \env ->
  let doDownload = (fn . T.getParams) env
  in  case doDownload of
        True  -> let dt = downloadAbleTickers tix in mapM_ tixFn dt
        False -> pure ()

tr :: Ticker -> REIO [Tag String]
tr t = pathName (OpeningPrices t) >>= \pn ->
  let fname = Printf.printf "%s/%s.html" pn (T.ticker t)
  in  (liftIO $ soup fname) >>= \soupx ->
        let table = dropWhile (~/= ("<table>" :: String)) soupx
            tbody = dropWhile (~/= ("<tbody>" :: String)) table
        in  pure $ dropWhile (~/= ("<tr>" :: String)) tbody

openingPrice :: Ticker -> REIO OpeningPrice
openingPrice t = tr t >>= \trx ->
  let td =
          dropWhile (~/= TagOpen ("td" :: String) [("data-title", "Siste")]) trx
      txt = (TS.fromTagText . head . drop 1) $ dropWhile
        (~/= TagOpen ("span" :: String) [("aria-hidden", "true")])
        td
  in  pure $ OpeningPrice (T.ticker t) (decimalStrToAscii txt)
