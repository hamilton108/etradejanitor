{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Nordnet where

import qualified Control.Monad.Reader as Reader
import qualified Data.Vector as Vector
import qualified Text.Printf as Printf
import qualified Data.Text as Text
import Control.Monad.IO.Class (liftIO)
import qualified Data.Time.Calendar as Calendar
import qualified System.Directory as Directory

import Network.HTTP.Req ((/:),(=:))
import qualified Network.HTTP.Req as R
import qualified Data.ByteString.Char8 as Char8

import qualified EtradeJanitor.Repos.Nordnet.RedisRepos as RedisRepos 
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Params as Params

import EtradeJanitor.Common.Types 
    ( Ticker
    , Tickers
    , NordnetExpiry
    , REIO 
    )

import EtradeJanitor.Params (Params)

data Prices = 
      DerivativePrices Ticker
    | OpeningPrices Ticker


responseGET :: Ticker -> NordnetExpiry -> R.Req R.BsResponse
responseGET t unixTime = 
    let
        myUrl = R.https "www.nordnet.no" /: "market" /: "options"
        optionName = T.ticker t
    in
    R.req R.GET myUrl R.NoReqBody R.bsResponse $ 
        "currency" =: ("NOK":: Text.Text) 
        <> "underlyingSymbol" =: (optionName :: Text.Text) 
        <> "expireDate" =: (unixTime :: Int) 

nordNetExpiry :: Ticker -> REIO [NordnetExpiry]
nordNetExpiry ticker =
    Reader.ask >>= \env ->
    let
        expiry = 
            Reader.runReaderT (RedisRepos.expiryTimes ticker) env
    in
    liftIO expiry

downloadAbleTickers :: Tickers -> Tickers
downloadAbleTickers allTix = 
    Vector.filter (\x -> T.category x == 1) allTix

pathName :: Prices -> REIO FilePath
pathName (OpeningPrices _)= 
    Reader.ask >>= \env ->
    let
        feed = (Params.feed . T.getParams) env
    in
    pure $ Printf.printf "%s/openingprices" feed 
pathName (DerivativePrices t) = 
    Reader.ask >>= \env ->
    let 
        feed = (Params.feed . T.getParams) env
        curDay = T.getDownloadDate env
        ticker = T.ticker t
        (y,m,d) = Calendar.toGregorian curDay
    in
    pure $ Printf.printf "%s/%d/%d/%d/%s" feed y m d ticker

fileName :: Prices -> REIO String
fileName p@(OpeningPrices t) = 
    pathName p >>= \pn -> 
        pure (Printf.printf "%s/%s.html" pn (T.ticker t))
fileName _ = 
    pure "N/A"


mkDir :: FilePath -> REIO ()
mkDir fp = 
    liftIO (Directory.createDirectoryIfMissing True fp) 

download' :: Ticker -> FilePath -> Bool -> NordnetExpiry -> REIO ()
download' t filePath skipIfExists unixTime = 
    let
        fileName = Printf.printf "%s/%d.html" filePath unixTime -- expiryAsUnixTime
        doDownloadIO = (Directory.doesFileExist fileName >>= \fileExist ->
                        pure $ not $ skipIfExists && fileExist) :: IO Bool
    in
    liftIO $ 
    doDownloadIO >>= \doDownload -> 
        case doDownload of 
            False -> putStrLn (Printf.printf "Skipping download of %s" fileName) >> pure ()
            True -> 
                putStrLn (Printf.printf "Downloading %s" fileName) >> 
                R.runReq R.defaultHttpConfig (responseGET t unixTime) >>= \bs -> 
                Char8.writeFile fileName (R.responseBody bs)

download :: Prices -> REIO ()
download p@(OpeningPrices t) =
    pathName p >>= \pn ->
        nordNetExpiry t >>= \unixTimes -> 
            mkDir pn >>
            let 
                unixTime = head unixTimes
                fileName = Printf.printf "%s/%s.html" pn (T.ticker t) 
            in
            liftIO $
            putStrLn (Printf.printf "Downloading %s" fileName) >> 
            R.runReq R.defaultHttpConfig (responseGET t unixTime) >>= \bs -> 
            Char8.writeFile fileName (R.responseBody bs)
download p@(DerivativePrices t) =
    Reader.ask >>= \env ->
    let
        skipIfExists = (Params.skipIfDownloadFileExists . T.getParams) env
    in
    pathName p >>= \pn ->
        nordNetExpiry t >>= \unixTimes -> 
            mkDir pn >>
            let
                dlfn = download' t pn skipIfExists
            in 
            mapM_ dlfn unixTimes


downloadOpeningPrices :: Tickers -> REIO ()
downloadOpeningPrices tix = 
    dl Params.openingPricesToRedis (\t -> download (OpeningPrices t)) tix

downloadDerivativePrices :: Tickers -> REIO ()
downloadDerivativePrices tix = 
    dl Params.downloadDerivatives (\t -> download (DerivativePrices t)) tix

dl :: (Params -> Bool) -> (Ticker -> REIO ()) -> Tickers -> REIO ()
dl fn tixFn tix = 
    Reader.ask >>= \env ->
    let 
        doDownload = (fn . T.getParams) env
    in
    case doDownload of 
        True -> 
            let
                dt = downloadAbleTickers tix
            in
            mapM_ tixFn dt
        False -> 
            pure ()