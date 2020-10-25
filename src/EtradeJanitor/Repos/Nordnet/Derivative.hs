{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Nordnet.Derivative where

import qualified Control.Monad.Reader as Reader
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as Text
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Clock as Clock
import qualified Data.Vector as Vector
import qualified Text.Printf as Printf

import Network.HTTP.Req ((/:),(=:))
import qualified Network.HTTP.Req as R
import qualified Data.ByteString.Char8 as Char8

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import qualified EtradeJanitor.Params as Params
import EtradeJanitor.Common.Types (REIO,getParams,getDownloadDate)
import qualified EtradeJanitor.Common.Misc as Misc
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Common.CalendarUtil as CalendarUtil
import qualified EtradeJanitor.Repos.Nordnet.RedisRepos as RedisRepos 

-- import qualified EtradeJanitor.Common.Types as Types

-- https://www.nordnet.no/market/options?currency=NOK&underlyingSymbol=BAKKA&expireDate=1565906400000

-- nordNetUrl :: Text.Text
-- nordNetUrl = "www.nordnet.no/market/options" -- ?currency=NOK&underlyingSymbol=BAKKA&expireDate=1565906400000

{-
testParams :: Params.Params
testParams = 
    Params.Params 
    { Params.databaseIp = "172.17.0.2"
    , Params.feed = Misc.feedRoot ++ "/test/testfeed" 
    , Params.downloadOnly = True
    , Params.updateDbOnly = True
    }

d = Calendar.fromGregorian 2019 3 30

t = Ticker "NHY"

env = Types.Env testParams
-}

data Price = 
      DerivativePrice T.Ticker T.NordnetExpiry
    | OpeningPrice T.NordnetExpiry

pathNameOpeningPrices :: REIO FilePath
pathNameOpeningPrices = 
    Reader.ask >>= \env ->
    let 
        feed = (Params.feed . getParams) env
    in
    pure $ Printf.printf "%s/openingprices" feed 

mkDirOpeningPrices :: REIO String
mkDirOpeningPrices = 
    pathNameOpeningPrices >>= \pn ->
    liftIO (Directory.createDirectoryIfMissing True pn) >>
    pure pn

pathNameFor :: T.Ticker -> REIO FilePath
pathNameFor t = 
    Reader.ask >>= \env ->
    let 
        curDay = getDownloadDate env
        ticker = T.ticker t
        feed = (Params.feed . getParams) env
        (y,m,d) = Calendar.toGregorian curDay
    in
    pure $ Printf.printf "%s/%d/%d/%d/%s" feed y m d ticker

mkDir :: T.Ticker -> REIO String
mkDir ticker = 
    pathNameFor ticker >>= \pn ->
    liftIO (Directory.createDirectoryIfMissing True pn) >>
    pure pn

{-
mkDir :: T.Ticker -> REIO String
mkDir ticker = 
    pathNameFor ticker >>= \pn ->
    liftIO (Directory.createDirectoryIfMissing True pn) >>
    pure pn
    
unixTimeToNordnetExpireDate :: T.NordnetExpiry -> Int
unixTimeToNordnetExpireDate unixTime =
    unixTime * 1000
-}
    
responseGET :: T.Ticker -> T.NordnetExpiry -> R.Req R.BsResponse
responseGET t unixTime = 
    let
        myUrl = R.https "www.nordnet.no" /: "market" /: "options"
        optionName = T.ticker t
        -- nordnetExpiry = unixTimeToNordnetExpireDate unixTime
    in
    R.req R.GET myUrl R.NoReqBody R.bsResponse $ 
        "currency" =: ("NOK":: Text.Text) 
        <> "underlyingSymbol" =: (optionName :: Text.Text) 
        <> "expireDate" =: (unixTime :: Int) 

download' :: T.Ticker -> FilePath -> Bool -> T.NordnetExpiry -> REIO ()
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

nordNetExpiry :: T.Ticker -> REIO [T.NordnetExpiry]
nordNetExpiry ticker =
    Reader.ask >>= \env ->
    let
        expiry = 
            Reader.runReaderT (RedisRepos.expiryTimes ticker) env
    in
    liftIO expiry

{-
willDownload :: FilePath -> T.NordnetExpiry -> REIO Bool 
willDownload filePath unixTime = 
    Reader.ask >>= \env ->
    let
        fileName = Printf.printf "%s/%d.html" filePath unixTime -- expiryAsUnixTime
        skipIfExists = (Params.skipIfDownloadFileExists . getParams) env
    in
    liftIO ((Directory.doesFileExist fileName >>= \fileExist ->
             pure $ not $ skipIfExists && fileExist) :: IO Bool)
-}

download :: T.Ticker -> REIO ()
download ticker = 
    Reader.ask >>= \env ->
    let
        skipIfExists = (Params.skipIfDownloadFileExists . getParams) env
    in
    mkDir ticker >>= \filePath ->
        nordNetExpiry ticker >>= \unixTimes -> 
            let
                dlfn = download' ticker filePath skipIfExists
            in 
            mapM_ dlfn unixTimes

openingPriceFileName :: T.Ticker -> REIO String
openingPriceFileName t = 
    Reader.ask >>= \env ->
        pathNameOpeningPrices >>= \pathName ->
        pure $ Printf.printf "%s/%s.html" pathName (T.ticker t)

downloadOpeningPrice :: T.Ticker -> REIO ()
downloadOpeningPrice t = 
    mkDirOpeningPrices >>= \filePath ->
        nordNetExpiry t >>= \unixTimes -> 
            let 
                unixTime = head unixTimes
                fileName = Printf.printf "%s/%s.html" filePath (T.ticker t) 
            in
            liftIO $
            putStrLn (Printf.printf "Downloading %s" fileName) >> 
            R.runReq R.defaultHttpConfig (responseGET t unixTime) >>= \bs -> 
            Char8.writeFile fileName (R.responseBody bs)

downloadAbleTickers :: T.Tickers -> T.Tickers
downloadAbleTickers allTix = 
    Vector.filter (\x -> T.category x == 1) allTix

downloadTickers :: T.Tickers -> REIO ()
downloadTickers tix = 
    Reader.ask >>= \env ->
    let 
        skipDownload = (Params.skipDownloadDerivatives . getParams) env
    in
    case skipDownload of 
        True -> pure ()
        False -> 
            let
                dt = downloadAbleTickers tix
            in
            mapM_ (\t -> download t) dt

downloadOpeningPrices :: T.Tickers -> REIO ()
downloadOpeningPrices tix = 
    let 
        dt = downloadAbleTickers tix
    in
    mapM_ (\t -> downloadOpeningPrice t) dt

{-
        False -> nordNetExpiry >>= \expiry ->
                    let
                        dt = downloadAbleTickers tix
                    in
                    mapM_ (\t -> download t expiry) dt
-}
