{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Nordnet.Derivative where

import qualified Control.Monad.Reader as Reader
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as Text
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Clock as Clock
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
import qualified EtradeJanitor.StockExchange.OptionExpiry as OptionExpiry 

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
    
unixTimeToNordnetExpireDate :: T.NordnetExpiry -> Int
unixTimeToNordnetExpireDate unixTime =
    unixTime * 1000
    -- (CalendarUtil.unixTimeToInt unixTime) * 1000

responseGET :: T.Ticker -> T.NordnetExpiry -> R.Req R.BsResponse
responseGET t unixTime = 
    let
        myUrl = R.https "www.nordnet.no" /: "market" /: "options"
        optionName = T.ticker t
        nordnetExpiry = unixTimeToNordnetExpireDate unixTime
    in
    R.req R.GET myUrl R.NoReqBody R.bsResponse $ 
        "currency" =: ("NOK":: Text.Text) 
        <> "underlyingSymbol" =: (optionName :: Text.Text) 
        <> "expireDate" =: (nordnetExpiry :: Int) 

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


nordNetExpiry :: REIO [T.NordnetExpiry]
nordNetExpiry =
    Reader.ask >>= \env ->
    let
        expiry = 
            CalendarUtil.today >>= \today -> 
            Reader.runReaderT (OptionExpiry.expiryTimes today) env
    in
    liftIO expiry

download :: T.Ticker -> [T.NordnetExpiry] -> REIO ()
download ticker unixTimes = 
    Reader.ask >>= \env ->
    let
        skipIfExists = (Params.skipIfDownloadFileExists . getParams) env
    in
    mkDir ticker >>= \filePath ->
    let
        dlfn = download' ticker filePath skipIfExists 
    in 
    mapM_ dlfn unixTimes

downloadTickers :: T.Tickers -> REIO ()
downloadTickers tix = 
    Reader.ask >>= \env ->
    let 
        skipDownload = (Params.skipDownloadDerivatives . getParams) env
    in
    case skipDownload of 
        True -> pure ()
        False -> nordNetExpiry >>= \expiry ->
                    mapM_ (\t -> download t expiry) tix 
