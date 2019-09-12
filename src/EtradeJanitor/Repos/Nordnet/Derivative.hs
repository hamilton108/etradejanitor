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
import EtradeJanitor.Common.Types (REIO,getParams)
import qualified EtradeJanitor.Common.Misc as Misc
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Common.CalendarUtil as CalendarUtil

-- import qualified EtradeJanitor.Common.Types as Types

-- https://www.nordnet.no/market/options?currency=NOK&underlyingSymbol=BAKKA&expireDate=1565906400000

nordNetUrl :: Text.Text
nordNetUrl = "www.nordnet.no/market/options" -- ?currency=NOK&underlyingSymbol=BAKKA&expireDate=1565906400000

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
        curDay = T.date t
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
    
        
responseGET :: T.Ticker -> POSIX.POSIXTime -> R.Req R.BsResponse 
responseGET t unixTime = 
    let
        myUrl = R.https "www.nordnet.no" /: "market" /: "options"
        optionName = T.ticker t
    in
    R.req R.GET myUrl R.NoReqBody R.bsResponse $ 
        "currency" =: ("NOK":: Text.Text) 
        <> "underlyingSymbol" =: (optionName :: Text.Text) 
        <> "expireDate" =: (unixTime :: Clock.NominalDiffTime)

download_ :: T.Ticker -> FilePath -> POSIX.POSIXTime -> REIO ()
download_ t filePath unixTime = 
    R.runReq R.defaultHttpConfig (responseGET t unixTime) >>= \bs -> 
    liftIO $ 
        let 
            expiryAsUnixTime = CalendarUtil.unixTimeToInt unixTime
            fileName = Printf.printf "%s/%d.html" filePath expiryAsUnixTime
        in
        Char8.writeFile fileName (R.responseBody bs)
    --liftIO $ Char8.putStrLn (R.responseBody bs)


download :: T.Ticker -> [POSIX.POSIXTime] -> REIO ()
download ticker unixTimes = 
    mkDir ticker >>= \filePath ->
    let
        dlfn = download_ ticker filePath
    in 
    mapM_ dlfn unixTimes