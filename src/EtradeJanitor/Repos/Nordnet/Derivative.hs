{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Nordnet.Derivative where

import qualified Control.Monad.Reader as Reader
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as Text
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Text.Printf as Printf

import Network.HTTP.Req ((=:))
import qualified Network.HTTP.Req as R
import qualified Data.ByteString.Char8 as Char8

import qualified System.Directory as Directory

import qualified EtradeJanitor.Params as Params
import EtradeJanitor.Common.Types (REIO,getParams)
import qualified EtradeJanitor.Common.Misc as Misc
import qualified EtradeJanitor.Common.Types as T

-- import qualified EtradeJanitor.Common.Types as Types

-- https://www.nordnet.no/market/options?currency=NOK&underlyingSymbol=BAKKA&expireDate=1565906400000

nordNetUrl :: Text.Text
nordNetUrl = "www.nordnet.no/market/options" -- ?currency=NOK&underlyingSymbol=BAKKA&expireDate=1565906400000

newtype Ticker = 
    Ticker { getTicker :: String }
    deriving (Show)


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

pathNameFor :: Ticker -> Calendar.Day -> REIO String
pathNameFor (Ticker ticker) curDay = 
    Reader.ask >>= \env ->
    let 
        feed = (Params.feed . getParams) env
        (y,m,d) = Calendar.toGregorian curDay
    in
    pure $ Printf.printf "%s/%d/%d/%d/%s" feed y m d ticker

mkDir :: Ticker -> Calendar.Day -> REIO ()
mkDir ticker curDay = 
    pathNameFor ticker curDay >>= \pn ->
    liftIO $ Directory.createDirectoryIfMissing True pn
        
downloadResponse :: Ticker -> POSIX.POSIXTime -> R.Req R.BsResponse 
downloadResponse t posix = 
    let
        myUrl = 
            R.https nordNetUrl 
        --optionName = Types.ticker t
    in
    R.req R.GET myUrl R.NoReqBody R.bsResponse $ 
        "currency" =: ("NOK":: Text.Text) 
        <> "underlyingSymbol" =: ("BAKKA" :: Text.Text) 
        <> "expireDate" =: (123213124 :: Int)

download_ :: Ticker -> POSIX.POSIXTime -> REIO ()
download_ (Ticker ticker) unixTime = 
    pure ()


download :: Ticker -> Calendar.Day -> [POSIX.POSIXTime] -> REIO ()
download ticker curDay unixTimes = 
    mkDir ticker curDay >>
    let
        dlfn = download_ ticker 
    in 
    mapM_ dlfn unixTimes