
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified System.Directory as Dir
import qualified Data.Vector as V
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Repos.Common as RC
-- import qualified EtradeJanitor.Netfonds as NF
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Repos.PaperHistory as PH
import qualified EtradeJanitor.Params as PA
import qualified Data.Dates as DT
import qualified Data.Time.Calendar as Cal
import Text.Printf (printf)
import Control.Monad.Reader (runReaderT,ask)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = PA.cmdLineParser >>= work

{-
    case (PA.isMock cmd) of
      True -> mockWork cmd
      False -> work cmd
-}

work :: PA.Params -> IO ()
work params = 
    putStrLn $ show params

{-
processTickers :: T.Tickers -> T.REIO (Either RC.SessionError ())
processTickers tix =
  let
    catNot3 = V.filter (\t -> (T.category t) /= 3) tix
  in
  NF.saveDerivativesTickers tix >>
  NF.saveTradingDepthTickers tix >>
  NF.saveBuyersSellersTickers tix >>
  NF.fetchStockPrices catNot3 >>= \prices ->
  RS.insertStockPrices2 prices

processTickersCat3 :: T.Tickers -> T.REIO ()
processTickersCat3 tix =
  ask >>= \env ->
  if T.isDownloadOnly env == True then
    return ()
  else
    let
      cat3 = V.filter (\t -> (T.category t) == 3) tix
      feed = (PA.feed . T.getParams) env
    in
    liftIO (NF.savePaperHistoryTickers feed cat3) >>
    PH.updateStockPricesTickers cat3

processTickersAllPaperHistory :: T.Tickers -> T.REIO () --  ReaderT T.Env IO ()
processTickersAllPaperHistory tix =
  ask >>= \env ->
  let
    feed = (PA.feed . T.getParams) env
  in
  liftIO (NF.savePaperHistoryTickers feed tix) >>
  PH.updateStockPricesTickers tix

currentFilePath :: FilePath -> IO FilePath
currentFilePath feed =
  DT.getCurrentDateTime >>= \cdt ->
  let today = DT.dateTimeToDay cdt
      (y,m,d) = Cal.toGregorian today
      filePath = printf "%s/%d/%d/%d" feed y m d :: FilePath
  in
  Dir.createDirectoryIfMissing True filePath >>
  pure filePath

main :: IO ()
main = PA.cmdLineParser >>= work

work :: PA.Params -> IO ()
work params =
  RS.tickers (PA.databaseIp params) >>= \tix ->
      case tix of
        Right result ->
          case (PA.allPaper params) of
            True ->
              workPapers params result
            False ->
              workDefault params result
        Left err ->
          putStrLn $ show err

workDefault :: PA.Params -> T.Tickers -> IO ()
workDefault params tix =
  currentFilePath (PA.feed params) >>= \cfp ->
  let
    env = T.Env cfp params
  in
  runReaderT (processTickersCat3 tix) env >>
  runReaderT (processTickers tix) env >>
  putStrLn "Done workDefault!"

workPapers :: PA.Params -> T.Tickers -> IO ()
workPapers params tix =
  let
    env = T.Env "" params
  in
  runReaderT (processTickersAllPaperHistory tix) env >>
  putStrLn "Done workPapers!"

-}