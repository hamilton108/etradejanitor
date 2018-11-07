
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified System.Directory as Dir
import qualified Data.Vector as V
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Repos.Common as RC
import qualified EtradeJanitor.Netfonds as NF
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Repos.PaperHistory as PH
import qualified EtradeJanitor.Params as PA
import qualified Data.Dates as DT
import qualified Data.Time.Calendar as Cal
import Text.Printf (printf)
import Control.Monad.Reader (ReaderT,runReaderT)
import Control.Monad.IO.Class (liftIO)

processTickers :: T.Tickers -> ReaderT T.Env IO (Either RC.SessionError ())
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
  let
    cat3 = V.filter (\t -> (T.category t) == 3) tix
  in
  liftIO (NF.savePaperHistoryTickers cat3) >>
  PH.updateStockPricesTickers cat3

processTickersAllPaperHistory :: T.Tickers -> T.REIO () --  ReaderT T.Env IO ()
processTickersAllPaperHistory tix =
  liftIO (NF.savePaperHistoryTickers tix) >>
  PH.updateStockPricesTickers tix
-- xx =
--   RS.tickers >>= \tix ->
--   case tix of
--     Right result ->
--       pure $ V.filter (\t -> (T.category t) == 3) result
--     Left err ->
--       putStrLn (show err) >>
--       pure V.empty



xcurrentFilePath :: IO FilePath
xcurrentFilePath = pure "/home/rcs/opt/haskell/etradejanitor/feed/2018/10/31"

currentFilePath :: IO FilePath
currentFilePath =
  DT.getCurrentDateTime >>= \cdt ->
  let today = DT.dateTimeToDay cdt
      (y,m,d) = Cal.toGregorian today
      filePath = printf "%s/%d/%d/%d" T.feed y m d :: FilePath
  in
  Dir.createDirectoryIfMissing True filePath >>
  pure filePath

main :: IO ()
main = PA.cmdLineParser >>= work

-- main :: IO ()
-- main = work $ PA.Params "172.17.0.2"

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
  currentFilePath >>= \cfp ->
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
