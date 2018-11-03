
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified System.Directory as Dir
import qualified Data.Vector as V
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Repos.Common as RC
import qualified EtradeJanitor.Netfonds as NF
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Repos.PaperHistory as RP
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
  liftIO $ RS.insertStockPrices2 prices



processTickers2 :: T.Tickers -> IO ()
processTickers2 tix =
  let
    cat3 = V.filter (\t -> (T.category t) == 3) tix
  in
  NF.savePaperHistoryTickers cat3 >>
  RP.updateStockPricesTickers cat3

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

work :: PA.Params -> IO ()
work params =
  putStrLn (PA.containerIp params) 

xmain :: IO ()
xmain =
  RS.tickers >>= \tix ->
      case tix of
        Right result ->
          processTickers2 result >>
          currentFilePath >>= \cfp ->
          runReaderT (processTickers result) (T.Env cfp) >>
          putStrLn "Done"
        Left err ->
          putStrLn $ show err
