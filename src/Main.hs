
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified System.Directory as Dir
import qualified Data.Vector as V
import qualified EtradeJanitor.Common.Types as T
import qualified EtradeJanitor.Netfonds as NF
import qualified EtradeJanitor.Repos.Stocks as RS
import qualified EtradeJanitor.Repos.PaperHistory as RP
import qualified Data.Dates as DT
import qualified Data.Time.Calendar as Cal
import Text.Printf (printf)
import Control.Monad.Reader (ReaderT,runReaderT,ask)
import Control.Monad.IO.Class (liftIO)

-- processTickers :: T.Tickers -> IO ()
-- processTickers tix =
--     let
--         cat3 = V.filter (\t -> (T.category t) == 3) tix
--     in
--         NF.savePaperHistoryTickers cat3 >>
--         NF.saveDerivativesTickers tix >>
--         RP.updateStockPricesTickers cat3

-- xprocessTickers :: T.Tickers -> ReaderT T.Env IO ()
-- xprocessTickers tix =
--   ask >>= \env ->
--   liftIO $
--   let
--     putStrLn_ = putStrLn . (++(T.getHtmlPath env)) . Tx.unpack . T.ticker
--     one = V.head tix
--     rest = V.tail tix
--   in
--     V.mapM_ putStrLn_ rest >>
--     putStrLn "Here I come! " >>
--     putStrLn_ one >>
--     pure ()


processTickers :: T.Tickers -> ReaderT T.Env IO ()
processTickers tix =
  NF.saveDerivativesTickers tix

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
main =
  RS.tickers >>= \tix ->
      case tix of
        Right result ->
          currentFilePath >>= \cfp ->
          runReaderT (processTickers result) $ T.Env cfp
        Left err ->
          putStrLn (show err)
