{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistory where


-- Hasql
import qualified Hasql.Session as HS

import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.List.Split (splitOn)
import Control.Monad (forM,forM_)
import System.IO (openFile,hSetEncoding,hGetContents,latin1,IOMode(..))

-- Local
import qualified EtradeJanitor.Repos.Common as C
import qualified EtradeJanitor.Common.Types as T

-- oid | ticker_id |     dx     |  opn  |  hi   |  lo   |  cls  |   vol

-- s :: Int -> String
-- s v =
--     printf "insert into stockmarket.ax (ar) values (%d)" v
--
-- insertRowDemo :: Int -> HS.Session ()
-- insertRowDemo v =
--   let
--     stmt = B.pack $ printf "insert into stockmarket.ax (ar) values (%d)" v
--   in
--     HS.statement () $ C.plain $ stmt
--
-- demo :: IO (Either C.SessionError ())
-- demo =
--   C.session $ insertRowDemo 2 >> insertRowDemo 3

processLine :: String -> T.StockPrice
processLine line =
        let
          [dx',_,_,opn',hi',lo',cls',vol',_] = splitOn "," line
          dxx = asDateString dx'
        in
          -- forM_ lxx putStrLn >>
          T.StockPrice dxx opn' hi' lo' cls' vol'

fetchCsv :: T.Ticker -> IO [String]
fetchCsv tickr =
    let
        tickerCsv :: String
        tickerCsv = printf "%s.csv" tickr
    in
      openFile tickerCsv ReadMode >>= \inputHandle ->
      hSetEncoding inputHandle latin1 >> -- utf8
      hGetContents inputHandle >>= \theInput ->
      return $ tail $ L.lines theInput

fetchStockPrices :: T.Ticker -> IO [T.StockPrice]
fetchStockPrices tickr =
  fetchCsv tickr >>= \lx ->
  return $ map processLine lx

--one tickr = fetchStockPrices tickr >>= \t ->
--return $ head t

asDateString :: String -> String
asDateString v =
  let
    year :: String
    year = take 4 v

    month :: String
    month = take 2 $ drop 4 v

    day :: String
    day = take 2 $ drop 6 v
  in
    printf "%s-%s-%s" year month day

asSql :: T.Ticker -> T.StockPrice -> String -- B.ByteString
asSql (T.Ticker oid _) sp =
    -- B.pack $
    printf
      "insert into stockmarket.stockprice (ticker_id,dx,opn,hi,lo,cls,vol) values (oid,'%s',%s,%s,%s,%s,%s)"
      (T.dx sp)
      (T.opn sp)
      (T.hi sp)
      (T.lo sp)
      (T.cls sp)
      (T.vol sp)

insertRow :: T.Ticker -> T.StockPrice -> HS.Session ()
insertRow tickr sp =
  let
    -- stmt = B.pack $ printf "insert into stockmarket.ax (ar) values (%d)" 20
    stmt = B.pack $ asSql tickr sp
  in
    HS.statement () $ C.plain $ stmt

insertRows :: T.Ticker -> [T.StockPrice] -> IO (Either C.SessionError ())
insertRows tickr stockPrices =
  C.session $
  forM_ stockPrices (insertRow tickr)


updateStockPrices :: T.Ticker -> IO (Either C.SessionError ())
updateStockPrices tickr =
  fetchStockPrices tickr >>= \stockPrices ->
  insertRows tickr stockPrices
  -- insertRows tickr stockPrices >>= \e ->
  -- case e of
  --   Right () -> putStrLn "Done!"
  --   Left err -> putStrLn (show err)


updateStockPricesTickers :: [T.Ticker] -> IO ()
updateStockPricesTickers tix =
  forM_ tix updateStockPrices

-- data Person =
--     Person { name :: Text, gender :: Gender, age :: Int }
--
-- data Gender =
--     Male | Female
--
-- personParams :: Params Person
-- personParams =
--     contramap name (param text) <>
--     contramap gender (param genderValue) <>
--     contramap (fromIntegral . age) (param int8)
--
-- genderValue :: Value Gender
-- genderValue =
--     contramap genderText text
--     where
--         genderText ge =
--             case ge of
--                 Male -> "male"
--                 Female -> "female"
