{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistory where


-- Hasql
import qualified Hasql.Session as HS

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

s :: Int -> String
s v =
    printf "insert into stockmarket.ax (ar) values (%d)" v

insertRowDemo :: Int -> HS.Session ()
insertRowDemo v =
  let
    stmt = B.pack $ printf "insert into stockmarket.ax (ar) values (%d)" v
  in
    HS.statement () $ C.plain $ stmt

demo :: IO (Either C.SessionError ())
demo =
  C.session $ insertRowDemo 2 >> insertRowDemo 3

processLine :: String -> T.StockPrice
processLine line =
        let
          [dx',_,_,opn',hi',lo',cls',vol',_] = splitOn "," line
          dxx = asDateString dx'
        in
          -- forM_ lxx putStrLn >>
          T.StockPrice dxx opn' hi' lo' cls' vol'

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

asSql :: T.StockPrice -> String -- B.ByteString
asSql sp =
    -- B.pack $
    printf
      "insert into stockmarket.stockprice (ticker_id,dx,opn,hi,lo,cls,vol) values (1,'%s',%s,%s,%s,%s,%s)"
      (T.dx sp)
      (T.opn sp)
      (T.hi sp)
      (T.lo sp)
      (T.cls sp)
      (T.vol sp)

insertRow :: T.StockPrice -> HS.Session ()
insertRow sp =
  let
    -- stmt = B.pack $ printf "insert into stockmarket.ax (ar) values (%d)" 20
    stmt = B.pack $ asSql sp
  in
    HS.statement () $ C.plain $ stmt

insertRows :: [T.StockPrice] -> IO (Either C.SessionError ())
insertRows stockPrices =
  C.session $
  forM_ stockPrices insertRow


updateStockPrices :: T.Ticker -> IO ()
updateStockPrices ticker =
    let
        tickerCsv :: String
        tickerCsv = printf "%s.csv" ticker
    in
      openFile tickerCsv ReadMode >>= \inputHandle ->
      hSetEncoding inputHandle latin1 >> -- utf8
      hGetContents inputHandle >>= \theInput ->
      let
        lx = L.lines theInput
        stockPrices = map processLine lx
      in
        -- forM lx processLine >>= \stockPrices ->
        --forM_ stockPrices (putStrLn . asSql) >>
        insertRows stockPrices >>
        putStrLn "Done!"

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