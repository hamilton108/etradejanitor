{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistory where


-- Hasql
import qualified Hasql.Session as HS

import Text.Printf (printf)
import qualified Data.ByteString.Char8 as B

-- Local
import qualified EtradeJanitor.Repos.Common as C
import qualified EtradeJanitor.Common.Types as T

-- oid | ticker_id |     dx     |  opn  |  hi   |  lo   |  cls  |   vol

s :: Int -> String
s v =
    printf "insert into stockmarket.ax (ar) values (%d)" v

insertRow :: Int -> HS.Session ()
insertRow v =
  let
    stmt = B.pack $ printf "insert into stockmarket.ax (ar) values (%d)" v
  in
    HS.statement () $ C.plain $ stmt

demo :: IO (Either C.SessionError ())
demo =
  C.session $ insertRow 2 >> insertRow 3

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
