{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistoryRepos where


-- Hasql
import qualified Hasql.Session as HS

-- Local
import qualified EtradeJanitor.Repos.Common as C
import qualified EtradeJanitor.Common.Types as T

-- oid | ticker_id |     dx     |  opn  |  hi   |  lo   |  cls  |   vol


insertRow :: HS.Session ()
insertRow =
    HS.statement () $ C.plain $
    "insert into stockmarket.ax (ar) values (1)"


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
