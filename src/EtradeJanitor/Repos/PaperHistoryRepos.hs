{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistoryRepos where


import Control.Monad.Except (lift,runExceptT,ExceptT(..))
import Data.Text (Text,pack)
import Data.Functor.Contravariant (contramap)
import qualified Data.ByteString.Char8 as B
import Data.Either.Combinators (mapLeft)

-- Hasql
import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS
import qualified Hasql.Statement as HQ
import qualified Hasql.Decoders as HD
import Hasql.Encoders (Params(..),Value(..),text,int8,param)
import Hasql.Decoders (Row)

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
