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

demo :: IO ()
demo = HC.acquire C.conn >>= \c ->
    let msg = case c of
            Right cx -> "YES"
            Left err -> case err of
                            Just xxx -> B.unpack xxx
                            Nothing -> "??????"

    in
    putStrLn msg

plain :: B.ByteString -> HQ.Statement () ()
plain sql =
  HQ.Statement sql mempty HD.unit False

insertRow :: HS.Session ()
insertRow =
    HS.statement () $ plain $
    "insert into ax (ar) values (1)"

session :: HS.Session a -> IO (Either C.SessionError a)
session session =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ fmap (mapLeft C.ConnErr) $ HC.acquire C.conn
    use connection =
      ExceptT $
      fmap (mapLeft C.SessionError) $
      HS.run session connection
    release connection =
      lift $ HC.release connection

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
