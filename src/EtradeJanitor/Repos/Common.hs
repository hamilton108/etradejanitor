{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Common (conn,plain,SessionError(..)) where

import Control.Monad.Except (lift,runExceptT,ExceptT(..))
import Data.Either.Combinators (mapLeft)
import qualified Data.ByteString.Char8 as B

-- Hasql
import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS
import qualified Hasql.Statement as HQ
import qualified Hasql.Decoders as HD

-- Local

conn :: HC.Settings
conn = HC.settings "172.17.0.2" 5432 "trader" "ok" "trader"

data SessionError =
  ConnErr (HC.ConnectionError) |
  SessionError (HS.QueryError)
  deriving (Show, Eq)


plain :: B.ByteString -> HQ.Statement () ()
plain sql =
  HQ.Statement sql mempty HD.unit False

session :: HS.Session a -> IO (Either SessionError a)
session session =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ fmap (mapLeft ConnErr) $ HC.acquire conn
    use connection =
      ExceptT $
      fmap (mapLeft SessionError) $
      HS.run session connection
    release connection =
      lift $ HC.release connection