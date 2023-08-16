{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Common
  ( conn
  , plain
  , session
  , SessionError (..)
  )
where

import Control.Monad.Except
  ( ExceptT (..)
  , lift
  , runExceptT
  )
import qualified Data.ByteString.Char8 as B
import Data.Either.Combinators (mapLeft)

-- Hasql
import qualified Hasql.Connection as HC
import qualified Hasql.Decoders as HD
import qualified Hasql.Session as HS
import qualified Hasql.Statement as HQ

-- Local

conn :: B.ByteString -> HC.Settings
conn dbIp = HC.settings dbIp 5432 "trader" "ok" "trader"

data SessionError
  = ConnErr HC.ConnectionError
  | SessionError HS.QueryError
  | NoOp
  deriving (Show, Eq)

plain :: B.ByteString -> HQ.Statement () ()
plain sql = HQ.Statement sql mempty HD.noResult False

session :: String -> HS.Session a -> IO (Either SessionError a)
session dbIp sess = runExceptT $ acquire >>= \c -> use c <* release c
 where
  acquire = ExceptT $ fmap (mapLeft ConnErr) $ HC.acquire $ conn $ B.pack dbIp
  use connection =
    ExceptT $ fmap (mapLeft SessionError) $ HS.run sess connection
  release connection = lift $ HC.release connection
