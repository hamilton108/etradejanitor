{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Common (conn,SessionError(..)) where

import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS

conn :: HC.Settings
conn = HC.settings "172.17.0.2" 5432 "trader" "ok" "trader"

data SessionError =
  ConnErr (HC.ConnectionError) |
  SessionError (HS.QueryError)
  deriving (Show, Eq)
