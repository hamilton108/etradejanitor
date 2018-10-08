{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.Common (conn) where

import qualified Hasql.Connection as Connection

conn :: Connection.Settings
conn = Connection.settings "172.17.0.2" 5432 "trader" "ok" "trader" 