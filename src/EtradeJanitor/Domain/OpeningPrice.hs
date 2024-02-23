{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module EtradeJanitor.Domain.OpeningPrice where

import Data.Aeson (FromJSON (..))
import GHC.Generics (Generic)

data OpeningPrice = OpeningPrice
  { oid :: Int 
  , price :: Float
  }
  deriving (Eq, Show, Generic)

instance FromJSON OpeningPrice 