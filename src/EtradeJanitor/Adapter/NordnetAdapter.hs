{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module EtradeJanitor.Adapter.NordnetAdapter where

import Control.Monad.Reader
  ( MonadIO
  , MonadReader
  )
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson as Aeson

-- import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as BL
import Network.HTTP.Req
  ( (/:)
  -- HttpException (..)
  )
import qualified Network.HTTP.Req as R

{- import Rapanui.Common
  ( CritterType (..)
  , Env
  , NordnetHost (..)
  , NordnetPort (..)
  , OptionTicker (..)
  )
import qualified Rapanui.Common as Common
import Rapanui.Critters.OptionPurchase (OptionPurchase)
import Rapanui.StockOption (StockOption)
 -}

-- spotGET :: (MonadReader Env m) => OptionTicker -> m (R.Req R.BsResponse)
-- spotGET (OptionTicker ticker) =
--   Reader.ask >>= \env ->
--     let
--       NordnetHost host = Common.getHost env
--       NordnetPort port = Common.getPort env
--       myUrl = R.http host /: "option" /: ticker
--     in
--       pure $ R.req R.GET myUrl R.NoReqBody R.bsResponse $ R.port port

-- parseStockOptionJson :: BL.ByteString -> Maybe StockOption
-- parseStockOptionJson s =
--   maybe Nothing id $ Aeson.decodeStrict s

-- fetchStockOption :: (MonadIO m, MonadReader Env m) => OptionTicker -> m (Maybe StockOption)
-- fetchStockOption ticker =
--   stockOptionGET ticker >>= \s ->
--     R.runReq R.defaultHttpConfig s >>= \response ->
--       pure $ parseStockOptionJson $ R.responseBody response

x :: Int
x = 3