{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS -Wno-unused-imports #-}

module Demo4 where

import Control.Monad.Reader (ReaderT,ask,runReaderT)
import Control.Monad.IO.Class (liftIO)

import EtradeJanitor.Repos.Nordnet.Derivatives as Derivatives

