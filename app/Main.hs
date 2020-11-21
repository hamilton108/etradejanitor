
{-# LANGUAGE OverloadedStrings #-}

{-
AutoDeriveTypeable
BangPatterns
BinaryLiterals
ConstraintKinds
DataKinds
DefaultSignatures
DeriveDataTypeable
DeriveFoldable
DeriveFunctor
DeriveGeneric
DeriveTraversable
DoAndIfThenElse
EmptyDataDecls
ExistentialQuantification
FlexibleContexts
FlexibleInstances
FunctionalDependencies
GADTs
GeneralizedNewtypeDeriving
InstanceSigs
KindSignatures
LambdaCase
MonadFailDesugaring
MultiParamTypeClasses
MultiWayIf
NamedFieldPuns
NoImplicitPrelude
OverloadedStrings
PartialTypeSignatures
PatternGuards
PolyKinds
RankNTypes
RecordWildCards
ScopedTypeVariables
StandaloneDeriving
TupleSections
TypeFamilies
TypeSynonymInstances
ViewPatterns <<<===

TypeOperators
-}

module Main
  ( main
  )
where

import           Control.Monad.Reader           ( runReaderT
                                                , ask
                                                )
import           Control.Monad.IO.Class         ( liftIO )

--import qualified System.Directory as Dir
--import qualified Data.Vector as V

import           EtradeJanitor.Common.Types     ( REIO )
import qualified EtradeJanitor.Common.Types    as Types
import qualified EtradeJanitor.Common.CalendarUtil
                                               as CalendarUtil

--import qualified EtradeJanitor.Repos.Common as RC
--import qualified EtradeJanitor.Netfonds as NF

--import qualified EtradeJanitor.EuroInvestor as EuroInvestor
--import qualified EtradeJanitor.PaperHistory as PaperHistory
import qualified EtradeJanitor.Repos.Yahoo.PaperHistory
                                               as PaperHistory

import qualified EtradeJanitor.Repos.Stocks    as Stocks
import qualified EtradeJanitor.Params          as PA
import qualified EtradeJanitor.Repos.Nordnet   as Nordnet
--import qualified Data.Dates as DT
--import qualified Data.Time.Calendar as Cal
--import Text.Printf (printf)
--import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = PA.cmdLineParser >>= work

{-
main2 :: IO ()
main2 = 
    let
        p = PA.Params { 
              PA.databaseIp = "172.17.0.2"
            --, PA.feed = "/home/rcs/opt/haskell/etradejanitor/feed2"
            , PA.feed = "/home/rcs/opt/haskell/etradejanitor/python"
            , PA.skipDownloadDerivatives = True 
            , PA.skipDbUpdateStocks = False
            , PA.skipIfDownloadFileExists = True
            , PA.showStockTickers = True
        }
    in work p
-}

showStockTickers :: Types.Tickers -> REIO ()
showStockTickers tix = ask >>= \env ->
  let prms   = Types.getParams env
      doShow = PA.showStockTickers prms
  in  case doShow of
        True  -> liftIO $ mapM_ (putStrLn . show) tix
        False -> pure ()

work :: PA.Params -> IO ()
work params = putStrLn (show params) >> CalendarUtil.today >>= \today ->
  let env = Types.Env params today
  in
    Stocks.tickers (PA.databaseIp params) >>= \tix -> case tix of
      Right result ->
        runReaderT (Types.runApp $ showStockTickers result) env
          >> runReaderT (Types.runApp $ Nordnet.downloadOpeningPrices result)
                        env
          >> runReaderT (Types.runApp $ Nordnet.openingPricesToRedis result) env
          >> runReaderT
               (Types.runApp $ Nordnet.downloadDerivativePrices result)
               env
          >> runReaderT
               (Types.runApp $ PaperHistory.updateStockPricesTickers result)
               env
      Left err -> putStrLn $ show err


