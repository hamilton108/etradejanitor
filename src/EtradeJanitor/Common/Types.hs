{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EtradeJanitor.Common.Types where

import Control.Monad.Catch
  ( MonadCatch
  , MonadMask
  , MonadThrow
  )
-- (formatString,PrintfArg(..))

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Control.Monad.State
  ( MonadState
  , StateT
  )
import qualified Data.Int as DI
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Time.Calendar as Cal
import Data.UUID (UUID)
import qualified Data.Vector as DV
import qualified Text.Printf as TP

import qualified EtradeJanitor.Params as PA
import Network.AMQP (Connection)

-- feed :: FilePath
-- feed = "/home/rcs/opt/haskell/etradejanitor/feed2"

type NordnetExpiry = Int -- POSIX.POSIXTime

-- type REIO = ReaderT Env IO

-- newtype DbIP = DbIP { getIp :: String }

data Env = Env
  { getParams :: PA.Params
  , getDownloadDate :: Cal.Day
  , getRabbitConnection :: Maybe Connection
  , getUUID :: UUID
  } -- deriving (Show)

data OpeningPrice = OpeningPrice
  { opTicker :: Text
  , price :: String
  }
  deriving (Eq, Show)

type AppState = [Ticker]

newtype PosixTimeInt = PosixTimeInt Int deriving (Show)

newtype Iso8601 = Iso8601 String deriving (Show)

data TimeInfo = TimeInfo
  { posixTimeInt :: PosixTimeInt
  , iso8601 :: Iso8601
  }
  deriving (Show)

newtype RabbitHost = RabbitHost String

newtype RabbitPort = RabbitPort Integer

getRabbitHost :: PA.Params -> RabbitHost
getRabbitHost p =
  RabbitHost $ PA.rabbitHost p

getRabbitPort :: PA.Params -> RabbitPort
getRabbitPort p =
  RabbitPort $ PA.rabbitPort p

newtype REIO a = REIO
  { runApp :: ReaderT Env IO a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadReader Env
    )

newtype REIO2 a = REIO2
  { runApp2 :: ReaderT Env (StateT AppState IO) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadReader Env
    , MonadState AppState
    )

{-
isHtmlOnly :: Env -> Bool
isHtmlOnly env =
  (PA.htmlOnly . getParams) env

isDownloadOnly :: Env -> Bool
isDownloadOnly env =
  (PA.downloadOnly . getParams) env
-}

data Ticker = Ticker
  { oid :: DI.Int64
  , ticker :: Text
  , category :: DI.Int64
  , date :: Cal.Day
  }
  deriving (Eq, Show)

type Tickers = DV.Vector Ticker

instance TP.PrintfArg Ticker where
  formatArg (Ticker _ t _ _) fmt = TP.formatString (Tx.unpack t) fmt

data IsoDate = IsoDate
  { year :: String
  , month :: String
  , day :: String
  }
  deriving (Show)

isoDateStr :: IsoDate -> String
isoDateStr (IsoDate y m d) = y ++ "-" ++ m ++ "-" ++ d

data StockPrice = StockPrice
  { tick :: Ticker
  , dx2 :: Cal.Day
  , opn2 :: Float
  , hi2 :: Float
  , lo2 :: Float
  , cls2 :: Float
  , vol2 :: DI.Int64
  }
  deriving (Eq, Show)

data Envx a = Envx
  { ax :: Int
  , bx :: a
  }

newtype REIOX a = REIOX
  { runAppx :: ReaderT (Envx Int) (StateT AppState IO) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadReader (Envx Int)
    , MonadState AppState
    )
