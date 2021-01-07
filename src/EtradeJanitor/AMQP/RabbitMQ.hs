{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module EtradeJanitor.AMQP.RabbitMQ
  ( Payload(..)
  , RoutingKey
  , publish
  , rkInfo
  , rkError
  , myConnection
  )
where

import qualified Network.AMQP                  as AMQP
import           Network.AMQP                   ( Ack
                                                , Channel
                                                , Connection
                                                , DeliveryMode(..)
                                                , Envelope
                                                , Message
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , MonadIO
                                                )
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                , (.=)
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Text                      ( Text )
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.UUID.V4                   ( nextRandom )
import           EtradeJanitor.Common.Types     ( NordnetExpiry
                                                , Env
                                                , getRabbitConnection
                                                )

data Payload =
    Payload
    { utcTime :: Int
    , nex :: NordnetExpiry
    , msg :: Text
    }
    deriving Show


instance ToJSON Payload where
  toJSON (Payload utcTime nex msg) =
    Aeson.object ["utcTime" .= utcTime, "nx" .= nex, "msg" .= msg]
  toEncoding (Payload utcTime nex msg) =
    Aeson.pairs ("utcTime" .= utcTime <> "nx" .= nex <> "msg" .= msg)

newtype RoutingKey =
  RoutingKey Text

rkInfo :: RoutingKey
rkInfo = RoutingKey "etrade.info"

rkError :: RoutingKey
rkError = RoutingKey "etrade.error"

myExchangeName :: Text
myExchangeName = "etrade"


myQueueName :: Text
myQueueName = "hello"

myConnection :: IO Connection
myConnection = AMQP.openConnection "172.20.1.4"
                                   "etradejanitor_vhost"
                                   "etradejanitor"
                                   "VhCHeUJ40"

myExchange :: Channel -> IO ()
myExchange ch = AMQP.declareExchange
  ch
  AMQP.newExchange { AMQP.exchangeName       = myExchangeName
                   , AMQP.exchangeType       = "topic"
                   , AMQP.exchangePassive    = False
                   , AMQP.exchangeDurable    = False
                   , AMQP.exchangeAutoDelete = False
                   , AMQP.exchangeInternal   = False
                   }

{-
myRoutingKey :: Text
myRoutingKey = "etrade.info"

doBindQueue :: Channel -> Text -> IO ()
doBindQueue ch queueName =
    AMQP.bindQueue ch queueName myExchangeName myRoutingKey 

myQueue :: Channel -> IO (Text,Int,Int)
myQueue ch = 
    AMQP.declareQueue ch AMQP.newQueue {AMQP.queueName       = myQueueName,
                               AMQP.queueAutoDelete = False,
                            AMQP.queueDurable    = False} 

-}


publish :: (MonadIO m, MonadReader Env m) => Payload -> RoutingKey -> m ()
publish p (RoutingKey rk) = Reader.ask >>= \env ->
  let conn = getRabbitConnection env
  in  liftIO $ AMQP.openChannel conn >>= \ch ->
        putStrLn "Channel opened"
          >> myExchange ch
          >> putStrLn "myExchange"
          >>

      --myQueue ch >>= \(q,_,_) ->

      --doBindQueue ch q >>
             AMQP.publishMsg
               ch
               myExchangeName
               rk
               (AMQP.newMsg { AMQP.msgBody         = Aeson.encode p
                            , AMQP.msgDeliveryMode = Just NonPersistent
                            }
               )
          >>
            putStrLn "Done publish"

