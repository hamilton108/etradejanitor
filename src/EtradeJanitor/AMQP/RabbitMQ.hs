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
  , myConnection'
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
import           Network.Socket                 ( PortNumber
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , MonadIO
                                                )
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                , Value(..)
                                                , (.=)
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Text                      ( Text )
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.UUID                      ( UUID )
import           EtradeJanitor.Common.Types     ( NordnetExpiry
                                                , Env
                                                , RabbitHost(..)
                                                , RabbitPort(..)
                                                , getRabbitConnection
                                                )


data Payload =
    NordnetPayload
    { msgId :: UUID
    , utcTime :: Int
    , iso8601 :: String
    , ticker :: Text
    , fun :: Text
    , nex :: NordnetExpiry
    , msg :: Text
    }
    deriving Show


instance ToJSON Payload where
  toJSON (NordnetPayload msgId utcTime iso8601 ticker method nex msg) = Aeson.object
    [ "msgid" .= msgId
    , "utctime" .= utcTime
    , "iso8601" .= iso8601
    , "ticker" .= ticker
    , "method" .= method
    , "nx" .= nex
    , "msg" .= msg
    ]
  toEncoding (NordnetPayload msgId utcTime iso8601 ticker method nex msg) = Aeson.pairs
    (  "msgid"
    .= msgId
    <> "utctime"
    .= utcTime
    <> "iso8601" 
    .= iso8601
    <> "ticker"
    .= ticker
    <> "method"
    .= method
    <> "nx"
    .= nex
    <> "msg"
    .= msg
    )

{-
data Pay1 = Pay1
    { p1x :: Int 
    } deriving Show

instance ToJSON Pay1 where 
  toJSON (Pay1 p1x) = Aeson.object
    [ "p1x" .= p1x
    ]

data Pay2 = Pay2
    { p2x :: Int
    , p :: Pay1
    } deriving Show

toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "toObject: value isn't an Object"

instance ToJSON Pay2 where 
  toJSON (Pay2 p2x p) = Aeson.object
    [ "p2x" .= p2x
    , toJSON p
    ]

payx = 
    let 
        pay1 = Pay1 12
        pay2 = Pay2 15 pay1
    in
    Aeson.encode pay2
    -- Pay2 14 pay1
-}


newtype RoutingKey =
  RoutingKey Text

rkInfo :: RoutingKey
rkInfo = RoutingKey "etrade.info"

rkError :: RoutingKey
rkError = RoutingKey "etrade.error"

myExchangeName :: Text
myExchangeName = "etrade"


--myQueueName :: Text
--myQueueName = "hello"

myConnection' :: RabbitHost -> RabbitPort -> IO Connection
myConnection' (RabbitHost host) (RabbitPort port) = 
    let 
        portI = read port :: Integer
        portR = fromIntegral portI :: PortNumber
    in
    AMQP.openConnection' host
                         portR
                         "etradejanitor_vhost"
                         "etradejanitor"
                         "VhCHeUJ40"

myConnection :: RabbitHost -> IO Connection
myConnection (RabbitHost host) = 
    AMQP.openConnection  host
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
  in  case conn of
        Just conn1 -> liftIO $ AMQP.openChannel conn1 >>= \ch ->
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
            >> putStrLn "Done publish"
        Nothing -> liftIO $ putStrLn "No connection object set"

