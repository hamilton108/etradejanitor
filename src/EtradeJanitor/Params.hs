module EtradeJanitor.Params where

import Options.Applicative
  ( Parser
  , fullDesc
  , helper
  , info
  , progDesc
  , showDefault
  , (<**>)
  )
import Options.Applicative.Builder
  ( help
  , long
  , metavar
  , short
  , strArgument
  , strOption
  , switch
  , value
  , option
  , auto
  )
import Options.Applicative.Extra (execParser)

import qualified EtradeJanitor.Common.Misc as Misc

data Params = Params
  { databaseIp :: String
  , redisHost :: String
  , rabbitHost :: String
  , nordnetHost :: String
  , redisPort :: Int
  , redisDatabase :: Integer
  , rabbitPort :: Integer
  , nordnetPort :: Int
  , feed :: String
  , downloadDerivatives :: Bool
  , dbUpdateStocks :: Bool
  , skipIfDownloadFileExists :: Bool
  , showStockTickers :: Bool
  , openingPricesToRedis :: Bool
  --, nordnetServiceHost :: String
  }
  deriving (Show)

defaultFeed :: String
defaultFeed = Misc.feedRoot ++ "/feed2"

defaultRedisDatabase :: Integer
defaultRedisDatabase = 0

defaultRedisPort :: Int 
defaultRedisPort = 6379

defaultRabbitPort :: Integer
defaultRabbitPort = 5672

defaultNordnetPort :: Int
defaultNordnetPort = 8082

mkParams :: Parser Params
mkParams =
  Params
    <$> strArgument (metavar "IP" <> help "Database ip address")
    <*> strArgument (metavar "REDIS" <> help "Redis ip address")
    <*> strArgument (metavar "RABBIT" <> help "RabbitMQ ip address")
    <*> strArgument (metavar "NORDNET" <> help "Nordnet Service ip address")
    <*> option auto 
      ( long "redis-port"
          <> short 'p'
          <> help "Redis port"
          <> value defaultRedisPort
          <> showDefault
      )
    <*> option auto 
      ( long "redis-database"
          <> short 'd'
          <> help "Redis database"
          <> value defaultRedisDatabase
          <> showDefault
      )
    <*> option auto 
      ( long "rabbit-port"
          <> short 'l'
          <> help "RabbitMQ port"
          <> value defaultRabbitPort
          <> showDefault
      )
    <*> option auto
      ( long "nordnet-port"
          <> short 'm'
          <> help "Nordnet Service port"
          <> value defaultNordnetPort
          <> showDefault
      )
    <*> strOption
      ( long "feed"
          <> short 'f'
          <> help "Feed path"
          <> value defaultFeed
          <> showDefault
      )
    <*> switch
      ( long "download-derivatives"
          <> short 'q'
          <> help
            "Download derivatives from Nordnet"
      )
    <*> switch
      ( long "db-update-stocks"
          <> short 'r'
          <> help
            "Update database stock prices"
      )
    <*> switch
      ( long "skip-if-download-exists"
          <> short 'x'
          <> help
            "Do not download if file exists"
      )
    <*> switch
      ( long "show-stock-tickers"
          <> short 't'
          <> help
            "Show current stock tickers"
      )
    <*> switch
      ( long "opening-prices-to-redis"
          <> short 'o'
          <> help
            "Download and save opening prices to Redis"
      )

{-
data Params = Params {
    databaseIp :: String
    , allPaper :: Bool
    , downloadOnly :: Bool
    , htmlOnly :: Bool
    , feed :: String
  } deriving (Show)

mkParams :: Parser Params
mkParams =
  Params
    <$> strArgument (metavar "IP"  <> help "Database ip address")
    <*> switch (long "all-paper" <> short 'p' <> help "All stock prices from paper history")
    <*> switch (long "download-only" <> short 'd' <> help "Download only, no update database")
    <*> switch (long "html-only" <> short 'x' <> help "Download html only, no trading depth or buyers/sellers")
    <*> strOption (long "feed" <> short 'f' <> help "Feed path" <> value "/home/rcs/opt/haskell/etradejanitor/feed" <> showDefault)
-}

cmdLineParser :: IO Params
cmdLineParser =
  let
    opts = info (mkParams <**> helper) (fullDesc <> progDesc "EtradeJanitor")
  in
    execParser opts
