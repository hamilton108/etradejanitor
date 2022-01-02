module EtradeJanitor.Params where

import           Options.Applicative            ( Parser
                                                , info
                                                , helper
                                                , showDefault
                                                , fullDesc
                                                , progDesc
                                                , (<**>)
                                                )
import           Options.Applicative.Builder    ( strArgument
                                                , strOption
                                                , switch
                                                , metavar
                                                , long
                                                , short
                                                , value
                                                , help
                                                )
import           Options.Applicative.Extra      ( execParser )

import qualified EtradeJanitor.Common.Misc     as Misc

data Params =
    Params
    { databaseIp :: String
    , redisHost :: String
    , redisPort :: String
    , redisDatabase :: String
    , rabbitHost :: String
    , rabbitPort :: String
    , feed :: String
    , downloadDerivatives :: Bool
    , dbUpdateStocks :: Bool
    , skipIfDownloadFileExists :: Bool
    , showStockTickers :: Bool
    , openingPricesToRedis :: Bool
    } deriving (Show)

defaultFeed :: String
defaultFeed = Misc.feedRoot ++ "/feed2"

defaultRedisDatabase :: String
defaultRedisDatabase = "0"

defaultRedisPort :: String
defaultRedisPort = "6379"

defaultRabbitPort :: String
defaultRabbitPort = "5672"

mkParams :: Parser Params
mkParams =
  Params
    <$> strArgument (metavar "IP" <> help "Database ip address")
    <*> strArgument (metavar "REDIS" <> help "Redis ip address")
    <*> strArgument (metavar "RABBIT" <> help "RabbitMQ ip address")
    <*> strOption
          (  long "redis-port"
          <> short 'p'
          <> help "Redis port"
          <> value defaultRedisPort
          <> showDefault
          )
    <*> strOption
          (  long "redis-database"
          <> short 'd'
          <> help "Redis database"
          <> value defaultRedisDatabase
          <> showDefault
          )
    <*> strOption
          (  long "rabbit-port"
          <> short 'l'
          <> help "RabbitMQ port"
          <> value defaultRabbitPort
          <> showDefault
          )
    <*> strOption
          (  long "feed"
          <> short 'f'
          <> help "Feed path"
          <> value defaultFeed
          <> showDefault
          )
    <*> switch
          (long "download-derivatives" <> short 'q' <> help
            "Download derivatives from Nordnet"
          )
    <*> switch
          (long "db-update-stocks" <> short 'r' <> help
            "Update database stock prices"
          )
    <*> switch
          (long "skip-if-download-exists" <> short 'x' <> help
            "Do not download if file exists"
          )
    <*> switch
          (long "show-stock-tickers" <> short 't' <> help
            "Show current stock tickers"
          )
    <*> switch
          (long "opening-prices-to-redis" <> short 'o' <> help
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
  let opts = info (mkParams <**> helper) (fullDesc <> progDesc "EtradeJanitor")
  in  execParser opts
