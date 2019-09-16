module EtradeJanitor.Params where

import Data.Semigroup ((<>))
import Options.Applicative (Parser,info,helper,showDefault,fullDesc,progDesc,(<**>))
import Options.Applicative.Builder (strArgument,strOption,switch,metavar,long,short,value,help)
import Options.Applicative.Extra (execParser)

import qualified EtradeJanitor.Common.Misc as Misc

data Params = 
    Params 
    { databaseIp :: String
    , feed :: String
    , skipDownloadStockPrices :: Bool
    , skipDownloadDerivatives :: Bool
    , skipDbUpdateStocks :: Bool
    , skipIfDownloadFileExists :: Bool
    , showStockTickers :: Bool
    } deriving (Show)

defaultFeed :: String
defaultFeed = Misc.feedRoot ++ "/feed2"

mkParams :: Parser Params
mkParams =
    Params
        <$> strArgument (metavar "IP"  <> help "Database ip address")
        <*> strOption (long "feed" <> short 'f' <> help "Feed path" <> value defaultFeed <> showDefault)
        <*> switch (long "skip-download-stocks" <> short 'q' <> help "Do not download stock prices from Euroinvestor" )
        <*> switch (long "skip-download-derivatives" <> short 'Q' <> help "Do not download derivatives from Nordnet" )
        <*> switch (long "skip-db-update-stocks" <> short 'r' <> help "Do not update database stock prices" )
        <*> switch (long "skip-if-download-exists" <> short 'x' <> help "Do not download if file exists" )
        <*> switch (long "show-stock-tickers" <> short 't' <> help "Show current stock tickers" )
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
    let opts = info (mkParams <**> helper)
                    (fullDesc <> progDesc "EtradeJanitor")
    in
    execParser opts
