module EtradeJanitor.Params where

import Data.Semigroup ((<>))
import Options.Applicative (Parser,info,helper,showDefault,fullDesc,progDesc,(<**>))
import Options.Applicative.Builder (strArgument,strOption,switch,metavar,long,short,value,help)
import Options.Applicative.Extra (execParser)

data Params = 
    Params 
    { databaseIp :: String
    , feed :: String
    , downloadOnly :: Bool
    , updateDbOnly :: Bool
    } deriving (Show)

mkParams :: Parser Params
mkParams =
    Params
        <$> strArgument (metavar "IP"  <> help "Database ip address")
        <*> strOption (long "feed" <> short 'f' <> help "Feed path" <> value "/home/rcs/opt/haskell/etradejanitor/feed2" <> showDefault)
        <*> switch (long "download-only" <> short 'q' <> help "Download only, no update database" )
        <*> switch (long "db-only" <> short 'Q' <> help "Update database only, no downloads" )
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
