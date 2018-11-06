module EtradeJanitor.Params where

import Data.Semigroup ((<>))
import Options.Applicative (Parser,info,helper,fullDesc,progDesc,(<**>))
import Options.Applicative.Builder (strArgument,switch,metavar,long,short,help)
import Options.Applicative.Extra (execParser)

data Params = Params {
    databaseIp :: String
    , allPaper :: Bool
  } deriving (Show)

mkParams :: Parser Params
mkParams =
  Params
    <$> strArgument (metavar "IP"  <> help "Database ip address")
    <*> switch (long "all-paper" <> short 'p' <> help "All stock prices from paper history")

cmdLineParser :: IO Params
cmdLineParser =
  let opts = info (mkParams <**> helper)
                  (fullDesc <> progDesc "EtradeJanitor")
  in
  execParser opts
