module EtradeJanitor.Params where

import Data.Semigroup ((<>))
import Options.Applicative (Parser,info,helper,fullDesc,progDesc,(<**>))
import Options.Applicative.Builder (strArgument,metavar,help)
import Options.Applicative.Extra (execParser)

data Params = Params {
    containerIp :: String
  }

mkParams :: Parser Params
mkParams =
  Params <$>
    strArgument
    (metavar "IP"  <> help "Docker container ip adress")

cmdLineParser :: IO Params
cmdLineParser =
  let opts = info (mkParams <**> helper)
                  (fullDesc <> progDesc "EtradeJanitor")
  in
  execParser opts
