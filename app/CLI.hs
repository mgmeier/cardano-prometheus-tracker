module CLI (
    Command (..)
  , getOpts
  ) where

import           Options.Applicative

import           Network.HTTP.PrometheusTracker.Types


data Command =
      CScrape           !ScrapeConfig
    | CSummary          !FilePath
    deriving Show


getOpts :: IO Command
getOpts = execParser $ info (parseCLI <**> helper) (fullDesc <> progDesc "Track Cardano Prometheus metrics")

parseCLI :: Parser Command
parseCLI = subparser $ mconcat
  [ op "scrape" "Scrape a Prometheus URL"
      (CScrape <$> parseScrapeConfig)
  , op "summarize" "Join all scraped JSONs in `cwd` into a summary"
      (CSummary <$> parseOutFileName "output JSON file")
  ]
  where
    op :: String -> String -> Parser a -> Mod CommandFields a
    op c descr p =
     command c $ info (p <**> helper) $
       mconcat [ progDesc descr ]

    parseScrapeConfig :: Parser ScrapeConfig
    parseScrapeConfig = ScrapeConfig <$> parseDelay <*> parseURL

    parseDelay :: Parser Int
    parseDelay = option auto
      (mconcat
        [ short 'd'
        , long "delay"
        , value 15
        , showDefault
        , help "delay (seconds) between scrapes"
        ])

    parseOutFileName :: String -> Parser FilePath
    parseOutFileName helpText = strArgument
      (mconcat
        [ metavar "FILE"
        , help helpText
        ])

    parseURL :: Parser String
    parseURL = strArgument
      (mconcat
        [ metavar "URL"
        , help "Prometheus URL, either from a node (legacy) or cardano-tracer (new)"
        ])
