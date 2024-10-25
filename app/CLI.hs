module CLI (
    Command (..)
  , getOpts
  ) where

import           Options.Applicative

import           Network.HTTP.PrometheusTracker.Types


data Command =
      CScrape           !ScrapeConfig
    | CSummary          !FilePath
    | CCompare          !FilePath        !FilePath
    | CPlot             !MetricInSummary !MetricInSummary
    | CNames            !FilePath
    deriving Show

data MetricInSummary = MetricInSummary {misFile :: !FilePath, misName :: !String}

instance Show MetricInSummary where
  show (MetricInSummary f n) = f ++ ":" ++ n


getOpts :: IO Command
getOpts = execParser $ info (parseCLI <**> helper) (fullDesc <> progDesc "Track Cardano Prometheus metrics")

parseCLI :: Parser Command
parseCLI = subparser $ mconcat
  [ op "scrape" "Scrape a Prometheus URL"
      (CScrape <$> parseScrapeConfig)
  , op "summarize" "Join all scraped JSONs in $PWD into a summary"
      (CSummary <$> parseOutFileName "output JSON file")
  , op "compare" "Print comparison between two summaries to stdout"
      (CCompare <$> parseSummaryFileName "FILE1" <*> parseSummaryFileName "FILE2")
  , op "names" "List all metrics names observed in summary to stdout"
      (CNames <$> parseSummaryFileName "FILE")
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

    parseSummaryFileName :: String -> Parser FilePath
    parseSummaryFileName meta = strArgument
      (mconcat
        [ metavar meta
        , action "file"
        , help "summary JSON"
        ])

    parseURL :: Parser String
    parseURL = strArgument
      (mconcat
        [ metavar "URL"
        , help "Prometheus URL, either from a node (legacy) or cardano-tracer (new)"
        ])
