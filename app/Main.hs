module Main where

import           Network.HTTP.Client

import           Network.HTTP.PrometheusTracker
import           Network.HTTP.PrometheusTracker.Types
import           Network.HTTP.PrometheusTracker.Utils

import           CLI


-- Examples for commonly configured Prometheus URLs in workbench:
-- * old system, node-0: http://localhost:12798/metrics
-- * new system, node-0: http://localhost:3200/tracersocket0 or http://localhost:3200/12700130000

main :: IO ()
main = getOpts >>= \case

  CSummary _outFile -> do
    scrapes <- listScrapeFiles
    if null scrapes
      then putStrLn "--> no scrape JSON files found"
      else putStrLn $ "--> summarizing " ++ show (length scrapes) ++ " scrape JSON files"

  CScrape conf -> do
    putStrLn $ "--> looking for Prometheus metrics at: " ++ scrapeUrl conf
    putStrLn   "--> hit Ctrl-C to quit, or wait for the scraper to auto-exit..."
    manager <- newManager defaultManagerSettings
    scrapeWhileValid manager conf
