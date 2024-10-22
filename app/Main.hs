module Main where

import           Network.HTTP.Client

import           Network.HTTP.PrometheusTracker
import           Network.HTTP.PrometheusTracker.Types


-- old tracing, node-0
testCfg :: ScrapeConfig
testCfg = ScrapeConfig
  { scrapeSeconds = 15
  , scrapeUrl     = "http://localhost:12798/metrics"
  }

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  scrapeWhileValid manager testCfg
