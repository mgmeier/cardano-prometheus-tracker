
module Network.HTTP.PrometheusTracker.Utils where

import           Data.Char
import           System.Directory
import           System.FilePath


listScrapeFiles :: IO [FilePath]
listScrapeFiles =
  filter isPertinent <$> listDirectory "."

-- matches scrape JSON files
isPertinent :: FilePath -> Bool
isPertinent fn
  | ext /= ".json" = False
  | otherwise = case name of
    's':'c':'r':'a':'p':'e':'-' : rest@(_:_) -> all isDigit rest
    _                                        -> False
  where
    ext     = takeExtension fn
    name    = dropExtension fn
