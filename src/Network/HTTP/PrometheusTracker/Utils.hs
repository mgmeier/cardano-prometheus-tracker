
module Network.HTTP.PrometheusTracker.Utils where

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy     as BL (writeFile)
import           Data.Maybe
import           System.Directory
import           System.FilePath
import           Text.Read

listScrapeFiles :: IO [FilePath]
listScrapeFiles =
  filter (isJust . timestampOfScrape) <$> listDirectory "."

-- matches scrape JSON files
timestampOfScrape :: FilePath -> Maybe Int
timestampOfScrape fn
  | ext /= ".json" = Nothing
  | otherwise = case name of
    's':'c':'r':'a':'p':'e':'-' : rest -> readMaybe rest
    _                                  -> Nothing
  where
    ext     = takeExtension fn
    name    = dropExtension fn

writeFilePretty :: ToJSON a => FilePath -> a -> IO ()
writeFilePretty fn =
  BL.writeFile fn . encodePretty' prettyConfig
  where
    prettyConfig :: Config
    prettyConfig = defConfig { confCompare = compare, confTrailingNewline = True }
