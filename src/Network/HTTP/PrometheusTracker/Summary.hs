{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE ViewPatterns      #-}

module Network.HTTP.PrometheusTracker.Summary
       ( createSummaryFromScrapes
       , compareSummaries
       , printNames
       ) where

import           Control.Applicative
import           Data.Aeson                           (decodeFileStrict',
                                                       eitherDecodeFileStrict')
import           Data.List                            (sort)
import qualified Data.Map.Strict                      as M
import           Data.Maybe
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Text.IO                         as T (putStrLn)

import           Network.HTTP.PrometheusTracker.Types
import           Network.HTTP.PrometheusTracker.Utils


createSummaryFromScrapes :: FilePath -> IO ()
createSummaryFromScrapes outfile = do
  scrapes <- sort . catMaybes <$> (mapM loadScrape =<< listScrapeFiles)
  if null scrapes
    then putStrLn "--> no scrape JSON files found, exiting"
    else do
      -- adjust the scrape timeline such that:
      -- 1. the offset equals the slot number metric
      -- 2. the first offset equals 0, if no slot number could be determined
      let
        offset0 = fromMaybe (offset $ head scrapes) (firstSlotMetric scrapes)
        adjust0 = adjustOffsets (subtract offset0) scrapes

      putStrLn $ "--> summarizing " ++ show (length scrapes) ++ " scrape JSON files into: " ++ outfile
      writeFilePretty outfile adjust0

loadScrape :: FilePath -> IO (Maybe Scrape)
loadScrape f = eitherDecodeFileStrict' f >>= \case
    Left err -> do
      putStrLn $ "--> error in file: " ++ f
      print err
      pure Nothing
    Right parse -> pure $ Scrape parse <$> timestampOfScrape f

adjustOffsets :: (Int -> Int) -> [Scrape] -> [Scrape]
adjustOffsets f = map (\s -> s {offset = f $ offset s})

firstSlotMetric :: [Scrape] -> Maybe Int
firstSlotMetric [] = Nothing
firstSlotMetric (Scrape (MM mm) ofs:xs) =
      offsetFromSlotMetric "slotNum_int"        -- identical for old and new tracing
  <|> firstSlotMetric xs
  where
    offsetFromSlotMetric key
      | Just (MVInt slot) <- M.lookup key mm    = Just (ofs - slot)
      | otherwise                               = Nothing

compareSummaries :: FilePath -> FilePath -> IO ()
compareSummaries f1 f2 = do
  load1 <- decodeFileStrict' f1
  load2 <- decodeFileStrict' f2
  maybe
    (putStrLn $ "--> unable to load " ++ f1 ++ " and/or " ++ f2)
    (uncurry renderComparison)
    ((,) <$> load1 <*> load2)
  where
    renderComparison :: [Scrape] -> [Scrape] -> IO ()
    renderComparison (S.fromList . getNames -> s1) (S.fromList . getNames -> s2) = do

      putStrLn $ "--> metrics in " ++ f1 ++ " but not in " ++ f2
      mapM_ T.putStrLn $ S.toAscList onlyInM1

      putStrLn $ "\n--> metrics in " ++ f2 ++ " but not in " ++ f1
      mapM_ T.putStrLn $ S.toAscList onlyInM2

      where
        onlyInM1 = s1 `S.difference` s2
        onlyInM2 = s2 `S.difference` s1

printNames :: FilePath -> IO ()
printNames fn =
  decodeFileStrict' fn >>= maybe
    (putStrLn $ "--> unable to load " ++ fn)
    go
  where
    go :: [Scrape] -> IO ()
    go  = mapM_ T.putStrLn . getNames

getNames :: [Scrape] -> [Text]
getNames (last -> Scrape{scrape = MM mm}) = M.keys mm
