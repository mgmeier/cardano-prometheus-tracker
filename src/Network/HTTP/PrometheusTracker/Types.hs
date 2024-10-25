{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Network.HTTP.PrometheusTracker.Types where

import           GHC.Generics        (Generic)

import           Control.Applicative
import           Data.Aeson          (FromJSON (..), ToJSON (..))
import           Data.Function       (on)
import           Data.Map.Strict     as M (Map, null)
import           Data.Text           (Text)


newtype MetricsMap = MM (Map Text MetricsValue)
  deriving (Show, FromJSON, ToJSON) via (Map Text MetricsValue)

mmNull :: MetricsMap -> Bool
mmNull (MM m) = M.null m

data MetricsValue
  = MVInt       Int
  | MVDouble    Double
  | MVText      Text
  deriving Show

instance ToJSON MetricsValue where
    toJSON = \case
        MVInt i    -> toJSON i
        MVDouble d -> toJSON d
        MVText t   -> toJSON t

instance FromJSON MetricsValue where
  parseJSON v =
        (MVInt    <$> parseJSON v)
    <|> (MVDouble <$> parseJSON v)
    <|> (MVText   <$> parseJSON v)

data ScrapeConfig = ScrapeConfig
  { scrapeSeconds :: Int
  , scrapeUrl     :: String
  }
  deriving Show

data Scrape = Scrape
  { scrape :: !MetricsMap
  , offset :: !Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Eq Scrape where
  (==) = (==) `on` offset

instance Ord Scrape where
  compare = compare `on` offset
