module Main where

import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson.Key as K
import Data.Text (Text)

-- any other stats we need from player_advanced?
data PlayerAdvanced = PlayerAdvanced
  { playerName :: Text,
    gamesPlayed :: Int,
    defRating  :: Double,
    defRebPercentage  :: Double
  } deriving (Show)

data PlayerTracking = PlayerTracking
  { playerNameT :: Text,
    position :: Text,
    gamesPlayedT :: Int,
    dFGM  :: Double,
    dFGP  :: Double
  } deriving (Show)

parseAdvanced :: [Value] -> Parser PlayerAdvanced
parseAdvanced row = PlayerAdvanced
  <$> parseJSON (row !! 1)  -- name
  <*> parseJSON (row !! 6) -- gp
  <*> parseJSON (row !! 15) -- drtg
  <*> parseJSON (row !! 24) -- drb%


parseTracking :: [Value] -> Parser PlayerTracking
parseTracking row = PlayerTracking
  <$> parseJSON (row !! 1)  -- name
  <*> parseJSON (row !! 4) -- pos
  <*> parseJSON (row !! 6) -- gp
  <*> parseJSON (row !! 9) -- dfgm
  <*> parseJSON (row !! 11) -- dfg%

decodeStats :: B.ByteString -> ([Value] -> Parser a) -> Maybe [a]
decodeStats jsonBS statParser = Data.Aeson.decode jsonBS >>= parseMaybe parser
  where
    parser = withObject "root" $ \obj -> do
        resultSets <- obj .: K.fromString "resultSets"
        let resultSet = head resultSets -- resultSets only has one obj so we grab the head (only obj)
        rows <- resultSet .: K.fromString "rowSet"
        mapM statParser rows

main :: IO ()
main = do
    advJsonData <- B.readFile "../data/raw/player_advanced_2024_25.json"
    let player_advanced_2024_25 = decodeStats advJsonData parseAdvanced -- looks like: Just [PlayerAdvanced {playerName = "A.J. Lawson",.....
    trackingJsonData <- B.readFile "../data/raw/player_tracking_2024_25.json"
    let player_tracking_2024_25 = decodeStats trackingJsonData parseTracking
