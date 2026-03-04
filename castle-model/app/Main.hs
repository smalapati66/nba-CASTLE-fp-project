module Main where

import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson.Key as K
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.List (foldl')

--------------------------------------------------------------------------------
-- Sections: Data Types/Structures
--------------------------------------------------------------------------------

-- type for readability
type Season = String

-- store data as player seasons -> one data struct for a player's given season
data PlayerSeason = PlayerSeason
  { playerID :: Int,
    season :: Season,
    playerName :: Maybe Text,
    position :: Maybe Text,
    gamesPlayed :: Maybe Int,

    -- from playerAdvanced
    defRating :: Maybe Double,
    defRebPercentage :: Maybe Double,

    -- from PlayerTracking
    dFGM :: Maybe Double,
    dFGP :: Maybe Double
  } deriving (Show)

-- for making blank struct
emptyPlayerSeason :: Int -> Season -> PlayerSeason
emptyPlayerSeason pid seasonStr =
  PlayerSeason pid seasonStr Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- any other stats we need from player_advanced?
data PlayerAdvanced = PlayerAdvanced
  { advPlayerID :: Int,
    advPlayerName :: Text,
    advGamesPlayed :: Int,
    advDefRating  :: Double,
    advDefRebPercentage  :: Double
  } deriving (Show)

data PlayerTracking = PlayerTracking
  { trkPlayerID :: Int,
    trkPlayerName :: Text,
    trkPosition :: Text,
    trkGamesPlayed :: Int,
    trkDFGM  :: Double,
    trkDFGP :: Double
  } deriving (Show)

-- table of player seasons, indexed by playerID and season
type PlayerTable = M.Map (Int, Season) PlayerSeason

--------------------------------------------------------------------------------
-- Merging functions (updating the player table)
--------------------------------------------------------------------------------

mergeAdvanced :: PlayerAdvanced -> PlayerSeason -> PlayerSeason
mergeAdvanced a ps =
  ps
    { playerName = Just (advPlayerName a),
      gamesPlayed = Just (advGamesPlayed a),
      defRating = Just (advDefRating a),
      defRebPercentage = Just (advDefRebPercentage a)
    }

mergeTracking :: PlayerTracking -> PlayerSeason -> PlayerSeason
mergeTracking t ps =
  ps
    { playerName = Just (trkPlayerName t),
      position = Just (trkPosition t),
      gamesPlayed = Just (trkGamesPlayed t),
      dFGM = Just (trkDFGM t),
      dFGP = Just (trkDFGP t)
    }

insertAdvanced :: Season -> PlayerAdvanced -> PlayerTable -> PlayerTable
insertAdvanced seasonStr a =
  M.alter step (advPlayerID a, seasonStr)
  where
    step Nothing   = Just (mergeAdvanced a (emptyPlayerSeason (advPlayerID a) seasonStr))
    step (Just ps) = Just (mergeAdvanced a ps)

insertTracking :: Season -> PlayerTracking -> PlayerTable -> PlayerTable
insertTracking seasonStr t =
  M.alter step (trkPlayerID t, seasonStr)
  where
    step Nothing   = Just (mergeTracking t (emptyPlayerSeason (trkPlayerID t) seasonStr))
    step (Just ps) = Just (mergeTracking t ps)

-- if we have more types to combine, add to this function and above functions
buildPlayerTable
  :: Season
  -> [PlayerAdvanced]
  -> [PlayerTracking]
  -> PlayerTable
buildPlayerTable seasonStr a t =
  let afterAdv = foldl' (flip (insertAdvanced seasonStr)) M.empty a
  in  foldl' (flip (insertTracking seasonStr)) afterAdv t

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parseAdvanced :: [Value] -> Parser PlayerAdvanced
parseAdvanced row = PlayerAdvanced
  <$> parseJSON (head row) -- ID
  <*> parseJSON (row !! 1)  -- name
  <*> parseJSON (row !! 6) -- gp
  <*> parseJSON (row !! 15) -- drtg
  <*> parseJSON (row !! 24) -- drb%

parseTracking :: [Value] -> Parser PlayerTracking
parseTracking row = PlayerTracking
  <$> parseJSON (head row) -- ID
  <*> parseJSON (row !! 1)  -- name
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

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  advJsonData <- B.readFile "../data/raw/player_advanced_2024_25.json"
  trackingJsonData <- B.readFile "../data/raw/player_tracking_2024_25.json"

  let season24_25 = "2024-25"

  -- in the future, we should probably revise this to make it more concise
  let maybeAdvanced24_25 = decodeStats advJsonData parseAdvanced
      maybeTracking24_25 = decodeStats trackingJsonData parseTracking

  case (maybeAdvanced24_25, maybeTracking24_25) of
    (Just advancedRows, Just trackingRows) -> do
      let playerTable = buildPlayerTable season24_25 advancedRows trackingRows
      putStrLn $ "Built player table with " ++ show (M.size playerTable) ++ " player-seasons."
      putStrLn $ "Here's an example entry:"
      print (M.lookup (203081, season24_25) playerTable)
    (Nothing, _) ->
      putStrLn "Failed to decode player_advanced_2024_25.json"
    (_, Nothing) ->
      putStrLn "Failed to decode player_tracking_2024_25.json"
