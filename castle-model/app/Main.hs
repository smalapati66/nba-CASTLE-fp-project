module Main where

import Castle.ModelV1
import Castle.NBAApiData
import Data.List (sortBy)
import Data.Ord (Down(..), comparing)
import qualified Data.Text as T

main :: IO ()
main = do
  rows <-
    loadSeasonRowsFromFiles
      "../data/raw/player_advanced_2025_26.json"
      "../data/raw/player_tracking_2025_26.json"
      "../data/raw/player_hustle_2025_26.json"
      "../data/raw/player_onoff_2025_26.json"
      "../data/raw/player_tracking_2pt_2025_26.json"
      "../data/raw/player_tracking_3pt_2025_26.json"
      "../data/raw/player_tracking_rim_2025_26.json"
      (T.pack "2025-26")

  let scores = computeCastleV1 defaultCastleParamsV1 rows
      top10 = take 10 (sortBy (comparing (Down . scoreShrunkComposite)) scores)

  putStrLn $ "Loaded eligible players: " ++ show (length scores)
  putStrLn "Top 10 CASTLE v1 scores (2025-26):"
  mapM_ printTop top10

printTop :: CastleScoreV1 -> IO ()
printTop s =
  putStrLn $
    "- "
      ++ show (scorePlayerName s)
      ++ " ["
      ++ show (scoreRole s)
      ++ "] score="
      ++ show4 (scoreShrunkComposite s)
      ++ " overall_pct="
      ++ show4 (scoreOverallPercentile s)

show4 :: Double -> String
show4 x = show (fromIntegral (round (x * 10000) :: Integer) / (10000 :: Double))
