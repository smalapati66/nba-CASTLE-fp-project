module Main where

import Castle.EvalV1
import Castle.ModelV1
import Castle.NBAApiData
import qualified Data.Text as T

main :: IO ()
main = do
  rows24 <-
    loadSeasonRowsFromFiles
      "../data/raw/player_advanced_2024_25.json"
      "../data/raw/player_tracking_2024_25.json"
      "../data/raw/player_hustle_2024_25.json"
      "../data/raw/player_onoff_2024_25.json"
      "../data/raw/player_tracking_2pt_2024_25.json"
      "../data/raw/player_tracking_3pt_2024_25.json"
      "../data/raw/player_tracking_rim_2024_25.json"
      (T.pack "2024-25")
  rows25 <-
    loadSeasonRowsFromFiles
      "../data/raw/player_advanced_2025_26.json"
      "../data/raw/player_tracking_2025_26.json"
      "../data/raw/player_hustle_2025_26.json"
      "../data/raw/player_onoff_2025_26.json"
      "../data/raw/player_tracking_2pt_2025_26.json"
      "../data/raw/player_tracking_3pt_2025_26.json"
      "../data/raw/player_tracking_rim_2025_26.json"
      (T.pack "2025-26")

  let scores24 = computeCastleV1 defaultCastleParamsV1 rows24
      scores25 = computeCastleV1 defaultCastleParamsV1 rows25
      report = evaluateTwoSeasonWindow scores24 scores25 rows25

  putStrLn (formatEvalReport report)
