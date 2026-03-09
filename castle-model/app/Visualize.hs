{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Castle.ModelV1
import Castle.NBAApiData
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromJust)
import Castle.ModelV1
import Granite (scatter, defPlot)
import Castle.ModelV1 (getSimilarProfiles)


displaySimilarityPlot :: CastleProfileV1 -> [CastleProfileV1] -> IO ()
displaySimilarityPlot target similarPlayers = do
    let allProfiles = target : similarPlayers
        rows = map (\p -> ( T.unpack . scorePlayerName . profileCastleScorev1 $ p
                          , defRating p
                          , scoreShrunkComposite (profileCastleScorev1 p)
                          )) allProfiles
        graphPoints = map (\(_, drtg, castle) -> (drtg, castle)) rows

    -- Text table
    putStrLn "\nPlayer                    DRTG    CASTLE"
    putStrLn "\n"
    mapM_ (\(name, drtg, castle) ->
        putStrLn $ padRight 26 name ++ padRight 8 (show drtg) ++ show castle
        ) rows

    -- Terminal scatter plot

    putStrLn "\n"
    T.putStrLn $ scatter [("DRTG vs. CASTLE", graphPoints)] defPlot
  where
    padRight n s = take n (s ++ repeat ' ')

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

  putStrLn "Enter a player:"
  name <- getLine
  let params         = defaultCastleParamsV1
      scores         = computeCastleV1 params rows
      zFeatures      = getZFeatures params rows
      profiles       = createProfiles params scores zFeatures rows
      target         = fromJust $ L.find (\p -> scorePlayerName (profileCastleScorev1 p) == T.pack name) profiles
      similarPlayers = getSimilarProfiles target profiles
      

  displaySimilarityPlot target similarPlayers
