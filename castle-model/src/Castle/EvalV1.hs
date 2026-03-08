module Castle.EvalV1
  ( EvalReportV1 (..),
    evaluateTwoSeasonWindow,
    formatEvalReport
  ) where

import Castle.ModelV1
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import Statistics.Correlation (pearson)

-- basic v1 diagnostic report across a t -> t+1 window.
data EvalReportV1 = EvalReportV1
  { sampleSizeOverlap :: Int,
    yearToYearScoreCorr :: Maybe Double,
    predictiveCorrNextSeasonDefRating :: Maybe Double,
    topDecileBetterThanMedianRate :: Maybe Double
  }
  deriving (Eq, Show)

-- calcualtes the evaluation report over a 2 season window
evaluateTwoSeasonWindow :: [CastleScoreV1] -> [CastleScoreV1] -> [PlayerSeasonRaw] -> EvalReportV1
evaluateTwoSeasonWindow seasonT seasonT1 rawsT1 =
  let scoreMapT = M.fromList [(scorePlayerId s, scoreShrunkComposite s) | s <- seasonT]
      scoreMapT1 = M.fromList [(scorePlayerId s, scoreShrunkComposite s) | s <- seasonT1]
      overlapScores =
        [ (s0, s1)
          | (pid, s0) <- M.toList scoreMapT,
            Just s1 <- [M.lookup pid scoreMapT1]
        ]
      nextDefByPlayer = M.fromList [(rawPlayerId r, rawDefRating r) | r <- rawsT1]
      predictivePairs =
        [ (s0, def1)
          | (pid, s0) <- M.toList scoreMapT,
            Just def1 <- [M.lookup pid nextDefByPlayer]
        ]
      overlapN = length overlapScores
      scoreCorr = corrFromPairs overlapScores
      -- better defenders have lower DEF_RATING, so negate DEF_RATING to align direction.
      predictiveCorr = corrFromPairs [(s, -d) | (s, d) <- predictivePairs]
      topDecileRate = computeTopDecileRate scoreMapT nextDefByPlayer
   in EvalReportV1 overlapN scoreCorr predictiveCorr topDecileRate

-- for top 10% in castle in a season, what % of those are >median def. rating next year?
computeTopDecileRate :: M.Map Int Double -> M.Map Int Double -> Maybe Double
computeTopDecileRate scoreMap nextDefMap
  | null topPlayers = Nothing
  | otherwise = Just (fromIntegral betterCount / fromIntegral (length topPlayers))
  where
    sorted = L.sortBy (\(_, s1) (_, s2) -> compare s2 s1) (M.toList scoreMap)
    n = length sorted
    topK = max 1 (n `div` 10)
    topPlayers = take topK sorted
    defRatings = M.elems nextDefMap
    medianDef = median defRatings
    betterCount =
      length
        [ ()
          | (pid, _) <- topPlayers,
            Just defR <- [M.lookup pid nextDefMap],
            defR < medianDef
        ]

-- pearson correlation helper
corrFromPairs :: [(Double, Double)] -> Maybe Double
corrFromPairs pairs
  | length pairs < 3 = Nothing
  | otherwise =
      let vec = U.fromList pairs
          r = pearson vec
       in if isNaN r then Nothing else Just r

median :: [Double] -> Double
median [] = 0
median xs =
  let sorted = L.sort xs
      n = length sorted
   in if odd n
        then sorted !! (n `div` 2)
        else (sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2)) / 2

formatEvalReport :: EvalReportV1 -> String
formatEvalReport r =
  unlines
    [ "CASTLE v1 Eval Report",
      "- Overlap Sample Size: " ++ show (sampleSizeOverlap r),
      "- Year-to-year score correlation: " ++ showMaybe4 (yearToYearScoreCorr r),
      "- Predictive correlation (score_t vs -DEF_RATING_t+1): " ++ showMaybe4 (predictiveCorrNextSeasonDefRating r),
      "- Top decile better-than-median next-season DEF_RATING rate: " ++ showMaybe4 (topDecileBetterThanMedianRate r)
    ]

showMaybe4 :: Maybe Double -> String
showMaybe4 Nothing = "N/A"
showMaybe4 (Just x) = show (fromIntegral (round (x * 10000) :: Integer) / (10000 :: Double))
