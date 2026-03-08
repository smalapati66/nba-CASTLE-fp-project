{-# LANGUAGE OverloadedStrings #-}

module Castle.ModelV1
  ( PositionGroup (..),
    PlayerSeasonRaw (..),
    CastleWeights (..),
    CastleParamsV1 (..),
    CastleScoreV1 (..),
    defaultCastleParamsV1,
    inferPositionGroup,
    computeCastleV1
  ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T

-- Coarse role buckets for role-aware normalization/weighting.
data PositionGroup = Guard | Wing | Big | Hybrid
  deriving (Eq, Ord, Show)

data PlayerSeasonRaw = PlayerSeasonRaw
  { rawPlayerId :: Int,
    rawSeason :: Text,
    rawPlayerName :: Text,
    rawTeamId :: Int,
    rawPositionText :: Text,
    rawPositionGroup :: PositionGroup,
    rawGp :: Int,
    rawMin :: Double,
    rawPoss :: Double,
    rawDefRating :: Double,
    rawDrebPct :: Double,
    rawDFga :: Double,
    rawDFgm :: Double,
    rawDFgPct :: Double,
    rawNormalFgPct :: Double,
    rawPctPlusMinus :: Double,
    rawSteals :: Maybe Double,
    rawBlocks :: Maybe Double,
    rawDeflections :: Maybe Double,
    rawOnDefRating :: Maybe Double,
    rawOffDefRating :: Maybe Double,
    rawZone2Suppression :: Maybe Double,
    rawZone3Suppression :: Maybe Double,
    rawRimSuppression :: Maybe Double
  }
  deriving (Eq, Show)

data CastleWeights = CastleWeights
  { wContestLoad :: Double,
    wTeamAdjustedDef :: Double,
    wPossessionFinish :: Double,
    wDisruption :: Double,
    wOnOffImpact :: Double,
    wZoneSuppression :: Double
  }
  deriving (Eq, Show)

data CastleParamsV1 = CastleParamsV1
  { weightsGuard :: CastleWeights,
    weightsWing :: CastleWeights,
    weightsBig :: CastleWeights,
    weightsHybrid :: CastleWeights,
    shrinkageK :: Double,
    minPoss :: Double,
    minGp :: Int
  }
  deriving (Eq, Show)

data CastleScoreV1 = CastleScoreV1
  { scorePlayerId :: Int,
    scoreSeason :: Text,
    scorePlayerName :: Text,
    scoreRole :: PositionGroup,
    scoreRawComposite :: Double,
    scoreShrunkComposite :: Double,
    scoreOverallPercentile :: Double,
    scoreRolePercentile :: Double,
    scoreConfidence :: Double
  }
  deriving (Eq, Show)

data FeatureRow = FeatureRow
  { featPlayerId :: Int,
    featSeason :: Text,
    featPlayerName :: Text,
    featRole :: PositionGroup,
    featPoss :: Double,
    featContestLoad :: Double,
    featTeamAdjustedDef :: Double,
    featPossessionFinish :: Double,
    featDisruption :: Double,
    featOnOffImpact :: Double,
    featZoneSuppression :: Double
  }

data ZFeatureRow = ZFeatureRow
  { zFeatPlayerId :: Int,
    zFeatSeason :: Text,
    zFeatPlayerName :: Text,
    zFeatRole :: PositionGroup,
    zFeatPoss :: Double,
    zContestLoad :: Double,
    zTeamAdjustedDef :: Double,
    zPossessionFinish :: Double,
    zDisruption :: Double,
    zOnOffImpact :: Double,
    zZoneSuppression :: Double
  }

data RoleStats = RoleStats
  { muLoad :: Double,
    sdLoad :: Double,
    muTeam :: Double,
    sdTeam :: Double,
    muFinish :: Double,
    sdFinish :: Double,
    muDisruption :: Double,
    sdDisruption :: Double,
    muOnOff :: Double,
    sdOnOff :: Double,
    muZone :: Double,
    sdZone :: Double
  }

defaultCastleParamsV1 :: CastleParamsV1
defaultCastleParamsV1 =
  CastleParamsV1
    { weightsGuard = CastleWeights { wContestLoad = 0.14, wTeamAdjustedDef = 0.18, wPossessionFinish = 0.12, wDisruption = 0.17, wOnOffImpact = 0.21, wZoneSuppression = 0.18 },
      weightsWing = CastleWeights { wContestLoad = 0.15, wTeamAdjustedDef = 0.17, wPossessionFinish = 0.15, wDisruption = 0.15, wOnOffImpact = 0.18, wZoneSuppression = 0.20 },
      weightsBig = CastleWeights { wContestLoad = 0.18, wTeamAdjustedDef = 0.13, wPossessionFinish = 0.22, wDisruption = 0.12, wOnOffImpact = 0.15, wZoneSuppression = 0.20 },
      weightsHybrid = CastleWeights { wContestLoad = 0.16, wTeamAdjustedDef = 0.16, wPossessionFinish = 0.16, wDisruption = 0.16, wOnOffImpact = 0.16, wZoneSuppression = 0.20 },
      shrinkageK = 2000.0,
      minPoss = 500.0,
      minGp = 20
    }

inferPositionGroup :: Text -> PositionGroup
inferPositionGroup posText
  | hasG && hasC = Hybrid
  | hasC = Big
  | hasG = Guard
  | hasF = Wing
  | otherwise = Hybrid
  where
    up = T.toUpper posText
    hasG = "G" `T.isInfixOf` up
    hasF = "F" `T.isInfixOf` up
    hasC = "C" `T.isInfixOf` up

computeCastleV1 :: CastleParamsV1 -> [PlayerSeasonRaw] -> [CastleScoreV1]
computeCastleV1 params raws =
  map (attachPercentiles overallPctMap rolePctMaps) scored
  where
    eligible = filter (isEligible params) raws
    teamDefByTeam = mkTeamDefByTeam eligible
    features = mapMaybe (toFeatureRow teamDefByTeam) eligible
    zFeatures = zScoreByRole features
    scored = map (scoreOne params) zFeatures
    overallPctMap = rankScores scored
    rolePctMaps = M.map rankScores (M.fromListWith (++) [(scoreRole s, [s]) | s <- scored])

isEligible :: CastleParamsV1 -> PlayerSeasonRaw -> Bool
isEligible params r = rawPoss r >= minPoss params && rawGp r >= minGp params

attachPercentiles :: M.Map Int Double -> M.Map PositionGroup (M.Map Int Double) -> CastleScoreV1 -> CastleScoreV1
attachPercentiles overall roleMaps s =
  s
    { scoreOverallPercentile = M.findWithDefault 0 (scorePlayerId s) overall,
      scoreRolePercentile = M.findWithDefault 0 (scorePlayerId s) roleMap
    }
  where
    roleMap = M.findWithDefault M.empty (scoreRole s) roleMaps

mkTeamDefByTeam :: [PlayerSeasonRaw] -> M.Map Int Double
mkTeamDefByTeam rows = M.map weightedMean grouped
  where
    grouped = foldr step M.empty rows
    step r = M.insertWith (++) (rawTeamId r) [(rawDefRating r, max 1.0 (rawPoss r))]

weightedMean :: [(Double, Double)] -> Double
weightedMean pairs
  | denom <= 0 = 0
  | otherwise = numer / denom
  where
    numer = sum [v * w | (v, w) <- pairs]
    denom = sum [w | (_, w) <- pairs]

toFeatureRow :: M.Map Int Double -> PlayerSeasonRaw -> Maybe FeatureRow
toFeatureRow teamDefByTeam r = do
  teamDef <- M.lookup (rawTeamId r) teamDefByTeam
  pure
    FeatureRow
      { featPlayerId = rawPlayerId r,
        featSeason = rawSeason r,
        featPlayerName = rawPlayerName r,
        featRole = rawPositionGroup r,
        featPoss = rawPoss r,
        featContestLoad = log (1.0 + max 0.0 (rawDFga r)),
        featTeamAdjustedDef = teamDef - rawDefRating r,
        featPossessionFinish = rawDrebPct r,
        featDisruption = disruptionSignal r,
        featOnOffImpact = onOffSignal r,
        featZoneSuppression = zoneSuppressionSignal (rawPositionGroup r) r
      }

disruptionSignal :: PlayerSeasonRaw -> Double
disruptionSignal r
  | hasHustle = (stlRate + blkRate + 0.25 * deflRate) / 3.0
  | otherwise = (-rawPctPlusMinus r) * 100.0
  where
    possScale = 100.0 / max 1.0 (rawPoss r)
    stlRate = possScale * fromMaybe 0 (rawSteals r)
    blkRate = possScale * fromMaybe 0 (rawBlocks r)
    deflRate = possScale * fromMaybe 0 (rawDeflections r)
    hasHustle = any (/= Nothing) [rawSteals r, rawBlocks r, rawDeflections r]

onOffSignal :: PlayerSeasonRaw -> Double
onOffSignal r =
  case (rawOnDefRating r, rawOffDefRating r) of
    (Just onDef, Just offDef) -> offDef - onDef
    _ -> 0

zoneSuppressionSignal :: PositionGroup -> PlayerSeasonRaw -> Double
zoneSuppressionSignal role r =
  case (rawZone2Suppression r, rawZone3Suppression r, rawRimSuppression r) of
    (Just z2, Just z3, Just rim) ->
      let (w2, w3, wRim) = zoneWeights role
       in w2 * z2 + w3 * z3 + wRim * rim
    _ -> rawNormalFgPct r - rawDFgPct r
  where
    zoneWeights Guard = (0.35, 0.45, 0.20)
    zoneWeights Wing = (0.35, 0.30, 0.35)
    zoneWeights Big = (0.20, 0.20, 0.60)
    zoneWeights Hybrid = (0.30, 0.30, 0.40)

zScoreByRole :: [FeatureRow] -> [ZFeatureRow]
zScoreByRole rows = map toZ rows
  where
    grouped = M.fromListWith (++) [(featRole r, [r]) | r <- rows]
    statsByRole = M.map mkRoleStats grouped

    mkRoleStats rs =
      RoleStats
        { muLoad = mean (map featContestLoad rs),
          sdLoad = stdDev (map featContestLoad rs),
          muTeam = mean (map featTeamAdjustedDef rs),
          sdTeam = stdDev (map featTeamAdjustedDef rs),
          muFinish = mean (map featPossessionFinish rs),
          sdFinish = stdDev (map featPossessionFinish rs),
          muDisruption = mean (map featDisruption rs),
          sdDisruption = stdDev (map featDisruption rs),
          muOnOff = mean (map featOnOffImpact rs),
          sdOnOff = stdDev (map featOnOffImpact rs),
          muZone = mean (map featZoneSuppression rs),
          sdZone = stdDev (map featZoneSuppression rs)
        }

    toZ r =
      let rs = M.findWithDefault defaultRoleStats (featRole r) statsByRole
       in ZFeatureRow
            { zFeatPlayerId = featPlayerId r,
              zFeatSeason = featSeason r,
              zFeatPlayerName = featPlayerName r,
              zFeatRole = featRole r,
              zFeatPoss = featPoss r,
              zContestLoad = zValue (featContestLoad r) (muLoad rs) (sdLoad rs),
              zTeamAdjustedDef = zValue (featTeamAdjustedDef r) (muTeam rs) (sdTeam rs),
              zPossessionFinish = zValue (featPossessionFinish r) (muFinish rs) (sdFinish rs),
              zDisruption = zValue (featDisruption r) (muDisruption rs) (sdDisruption rs),
              zOnOffImpact = zValue (featOnOffImpact r) (muOnOff rs) (sdOnOff rs),
              zZoneSuppression = zValue (featZoneSuppression r) (muZone rs) (sdZone rs)
            }

defaultRoleStats :: RoleStats
defaultRoleStats = RoleStats 0 1 0 1 0 1 0 1 0 1 0 1

scoreOne :: CastleParamsV1 -> ZFeatureRow -> CastleScoreV1
scoreOne params zf =
  CastleScoreV1
    { scorePlayerId = zFeatPlayerId zf,
      scoreSeason = zFeatSeason zf,
      scorePlayerName = zFeatPlayerName zf,
      scoreRole = zFeatRole zf,
      scoreRawComposite = rawScore,
      scoreShrunkComposite = shrunkScore,
      scoreOverallPercentile = 0,
      scoreRolePercentile = 0,
      scoreConfidence = min 1.0 alpha
    }
  where
    w = weightsForRole params (zFeatRole zf)
    rawScore =
      wContestLoad w * zContestLoad zf
        + wTeamAdjustedDef w * zTeamAdjustedDef zf
        + wPossessionFinish w * zPossessionFinish zf
        + wDisruption w * zDisruption zf
        + wOnOffImpact w * zOnOffImpact zf
        + wZoneSuppression w * zZoneSuppression zf
    alpha = zFeatPoss zf / (zFeatPoss zf + shrinkageK params)
    shrunkScore = alpha * rawScore

weightsForRole :: CastleParamsV1 -> PositionGroup -> CastleWeights
weightsForRole params role =
  case role of
    Guard -> weightsGuard params
    Wing -> weightsWing params
    Big -> weightsBig params
    Hybrid -> weightsHybrid params

rankScores :: [CastleScoreV1] -> M.Map Int Double
rankScores xs =
  M.fromList [(scorePlayerId s, pct i) | (i, s) <- zip [0 ..] sorted]
  where
    sorted = L.sortBy (comparing (Down . scoreShrunkComposite)) xs
    n = length sorted
    pct i
      | n <= 1 = 1.0
      | otherwise = fromIntegral (n - 1 - i) / fromIntegral (n - 1)

mean :: [Double] -> Double
mean [] = 0
mean ys = sum ys / fromIntegral (length ys)

stdDev :: [Double] -> Double
stdDev ys
  | length ys < 2 = 1
  | otherwise =
      if s <= 1e-9 then 1 else s
  where
    m = mean ys
    var = sum [(x - m) ^ (2 :: Int) | x <- ys] / fromIntegral (length ys - 1)
    s = sqrt var

zValue :: Double -> Double -> Double -> Double
zValue x mu sigma
  | sigma <= 1e-9 = 0
  | otherwise = (x - mu) / sigma
