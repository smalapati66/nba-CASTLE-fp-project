{-# LANGUAGE OverloadedStrings #-}

module Castle.NBAApiData
  ( loadSeasonRows,
    loadSeasonRowsFromFiles
  ) where

import Control.Applicative ((<|>))
import Castle.ModelV1
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)

data PlayerAdvanced = PlayerAdvanced
  { advPlayerId :: Int,
    advPlayerName :: Text,
    advTeamId :: Int,
    advGp :: Int,
    advMin :: Double,
    advDefRating :: Double,
    advDrebPct :: Double,
    advPoss :: Double
  }

data PlayerTracking = PlayerTracking
  { trkPlayerId :: Int,
    trkPosition :: Text,
    trkGp :: Int,
    trkDFgm :: Double,
    trkDFga :: Double,
    trkDFgPct :: Double,
    trkNormalFgPct :: Double,
    trkPctPlusMinus :: Double
  }

data PlayerHustle = PlayerHustle
  { husPlayerId :: Int,
    husSteals :: Double,
    husBlocks :: Double,
    husDeflections :: Double
  }

data PlayerOnOff = PlayerOnOff
  { onOffPlayerId :: Int,
    onDefRating :: Double,
    offDefRating :: Double
  }

data PlayerZoneSupp = PlayerZoneSupp
  { zonePlayerId :: Int,
    zoneSuppression :: Double
  }

data ZoneKind = Zone2 | Zone3 | Rim

data PartialSeason = PartialSeason
  { pPlayerId :: Int,
    pPlayerName :: Maybe Text,
    pTeamId :: Maybe Int,
    pPosition :: Maybe Text,
    pGp :: Maybe Int,
    pMin :: Maybe Double,
    pPoss :: Maybe Double,
    pDefRating :: Maybe Double,
    pDrebPct :: Maybe Double,
    pDFgm :: Maybe Double,
    pDFga :: Maybe Double,
    pDFgPct :: Maybe Double,
    pNormalFgPct :: Maybe Double,
    pPctPlusMinus :: Maybe Double,
    pSteals :: Maybe Double,
    pBlocks :: Maybe Double,
    pDeflections :: Maybe Double,
    pOnDefRating :: Maybe Double,
    pOffDefRating :: Maybe Double,
    pZone2Suppression :: Maybe Double,
    pZone3Suppression :: Maybe Double,
    pRimSuppression :: Maybe Double
  }

type Row = M.Map Text Value

emptyPartial :: Int -> PartialSeason
emptyPartial pid =
  PartialSeason
    { pPlayerId = pid,
      pPlayerName = Nothing,
      pTeamId = Nothing,
      pPosition = Nothing,
      pGp = Nothing,
      pMin = Nothing,
      pPoss = Nothing,
      pDefRating = Nothing,
      pDrebPct = Nothing,
      pDFgm = Nothing,
      pDFga = Nothing,
      pDFgPct = Nothing,
      pNormalFgPct = Nothing,
      pPctPlusMinus = Nothing,
      pSteals = Nothing,
      pBlocks = Nothing,
      pDeflections = Nothing,
      pOnDefRating = Nothing,
      pOffDefRating = Nothing,
      pZone2Suppression = Nothing,
      pZone3Suppression = Nothing,
      pRimSuppression = Nothing
    }

loadSeasonRowsFromFiles
  :: FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> Text
  -> IO [PlayerSeasonRaw]
loadSeasonRowsFromFiles advPath trkPath hustlePath onOffPath zone2Path zone3Path rimPath seasonTxt = do
  advJson <- B.readFile advPath
  trkJson <- B.readFile trkPath
  hustleJson <- B.readFile hustlePath
  onOffJson <- B.readFile onOffPath
  zone2Json <- B.readFile zone2Path
  zone3Json <- B.readFile zone3Path
  rimJson <- B.readFile rimPath
  pure (loadSeasonRows advJson trkJson hustleJson onOffJson zone2Json zone3Json rimJson seasonTxt)

loadSeasonRows
  :: B.ByteString
  -> B.ByteString
  -> B.ByteString
  -> B.ByteString
  -> B.ByteString
  -> B.ByteString
  -> B.ByteString
  -> Text
  -> [PlayerSeasonRaw]
loadSeasonRows advJson trkJson hustleJson onOffJson zone2Json zone3Json rimJson seasonTxt =
  case (advRows, trkRows, hustleRows, onOffRows, zone2Rows, zone3Rows, rimRows) of
    (Just aRows, Just tRows, Just hRows, Just ooRows, Just z2Rows, Just z3Rows, Just rRows) ->
      buildSeasonRows seasonTxt aRows tRows hRows ooRows z2Rows z3Rows rRows
    _ -> []
  where
    advRows = decodeRows advJson parseAdvancedRow
    trkRows = decodeRows trkJson parseTrackingRow
    hustleRows = decodeRows hustleJson parseHustleRow
    onOffRows = decodeRows onOffJson parseOnOffRow
    zone2Rows = decodeRows zone2Json parseZoneSuppRow
    zone3Rows = decodeRows zone3Json parseZoneSuppRow
    rimRows = decodeRows rimJson parseZoneSuppRow

buildSeasonRows
  :: Text
  -> [PlayerAdvanced]
  -> [PlayerTracking]
  -> [PlayerHustle]
  -> [PlayerOnOff]
  -> [PlayerZoneSupp]
  -> [PlayerZoneSupp]
  -> [PlayerZoneSupp]
  -> [PlayerSeasonRaw]
buildSeasonRows seasonTxt advRows trkRows hustleRows onOffRows zone2Rows zone3Rows rimRows =
  mapMaybe (toRaw seasonTxt) (M.elems merged)
  where
    withAdv = foldr insertAdv M.empty advRows
    withTrk = foldr insertTrk withAdv trkRows
    withHustle = foldr insertHustle withTrk hustleRows
    withOnOff = foldr insertOnOff withHustle onOffRows
    withZone2 = foldr (insertZone Zone2) withOnOff zone2Rows
    withZone3 = foldr (insertZone Zone3) withZone2 zone3Rows
    merged = foldr (insertZone Rim) withZone3 rimRows

insertAdv :: PlayerAdvanced -> M.Map Int PartialSeason -> M.Map Int PartialSeason
insertAdv a =
  M.alter
    (Just . apply . fromMaybe (emptyPartial (advPlayerId a)))
    (advPlayerId a)
  where
    apply p =
      p
        { pPlayerName = Just (advPlayerName a),
          pTeamId = Just (advTeamId a),
          pGp = Just (advGp a),
          pMin = Just (advMin a),
          pPoss = Just (advPoss a),
          pDefRating = Just (advDefRating a),
          pDrebPct = Just (advDrebPct a)
        }

insertTrk :: PlayerTracking -> M.Map Int PartialSeason -> M.Map Int PartialSeason
insertTrk t =
  M.alter
    (Just . apply . fromMaybe (emptyPartial (trkPlayerId t)))
    (trkPlayerId t)
  where
    apply p =
      p
        { pPosition = Just (trkPosition t),
          pGp = Just (trkGp t),
          pDFgm = Just (trkDFgm t),
          pDFga = Just (trkDFga t),
          pDFgPct = Just (trkDFgPct t),
          pNormalFgPct = Just (trkNormalFgPct t),
          pPctPlusMinus = Just (trkPctPlusMinus t)
        }

insertHustle :: PlayerHustle -> M.Map Int PartialSeason -> M.Map Int PartialSeason
insertHustle h =
  M.alter
    (Just . apply . fromMaybe (emptyPartial (husPlayerId h)))
    (husPlayerId h)
  where
    apply p =
      p
        { pSteals = Just (husSteals h),
          pBlocks = Just (husBlocks h),
          pDeflections = Just (husDeflections h)
        }

insertOnOff :: PlayerOnOff -> M.Map Int PartialSeason -> M.Map Int PartialSeason
insertOnOff x =
  M.alter
    (Just . apply . fromMaybe (emptyPartial (onOffPlayerId x)))
    (onOffPlayerId x)
  where
    apply p =
      p
        { pOnDefRating = Just (onDefRating x),
          pOffDefRating = Just (offDefRating x)
        }

insertZone
  :: ZoneKind
  -> PlayerZoneSupp
  -> M.Map Int PartialSeason
  -> M.Map Int PartialSeason
insertZone zoneKind z =
  M.alter
    (Just . apply . fromMaybe (emptyPartial (zonePlayerId z)))
    (zonePlayerId z)
  where
    apply p =
      case zoneKind of
        Zone2 -> p {pZone2Suppression = Just (zoneSuppression z)}
        Zone3 -> p {pZone3Suppression = Just (zoneSuppression z)}
        Rim -> p {pRimSuppression = Just (zoneSuppression z)}

toRaw :: Text -> PartialSeason -> Maybe PlayerSeasonRaw
toRaw seasonTxt p = do
  playerName <- pPlayerName p
  teamId <- pTeamId p
  pos <- pPosition p
  gp <- pGp p
  mins <- pMin p
  poss <- pPoss p
  defRtg <- pDefRating p
  dreb <- pDrebPct p
  dfgm <- pDFgm p
  dfga <- pDFga p
  dfgPct <- pDFgPct p
  normalPct <- pNormalFgPct p
  ppm <- pPctPlusMinus p
  pure
    PlayerSeasonRaw
      { rawPlayerId = pPlayerId p,
        rawSeason = seasonTxt,
        rawPlayerName = playerName,
        rawTeamId = teamId,
        rawPositionText = pos,
        rawPositionGroup = inferPositionGroup pos,
        rawGp = gp,
        rawMin = mins,
        rawPoss = poss,
        rawDefRating = defRtg,
        rawDrebPct = dreb,
        rawDFga = dfga,
        rawDFgm = dfgm,
        rawDFgPct = dfgPct,
        rawNormalFgPct = normalPct,
        rawPctPlusMinus = ppm,
        rawSteals = pSteals p,
        rawBlocks = pBlocks p,
        rawDeflections = pDeflections p,
        rawOnDefRating = pOnDefRating p,
        rawOffDefRating = pOffDefRating p,
        rawZone2Suppression = pZone2Suppression p,
        rawZone3Suppression = pZone3Suppression p,
        rawRimSuppression = pRimSuppression p
      }

decodeRows :: B.ByteString -> (Row -> Parser a) -> Maybe [a]
decodeRows jsonBS rowParser = do
  rows <- decodeTableRows jsonBS
  pure (mapMaybe (parseMaybe rowParser) rows)

decodeTableRows :: B.ByteString -> Maybe [Row]
decodeTableRows jsonBS = parseMaybe parser =<< decode jsonBS
  where
    parser = withObject "root" $ \obj -> do
      resultSets <- obj .: K.fromString "resultSets"
      table <- parseFirst resultSets
      headers <- table .: K.fromString "headers"
      rowSet <- table .: K.fromString "rowSet"
      pure (zipRows headers rowSet)

zipRows :: [Text] -> [[Value]] -> [Row]
zipRows headers rows = map (M.fromList . zip headers) rows

parseFirst :: [a] -> Parser a
parseFirst [] = fail "Expected non-empty resultSets"
parseFirst (x : _) = pure x

required :: FromJSON a => Text -> Row -> Parser a
required key row =
  case M.lookup key row of
    Nothing -> fail ("Missing required field: " ++ show key)
    Just v -> parseJSON v

optional :: FromJSON a => Text -> Row -> Parser (Maybe a)
optional key row =
  case M.lookup key row of
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just v -> Just <$> parseJSON v

parseAdvancedRow :: Row -> Parser PlayerAdvanced
parseAdvancedRow row =
  PlayerAdvanced
    <$> required "PLAYER_ID" row
    <*> required "PLAYER_NAME" row
    <*> required "TEAM_ID" row
    <*> required "GP" row
    <*> required "MIN" row
    <*> required "DEF_RATING" row
    <*> required "DREB_PCT" row
    <*> required "POSS" row

parseTrackingRow :: Row -> Parser PlayerTracking
parseTrackingRow row =
  PlayerTracking
    <$> required "CLOSE_DEF_PERSON_ID" row
    <*> required "PLAYER_POSITION" row
    <*> required "GP" row
    <*> required "D_FGM" row
    <*> required "D_FGA" row
    <*> required "D_FG_PCT" row
    <*> required "NORMAL_FG_PCT" row
    <*> required "PCT_PLUSMINUS" row

parseHustleRow :: Row -> Parser PlayerHustle
parseHustleRow row =
  PlayerHustle
    <$> required "PLAYER_ID" row
    <*> required "STL" row
    <*> required "BLK" row
    <*> (required "DEFLECTIONS" row <|> fallbackDeflections row)

parseOnOffRow :: Row -> Parser PlayerOnOff
parseOnOffRow row =
  PlayerOnOff
    <$> required "PLAYER_ID" row
    <*> required "ON_DEF_RATING" row
    <*> required "OFF_DEF_RATING" row

parseZoneSuppRow :: Row -> Parser PlayerZoneSupp
parseZoneSuppRow row =
  PlayerZoneSupp
    <$> required "CLOSE_DEF_PERSON_ID" row
    <*> suppression
  where
    suppression = do
      dfg <- required "D_FG_PCT" row
      normal <- required "NORMAL_FG_PCT" row
      pure (normal - dfg)

fallbackDeflections :: Row -> Parser Double
fallbackDeflections row = do
  mVal <- optional "DEFLECTION" row
  pure (fromMaybe 0 mVal)
