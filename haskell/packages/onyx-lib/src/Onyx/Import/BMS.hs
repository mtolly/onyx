{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Import.BMS where

import           Control.Monad                    (forM, guard)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Bifunctor                   (first)
import           Data.Char                        (isDigit, toLower)
import qualified Data.Conduit.Audio               as CA
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.Audio
import           Onyx.Beatmania.BMS
import           Onyx.DTXMania.DTX
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Edge (..), blipEdgesRB_,
                                                   joinEdgesSimple,
                                                   pattern RNil, pattern Wait)
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.MIDI.Track.Mania
import           Onyx.PhaseShift.Dance            (NoteType (NoteNormal))
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Files                  (fixFileCase)
import           Onyx.Util.Handle                 (fileReadable)
import qualified Sound.MIDI.Util                  as U
import           System.Directory                 (doesFileExist, makeAbsolute)
import           System.FilePath                  (takeDirectory, takeExtension,
                                                   (<.>), (</>))

processLongObj :: (NNC.C t) =>
  Maybe Chip -> RTB.T t (BMKey, Chip) -> RTB.T t (BMKey, Chip, Maybe t)
processLongObj Nothing      chips = (\(key, chip) -> (key, chip, Nothing)) <$> chips
processLongObj (Just lnobj) chips = let
  eachKey = flip map [minBound .. maxBound] $ \key -> let
    go = \case
      Wait t1 (_, c1) after1@(Wait t2 (_, c2) rest) -> if c2 == lnobj
        then Wait t1 (key, c1, Just t2) $ RTB.delay t2 $ go rest
        else Wait t1 (key, c1, Nothing) $ go after1
      Wait t1 (_, c1) RNil -> Wait t1 (key, c1, Nothing) RNil
      RNil -> RNil
    in go $ RTB.filter ((== key) . fst) chips
  in foldr RTB.merge RTB.empty eachKey

joinLongNotes :: (NNC.C t) =>
  RTB.T t (BMKey, Chip, Bool) -> RTB.T t (BMKey, Chip, t)
joinLongNotes
  = fmap (\(chip, key, t) -> (key, chip, t))
  . joinEdgesSimple
  . fmap (\(key, chip, b) -> if b then EdgeOn chip key else EdgeOff key)

importBMS :: (SendMessage m, MonadIO m) => FilePath -> Import m
importBMS bmsPath level = do
  let stripQuick2 bms = case level of
        ImportFull  -> bms
        ImportQuick -> bms
          { bms_Player1     = RTB.empty
          , bms_Player2     = RTB.empty
          , bms_Player1Long = RTB.empty
          , bms_Player2Long = RTB.empty
          , bms_BGM         = RTB.empty
          , bms_BGA         = RTB.empty
          , bms_WAV         = HM.empty
          , bms_VOLWAV      = HM.empty
          , bms_BMP         = HM.empty
          }
      stripQuick1 lns = case level of
        ImportFull  -> lns
        ImportQuick -> flip filter lns $ \(k, _) -> not
          $  T.any isDigit (T.take 1 k)
          || ("WAV"  `T.isPrefixOf` k)
          || ("BMP"  `T.isPrefixOf` k)
          || ("STOP" `T.isPrefixOf` k)
  bms <- stackIO $ stripQuick2 . readBMSLines . stripQuick1 <$> loadBMSLines bmsPath

  let isPMS = map toLower (takeExtension bmsPath) == ".pms"

  chipAudio <- case level of
    ImportQuick -> return []
    ImportFull -> fmap catMaybes $ forM (HM.toList $ bms_WAV bms) $ \(chip, fp) -> do
      msrc <- dtxAudioSource
        $ takeDirectory bmsPath
        </> map (\case '¥' -> '/'; '\\' -> '/'; c -> c) fp
        -- ¥ is the backslash when Shift-JIS decoded
      return $ flip fmap msrc $ \src -> let
        -- could be smarter about this (apply volume later) but this works
        adjustVolume = case fromMaybe 100 $ HM.lookup chip $ bms_VOLWAV bms of
          100 -> id
          vol -> CA.gain $ realToFrac vol / 100
        fixMono = case CA.channels src of
          1 -> applyPansVols [0] [0]
          _ -> id
        in (chip, AudioFile AudioInfo
          { md5 = Nothing
          , frames = Nothing
          , commands = []
          , filePath = Just $ SoftFile ("samples" </> T.unpack chip <.> "wav") $ SoftAudio
            $ fixMono $ adjustVolume src
          , rate = Nothing
          , channels = 2
          })

  let foundChips = HS.fromList $ map fst chipAudio
      audioForChips name chips = if RTB.null chips
        then ([], Nothing)
        else let
          poly = SamplesInfo
            { groupPolyphony = Just 1
            , groupCrossfade = 0.002
            }
          audios = [(name, AudioSamples poly)]
          track = Just $ F.SamplesTrack
            $ fmap (\chip -> F.SampleTrigger chip chip)
            -- don't include sample if we didn't find its audio, or if it's the long note end
            $ RTB.filter (\chip -> HS.member chip foundChips && Just chip /= bms_LNOBJ bms) chips
          in (audios, track)

      audioExpr name = Input $ Named name

      loadManiaTrack chips chipsLong = let
        short = flip fmap (processLongObj (bms_LNOBJ bms) chips)
          $ \(key, _, mlen) -> (key, mlen)
        long = flip fmap (joinLongNotes chipsLong)
          $ \(key, _, len) -> (key, Just len)
        in RTB.merge short long

      player1, player2 :: RTB.T U.Beats (BMKey, Maybe U.Beats)
      player1 = loadManiaTrack (bms_Player1 bms) (bms_Player1Long bms)
      player2 = loadManiaTrack (bms_Player2 bms) (bms_Player2Long bms)

      -- For now, we'll import BMS double play as 2 tracks, but PMS as 1 track
      shouldCombine = isPMS

      usedKeys1 = Set.fromList $ map fst $ RTB.getBodies player1
      usedKeys2 = Set.fromList $ map fst $ RTB.getBodies player2

      keyIndex1, keyIndex2 :: BMKey -> Int
      keyIndex1 k = fromMaybe (-1) $ Set.lookupIndex k usedKeys1
      keyIndex2 k = if shouldCombine
        then maybe (-1) (+ Set.size usedKeys1) $ Set.lookupIndex k usedKeys2
        else fromMaybe (-1)                    $ Set.lookupIndex k usedKeys2

      player1Indexed, player2Indexed :: RTB.T U.Beats (Int, Maybe U.Beats)
      player1Indexed = if shouldCombine
        then RTB.merge
          (first keyIndex1 <$> player1)
          (first keyIndex2 <$> player2)
        else first keyIndex1 <$> player1
      player2Indexed = if shouldCombine
        then RTB.empty
        else first keyIndex2 <$> player2

      (songAudios, songSampleTrack) = audioForChips "audio-bgm" $ bms_BGM bms
      (p1Audios, p1SampleTrack) = if shouldCombine
        then audioForChips "audio-p1" $ RTB.merge
          (snd <$> RTB.merge (bms_Player1 bms) (bms_Player2 bms))
          (RTB.mapMaybe (\(_key, chip, b) -> guard b >> Just chip) $ RTB.merge (bms_Player1Long bms) (bms_Player2Long bms))
        else audioForChips "audio-p1" $ RTB.merge
          (snd <$> bms_Player1 bms)
          (RTB.mapMaybe (\(_key, chip, b) -> guard b >> Just chip) $ bms_Player1Long bms)
      (p2Audios, p2SampleTrack) = if shouldCombine
        then ([], Nothing)
        else audioForChips "audio-p2" $ RTB.merge
          (snd <$> bms_Player2 bms)
          (RTB.mapMaybe (\(_key, chip, b) -> guard b >> Just chip) $ bms_Player2Long bms)

      midi = case level of
        ImportQuick -> emptyChart
        ImportFull -> F.Song (bms_TempoMap bms) (bms_MeasureMap bms) mempty
          { F.onyxParts = Map.fromList $ do
            (fpart, indexed) <-
              [ (F.FlexKeys          , player1Indexed)
              , (F.FlexExtra "rhythm", player2Indexed)
              ]
            guard $ not $ RTB.null indexed
            let opart = mempty
                  { F.onyxPartMania = Map.singleton "chart" ManiaTrack
                    { maniaNotes = fmap (, NoteNormal) <$> blipEdgesRB_ indexed
                    , maniaOverdrive = RTB.empty
                    }
                  }
            return (fpart, opart)
          , F.onyxSamples = Map.fromList $ catMaybes
            [ ("audio-bgm",) <$> songSampleTrack
            , ("audio-p1" ,) <$> p1SampleTrack
            , ("audio-p2" ,) <$> p2SampleTrack
            ]
          }

  background <- case bms_BGA bms of
    _ | level == ImportQuick -> return Nothing
    RNil -> return Nothing
    Wait bts chip RNil -> case HM.lookup chip $ bms_BMP bms of
      Nothing -> do
        warn $ "Couldn't find BGA chip: " <> show chip
        return Nothing
      Just fp -> do
        fp' <- stackIO $ fixFileCase (takeDirectory bmsPath </> fp) >>= makeAbsolute
        let ext = map toLower $ takeExtension fp'
        stackIO (doesFileExist fp') >>= \case
          False -> do
            warn $ "Background file " <> show fp <> " was not found"
            return Nothing
          True -> if elem ext [".png", ".bmp", ".gif", ".jpg", ".jpeg"]
            then return $ Just $ Left $ SoftFile ("background" <> ext)
              $ SoftReadable $ fileReadable fp'
            else return $ Just $ Right VideoInfo
              { fileVideo = SoftFile ("background" <> ext)
                $ SoftReadable $ fileReadable fp'
              , videoStartTime = Just $ negate $ realToFrac
                $ U.applyTempoMap midi.s_tempos bts
              , videoEndTime = Nothing
              , videoLoop = False
              }
    _ -> do
      warn "Can't import background yet (more than one BGA chip)"
      return Nothing

  return SongYaml
    { metadata = def'
      -- may need to adjust these title suffixes, sometimes there is SUBTITLE but it's same across difficulties
      { title        = flip fmap (bms_TITLE bms) $ \title -> title <> case bms_SUBTITLE bms of
        Just sub -> " " <> sub
        Nothing -> case bms_PLAYLEVEL bms of
          Nothing  -> ""
          Just lvl -> " [" <> T.pack (show lvl) <> "]"
      , artist       = bms_ARTIST bms
      , genre        = bms_GENRE bms
      , fileAlbumArt = Nothing
      }
    , global = def'
      { backgroundVideo = background >>= either (const Nothing) Just
      , fileBackgroundImage = background >>= either Just (const Nothing)
      , fileMidi = SoftFile "notes.mid" $ SoftChart midi
      , fileSongAnim = Nothing
      }
    , audio = case level of
      ImportQuick -> HM.empty
      ImportFull  -> HM.fromList $ chipAudio <> songAudios <> p1Audios <> p2Audios
    , jammit = HM.empty
    , plans = HM.singleton "bms" $ StandardPlan StandardPlanInfo
      { song        = guard (isJust songSampleTrack) >> Just (audioExpr "audio-bgm")
      , parts       = Parts $ HM.fromList $ catMaybes
        [ guard (isJust p1SampleTrack) >> Just (F.FlexKeys          , PartSingle $ audioExpr "audio-p1")
        , guard (isJust p2SampleTrack) >> Just (F.FlexExtra "rhythm", PartSingle $ audioExpr "audio-p2")
        ]
      , crowd       = Nothing
      , comments    = []
      , tuningCents = 0
      , fileTempo   = Nothing
      }
    , targets = HM.empty
    , parts = Parts $ HM.fromList $ do
      (fpart, indexed, p1) <-
        [ (F.FlexKeys          , player1Indexed, True )
        , (F.FlexExtra "rhythm", player2Indexed, False)
        ]
      guard $ not $ RTB.null indexed
      return (fpart, emptyPart
        { mania = Just PartMania
          { keys      = if p1
            then if shouldCombine
              then Set.size usedKeys1 + Set.size usedKeys2
              else Set.size usedKeys1
            else if shouldCombine
              then 0 -- shouldn't happen
              else Set.size usedKeys2
          , turntable = if p1
            then Set.member BMScratch usedKeys1
            else if shouldCombine
              then False -- shouldn't happen
              else Set.member BMScratch usedKeys2
          , difficulty = Tier 1 -- ?
          , instrument = Nothing
          , charts = pure "chart"
          }
        })
    }
