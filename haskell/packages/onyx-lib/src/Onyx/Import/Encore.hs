{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
module Onyx.Import.Encore where

import           Control.Monad.Extra              (forM, guard, mapMaybeM, when)
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import qualified Data.HashMap.Strict              as HM
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (listToMaybe)
import qualified Data.Text                        as T
import           Onyx.Audio                       (Audio (..),
                                                   audioChannelsReadable)
import           Onyx.Encore
import           Onyx.Import.Base
import           Onyx.MIDI.Track.Events           (EventsTrack (..))
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.MIDI.Track.Mania            (ManiaTrack (..))
import           Onyx.Project                     hiding (Difficulty)
import qualified Onyx.Project                     as P
import           Onyx.StackTrace                  (SendMessage, lg, stackIO)
import           Onyx.Util.Files                  (fixFileCase)
import           Onyx.Util.Handle                 (Readable, fileReadable)
import           Optics                           ((.~))
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.FilePath                  (takeExtension, (<.>), (</>))
import           Text.Read                        (readMaybe)

-- TODO maybe import lead to PartKeys if sig/icon_guitar == Keyboard

importEncore :: (SendMessage m, MonadIO m) => FilePath -> Import m
importEncore dir level = do
  when (level == ImportFull) $ lg $ "Importing Encore song from: " <> dir

  info <- fixFileCase (dir </> "info.json") >>= loadEncoreInfo

  origMidi <- case level of
    ImportQuick -> return Nothing
    ImportFull  -> Just . fileReadable <$> fixFileCase info.midi
  mid <- case origMidi of
    Nothing -> return emptyChart
    Just m  -> F.loadMIDIReadable @EncoreFile m

  let existingPaths :: [FilePath] -> IO [FilePath]
      existingPaths paths = flip mapMaybeM paths \p -> do
        p' <- fixFileCase p
        e <- Dir.doesFileExist p'
        return $ guard e >> Just p'
      audioFor :: T.Text -> IO (Maybe (Audio t (FilePath, Readable)))
      audioFor key = mapM existingPaths (HM.lookup key info.stems) <&> \case
        Just [x]    -> Just $ Input (T.unpack key <.> takeExtension x, fileReadable x)
        Just (x : xs) -> Just $ Mix $ NE.zipWith
          (\i y -> Input ((T.unpack key <> "-" <> show i) <.> takeExtension y, fileReadable y))
          ((1 :: Int) NE.:| [2..])
          (x NE.:| xs)
        _ -> Nothing
  audioDrums   <- stackIO $ audioFor "drums"
  audioBass    <- stackIO $ audioFor "bass"
  audioLead    <- stackIO $ audioFor "lead"
  audioGuitar  <- stackIO $ audioFor "guitar" -- not seen, adding just in case
  audioKeys    <- stackIO $ audioFor "keys"
  audioVocals  <- stackIO $ audioFor "vocals"
  audioBacking <- stackIO $ audioFor "backing"

  art <- case info.art of
    Nothing -> return Nothing
    Just f  -> do
      f' <- fixFileCase f
      e <- stackIO $ Dir.doesFileExist f'
      return $ guard e >> Just f'

  audios <- do
    let pairs = [audioDrums, audioBass, audioLead, audioGuitar, audioKeys, audioVocals, audioBacking] >>= toList >>= toList
    fmap HM.fromList $ forM pairs \(f, r) -> do
      chans <- audioChannelsReadable r
      return (T.pack f, AudioFile AudioInfo
        { md5      = Nothing
        , frames   = Nothing
        , filePath = Just $ SoftFile f $ SoftReadable r
        , commands = []
        , rate     = Nothing
        , channels = chans
        })

  return SongYaml
    { metadata = Metadata
      { title         = Just info.title
      , titleJP       = Nothing
      , artist        = Just info.artist
      , artistJP      = Nothing
      , album         = info.album
      , genre         = case info.genres of
        [] -> Nothing
        xs -> Just $ T.intercalate ", " xs
      , subgenre      = Nothing
      , year          = info.release_year >>= readMaybe . T.unpack
      , fileAlbumArt  = art <&> \f ->
        SoftFile ("album" <.> takeExtension f) $ SoftReadable $ fileReadable f
      , trackNumber   = Nothing
      , comments      = []
      , difficulty    = Tier 1
      , key           = Nothing
      , author        = case info.charters of
        [] -> Nothing
        xs -> Just $ T.intercalate ", " xs
      , rating        = Unrated
      , previewStart  = PreviewSeconds . (/ 1000) . fromIntegral <$> info.preview_start_time
      , previewEnd    = Nothing
      , languages     = []
      , convert       = False
      , rhythmKeys    = False
      , rhythmBass    = False
      , catEMH        = False
      , expertOnly    = False
      , cover         = False
      , loadingPhrase = info.loading_phrase
      }
    , global = let Global{..} = def' in Global
      { fileMidi = SoftFile "notes.mid" $ SoftChart $ encoreToOnyxMIDI mid
      , fileMidiOriginal = SoftFile "original.mid" . SoftReadable <$> origMidi
      , fileSongAnim = Nothing
      , backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      , ..
      }
    , audio = audios
    , jammit = HM.empty
    , plans = HM.singleton "encore" $ StandardPlan StandardPlanInfo
      { song        = fmap (\(f, _) -> Named $ T.pack f) <$> audioBacking
      , parts       = Parts $ HM.fromList do
        let (newLead, newGuitar) = case (audioLead, audioGuitar) of
              -- move lead to guitar if possible
              (Just _, Nothing) -> (Nothing  , audioLead  )
              -- otherwise, either no lead track (all good)
              -- or there's lead and guitar (confusing! move lead to backing)
              _                 -> (audioLead, audioGuitar)
        (part, audio) <-
          [ (F.PartDrums      , audioDrums )
          , (F.PartBass       , audioBass  )
          , (F.PartGuitar     , newGuitar  )
          , (F.PartKeys       , audioKeys  )
          , (F.PartVocal      , audioVocals)
          , (F.PartName "lead", newLead    )
          ]
        a <- toList audio
        return (part, PartSingle $ fmap (\(f, _) -> Named $ T.pack f) a)
      , crowd       = Nothing
      , comments    = []
      , tuningCents = 0
      , fileTempo   = Nothing
      }
    , targets = HM.empty
    , parts = encorePartData info.diff mid
    }

encoreToOnyxMIDI :: F.Song (EncoreFile U.Beats) -> F.Song (F.OnyxFile U.Beats)
encoreToOnyxMIDI = fmap (\encore -> mempty
  { F.onyxParts    = Map.fromList
    [ (F.PartGuitar, mempty
      { F.onyxPartGuitar = encore.plasticGuitar
      , F.onyxPartMania = Map.fromList $ encorePart encore.partGuitar.part
      })
    , (F.PartKeys, mempty
      { F.onyxPartGuitar = encore.plasticKeys
      , F.onyxPartMania = Map.fromList $ encorePart encore.partKeys.part
      })
    , (F.PartBass, mempty
      { F.onyxPartGuitar = encore.plasticBass
      , F.onyxPartMania = Map.fromList $ encorePart encore.partBass.part
      })
    , (F.PartDrums, mempty
      { F.onyxPartDrums = encore.plasticDrums
      , F.onyxPartMania = Map.fromList $ encorePart encore.partDrums.part
      })
    , (F.PartVocal, mempty
      { F.onyxPartVocals = encore.pitchedVocals
      , F.onyxPartMania = Map.fromList $ encorePart encore.partVocals
      })
    ]
  , F.onyxEvents   = if RTB.null encore.events.sections
    then encore.events & #sections .~ convertEncoreSections encore.section
    else encore.events
  , F.onyxBeat     = encore.beat
  }) . checkEnableDynamicsEncore

importPadMania :: P.Difficulty -> EncorePart U.Beats -> Maybe ModeMania
importPadMania tier part = do
  convertedDiffs <- NE.nonEmpty $ encorePart part
  return $ ModeMania $ convertedDiffs <&> \(name, diff) -> ManiaChart
    { name = name
    , keys = if name == "expert" || elem 4 [ k | edge <- toList diff.notes, (k, _) <- toList edge ]
      then 5
      else 4
    , style = ManiaEncore
    , difficulty = tier
    }

encorePartData :: HM.HashMap T.Text Int -> F.Song (EncoreFile U.Beats) -> Parts (Part f)
encorePartData diffs mid = let

  getTier :: [T.Text] -> Maybe P.Difficulty
  getTier options = listToMaybe do
    o <- options
    case HM.lookup o diffs of
      Just d | d >= 0 -> [Tier $ fromIntegral d + 1] -- 0..6 -> 1..7
      _               -> []

  in Parts $ HM.fromList
    [ (F.PartGuitar, emptyPart
      -- Run It (Epic Games) has no pg/gr difficulties but seems to have guitar, huh?
      & #grybo .~ do
        tier <- getTier ["pg", "plastic_guitar"]
        Just ModeFive
          { difficulty = tier
          , hopoThreshold = 170
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
      & #mania .~ do
        tier <- getTier ["gr", "guitar"]
        importPadMania tier mid.tracks.partGuitar.part
      )
    , (F.PartKeys, emptyPart
      & #grybo .~ do
        tier <- getTier ["pk", "plastic_keys"] -- pk not seen but just in case
        Just ModeFive
          { difficulty = tier
          , hopoThreshold = 170
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
      & #mania .~ do
        tier <- getTier ["keys"]
        importPadMania tier mid.tracks.partKeys.part
      )
    , (F.PartBass, emptyPart
      & #grybo .~ do
        tier <- getTier ["pb", "plastic_bass"]
        Just ModeFive
          { difficulty = tier
          , hopoThreshold = 170
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
      & #mania .~ do
        tier <- getTier ["ba", "bass"]
        importPadMania tier mid.tracks.partBass.part
      )
    , (F.PartDrums, emptyPart
      & #drums .~ do
        tier <- getTier ["pd", "plastic_drums"]
        Just $ emptyPartDrums DrumsPro Kicks1x
          & #difficulty .~ tier
      & #mania .~ do
        tier <- getTier ["ds", "drums"]
        importPadMania tier mid.tracks.partDrums.part
      )
    , (F.PartVocal, emptyPart
      & #mania .~ do
        tier <- getTier ["vl", "vocals"]
        importPadMania tier mid.tracks.partVocals
      & #vocal .~ do
        tier <- getTier ["pv", "pitched_vocals", "pro_vocals"]
        Just ModeVocal
          { difficulty = tier
          , count      = Vocal1
          , gender     = Nothing
          , key        = Nothing
          , lipsyncRB3 = Nothing
          }
      )
    ]
