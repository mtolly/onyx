{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Import.BMS where

import           Control.Monad                    (forM, guard)
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust)
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.Audio
import           Onyx.Beatmania.BMS
import           Onyx.DTXMania.DTX
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Edge (..), Key (..),
                                                   blipEdgesRB_,
                                                   joinEdgesSimple)
import           Onyx.MIDI.Track.File             (FlexPartName (..))
import qualified Onyx.MIDI.Track.File             as RBFile
import           Onyx.MIDI.Track.ProKeys
import           Onyx.Project
import           Onyx.StackTrace
import           System.FilePath                  (takeDirectory, (<.>), (</>))

joinLongNotes :: (NNC.C t) =>
  RTB.T t (BMKey, Chip, Bool) -> RTB.T t (BMKey, Chip, t)
joinLongNotes
  = fmap (\(chip, key, t) -> (key, chip, t))
  . joinEdgesSimple
  . fmap (\(key, chip, b) -> if b then EdgeOn chip key else EdgeOff key)

importBMS :: (SendMessage m, MonadIO m) => FilePath -> Import m
importBMS bmsPath level = do
  bms <- stackIO $ readBMSLines <$> loadDTXLines bmsPath

  -- TODO if .pms extension, combine notes (on p1 and p2) into 9k track

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
          track = Just $ RBFile.SamplesTrack
            $ fmap (\chip -> RBFile.SampleTrigger chip chip)
            -- don't include sample if we didn't find its audio
            $ RTB.filter (\chip -> HS.member chip foundChips) chips
          in (audios, track)
      (songAudios, songSampleTrack) = audioForChips "audio-bgm" $ bms_BGM bms
      (p1Audios, p1SampleTrack) = audioForChips "audio-p1" $ RTB.merge
        (snd <$> bms_Player1 bms)
        (RTB.mapMaybe (\(_key, chip, b) -> guard b >> Just chip) $ bms_Player1Long bms)
      (p2Audios, p2SampleTrack) = audioForChips "audio-p2" $ RTB.merge
        (snd <$> bms_Player2 bms)
        (RTB.mapMaybe (\(_key, chip, b) -> guard b >> Just chip) $ bms_Player2Long bms)

  let audioExpr name = PlanAudio
        { expr = Input $ Named name
        , pans = []
        , vols = []
        }
      midi = case level of
        ImportQuick -> emptyChart
        ImportFull -> RBFile.Song (bms_TempoMap bms) (bms_MeasureMap bms) mempty
          { RBFile.onyxParts = Map.fromList $ do
            (fpart, chips, chipsLong) <-
              [ (FlexExtra "player1", bms_Player1 bms, bms_Player1Long bms)
              , (FlexExtra "player2", bms_Player2 bms, bms_Player2Long bms)
              ]
            guard $ not $ RTB.null chips && RTB.null chipsLong
            let opart = mempty
                  { RBFile.onyxPartRealKeysX = mempty
                    { pkLanes = RTB.singleton 0 RangeC
                    , pkNotes = let
                      lookupKey = \case
                        BMScratch -> RedYellow E
                        BMKey1    -> RedYellow F
                        BMKey2    -> RedYellow Fs
                        BMKey3    -> RedYellow G
                        BMKey4    -> RedYellow Gs
                        BMKey5    -> RedYellow A
                        BMKey6    -> RedYellow As
                        BMKey7    -> RedYellow B
                      short = flip fmap chips
                        $ \(key, _) -> (lookupKey key, Nothing)
                      long = flip fmap (joinLongNotes chipsLong)
                        $ \(key, _, len) -> (lookupKey key, Just len)
                      in blipEdgesRB_ $ RTB.merge short long
                    }
                  }
            return (fpart, opart)
          , RBFile.onyxSamples = Map.fromList $ catMaybes
            [ ("audio-bgm",) <$> songSampleTrack
            , ("audio-p1" ,) <$> p1SampleTrack
            , ("audio-p2" ,) <$> p2SampleTrack
            ]
          }

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
      { backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      , fileMidi = SoftFile "notes.mid" $ SoftChart midi
      , fileSongAnim = Nothing
      }
    , audio = HM.fromList $ chipAudio <> songAudios <> p1Audios <> p2Audios
    , jammit = HM.empty
    , plans = HM.singleton "bms" $ StandardPlan StandardPlanInfo
      { song        = guard (isJust songSampleTrack) >> Just (audioExpr "audio-bgm")
      , countin     = Countin []
      , parts       = Parts $ HM.fromList $ catMaybes
        [ guard (isJust p1SampleTrack) >> Just (FlexExtra "player1", PartSingle $ audioExpr "audio-p1")
        , guard (isJust p2SampleTrack) >> Just (FlexExtra "player2", PartSingle $ audioExpr "audio-p2")
        ]
      , crowd       = Nothing
      , comments    = []
      , tuningCents = 0
      , fileTempo   = Nothing
      }
    , targets = HM.empty
    , parts = Parts $ HM.fromList $ do
      (fpart, chips) <-
        [ (FlexExtra "player1", bms_Player1 bms)
        , (FlexExtra "player2", bms_Player2 bms)
        ]
      guard $ not $ RTB.null chips
      return $ (fpart,) def
        { proKeys = Just PartProKeys
          { difficulty  = Tier 1
          , fixFreeform = True
          }
        }
    }
