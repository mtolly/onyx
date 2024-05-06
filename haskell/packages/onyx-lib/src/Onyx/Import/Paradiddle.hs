{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.Paradiddle (importParadiddle, findParadiddle) where

import           Control.Monad                    (forM, guard)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Reader       (runReaderT)
import qualified Data.ByteString                  as B
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrdOn, sort)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
import qualified Data.Text                        as T
import           Linear                           (V3 (..))
import           Onyx.Audio                       (Audio (..), audioChannels)
import           Onyx.Codec.JSON
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..))
import           Onyx.MIDI.Read                   (mapTrack)
import           Onyx.MIDI.Track.Drums            (DrumVelocity (..))
import           Onyx.MIDI.Track.Drums.Elite
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.Paradiddle
import           Onyx.Project                     hiding (Difficulty (..))
import           Onyx.StackTrace
import           Onyx.Util.Handle                 (fileReadable)
import           Onyx.Util.Text.Decode            (decodeGeneral)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeDirectory, takeExtension,
                                                   (<.>), (</>))

paraToTrue :: Difficulty -> RLRR -> EliteDrumTrack U.Seconds
paraToTrue diff rlrr = let
  ghostThreshold = rlrr.highwaySettings >>= \hs -> do
    guard $ fromMaybe False hs.ghostNotes
    hs.ghostNoteThreshold
  accentThreshold = rlrr.highwaySettings >>= \hs -> do
    guard $ fromMaybe False hs.accentNotes
    hs.accentNoteThreshold
  getVelocity vel
    | maybe False (vel <=) ghostThreshold  = VelocityGhost
    | maybe False (vel >=) accentThreshold = VelocityAccent
    | otherwise                            = VelocityNormal
  instMapping = Map.fromList $ do
    inst <- rlrr.instruments
    let isRightSide = case inst.location of V3 _ y _ -> y >= 0
    gem <- if
      -- should check for other classes
      -- TODO "Tambourine"
      | "Kick"     `T.isInfixOf` inst.class_ -> return Kick
      | "Snare"    `T.isInfixOf` inst.class_ -> return Snare
      | "Crash"    `T.isInfixOf` inst.class_ -> return $ if isRightSide then CrashR else CrashL
      | "China"    `T.isInfixOf` inst.class_ -> return $ if isRightSide then CrashR else CrashL
      | "Tom1"     `T.isInfixOf` inst.class_ -> return Tom1
      | "Tom2"     `T.isInfixOf` inst.class_ -> return Tom2
      | "FloorTom" `T.isInfixOf` inst.class_ -> return Tom3
      | "HiHat"    `T.isInfixOf` inst.class_ -> return $ if isRightSide then Ride else Hihat
      | "Ride"     `T.isInfixOf` inst.class_ -> return $ if isRightSide then Ride else Hihat
      | otherwise                            -> []
    return (inst.name, gem)
  gems = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort $ do
    event <- rlrr.events
    gem <- toList $ Map.lookup event.name instMapping
    return (realToFrac event.time :: U.Seconds, (gem, getVelocity event.vel))
  -- TODO in case of a flam this will randomly pick one of the velocities to keep
  gemsNoDupe = RTB.flatten $ fmap (nubOrdOn fst) $ RTB.collectCoincident gems
  flams = RTB.mapMaybe (guard . hasDupe) $ RTB.collectCoincident $ RTB.filter (\(gem, _) -> gem /= Kick) gems
  hasDupe xs = nubOrdOn fst xs /= xs
  in mempty
    { tdDifficulties = Map.singleton diff EliteDrumDifficulty
      { tdGems        = fmap (\(gem, vel) -> (gem, TBDefault, vel)) gemsNoDupe
      , tdKick2       = RTB.empty -- TODO kick flams
      , tdFlam        = flams
      , tdHihatOpen   = RTB.empty
      , tdHihatClosed = RTB.empty
      , tdDisco       = RTB.empty
      , tdRim         = RTB.empty
      , tdChoke       = RTB.empty
      }
    }

paraTempoMap :: RLRR -> U.TempoMap
paraTempoMap rlrr = U.tempoMapFromSecondsBPS $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ do
  bpm <- rlrr.bpmEvents
  return (realToFrac bpm.time :: U.Seconds, realToFrac bpm.bpm / 60 :: U.BPS)

importParadiddle :: (SendMessage m, MonadIO m) => NE.NonEmpty (Difficulty, FilePath) -> Import m
importParadiddle diffs level = do
  let (diff, path) = NE.head diffs
      dir = takeDirectory path
      loadJSON f = do
        -- usually should be utf-8, but utf-16 (with BOM) has been spotted
        json <- stackIO (B.readFile f) >>= decodeJSONText . decodeGeneral
        mapStackTraceT (`runReaderT` json) fromJSON
  rlrr <- loadJSON path
  let tmap = paraTempoMap    rlrr
      true = paraToTrue diff rlrr
      art = case rlrr.recordingMetadata.coverImagePath of
        Nothing -> Nothing
        Just f  -> let
          f' = dir </> T.unpack f
          in Just $ SoftFile ("cover" <.> takeExtension f') $ SoftReadable $ fileReadable f'
      getAudio = case level of
        ImportQuick -> const $ return []
        ImportFull  -> mapM $ \f -> do
          let f' = dir </> T.unpack f
          chans <- audioChannels f' >>= maybe (fatal $ "Couldn't query audio channels for: " <> show f') return
          return (f, AudioFile AudioInfo
            { md5      = Nothing
            , frames   = Nothing
            , filePath = Just $ SoftFile (T.unpack f) $ SoftReadable $ fileReadable f'
            , commands = []
            , rate     = Nothing
            , channels = chans
            })
  lowerDiffs <- case level of
    ImportQuick -> return []
    ImportFull  -> forM (NE.tail diffs) $ \(lowerDiff, lowerPath) -> do
      lowerRLRR <- loadJSON lowerPath
      return $ paraToTrue lowerDiff lowerRLRR
  songAudio <- getAudio rlrr.audioFileData.songTracks
  drumAudio <- getAudio rlrr.audioFileData.drumTracks
  -- TODO probably need to apply calibrationOffset somewhere
  let audioExpr auds = do
        auds' <- NE.nonEmpty $ map fst auds
        Just $ case auds' of
          aud NE.:| [] -> Input $ Named aud
          _            -> Mix $ fmap (Input . Named) auds'
  return SongYaml
    { metadata = def'
      { title        = Just rlrr.recordingMetadata.title
      , artist       = rlrr.recordingMetadata.artist
      , author       = rlrr.recordingMetadata.creator
      , fileAlbumArt = art
      }
    , jammit = mempty
    , global = def'
      { backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      , fileMidi = SoftFile "notes.mid" $ SoftChart $ case level of
        ImportFull  -> F.Song
          { F.s_tempos = tmap
          , F.s_signatures = U.measureMapFromTimeSigs U.Error RTB.empty
          , F.s_tracks = mempty
            { F.onyxParts = Map.singleton F.FlexDrums mempty
              { F.onyxPartEliteDrums = mapTrack (U.unapplyTempoTrack tmap)
                $ mconcat $ true : lowerDiffs
              }
            }
          }
        ImportQuick -> emptyChart
      , fileSongAnim = Nothing
      }
    , audio = HM.fromList $ songAudio <> drumAudio
    , plans = HM.singleton "plan" $ StandardPlan StandardPlanInfo
      { song = audioExpr songAudio
      , parts = Parts $ HM.fromList $ do
        drumExpr <- toList $ audioExpr drumAudio
        return (F.FlexDrums, PartSingle drumExpr)
      , crowd = Nothing
      , comments = []
      , tuningCents = 0
      , fileTempo = Nothing
      }
    , targets = HM.empty
    , parts = Parts $ HM.singleton F.FlexDrums (emptyPart :: Part SoftFile)
      { drums = Just $ emptyPartDrums DrumsTrue Kicks1x
      }
    }

findParadiddle :: [FilePath] -> [NE.NonEmpty (Difficulty, FilePath)]
findParadiddle fs = let
  prefixed = do
    f <- fs
    let t = T.pack f
        tryDifficulty diff = do
          prefix <- T.stripSuffix ("_" <> T.pack (show diff) <> ".rlrr") t
          return $ Map.singleton prefix $ Map.singleton diff f
    take 1 $ mapMaybe tryDifficulty [Easy, Medium, Hard, Expert]
  in mapMaybe (NE.nonEmpty . Map.toDescList) $ Map.elems $ Map.unionsWith Map.union prefixed
