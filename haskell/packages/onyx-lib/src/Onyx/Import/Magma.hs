{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.Magma where

import           Control.Monad                     (guard, when)
import           Control.Monad.IO.Class            (MonadIO)
import qualified Data.ByteString.Char8             as B8
import           Data.Char                         (toLower)
import           Data.Default.Class                (def)
import           Data.Foldable                     (toList)
import qualified Data.HashMap.Strict               as HM
import           Data.Maybe                        (catMaybes, fromMaybe,
                                                    isJust)
import qualified Data.Text                         as T
import           Onyx.Audio
import qualified Onyx.C3                           as C3
import           Onyx.Difficulty
import qualified Onyx.Harmonix.DTA                 as D
import           Onyx.Harmonix.DTA.Parse           (parseStack)
import           Onyx.Harmonix.DTA.Scan            (scanStack)
import qualified Onyx.Harmonix.DTA.Serialize       as D
import qualified Onyx.Harmonix.DTA.Serialize.Magma as RBProj
import           Onyx.Import.Base
import           Onyx.MIDI.Common
import           Onyx.MIDI.Track.File              (FlexPartName (..))
import qualified Onyx.MIDI.Track.File              as F
import           Onyx.MIDI.Track.ProGuitar         (GtrBase (..),
                                                    GtrTuning (..))
import           Onyx.Project                      hiding (Difficulty)
import           Onyx.StackTrace
import           Onyx.Util.Files                   (fixFileCase)
import           Onyx.Util.Handle                  (fileReadable)
import           Onyx.Util.Text.Decode             (decodeGeneral)
import qualified System.Directory                  as Dir
import           System.FilePath
import           Text.Read                         (readMaybe)

importMagma :: (SendMessage m, MonadIO m) => FilePath -> Import m
importMagma fin level = do
  when (level == ImportFull) $ lg $ "Importing Magma project from: " <> fin

  let oldDir = takeDirectory fin
      locate f = fixFileCase $ oldDir </> f
  RBProj.RBProj rbproj <- stackIO (D.readFileDTA fin) >>= D.unserialize D.stackChunks

  midiLoc <- locate $ T.unpack $ RBProj.midiFile $ RBProj.midi rbproj
  let midi = SoftFile "notes.mid" $ SoftReadable $ fileReadable midiLoc
  bassBase <- detectExtProBass . F.s_tracks <$> case level of
    ImportFull  -> F.loadMIDI midiLoc
    ImportQuick -> return emptyChart

  c3 <- do
    pathC3 <- fixFileCase $ fin -<.> "c3"
    hasC3 <- stackIO $ Dir.doesFileExist pathC3
    if hasC3
      then fmap Just $ stackIO (decodeGeneral <$> B8.readFile pathC3) >>= C3.readC3
      else return Nothing

  artLoc <- locate $ maybe (T.unpack $ RBProj.albumArtFile $ RBProj.albumArt rbproj) C3.songAlbumArt c3
  let art = SoftFile ("album" <.> map toLower (takeExtension artLoc)) $ SoftReadable $ fileReadable artLoc

  let hopoThresh = case fmap C3.hopoThresholdIndex c3 of
        Nothing -> 170
        Just 0  -> 90
        Just 1  -> 130
        Just 2  -> 170
        Just 3  -> 250
        Just _  -> 170

  -- TODO detect silent audio files and don't import them
  let getTrack s f = let
        aud = f $ RBProj.tracks rbproj
        in if RBProj.audioEnabled aud
          then do
            src <- locate $ T.unpack $ RBProj.audioFile aud
            let dst = s -<.> map toLower (takeExtension src)
            return $ Just
              ( PansVols
                (map realToFrac $ RBProj.pan aud)
                (map realToFrac $ RBProj.vol aud)
                (Input $ Named $ T.pack s)
              , ( T.pack s
                , AudioFile AudioInfo
                  { md5 = Nothing
                  , frames = Nothing
                  , filePath = Just $ SoftFile dst $ SoftReadable $ fileReadable src
                  , commands = []
                  , rate = Nothing
                  , channels = fromIntegral $ RBProj.channels aud
                  }
                )
              )
          else return Nothing
  drums <- getTrack "drums" RBProj.drumKit
  kick <- getTrack "kick" RBProj.drumKick
  snare <- getTrack "snare" RBProj.drumSnare
  gtr <- getTrack "guitar" RBProj.guitar
  bass <- getTrack "bass" RBProj.bass
  keys <- getTrack "keys" RBProj.keys
  vox <- getTrack "vocal" RBProj.vocals
  song <- getTrack "song" RBProj.backing
  crowd <- case c3 >>= C3.crowdAudio of
    Nothing -> return Nothing
    Just f  -> do
      src <- locate $ T.unpack f
      let s = "crowd"
          dst = s -<.> map toLower (takeExtension src)
      chans <- audioChannels src >>= \case
        Just c -> return c
        Nothing -> do
          warn "Couldn't detect crowd audio channels; assuming 2."
          return 2
      return $ Just
        ( PansVols
          (case chans of 2 -> [-1, 1]; _ -> replicate chans 0) -- eh
          (replicate chans $ maybe 0 realToFrac $ c3 >>= C3.crowdVol)
          (Input $ Named $ T.pack s)
        , ( T.pack s
          , AudioFile AudioInfo
            { md5 = Nothing
            , frames = Nothing
            , filePath = Just $ SoftFile dst $ SoftReadable $ fileReadable src
            , commands = []
            , rate = Nothing
            , channels = chans
            }
          )
        )
  let allAudio = map snd $ catMaybes [drums, kick, snare, gtr, bass, keys, vox, song, crowd]

  let (title, is2x) = case c3 of
        Nothing     -> determine2xBass $ RBProj.songName $ RBProj.metadata rbproj
        Just c3file -> (C3.song c3file, C3.is2xBass c3file)
      -- TODO support dual 1x+2x projects
      targetName = if is2x then "rb3-2x" else "rb3"
      target = (def :: TargetRB3 SoftFile)
        { is2xBassPedal = is2x
        , songID = case c3 of
          Nothing -> SongIDAutoSymbol
          Just c3file -> if C3.useNumericID c3file
            then maybe SongIDAutoSymbol SongIDSymbol $ readMaybe $ T.unpack (C3.uniqueNumericID c3file)
            else case C3.customID c3file of "" -> SongIDAutoSymbol; cid -> SongIDSymbol cid
        }

  let readTuning c3fn k = case c3 >>= c3fn of
        Nothing -> return Nothing
        Just tune -> errorToWarning (scanStack tune >>= parseStack) >>= \case
          Just (D.DTA _ (D.Tree _ [D.Parens (D.Tree _ [D.Sym k', D.Parens (D.Tree _ mints)])])) | k == k' ->
            case mapM (\case D.Int i -> Just $ fromIntegral i; _ -> Nothing) mints of
              Just ints -> return $ Just ints
              Nothing   -> warn "Non-integer value in tuning" >> return Nothing
          _ -> warn "Couldn't read DTA-snippet tuning format" >> return Nothing
  tuneGtr <- inside "Reading pro guitar tuning" $ readTuning C3.proGuitarTuning "real_guitar_tuning"
  tuneBass <- inside "Reading pro bass tuning" $ readTuning C3.proBassTuning4 "real_bass_tuning"

  return SongYaml
    { metadata = Metadata
      { title        = Just title
      , titleJP      = Nothing
      , artist       = Just $ maybe (RBProj.artistName $ RBProj.metadata rbproj) C3.artist c3
      , artistJP     = Nothing
      , album        = Just $ maybe (RBProj.albumName $ RBProj.metadata rbproj) C3.album c3
      , genre        = Just $ RBProj.genre $ RBProj.metadata rbproj
      , subgenre     = Just $ RBProj.subGenre $ RBProj.metadata rbproj
      , year         = Just $ fromIntegral $ RBProj.yearReleased $ RBProj.metadata rbproj
      , fileAlbumArt = Just art
      , trackNumber  = Just $ fromIntegral $ RBProj.trackNumber $ RBProj.metadata rbproj
      , comments     = []
      , difficulty   = Tier $ RBProj.rankBand $ RBProj.gamedata rbproj
      , key          = fmap (`SongKey` Major) $ c3 >>= C3.tonicNote
      , author       = Just $ RBProj.author $ RBProj.metadata rbproj
      , rating       = case fmap C3.songRating c3 of
        Nothing -> Unrated
        Just 1  -> FamilyFriendly
        Just 2  -> SupervisionRecommended
        Just 3  -> Mature
        Just 4  -> Unrated
        Just _  -> Unrated
      , previewStart = Just $ PreviewSeconds $ fromIntegral (RBProj.previewStartMs $ RBProj.gamedata rbproj) / 1000
      , previewEnd   = Nothing
      , languages    = let
        lang s f = [s | fromMaybe False $ f $ RBProj.languages rbproj]
        in concat
          [ lang "English"  RBProj.english
          , lang "French"   RBProj.french
          , lang "Italian"  RBProj.italian
          , lang "Spanish"  RBProj.spanish
          , lang "German"   RBProj.german
          , lang "Japanese" RBProj.japanese
          ]
      , convert      = maybe False C3.convert c3
      , rhythmKeys   = maybe False C3.rhythmKeys c3
      , rhythmBass   = maybe False C3.rhythmBass c3
      , catEMH       = False -- not stored in .c3 file
      , expertOnly   = maybe False C3.expertOnly c3
      , cover        = maybe False (not . C3.isMaster) c3
      , loadingPhrase = Nothing
      }
    , global = Global
      { fileMidi = midi
      , fileSongAnim = Nothing
      , autogenTheme = Just $ case RBProj.autogenTheme $ RBProj.midi rbproj of
        Left theme -> theme
        Right _str -> RBProj.DefaultTheme -- TODO
      , animTempo    = Right $ RBProj.animTempo $ RBProj.gamedata rbproj
      , backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      }
    , audio = HM.fromList allAudio
    , jammit = HM.empty
    , plans = HM.singleton "rbproj" $ StandardPlan StandardPlanInfo
      { song = fmap fst song
      , parts = Parts $ HM.fromList $ concat
        [ case drums of
          Nothing -> []
          Just (drumsAud, _) ->
            [(FlexDrums, case (kick, snare) of
              (Nothing, Nothing) -> PartSingle drumsAud
              _ -> PartDrumKit
                { kick = fmap fst kick
                , snare = fmap fst snare
                , toms = Nothing
                , kit = drumsAud
                }
            )]
        , toList $ fmap (\(aud, _) -> (FlexGuitar, PartSingle aud)) gtr
        , toList $ fmap (\(aud, _) -> (FlexBass  , PartSingle aud)) bass
        , toList $ fmap (\(aud, _) -> (FlexKeys  , PartSingle aud)) keys
        , toList $ fmap (\(aud, _) -> (FlexVocal , PartSingle aud)) vox
        ]
      , crowd = fmap fst crowd
      , comments = []
      , tuningCents = maybe 0 C3.tuningCents c3 -- TODO use this, or Magma.tuningOffsetCents?
      , fileTempo = Nothing
      }
    , targets = HM.singleton targetName $ RB3 target
    , parts = Parts $ HM.fromList
      [ ( FlexDrums, (emptyPart :: Part SoftFile)
        { drums = guard (isJust drums) >> let
          mode = DrumsPro -- TODO set to Drums4 for magma v1?
          kicks = if is2x then Kicks2x else Kicks1x
          in Just (emptyPartDrums mode kicks)
            { difficulty = Tier $ RBProj.rankDrum $ RBProj.gamedata rbproj
            , kit = case fmap C3.drumKitSFX c3 of
              Nothing -> HardRockKit
              Just 0  -> HardRockKit
              Just 1  -> ArenaKit
              Just 2  -> VintageKit
              Just 3  -> TrashyKit
              Just 4  -> ElectronicKit
              Just _  -> HardRockKit
            }
        })
      , ( FlexGuitar, emptyPart
        { grybo = guard (isJust gtr) >> Just PartGRYBO
          { difficulty = Tier $ RBProj.rankGuitar $ RBProj.gamedata rbproj
          , hopoThreshold = hopoThresh
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proGuitar = do
          diff <- guard (isJust gtr) >> c3 >>= C3.proGuitarDiff
          Just PartProGuitar
            { difficulty = Tier $ rankToTier proGuitarDiffMap $ fromIntegral diff
            , hopoThreshold = hopoThresh
            , tuning = GtrTuning
              { gtrBase = Guitar6
              , gtrOffsets = fromMaybe [] tuneGtr
              , gtrGlobal = 0
              , gtrCapo = 0
              , gtrName = Nothing
              }
            , fixFreeform = False
            , tones = Nothing
            , pickedBass = False
            , tuningRSBass = Nothing
            }
        })
      , ( FlexBass, emptyPart
        { grybo = guard (isJust bass) >> Just PartGRYBO
          { difficulty = Tier $ RBProj.rankBass $ RBProj.gamedata rbproj
          , hopoThreshold = hopoThresh
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proGuitar = do
          diff <- guard (isJust gtr) >> c3 >>= C3.proBassDiff
          Just PartProGuitar
            { difficulty = Tier $ rankToTier proBassDiffMap $ fromIntegral diff
            , hopoThreshold = hopoThresh
            , tuning = GtrTuning
              { gtrBase = bassBase
              , gtrOffsets = fromMaybe [] tuneBass
              , gtrGlobal = 0
              , gtrCapo = 0
              , gtrName = Nothing
              }
            , fixFreeform = False
            , tones = Nothing
            , pickedBass = False
            , tuningRSBass = Nothing
            }
        })
      , ( FlexKeys, emptyPart
        { grybo = guard (isJust keys) >> Just PartGRYBO
          { difficulty = Tier $ RBProj.rankKeys $ RBProj.gamedata rbproj
          , hopoThreshold = hopoThresh
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proKeys = guard (isJust keys && maybe False (not . C3.disableProKeys) c3) >> Just PartProKeys
          { difficulty = Tier $ RBProj.rankProKeys $ RBProj.gamedata rbproj
          , fixFreeform = False
          }
        })
      , ( FlexVocal, (emptyPart :: Part SoftFile)
        { vocal = guard (isJust vox) >> Just PartVocal
          { difficulty = Tier $ RBProj.rankVocals $ RBProj.gamedata rbproj
          , count = if
            | RBProj.dryVoxEnabled $ RBProj.part2 $ RBProj.dryVox rbproj -> Vocal3
            | RBProj.dryVoxEnabled $ RBProj.part1 $ RBProj.dryVox rbproj -> Vocal2
            | otherwise                                                  -> Vocal1
          , gender = Just $ RBProj.vocalGender $ RBProj.gamedata rbproj
          , key = Nothing
          , lipsyncRB3 = Nothing
          }
        })
      ]
    }
