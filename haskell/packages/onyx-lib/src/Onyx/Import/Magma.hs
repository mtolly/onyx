{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Onyx.Import.Magma where

import           Control.Monad                     (guard, when)
import           Control.Monad.IO.Class            (MonadIO)
import qualified Data.ByteString.Char8             as B8
import           Data.Char                         (toLower)
import           Data.Default.Class                (def)
import           Data.Foldable                     (toList)
import           Data.Function                     ((&))
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
import           Onyx.MIDI.Track.File              (pattern PartBass,
                                                    pattern PartDrums,
                                                    pattern PartGuitar,
                                                    pattern PartKeys,
                                                    pattern PartVocal)
import qualified Onyx.MIDI.Track.File              as F
import           Onyx.MIDI.Track.ProGuitar         (GtrBase (..),
                                                    GtrTuning (..))
import           Onyx.Project                      hiding (Difficulty)
import           Onyx.StackTrace
import           Onyx.Util.Files                   (fixFileCase)
import           Onyx.Util.Handle                  (fileReadable)
import           Onyx.Util.Text.Decode             (decodeGeneral)
import           Optics                            (set, (.~))
import qualified System.Directory                  as Dir
import           System.FilePath
import           Text.Read                         (readMaybe)

importMagma :: (SendMessage m, MonadIO m) => FilePath -> Import m
importMagma fin level = do
  when (level == ImportFull) $ lg $ "Importing Magma project from: " <> fin

  let oldDir = takeDirectory fin
      locate f = fixFileCase $ oldDir </> f
  RBProj.RBProj rbproj <- stackIO (D.readFileDTA fin) >>= D.unserialize D.stackChunks

  midiLoc <- locate $ T.unpack rbproj.midi.midiFile
  let midi = SoftFile "notes.mid" $ SoftReadable $ fileReadable midiLoc
  bassBase <- detectExtProBass . (.tracks) <$> case level of
    ImportFull  -> F.loadMIDI midiLoc
    ImportQuick -> return emptyChart

  c3 <- do
    pathC3 <- fixFileCase $ fin -<.> "c3"
    hasC3 <- stackIO $ Dir.doesFileExist pathC3
    if hasC3
      then fmap Just $ stackIO (decodeGeneral <$> B8.readFile pathC3) >>= C3.readC3
      else return Nothing

  artLoc <- locate $ maybe (T.unpack rbproj.albumArt.albumArtFile) (.songAlbumArt) c3
  let art = SoftFile ("album" <.> map toLower (takeExtension artLoc)) $ SoftReadable $ fileReadable artLoc

  let hopoThresh = case fmap (.hopoThresholdIndex) c3 of
        Nothing -> 170
        Just 0  -> 90
        Just 1  -> 130
        Just 2  -> 170
        Just 3  -> 250
        Just _  -> 170

  -- TODO detect silent audio files and don't import them
  let getTrack
        :: (MonadIO m)
        => String
        -> (RBProj.Tracks -> RBProj.AudioFile)
        -> StackTraceT m (Maybe (Audio t AudioInput, (T.Text, AudioFile SoftFile)))
      getTrack s f = let
        aud = f rbproj.tracks
        in if aud.audioEnabled
          then do
            src <- locate $ T.unpack aud.audioFile
            let dst = s -<.> map toLower (takeExtension src)
            return $ Just
              ( PansVols
                (map realToFrac aud.pan)
                (map realToFrac aud.vol)
                (Input $ Named $ T.pack s)
              , ( T.pack s
                , AudioFile AudioInfo
                  { md5 = Nothing
                  , frames = Nothing
                  , filePath = Just $ SoftFile dst $ SoftReadable $ fileReadable src
                  , commands = []
                  , rate = Nothing
                  , channels = fromIntegral aud.channels
                  }
                )
              )
          else return Nothing
  drums <- getTrack "drums"  (.drumKit)
  kick  <- getTrack "kick"   (.drumKick)
  snare <- getTrack "snare"  (.drumSnare)
  gtr   <- getTrack "guitar" (.guitar)
  bass  <- getTrack "bass"   (.bass)
  keys  <- getTrack "keys"   (.keys)
  vox   <- getTrack "vocal"  (.vocals)
  song  <- getTrack "song"   (.backing)
  crowd <- case c3 >>= (.crowdAudio) of
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
          (replicate chans $ maybe 0 realToFrac $ c3 >>= (.crowdVol))
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
        Nothing     -> determine2xBass rbproj.metadata.songName
        Just c3file -> (c3file.song, c3file.is2xBass)
      -- TODO support dual 1x+2x projects
      targetName = if is2x then "rb3-2x" else "rb3"
      target = (def :: TargetRB3 SoftFile)
        & do set #is2xBassPedal is2x
        & do set #songID
          case c3 of
            Nothing -> SongIDAutoSymbol
            Just c3file -> if c3file.useNumericID
              then maybe SongIDAutoSymbol SongIDSymbol $ readMaybe $ T.unpack c3file.uniqueNumericID
              else case c3file.customID of "" -> SongIDAutoSymbol; cid -> SongIDSymbol cid

  let readTuning c3fn k = case c3 >>= c3fn of
        Nothing -> return Nothing
        Just tune -> errorToWarning (scanStack tune >>= parseStack) >>= \case
          Just (D.DTA _ (D.Tree _ [D.Parens (D.Tree _ [D.Sym k', D.Parens (D.Tree _ mints)])])) | k == k' ->
            case mapM (\case D.Int i -> Just $ fromIntegral i; _ -> Nothing) mints of
              Just ints -> return $ Just ints
              Nothing   -> warn "Non-integer value in tuning" >> return Nothing
          _ -> warn "Couldn't read DTA-snippet tuning format" >> return Nothing
  tuneGtr  <- inside "Reading pro guitar tuning" $ readTuning (.proGuitarTuning) "real_guitar_tuning"
  tuneBass <- inside "Reading pro bass tuning"   $ readTuning (.proBassTuning4)  "real_bass_tuning"

  return SongYaml
    { metadata = Metadata
      { title        = Just title
      , titleJP      = Nothing
      , artist       = Just $ maybe rbproj.metadata.artistName (.artist) c3
      , artistJP     = Nothing
      , album        = Just $ maybe rbproj.metadata.albumName (.album) c3
      , genre        = Just rbproj.metadata.genre
      , subgenre     = Just rbproj.metadata.subGenre
      , year         = Just $ fromIntegral rbproj.metadata.yearReleased
      , fileAlbumArt = Just art
      , trackNumber  = Just $ fromIntegral rbproj.metadata.trackNumber
      , comments     = []
      , difficulty   = Tier rbproj.gamedata.rankBand
      , key          = fmap (`SongKey` Major) $ c3 >>= (.tonicNote)
      , author       = Just rbproj.metadata.author
      , rating       = case fmap (.songRating) c3 of
        Nothing -> Unrated
        Just 1  -> FamilyFriendly
        Just 2  -> SupervisionRecommended
        Just 3  -> Mature
        Just 4  -> Unrated
        Just _  -> Unrated
      , previewStart = Just $ PreviewSeconds $ fromIntegral rbproj.gamedata.previewStartMs / 1000
      , previewEnd   = Nothing
      , languages    = let
        lang s f = [s | fromMaybe False $ f rbproj.languages]
        in concat
          [ lang "English"  (.english)
          , lang "French"   (.french)
          , lang "Italian"  (.italian)
          , lang "Spanish"  (.spanish)
          , lang "German"   (.german)
          , lang "Japanese" (.japanese)
          ]
      , convert      = maybe False (.convert) c3
      , rhythmKeys   = maybe False (.rhythmKeys) c3
      , rhythmBass   = maybe False (.rhythmBass) c3
      , catEMH       = False -- not stored in .c3 file
      , expertOnly   = maybe False (.expertOnly) c3
      , cover        = maybe False (not . (.isMaster)) c3
      , loadingPhrase = Nothing
      }
    , global = Global
      { fileMidi = midi
      , fileMidiOriginal = Nothing -- don't need this, not editing midi on import
      , fileSongAnim = Nothing
      , autogenTheme = Just case rbproj.midi.autogenTheme of
        Left theme -> theme
        Right _str -> RBProj.DefaultTheme -- TODO
      , animTempo    = Right rbproj.gamedata.animTempo
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
            [(PartDrums, case (kick, snare) of
              (Nothing, Nothing) -> PartSingle drumsAud
              _ -> PartDrumKit
                { kick = fmap fst kick
                , snare = fmap fst snare
                , toms = Nothing
                , kit = drumsAud
                }
            )]
        , toList $ fmap (\(aud, _) -> (PartGuitar, PartSingle aud)) gtr
        , toList $ fmap (\(aud, _) -> (PartBass  , PartSingle aud)) bass
        , toList $ fmap (\(aud, _) -> (PartKeys  , PartSingle aud)) keys
        , toList $ fmap (\(aud, _) -> (PartVocal , PartSingle aud)) vox
        ]
      , crowd = fmap fst crowd
      , comments = []
      , tuningCents = maybe 0 (.tuningCents) c3 -- TODO use this, or Magma.tuningOffsetCents?
      , fileTempo = Nothing
      }
    , targets = HM.singleton targetName $ RB3 target
    , parts = Parts $ HM.fromList
      [ ( PartDrums, emptyPart & #drums .~ do
          guard $ isJust drums
          let mode = DrumsPro -- TODO set to Drums4 for magma v1?
              kicks = if is2x then Kicks2x else Kicks1x
          Just $ emptyPartDrums mode kicks
            & do set #difficulty $ Tier rbproj.gamedata.rankDrum
            &    set #kit do
              case fmap (.drumKitSFX) c3 of
                Nothing -> HardRockKit
                Just 0  -> HardRockKit
                Just 1  -> ArenaKit
                Just 2  -> VintageKit
                Just 3  -> TrashyKit
                Just 4  -> ElectronicKit
                Just _  -> HardRockKit
        )
      , ( PartGuitar, emptyPart
        { grybo = guard (isJust gtr) >> Just ModeFive
          { difficulty = Tier rbproj.gamedata.rankGuitar
          , hopoThreshold = hopoThresh
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proGuitar = do
          diff <- guard (isJust gtr) >> c3 >>= (.proGuitarDiff)
          Just ModeProGuitar
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
      , ( PartBass, emptyPart
        { grybo = guard (isJust bass) >> Just ModeFive
          { difficulty = Tier rbproj.gamedata.rankBass
          , hopoThreshold = hopoThresh
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proGuitar = do
          diff <- guard (isJust gtr) >> c3 >>= (.proBassDiff)
          Just ModeProGuitar
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
      , ( PartKeys, emptyPart
        { grybo = guard (isJust keys) >> Just ModeFive
          { difficulty = Tier rbproj.gamedata.rankKeys
          , hopoThreshold = hopoThresh
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proKeys = guard (isJust keys && maybe False (not . (.disableProKeys)) c3) >> Just ModeProKeys
          { difficulty = Tier rbproj.gamedata.rankProKeys
          , fixFreeform = False
          }
        })
      , ( PartVocal, emptyPart & #vocal .~ do
          guard (isJust vox) >> Just ModeVocal
            { difficulty = Tier rbproj.gamedata.rankVocals
            , count = if
              | rbproj.dryVox.part2.dryVoxEnabled -> Vocal3
              | rbproj.dryVox.part1.dryVoxEnabled -> Vocal2
              | otherwise                         -> Vocal1
            , gender = Just rbproj.gamedata.vocalGender
            , key = Nothing
            , lipsyncRB3 = Nothing
            }
        )
      ]
    }
