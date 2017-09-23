{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
module Build (shakeBuildTarget, shakeBuildMagmaProject, shakeBuildFiles, targetTitle, loadYaml) where

import           Audio
import           AudioSearch
import qualified C3
import qualified Codec.Archive.Zip                     as Zip
import           Codec.Picture
import           Config                                hiding (Difficulty)
import qualified Control.Exception                     as Exc
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class             (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Lazy                  as BL
import           Data.Char                             (isAscii, isControl,
                                                        isSpace)
import           Data.Conduit.Audio
import           Data.Conduit.Audio.SampleRate
import           Data.Conduit.Audio.Sndfile
import           Data.Default.Class                    (def)
import qualified Data.DTA                              as D
import qualified Data.DTA.Serialize                    as D
import qualified Data.DTA.Serialize.Magma              as Magma
import qualified Data.DTA.Serialize.RB3                as D
import qualified Data.EventList.Absolute.TimeBody      as ATB
import qualified Data.EventList.Relative.TimeBody      as RTB
import           Data.Fixed                            (Centi)
import           Data.Foldable                         (toList)
import           Data.Hashable                         (hash)
import qualified Data.HashMap.Strict                   as HM
import           Data.List                             (intercalate, nub)
import           Data.Maybe                            (fromMaybe, isJust,
                                                        isNothing, mapMaybe)
import           Data.Monoid                           ((<>))
import qualified Data.Set                              as Set
import           Data.String                           (IsString, fromString)
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as TIO
import           Development.Shake                     hiding (phony)
import qualified Development.Shake                     as Shake
import           Development.Shake.FilePath
import           Difficulty
import           DryVox                                (clipDryVox,
                                                        toDryVoxFormat,
                                                        vocalTubes)
import qualified FretsOnFire                           as FoF
import           Genre
import           GuitarHeroII.Audio                    (writeVGS)
import           GuitarHeroII.Convert
import           Image
import           JSONData                              (StackJSON (..),
                                                        fromJSON, stackShow)
import qualified Magma
import qualified MelodysEscape
import           MoggDecrypt
import           Path                                  (parseAbsDir, toFilePath)
import           PrettyDTA
import           ProKeysRanges
import           Reaper.Build                          (makeReaper)
import           RenderAudio
import           Resources                             (emptyMilo, emptyMiloRB2,
                                                        emptyWeightsRB2,
                                                        onyxAlbum, webDisplay)
import           RockBand.Common                       (Difficulty (..),
                                                        readCommandList)
import qualified RockBand.Drums                        as RBDrums
import qualified RockBand.Events                       as Events
import qualified RockBand.File                         as RBFile
import qualified RockBand.FiveButton                   as RBFive
import           RockBand.Parse                        (unparseBlip,
                                                        unparseList)
import           RockBand.PhaseShiftMessage            (discardPS)
import qualified RockBand.PhaseShiftMessage            as PSM
import qualified RockBand.ProGuitar                    as ProGtr
import qualified RockBand.ProGuitar.Play               as PGPlay
import           RockBand.Sections                     (makeRB2Section,
                                                        makeRBN2Sections)
import qualified RockBand.Vocals                       as RBVox
import qualified RockBand2                             as RB2
import qualified RockBand3                             as RB3
import           Scripts
import qualified Sound.File.Sndfile                    as Snd
import qualified Sound.Jammit.Base                     as J
import qualified Sound.Jammit.Export                   as J
import qualified Sound.MIDI.File                       as F
import qualified Sound.MIDI.File.Event                 as E
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.File.Load                  as Load
import qualified Sound.MIDI.File.Save                  as Save
import qualified Sound.MIDI.Util                       as U
import qualified System.Directory                      as Dir
import           System.Environment.Executable         (getExecutablePath)
import           System.IO                             (IOMode (ReadMode),
                                                        hFileSize,
                                                        withBinaryFile)
import           WebPlayer                             (makeDisplay)
import           X360DotNet
import           YAMLTree

targetTitle :: SongYaml -> Target -> T.Text
targetTitle songYaml target = let
  segments = getTitle (_metadata songYaml) : case target of
    RB3 TargetRB3{..} -> makeLabel []                rb3_Label rb3_2xBassPedal rb3_Speed
    RB2 TargetRB2{..} -> makeLabel ["(RB2 version)"] rb2_Label rb2_2xBassPedal rb2_Speed
    PS  TargetPS {..} -> makeLabel []                ps_Label  False           ps_Speed
    GH2 TargetGH2{..} -> []
  makeLabel sfxs explicit is2x speed = case explicit of
    Just lbl -> [lbl]
    Nothing  -> concat
      [ case speed of
        Nothing  -> []
        Just 1   -> []
        Just spd -> let
          intSpeed :: Int
          intSpeed = round $ spd * 100
          in ["(" <> T.pack (show intSpeed) <> "% Speed)"]
      , ["(2x Bass Pedal)" | is2x]
      , sfxs
      ]
  in T.intercalate " " segments

toValidFileName :: T.Text -> T.Text
toValidFileName t = let
  eachChar c = if isAscii c && not (isControl c) && notElem c ['/', '\\', '?']
    then c
    else '_'
  -- TODO better char filter
  in case T.map eachChar t of
    ""   -> "nothing"
    "."  -> "dot"
    ".." -> "dots"
    t'   -> t'

hashRB3 :: SongYaml -> TargetRB3 -> Int
hashRB3 songYaml rb3 = let
  hashed =
    ( rb3
    , _title $ _metadata songYaml
    , _artist $ _metadata songYaml
    )
  in hash hashed `mod` 1000000000

getPlan :: Maybe T.Text -> SongYaml -> Maybe (T.Text, Plan)
getPlan Nothing songYaml = case HM.toList $ _plans songYaml of
  [pair] -> Just pair
  _      -> Nothing
getPlan (Just p) songYaml = case HM.lookup p $ _plans songYaml of
  Just found -> Just (p, found)
  Nothing    -> Nothing

simpleHOPOThreshold :: (Monad m, Num a) => SongYaml -> StackTraceT m a
simpleHOPOThreshold songYaml = let
  parts = toList $ getParts $ _parts songYaml
  grybo = map gryboHopoThreshold $ mapMaybe partGRYBO parts
  pg = map pgHopoThreshold $ mapMaybe partProGuitar parts
  in case nub $ grybo ++ pg of
    []  -> return 170
    [n] -> return $ fromIntegral n
    ns  -> fatal $ "more than 1 HOPO threshold found: " ++ show ns

makeRB3DTA :: (Monad m) => SongYaml -> Plan -> TargetRB3 -> RBFile.Song (RBFile.OnyxFile U.Beats) -> T.Text -> StackTraceT m D.SongPackage
makeRB3DTA songYaml plan rb3 song filename = do
  ((kickPV, snarePV, kitPV), _) <- computeDrumsPart (rb3_Drums rb3) plan songYaml
  thresh <- simpleHOPOThreshold songYaml
  let (pstart, pend) = previewBounds songYaml song
      DifficultyRB3{..} = difficultyRB3 rb3 songYaml
      len = songLengthMS song
      perctype = getPercType song
      fullGenre = interpretGenre
        (_genre    $ _metadata songYaml)
        (_subgenre $ _metadata songYaml)
      lookupPart rank part parts = guard (rank /= 0) >> HM.lookup part (getParts parts)
      -- all the following are only used for Plan, not MoggPlan.
      -- we don't need to handle more than 1 game part mapping to the same flex part,
      -- because no specs will change - we'll just zero out the game parts
      channelIndices before inst = take (length inst) $ drop (length $ concat before) $ [0..]
      partChannels, drumChannels, bassChannels, guitarChannels, keysChannels, vocalChannels, crowdChannels, songChannels :: [(Double, Double)]
      partChannels = concat
        [ drumChannels
        , bassChannels
        , guitarChannels
        , keysChannels
        , vocalChannels
        ]
      drumChannels   = case rb3DrumsRank  of 0 -> []; _ -> kickPV ++ snarePV ++ kitPV
      bassChannels   = case rb3BassRank   of 0 -> []; _ -> computeSimplePart (rb3_Bass   rb3) plan songYaml
      guitarChannels = case rb3GuitarRank of 0 -> []; _ -> computeSimplePart (rb3_Guitar rb3) plan songYaml
      keysChannels   = case rb3KeysRank   of 0 -> []; _ -> computeSimplePart (rb3_Keys   rb3) plan songYaml
      vocalChannels  = case rb3VocalRank  of 0 -> []; _ -> computeSimplePart (rb3_Vocal  rb3) plan songYaml
      crowdChannels = case plan of
        MoggPlan{..} -> undefined -- not used
        Plan    {..} -> case _crowd of
          Nothing -> []
          Just _  -> [(-1, 0), (1, 0)]
      songChannels = [(-1, 0), (1, 0)]
  return D.SongPackage
    { D.name = targetTitle songYaml $ RB3 rb3
    , D.artist = getArtist $ _metadata songYaml
    , D.master = not $ _cover $ _metadata songYaml
    , D.songId = Just $ fromMaybe (Right filename) $ rb3_SongID rb3
    , D.song = D.Song
      { D.songName = "songs/" <> filename <> "/" <> filename
      , D.tracksCount = Nothing
      , D.tracks = fmap (map fromIntegral) $ HM.fromList $ filter (not . null . snd) $ case plan of
        MoggPlan{..} -> let
          getChannels rank fpart = maybe [] (concat . toList) $ lookupPart rank fpart _moggParts
          -- * the below trick does not work. RB3 freezes if a part doesn't have any channels.
          -- * so instead above we just allow doubling up on channels.
          -- * this works ok; the audio will cut out if either player misses, and whammy does not bend pitch.
          -- allParts = map ($ rb3) [rb3_Drums, rb3_Bass, rb3_Guitar, rb3_Keys, rb3_Vocal]
          -- getChannels rank fpart = case filter (== fpart) allParts of
          --   _ : _ : _ -> [] -- more than 1 game part maps to this flex part
          --   _         -> maybe [] (concat . toList) $ lookupPart rank fpart _moggParts
          in  [ ("drum"  , getChannels rb3DrumsRank  $ rb3_Drums  rb3)
              , ("bass"  , getChannels rb3BassRank   $ rb3_Bass   rb3)
              , ("guitar", getChannels rb3GuitarRank $ rb3_Guitar rb3)
              , ("keys"  , getChannels rb3KeysRank   $ rb3_Keys   rb3)
              , ("vocals", getChannels rb3VocalRank  $ rb3_Vocal  rb3)
              ]
        Plan{..} ->
          [ ("drum"  , channelIndices [] drumChannels)
          , ("bass"  , channelIndices [drumChannels] bassChannels)
          , ("guitar", channelIndices [drumChannels, bassChannels] guitarChannels)
          , ("keys"  , channelIndices [drumChannels, bassChannels, guitarChannels] keysChannels)
          , ("vocals", channelIndices [drumChannels, bassChannels, guitarChannels, keysChannels] vocalChannels)
          ]
      , D.vocalParts = Just $ case fmap vocalCount $ getPart (rb3_Vocal rb3) songYaml >>= partVocal of
        Nothing     -> 0
        Just Vocal1 -> 1
        Just Vocal2 -> 2
        Just Vocal3 -> 3
      , D.pans = map realToFrac $ case plan of
        MoggPlan{..} -> _pans
        Plan{..}     -> map fst $ partChannels ++ crowdChannels ++ songChannels
      , D.vols = map realToFrac $ case plan of
        MoggPlan{..} -> _vols
        Plan{..}     -> map snd $ partChannels ++ crowdChannels ++ songChannels
      , D.cores = case plan of
        MoggPlan{..} -> map (const (-1)) _pans
        Plan{..}     -> map (const (-1)) $ partChannels ++ crowdChannels ++ songChannels
        -- TODO: 1 for guitar channels?
      , D.drumSolo = D.DrumSounds $ T.words $ case fmap drumsLayout $ getPart (rb3_Drums rb3) songYaml >>= partDrums of
        Nothing             -> "kick.cue snare.cue tom1.cue tom2.cue crash.cue"
        Just StandardLayout -> "kick.cue snare.cue tom1.cue tom2.cue crash.cue"
        Just FlipYBToms     -> "kick.cue snare.cue tom2.cue tom1.cue crash.cue"
      , D.drumFreestyle = D.DrumSounds $ T.words
        "kick.cue snare.cue hat.cue ride.cue crash.cue"
      , D.crowdChannels = let
        chans = case plan of
          MoggPlan{..} -> _moggCrowd
          Plan{..}     -> take (length crowdChannels) [length partChannels ..]
        in guard (not $ null chans) >> Just (map fromIntegral chans)
      , D.hopoThreshold = Just thresh
      , D.muteVolume = Nothing
      , D.muteVolumeVocals = Nothing
      , D.midiFile = Nothing
      }
    , D.bank = Just $ case perctype of
      Nothing               -> "sfx/tambourine_bank.milo"
      Just RBVox.Tambourine -> "sfx/tambourine_bank.milo"
      Just RBVox.Cowbell    -> "sfx/cowbell_bank.milo"
      Just RBVox.Clap       -> "sfx/handclap_bank.milo"
    , D.drumBank = Just $ case fmap drumsKit $ getPart (rb3_Drums rb3) songYaml >>= partDrums of
      Nothing            -> "sfx/kit01_bank.milo"
      Just HardRockKit   -> "sfx/kit01_bank.milo"
      Just ArenaKit      -> "sfx/kit02_bank.milo"
      Just VintageKit    -> "sfx/kit03_bank.milo"
      Just TrashyKit     -> "sfx/kit04_bank.milo"
      Just ElectronicKit -> "sfx/kit05_bank.milo"
    , D.animTempo = Left D.KTempoMedium
    , D.bandFailCue = Nothing
    , D.songScrollSpeed = 2300
    , D.preview = (fromIntegral pstart, fromIntegral pend)
    , D.songLength = Just $ fromIntegral len
    , D.rank = HM.fromList
      [ ("drum"       , rb3DrumsRank    )
      , ("bass"       , rb3BassRank     )
      , ("guitar"     , rb3GuitarRank   )
      , ("vocals"     , rb3VocalRank    )
      , ("keys"       , rb3KeysRank     )
      , ("real_keys"  , rb3ProKeysRank  )
      , ("real_guitar", rb3ProGuitarRank)
      , ("real_bass"  , rb3ProBassRank  )
      , ("band"       , rb3BandRank     )
      ]
    , D.solo = let
      kwds :: [T.Text]
      kwds = concat
        [ ["guitar" | hasSolo Guitar song]
        , ["bass" | hasSolo Bass song]
        , ["drum" | hasSolo Drums song]
        , ["keys" | hasSolo Keys song]
        , ["vocal_percussion" | hasSolo Vocal song]
        ]
      in guard (not $ null kwds) >> Just kwds
    , D.songFormat = 10
    , D.version = fromMaybe 1 $ rb3_Version rb3
    , D.fake = Nothing
    , D.gameOrigin = Just "ugc_plus"
    , D.ugc = Nothing
    , D.rating = fromIntegral $ fromEnum (_rating $ _metadata songYaml) + 1
    , D.genre = rbn2Genre fullGenre
    , D.subGenre = Just $ "subgenre_" <> rbn2Subgenre fullGenre
    , D.vocalGender = Just $ fromMaybe Magma.Female $ getPart (rb3_Vocal rb3) songYaml >>= partVocal >>= vocalGender
    -- TODO is it safe to have no vocal_gender?
    , D.shortVersion = Nothing
    , D.yearReleased = fromIntegral $ getYear $ _metadata songYaml
    , D.albumArt = Just True
    , D.albumName = Just $ getAlbum $ _metadata songYaml
    , D.albumTrackNumber = Just $ fromIntegral $ getTrackNumber $ _metadata songYaml
    , D.packName = Nothing
    , D.vocalTonicNote = toEnum . fromEnum <$> _key (_metadata songYaml)
    , D.songTonality = Nothing
    , D.songKey = Nothing
    , D.tuningOffsetCents = Just 0
    , D.realGuitarTuning = flip fmap (getPart (rb3_Guitar rb3) songYaml >>= partProGuitar) $ \pg ->
      case pgTuning pg of
        []   -> [0, 0, 0, 0, 0, 0]
        tune -> map fromIntegral tune
    , D.realBassTuning = flip fmap (getPart (rb3_Bass rb3) songYaml >>= partProGuitar) $ \pg ->
      case pgTuning pg of
        []   -> [0, 0, 0, 0]
        tune -> map fromIntegral tune
    , D.guidePitchVolume = Just (-3)
    , D.encoding = Just "utf8"
    , D.extraAuthoring = Nothing
    , D.alternatePath = Nothing
    , D.context = Nothing
    , D.decade = Nothing
    , D.downloaded = Nothing
    , D.basePoints = Nothing
    }

phony :: FilePath -> Action () -> Rules ()
phony fp act = Shake.phony fp act >> Shake.phony (fp ++ "/") act

-- TODO use parts from target
printOverdrive :: FilePath -> StackTraceT Action ()
printOverdrive mid = do
  song <- shakeMIDI mid
  let trackTimes = Set.fromList . ATB.getTimes . RTB.toAbsoluteEventList 0
      fiveOverdrive t = trackTimes $ RTB.filter (== RBFive.Overdrive True) t
      drumOverdrive t = trackTimes $ RTB.filter (== RBDrums.Overdrive True) t
      gtr = fiveOverdrive $ discardPS $ RBFile.flexFiveButton $ RBFile.getFlexPart RBFile.FlexGuitar $ RBFile.s_tracks song
      bass = fiveOverdrive $ discardPS $ RBFile.flexFiveButton $ RBFile.getFlexPart RBFile.FlexBass $ RBFile.s_tracks song
      keys = fiveOverdrive $ discardPS $ RBFile.flexFiveButton $ RBFile.getFlexPart RBFile.FlexKeys $ RBFile.s_tracks song
      drums = drumOverdrive $ discardPS $ RBFile.flexPartDrums $ RBFile.getFlexPart RBFile.FlexDrums $ RBFile.s_tracks song
  lift $ forM_ (Set.toAscList $ Set.unions [gtr, bass, keys, drums]) $ \t -> let
    insts = intercalate "," $ concat
      [ ["guitar" | Set.member t gtr]
      , ["bass" | Set.member t bass]
      , ["keys" | Set.member t keys]
      , ["drums" | Set.member t drums]
      ]
    posn = RBFile.showPosition $ U.applyMeasureMap (RBFile.s_signatures song) t
    in putNormal $ posn ++ ": " ++ insts

makeC3 :: (Monad m) => SongYaml -> Plan -> TargetRB3 -> RBFile.Song (RBFile.OnyxFile U.Beats) -> T.Text -> StackTraceT m C3.C3
makeC3 songYaml plan rb3 midi pkg = do
  let (pstart, _) = previewBounds songYaml midi
      DifficultyRB3{..} = difficultyRB3 rb3 songYaml
      title = targetTitle songYaml $ RB3 rb3
      numSongID = case rb3_SongID rb3 of
        Just (Left i) -> Just i
        _             -> Nothing
      hasCrowd = case plan of
        MoggPlan{..} -> not $ null _moggCrowd
        Plan{..}     -> isJust _crowd
  threshIndex <- simpleHOPOThreshold songYaml >>= \case
    90  -> return 0
    130 -> return 1
    170 -> return 2
    250 -> return 3
    ht  -> fatal $ "C3 Magma does not support the HOPO threshold " ++ show (ht :: Int)
  return C3.C3
    { C3.song = getTitle $ _metadata songYaml
    , C3.artist = getArtist $ _metadata songYaml
    , C3.album = getAlbum $ _metadata songYaml
    , C3.customID = pkg
    , C3.version = fromIntegral $ fromMaybe 1 $ rb3_Version rb3
    , C3.isMaster = not $ _cover $ _metadata songYaml
    , C3.encodingQuality = 5
    , C3.crowdAudio = guard hasCrowd >> Just "crowd.wav"
    , C3.crowdVol = guard hasCrowd >> Just 0
    , C3.is2xBass = rb3_2xBassPedal rb3
    , C3.rhythmKeys = _rhythmKeys $ _metadata songYaml
    , C3.rhythmBass = _rhythmBass $ _metadata songYaml
    , C3.karaoke = getKaraoke plan
    , C3.multitrack = getMultitrack plan
    , C3.convert = _convert $ _metadata songYaml
    , C3.expertOnly = _expertOnly $ _metadata songYaml
    , C3.proBassDiff = case rb3ProBassRank of 0 -> Nothing; r -> Just $ fromIntegral r
    , C3.proBassTuning4 = flip fmap (getPart (rb3_Bass rb3) songYaml >>= partProGuitar) $ \pg ->
      case pgTuning pg of
        []   -> "(real_bass_tuning (0 0 0 0 ))"
        tune -> "(real_bass_tuning (" <> T.unwords (map (T.pack . show) tune) <> "))"
    , C3.proGuitarDiff = case rb3ProGuitarRank of 0 -> Nothing; r -> Just $ fromIntegral r
    , C3.proGuitarTuning = flip fmap (getPart (rb3_Guitar rb3) songYaml >>= partProGuitar) $ \pg ->
      case pgTuning pg of
        []   -> "(real_guitar_tuning (0 0 0 0 0 0))"
        tune -> "(real_guitar_tuning (" <> T.unwords (map (T.pack . show) tune) <> "))"
    , C3.disableProKeys = case getPart (rb3_Keys rb3) songYaml of
      Nothing   -> False
      Just part -> isJust (partGRYBO part) && isNothing (partProKeys part)
    , C3.tonicNote = _key $ _metadata songYaml
    , C3.tuningCents = 0
    , C3.songRating = fromEnum (_rating $ _metadata songYaml) + 1
    , C3.drumKitSFX = maybe 0 (fromEnum . drumsKit) $ getPart (rb3_Drums rb3) songYaml >>= partDrums
    , C3.hopoThresholdIndex = threshIndex
    , C3.muteVol = -96
    , C3.vocalMuteVol = -12
    , C3.soloDrums = hasSolo Drums midi
    , C3.soloGuitar = hasSolo Guitar midi
    , C3.soloBass = hasSolo Bass midi
    , C3.soloKeys = hasSolo Keys midi
    , C3.soloVocals = hasSolo Vocal midi
    , C3.songPreview = fromIntegral pstart
    , C3.checkTempoMap = True
    , C3.wiiMode = False
    , C3.doDrumMixEvents = True -- is this a good idea?
    , C3.packageDisplay = getArtist (_metadata songYaml) <> " - " <> title
    , C3.packageDescription = "Created with Magma: C3 Roks Edition (forums.customscreators.com) and ONYX (git.io/onyx)."
    , C3.songAlbumArt = "cover.bmp"
    , C3.packageThumb = ""
    , C3.encodeANSI = True  -- is this right?
    , C3.encodeUTF8 = False -- is this right?
    , C3.useNumericID = isJust numSongID
    , C3.uniqueNumericID = case numSongID of
      Nothing -> ""
      Just i  -> T.pack $ show i
    , C3.uniqueNumericID2X = "" -- will use later if we ever create combined 1x/2x C3 Magma projects
    , C3.toDoList = C3.defaultToDo
    }

-- Magma RBProj rules
makeMagmaProj :: SongYaml -> TargetRB3 -> Plan -> T.Text -> FilePath -> Action T.Text -> StackTraceT Action Magma.RBProj
makeMagmaProj songYaml rb3 plan pkg mid thisTitle = do
  song <- shakeMIDI mid
  ((kickPVs, snarePVs, kitPVs), mixMode) <- computeDrumsPart (rb3_Drums rb3) plan songYaml
  let (pstart, _) = previewBounds songYaml song
      maxPStart = 570000 :: Int -- 9:39.000
      DifficultyRB3{..} = difficultyRB3 rb3 songYaml
      fullGenre = interpretGenre
        (_genre    $ _metadata songYaml)
        (_subgenre $ _metadata songYaml)
      perctype = getPercType song
      silentDryVox :: Int -> Magma.DryVoxPart
      silentDryVox n = Magma.DryVoxPart
        { Magma.dryVoxFile = "dryvox" <> T.pack (show n) <> ".wav"
        , Magma.dryVoxEnabled = True
        }
      emptyDryVox = Magma.DryVoxPart
        { Magma.dryVoxFile = ""
        , Magma.dryVoxEnabled = False
        }
      disabledFile = Magma.AudioFile
        { Magma.audioEnabled = False
        , Magma.channels = 0
        , Magma.pan = []
        , Magma.vol = []
        , Magma.audioFile = ""
        }
      replaceŸ = T.map $ \case
        'ÿ' -> 'y'
        'Ÿ' -> 'Y'
        c   -> c
      voxCount = fmap vocalCount $ getPart (rb3_Vocal rb3) songYaml >>= partVocal
      pvFile :: [(Double, Double)] -> T.Text -> Magma.AudioFile
      pvFile pvs f = Magma.AudioFile
        { Magma.audioEnabled = True
        , Magma.channels = fromIntegral $ length pvs
        , Magma.pan = map (realToFrac . fst) pvs
        , Magma.vol = map (realToFrac . snd) pvs
        , Magma.audioFile = f
        }
  title <- T.map (\case '"' -> '\''; c -> c) <$> lift thisTitle
  pstart' <- if pstart > maxPStart
    then do
      warn $ "Preview start time of " ++ show pstart ++ "ms too late for C3 Magma; changed to " ++ show maxPStart ++ "ms"
      return maxPStart
    else return pstart
  return Magma.RBProj
    { Magma.project = Magma.Project
      { Magma.toolVersion = "110411_A"
      , Magma.projectVersion = 24
      , Magma.metadata = Magma.Metadata
        { Magma.songName = replaceŸ title
        , Magma.artistName = replaceŸ $ getArtist $ _metadata songYaml
        , Magma.genre = rbn2Genre fullGenre
        , Magma.subGenre = "subgenre_" <> rbn2Subgenre fullGenre
        , Magma.yearReleased = fromIntegral $ max 1960 $ getYear $ _metadata songYaml
        , Magma.albumName = replaceŸ $ getAlbum $ _metadata songYaml
        , Magma.author = getAuthor $ _metadata songYaml
        , Magma.releaseLabel = "Onyxite Customs"
        , Magma.country = "ugc_country_us"
        , Magma.price = 160
        , Magma.trackNumber = fromIntegral $ getTrackNumber $ _metadata songYaml
        , Magma.hasAlbum = True
        }
      , Magma.gamedata = Magma.Gamedata
        { Magma.previewStartMs = fromIntegral pstart'
        , Magma.rankDrum    = max 1 rb3DrumsTier
        , Magma.rankBass    = max 1 rb3BassTier
        , Magma.rankGuitar  = max 1 rb3GuitarTier
        , Magma.rankVocals  = max 1 rb3VocalTier
        , Magma.rankKeys    = max 1 rb3KeysTier
        , Magma.rankProKeys = max 1 rb3ProKeysTier
        , Magma.rankBand    = max 1 rb3BandTier
        , Magma.vocalScrollSpeed = 2300
        , Magma.animTempo = 32
        , Magma.vocalGender = fromMaybe Magma.Female $ getPart (rb3_Vocal rb3) songYaml >>= partVocal >>= vocalGender
        , Magma.vocalPercussion = case perctype of
          Nothing               -> Magma.Tambourine
          Just RBVox.Tambourine -> Magma.Tambourine
          Just RBVox.Cowbell    -> Magma.Cowbell
          Just RBVox.Clap       -> Magma.Handclap
        , Magma.vocalParts = case voxCount of
          Nothing     -> 0
          Just Vocal1 -> 1
          Just Vocal2 -> 2
          Just Vocal3 -> 3
        , Magma.guidePitchVolume = -3
        }
      , Magma.languages = let
        lang s = elem s $ _languages $ _metadata songYaml
        eng = lang "English"
        fre = lang "French"
        ita = lang "Italian"
        spa = lang "Spanish"
        ger = lang "German"
        jap = lang "Japanese"
        in Magma.Languages
          { Magma.english  = Just $ eng || not (or [eng, fre, ita, spa, ger, jap])
          , Magma.french   = Just fre
          , Magma.italian  = Just ita
          , Magma.spanish  = Just spa
          , Magma.german   = Just ger
          , Magma.japanese = Just jap
          }
      , Magma.destinationFile = T.pack $ T.unpack pkg <.> "rba"
      , Magma.midi = Magma.Midi
        { Magma.midiFile = "notes.mid"
        , Magma.autogenTheme = Left $ _autogenTheme $ _metadata songYaml
        }
      , Magma.dryVox = Magma.DryVox
        { Magma.part0 = case voxCount of
          Nothing     -> emptyDryVox
          Just Vocal1 -> silentDryVox 0
          _           -> silentDryVox 1
        , Magma.part1 = if voxCount == Just Vocal2 || voxCount == Just Vocal3
          then silentDryVox 2
          else emptyDryVox
        , Magma.part2 = if voxCount == Just Vocal3
          then silentDryVox 3
          else emptyDryVox
        , Magma.dryVoxFileRB2 = Nothing
        , Magma.tuningOffsetCents = 0
        }
      , Magma.albumArt = Magma.AlbumArt "cover.bmp"
      , Magma.tracks = Magma.Tracks
        { Magma.drumLayout = case mixMode of
          RBDrums.D0 -> Magma.Kit
          RBDrums.D1 -> Magma.KitKickSnare
          RBDrums.D2 -> Magma.KitKickSnare
          RBDrums.D3 -> Magma.KitKickSnare
          RBDrums.D4 -> Magma.KitKick
        , Magma.drumKick = if rb3DrumsRank == 0 || mixMode == RBDrums.D0
          then disabledFile
          else pvFile kickPVs "kick.wav"
        , Magma.drumSnare = if rb3DrumsRank == 0 || elem mixMode [RBDrums.D0, RBDrums.D4]
          then disabledFile
          else pvFile snarePVs "snare.wav"
        , Magma.drumKit = if rb3DrumsRank == 0
          then disabledFile
          else pvFile kitPVs "drums.wav"
        , Magma.bass = if rb3BassRank == 0
          then disabledFile
          else pvFile (computeSimplePart (rb3_Bass rb3) plan songYaml) "bass.wav"
        , Magma.guitar = if rb3GuitarRank == 0
          then disabledFile
          else pvFile (computeSimplePart (rb3_Guitar rb3) plan songYaml) "guitar.wav"
        , Magma.vocals = if rb3VocalRank == 0
          then disabledFile
          else pvFile (computeSimplePart (rb3_Vocal rb3) plan songYaml) "vocal.wav"
        , Magma.keys = if rb3KeysRank == 0
          then disabledFile
          else pvFile (computeSimplePart (rb3_Keys rb3) plan songYaml) "keys.wav"
        , Magma.backing = pvFile [(-1, 0), (1, 0)] "song-countin.wav"
        }
      }
    }

loadYaml :: (StackJSON a, MonadIO m) => FilePath -> StackTraceT m a
loadYaml fp = do
  yaml <- readYAMLTree fp
  mapStackTraceT (`runReaderT` yaml) fromJSON

shakeBuildTarget :: (MonadIO m) => [FilePath] -> FilePath -> Target -> StackTraceT m FilePath
shakeBuildTarget audioDirs yamlPath target = do
  let buildable = case target of
        RB3{} -> "gen/target" </> targetHash </> "rb3con"
        RB2{} -> "gen/target" </> targetHash </> "rb2con"
        PS {} -> "gen/target" </> targetHash </> "ps.zip"
        GH2{} -> "gen/target" </> targetHash </> undefined -- TODO
      targetHash = show $ hash target `mod` 100000000
  shakeBuild audioDirs yamlPath [(T.pack targetHash, target)] [buildable]
  return $ takeDirectory yamlPath </> buildable

shakeBuildMagmaProject :: (MonadIO m) => [FilePath] -> FilePath -> Target -> StackTraceT m FilePath
shakeBuildMagmaProject audioDirs yamlPath target = do
  let buildable = "gen/target" </> targetHash </> "magma"
      targetHash = show $ hash target `mod` 100000000
  shakeBuild audioDirs yamlPath [(T.pack targetHash, target)] [buildable]
  return $ takeDirectory yamlPath </> buildable

shakeBuildFiles :: (MonadIO m) => [FilePath] -> FilePath -> [FilePath] -> StackTraceT m ()
shakeBuildFiles audioDirs yamlPath = shakeBuild audioDirs yamlPath []

shakeBuild :: (MonadIO m) => [FilePath] -> FilePath -> [(T.Text, Target)] -> [FilePath] -> StackTraceT m ()
shakeBuild audioDirs yamlPath extraTargets buildables = do

  songYaml <- loadYaml yamlPath

  let fullGenre = interpretGenre
        (_genre    $ _metadata songYaml)
        (_subgenre $ _metadata songYaml)

  checkDefined songYaml

  exeTime <- stackIO $ getExecutablePath >>= Dir.getModificationTime
  yamlTime <- stackIO $ Dir.getModificationTime yamlPath
  let version = show exeTime ++ "," ++ show yamlTime

  audioLib <- newAudioLibrary
  forM_ audioDirs $ \dir -> do
    p <- parseAbsDir dir
    addAudioDir audioLib p

  -- we translate ShakeException (which may or may not have a StackTraceT fatal inside)
  -- to a StackTraceT fatal with layers
  let handleShakeErr se = let
        go (layer : layers) exc = inside ("shake: " ++ layer) $ go layers exc
        go []               exc = case Exc.fromException exc of
          Nothing   -> stackShowException exc
          Just msgs -> throwError msgs
        in go (shakeExceptionStack se) (shakeExceptionInner se)

  stackCatchIO handleShakeErr $ Dir.withCurrentDirectory (takeDirectory yamlPath) $ do

    shake shakeOptions{ shakeThreads = 0, shakeFiles = "gen", shakeVersion = version } $ do

      forM_ (HM.elems $ _audio songYaml) $ \case
        AudioFile AudioInfo{ _filePath = Just fp, _commands = cmds } | not $ null cmds -> do
          normaliseEx fp %> \_ -> mapM_ (Shake.unit . Shake.cmd . T.unpack) cmds
        _ -> return ()

      phony "yaml"  $ liftIO $ print songYaml
      phony "audio" $ liftIO $ print audioDirs
      phony "clean" $ cmd ("rm -rf gen" :: String)

      -- Find and convert all Jammit audio into the work directory
      let jammitAudioParts = map J.Only    [minBound .. maxBound]
                          ++ map J.Without [minBound .. maxBound]
      forM_ (HM.toList $ _jammit songYaml) $ \(jammitName, jammitQuery) ->
        forM_ jammitAudioParts $ \audpart ->
          jammitPath jammitName audpart ≡> \out -> do
            inside ("Looking for the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart) $ do
              let title  = fromMaybe (getTitle  $ _metadata songYaml) $ _jammitTitle  jammitQuery
                  artist = fromMaybe (getArtist $ _metadata songYaml) $ _jammitArtist jammitQuery
                  inst   = fromJammitInstrument $ J.audioPartToInstrument audpart
              p <- searchJammit audioLib (title, artist, inst)
              result <- stackIO $ fmap J.getAudioParts $ J.loadLibrary $ toFilePath p
              case [ jcfx | (audpart', jcfx) <- result, audpart == audpart' ] of
                jcfx : _ -> do
                  lift $ putNormal $ "Found the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart
                  stackIO $ J.runAudio [jcfx] [] out
                []       -> fail "Couldn't find a necessary Jammit track"

      -- Cover art
      let loadRGB8 = case _fileAlbumArt $ _metadata songYaml of
            Just img -> do
              need [img]
              liftIO $ if takeExtension img == ".png_xbox"
                then readPNGXbox <$> BL.readFile img
                else readImage img >>= \case
                  Left  err -> fail $ "Failed to load cover art (" ++ img ++ "): " ++ err
                  Right dyn -> return $ convertRGB8 dyn
            Nothing -> return onyxAlbum
      "gen/cover.bmp" %> \out -> loadRGB8 >>= liftIO . writeBitmap out . scaleSTBIR 256 256
      "gen/cover.png" %> \out -> loadRGB8 >>= liftIO . writePng    out . scaleSTBIR 256 256
      "gen/cover.png_xbox" %> \out -> case _fileAlbumArt $ _metadata songYaml of
        Just f | takeExtension f == ".png_xbox" -> copyFile' f out
        _      -> loadRGB8 >>= liftIO . BL.writeFile out . toPNG_XBOX

      "gen/notes.mid" %> \out -> do
        doesFileExist "notes.mid" >>= \b -> if b
          then copyFile' "notes.mid" out
          else saveMIDI out RBFile.Song
            { RBFile.s_tempos = U.tempoMapFromBPS RTB.empty
            , RBFile.s_signatures = U.measureMapFromTimeSigs U.Error RTB.empty
            , RBFile.s_tracks = def :: RBFile.OnyxFile U.Beats
            }

      let getAudioLength :: T.Text -> Action U.Seconds
          getAudioLength planName = do
            let allSourceAudio = "gen/plan" </> T.unpack planName </> "everything.wav"
            need [allSourceAudio]
            liftIO $ audioSeconds allSourceAudio

          adjustSpec :: Bool -> [(Double, Double)] -> [(Double, Double)]
          adjustSpec True  spec     = spec
          adjustSpec False [(0, 0)] = [(0, 0)]
          adjustSpec False _        = [(-1, 0), (1, 0)]

          padAudio pad src = if frames src == 0
            then src
            else padStart (Seconds $ realToFrac (pad :: Int)) src
          adjustAudioSpeed speed src = case speed of
            Nothing -> src
            Just 1  -> src
            Just n  -> stretchFull (1 / n) 1 src
          -- Silences out an audio stream if more than 1 game part maps to the same flex part
          zeroIfMultiple fparts fpart src = case filter (== fpart) fparts of
            _ : _ : _ -> takeStart (Frames 0) src
            _         -> src

          writeKick, writeSnare, writeKit, writeSimplePart
            :: [RBFile.FlexPartName] -> Maybe Double -> Int -> Bool -> T.Text -> Plan -> RBFile.FlexPartName -> Integer -> FilePath -> StackTraceT Action ()
          writeKick gameParts speed pad supportsOffMono planName plan fpart rank out = do
            ((spec', _, _), _) <- computeDrumsPart fpart plan songYaml
            let spec = adjustSpec supportsOffMono spec'
            src <- case plan of
              MoggPlan{..} -> channelsToSpec spec planName (zip _pans _vols) _silent $ do
                guard $ rank /= 0
                case HM.lookup fpart $ getParts _moggParts of
                  Just (PartDrumKit kick _ _) -> fromMaybe [] kick
                  _                           -> []
              Plan{..}     -> buildAudioToSpec audioLib songYaml spec $ do
                guard $ rank /= 0
                case HM.lookup fpart $ getParts _planParts of
                  Just (PartDrumKit kick _ _) -> kick
                  _                           -> Nothing
            lift $ runAudio (zeroIfMultiple gameParts fpart $ padAudio pad $ adjustAudioSpeed speed src) out
          writeSnare gameParts speed pad supportsOffMono planName plan fpart rank out = do
            ((_, spec', _), _) <- computeDrumsPart fpart plan songYaml
            let spec = adjustSpec supportsOffMono spec'
            src <- case plan of
              MoggPlan{..} -> channelsToSpec spec planName (zip _pans _vols) _silent $ do
                guard $ rank /= 0
                case HM.lookup fpart $ getParts _moggParts of
                  Just (PartDrumKit _ snare _) -> fromMaybe [] snare
                  _                            -> []
              Plan{..}     -> buildAudioToSpec audioLib songYaml spec $ do
                guard $ rank /= 0
                case HM.lookup fpart $ getParts _planParts of
                  Just (PartDrumKit _ snare _) -> snare
                  _                            -> Nothing
            lift $ runAudio (zeroIfMultiple gameParts fpart $ padAudio pad $ adjustAudioSpeed speed src) out
          writeKit gameParts speed pad supportsOffMono planName plan fpart rank out = do
            ((_, _, spec'), _) <- computeDrumsPart fpart plan songYaml
            let spec = adjustSpec supportsOffMono spec'
            src <- case plan of
              MoggPlan{..} -> channelsToSpec spec planName (zip _pans _vols) _silent $ do
                guard $ rank /= 0
                case HM.lookup fpart $ getParts _moggParts of
                  Just (PartDrumKit _ _ kit) -> kit
                  Just (PartSingle      kit) -> kit
                  _                          -> []
              Plan{..}     -> buildAudioToSpec audioLib songYaml spec $ do
                guard $ rank /= 0
                case HM.lookup fpart $ getParts _planParts of
                  Just (PartDrumKit _ _ kit) -> Just kit
                  Just (PartSingle      kit) -> Just kit
                  _                          -> Nothing
            lift $ runAudio (zeroIfMultiple gameParts fpart $ padAudio pad $ adjustAudioSpeed speed src) out
          getPartSource :: (MonadResource m) => [(Double, Double)] -> T.Text -> Plan -> RBFile.FlexPartName -> Integer -> StackTraceT Action (AudioSource m Float)
          getPartSource spec planName plan fpart rank = case plan of
            MoggPlan{..} -> channelsToSpec spec planName (zip _pans _vols) _silent $ do
              guard $ rank /= 0
              toList (HM.lookup fpart $ getParts _moggParts) >>= toList >>= toList
            Plan{..} -> buildPartAudioToSpec audioLib songYaml spec $ do
              guard $ rank /= 0
              HM.lookup fpart $ getParts _planParts
          writeStereoParts gameParts speed pad planName plan fpartranks out = do
            let spec = [(-1, 0), (1, 0)]
            srcs <- forM fpartranks $ \(fpart, rank)
              -> zeroIfMultiple gameParts fpart
              <$> getPartSource spec planName plan fpart rank
            src <- case srcs of
              []     -> buildAudioToSpec audioLib songYaml spec Nothing
              s : ss -> return $ foldr mix s ss
            lift $ runAudio (padAudio pad $ adjustAudioSpeed speed src) out
          writeSimplePart gameParts speed pad supportsOffMono planName plan fpart rank out = do
            let spec = adjustSpec supportsOffMono $ computeSimplePart fpart plan songYaml
            src <- getPartSource spec planName plan fpart rank
            lift $ runAudio (zeroIfMultiple gameParts fpart $ padAudio pad $ adjustAudioSpeed speed src) out
          writeCrowd speed pad planName plan out = do
            src <- case plan of
              MoggPlan{..} -> channelsToSpec [(-1, 0), (1, 0)] planName (zip _pans _vols) _silent _moggCrowd
              Plan{..}     -> buildAudioToSpec audioLib songYaml [(-1, 0), (1, 0)] _crowd
            lift $ runAudio (padAudio pad $ adjustAudioSpeed speed src) out
          sourceSongCountin :: (MonadResource m) => Maybe Double -> Int -> Bool -> T.Text -> Plan -> [(RBFile.FlexPartName, Integer)] -> StackTraceT Action (AudioSource m Float)
          sourceSongCountin speed pad includeCountin planName plan fparts = do
            let usedParts' = [ fpart | (fpart, rank) <- fparts, rank /= 0 ]
                usedParts =
                  [ fpart
                  | fpart <- usedParts'
                  , case filter (== fpart) usedParts' of
                    -- if more than 1 game part maps to this flex part,
                    -- the flex part's audio should go in backing track
                    _ : _ : _ -> False
                    _         -> True
                  ]
                spec = [(-1, 0), (1, 0)]
            src <- case plan of
              MoggPlan{..} -> channelsToSpec spec planName (zip _pans _vols) _silent $ let
                channelsFor fpart = toList (HM.lookup fpart $ getParts _moggParts) >>= toList >>= toList
                usedChannels = concatMap channelsFor usedParts ++ _moggCrowd
                in filter (`notElem` usedChannels) [0 .. length _pans - 1]
              Plan{..} -> let
                unusedParts = do
                  (fpart, pa) <- HM.toList $ getParts _planParts
                  guard $ notElem fpart usedParts
                  return pa
                partAudios = maybe id (\pa -> (PartSingle pa :)) _song unusedParts
                countinPath = "gen/plan" </> T.unpack planName </> "countin.wav"
                in do
                  unusedSrcs <- mapM (buildPartAudioToSpec audioLib songYaml spec . Just) partAudios
                  if includeCountin
                    then do
                      countinSrc <- lift $ buildSource $ Input countinPath
                      return $ foldr mix countinSrc unusedSrcs
                    else case unusedSrcs of
                      []     -> buildPartAudioToSpec audioLib songYaml spec Nothing
                      s : ss -> return $ foldr mix s ss
            return $ padAudio pad $ adjustAudioSpeed speed src
          writeSongCountin :: Maybe Double -> Int -> Bool -> T.Text -> Plan -> [(RBFile.FlexPartName, Integer)] -> FilePath -> StackTraceT Action ()
          writeSongCountin speed pad includeCountin planName plan fparts out = do
            src <- sourceSongCountin speed pad includeCountin planName plan fparts
            lift $ runAudio src out

          rbRules :: FilePath -> TargetRB3 -> Maybe TargetRB2 -> Rules ()
          rbRules dir rb3 mrb2 = do
            let pkg :: (IsString a) => a
                pkg = fromString $ "o" <> show (hashRB3 songYaml rb3)
                DifficultyRB3{..} = difficultyRB3 rb3 songYaml
            (planName, plan) <- case getPlan (rb3_Plan rb3) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show rb3
              Just pair -> return pair
            let planDir = "gen/plan" </> T.unpack planName

            let pathMagmaKick        = dir </> "magma/kick.wav"
                pathMagmaSnare       = dir </> "magma/snare.wav"
                pathMagmaDrums       = dir </> "magma/drums.wav"
                pathMagmaBass        = dir </> "magma/bass.wav"
                pathMagmaGuitar      = dir </> "magma/guitar.wav"
                pathMagmaKeys        = dir </> "magma/keys.wav"
                pathMagmaVocal       = dir </> "magma/vocal.wav"
                pathMagmaCrowd       = dir </> "magma/crowd.wav"
                pathMagmaDryvox0     = dir </> "magma/dryvox0.wav"
                pathMagmaDryvox1     = dir </> "magma/dryvox1.wav"
                pathMagmaDryvox2     = dir </> "magma/dryvox2.wav"
                pathMagmaDryvox3     = dir </> "magma/dryvox3.wav"
                pathMagmaDryvoxSine  = dir </> "magma/dryvox-sine.wav"
                pathMagmaSong        = dir </> "magma/song-countin.wav"
                pathMagmaCover       = dir </> "magma/cover.bmp"
                pathMagmaCoverV1     = dir </> "magma/cover-v1.bmp"
                pathMagmaMid         = dir </> "magma/notes.mid"
                pathMagmaRPP         = dir </> "magma/notes.RPP"
                pathMagmaMidV1       = dir </> "magma/notes-v1.mid"
                pathMagmaProj        = dir </> "magma/magma.rbproj"
                pathMagmaProjV1      = dir </> "magma/magma-v1.rbproj"
                pathMagmaC3          = dir </> "magma/magma.c3"
                pathMagmaSetup       = dir </> "magma"
                pathMagmaRba         = dir </> "magma.rba"
                pathMagmaRbaV1       = dir </> "magma-v1.rba"
                pathMagmaExport      = dir </> "notes-magma-export.mid"
                pathMagmaExport2     = dir </> "notes-magma-added.mid"
                pathMagmaDummyMono   = dir </> "magma/dummy-mono.wav"
                pathMagmaDummyStereo = dir </> "magma/dummy-stereo.wav"
                pathMagmaPad         = dir </> "magma/pad.txt"

            let magmaParts = map ($ rb3) [rb3_Drums, rb3_Bass, rb3_Guitar, rb3_Keys, rb3_Vocal]
            pathMagmaKick   ≡> \out -> do
              pad <- lift $ read <$> readFile' pathMagmaPad
              writeKick        magmaParts (rb3_Speed rb3) pad True planName plan (rb3_Drums  rb3) rb3DrumsRank out
            pathMagmaSnare  ≡> \out -> do
              pad <- lift $ read <$> readFile' pathMagmaPad
              writeSnare       magmaParts (rb3_Speed rb3) pad True planName plan (rb3_Drums  rb3) rb3DrumsRank out
            pathMagmaDrums  ≡> \out -> do
              pad <- lift $ read <$> readFile' pathMagmaPad
              writeKit         magmaParts (rb3_Speed rb3) pad True planName plan (rb3_Drums  rb3) rb3DrumsRank out
            pathMagmaBass   ≡> \out -> do
              pad <- lift $ read <$> readFile' pathMagmaPad
              writeSimplePart  magmaParts (rb3_Speed rb3) pad True planName plan (rb3_Bass   rb3) rb3BassRank out
            pathMagmaGuitar ≡> \out -> do
              pad <- lift $ read <$> readFile' pathMagmaPad
              writeSimplePart  magmaParts (rb3_Speed rb3) pad True planName plan (rb3_Guitar rb3) rb3GuitarRank out
            pathMagmaKeys   ≡> \out -> do
              pad <- lift $ read <$> readFile' pathMagmaPad
              writeSimplePart  magmaParts (rb3_Speed rb3) pad True planName plan (rb3_Keys   rb3) rb3KeysRank out
            pathMagmaVocal  ≡> \out -> do
              pad <- lift $ read <$> readFile' pathMagmaPad
              writeSimplePart  magmaParts (rb3_Speed rb3) pad True planName plan (rb3_Vocal  rb3) rb3VocalRank out
            pathMagmaCrowd  ≡> \out -> do
              pad <- lift $ read <$> readFile' pathMagmaPad
              writeCrowd                  (rb3_Speed rb3) pad      planName plan out
            pathMagmaSong   ≡> \out -> do
              pad <- lift $ read <$> readFile' pathMagmaPad
              writeSongCountin            (rb3_Speed rb3) pad True planName plan
                [ (rb3_Drums  rb3, rb3DrumsRank )
                , (rb3_Guitar rb3, rb3GuitarRank)
                , (rb3_Bass   rb3, rb3BassRank  )
                , (rb3_Keys   rb3, rb3KeysRank  )
                , (rb3_Vocal  rb3, rb3VocalRank )
                ] out
            let saveClip m out vox = lift $ do
                  let fmt = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
                      clip = clipDryVox $ U.applyTempoTrack (RBFile.s_tempos m) $ vocalTubes vox
                  unclippedVox <- buildSource $ Input pathMagmaVocal
                  unclipped <- case frames unclippedVox of
                    0 -> buildSource $ Input pathMagmaSong
                    _ -> return unclippedVox
                  putNormal $ "Writing a clipped dry vocals file to " ++ out
                  liftIO $ runResourceT $ sinkSnd out fmt $ toDryVoxFormat $ clip unclipped
                  putNormal $ "Finished writing dry vocals to " ++ out
            pathMagmaDryvox0 ≡> \out -> do
              m <- shakeMIDI pathMagmaMid
              saveClip m out $ RBFile.rb3PartVocals $ RBFile.s_tracks m
            pathMagmaDryvox1 ≡> \out -> do
              m <- shakeMIDI pathMagmaMid
              saveClip m out $ RBFile.rb3Harm1 $ RBFile.s_tracks m
            pathMagmaDryvox2 ≡> \out -> do
              m <- shakeMIDI pathMagmaMid
              saveClip m out $ RBFile.rb3Harm2 $ RBFile.s_tracks m
            pathMagmaDryvox3 ≡> \out -> do
              m <- shakeMIDI pathMagmaMid
              saveClip m out $ RBFile.rb3Harm3 $ RBFile.s_tracks m
            pathMagmaDryvoxSine ≡> \out -> do
              m <- shakeMIDI pathMagmaMid
              let fmt = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
              liftIO $ runResourceT $ sinkSnd out fmt $ RB2.dryVoxAudio m
            pathMagmaDummyMono   %> buildAudio (Silence 1 $ Seconds 31) -- we set preview start to 0:00 so these can be short
            pathMagmaDummyStereo %> buildAudio (Silence 2 $ Seconds 31)
            pathMagmaCover %> copyFile' "gen/cover.bmp"
            pathMagmaCoverV1 %> \out -> liftIO $ writeBitmap out $ generateImage (\_ _ -> PixelRGB8 0 0 255) 256 256
            let title = targetTitle songYaml $ RB3 rb3
            pathMagmaProj ≡> \out -> do
              p <- makeMagmaProj songYaml rb3 plan pkg pathMagmaMid $ return title
              liftIO $ D.writeFileDTA_latin1 out $ D.serialize D.stackChunks p
            pathMagmaC3 ≡> \out -> do
              midi <- shakeMIDI pathMagmaMid
              c3 <- makeC3 songYaml plan rb3 midi pkg
              liftIO $ TIO.writeFile out $ C3.showC3 c3
            let magmaNeededAudio = do
                  ((kickSpec, snareSpec, _), _) <- computeDrumsPart (rb3_Drums rb3) plan songYaml
                  return $ concat
                    [ guard (maybe False (/= def) $ getPart (rb3_Drums  rb3) songYaml) >> concat
                      [ [pathMagmaDrums]
                      , [pathMagmaKick | not $ null kickSpec]
                      , [pathMagmaSnare | not $ null snareSpec]
                      ]
                    , guard (maybe False (/= def) $ getPart (rb3_Bass   rb3) songYaml) >> [pathMagmaBass  ]
                    , guard (maybe False (/= def) $ getPart (rb3_Guitar rb3) songYaml) >> [pathMagmaGuitar]
                    , guard (maybe False (/= def) $ getPart (rb3_Keys   rb3) songYaml) >> [pathMagmaKeys  ]
                    , case fmap vocalCount $ getPart (rb3_Vocal rb3) songYaml >>= partVocal of
                      Nothing     -> []
                      Just Vocal1 -> [pathMagmaVocal, pathMagmaDryvox0]
                      Just Vocal2 -> [pathMagmaVocal, pathMagmaDryvox1, pathMagmaDryvox2]
                      Just Vocal3 -> [pathMagmaVocal, pathMagmaDryvox1, pathMagmaDryvox2, pathMagmaDryvox3]
                    , [pathMagmaSong, pathMagmaCrowd]
                    ]
            pathMagmaRPP ≡> \out -> do
              auds <- magmaNeededAudio
              let auds' = filter (`notElem` [pathMagmaDryvox1, pathMagmaDryvox2, pathMagmaDryvox3]) auds
              lift $ makeReaper pathMagmaMid pathMagmaMid auds' out
            phony pathMagmaSetup $ shakeTrace $ do
              -- Just make all the Magma prereqs, but don't actually run Magma
              auds <- magmaNeededAudio
              lift $ need $ auds ++ [pathMagmaCover, pathMagmaMid, pathMagmaProj, pathMagmaC3, pathMagmaRPP]
            pathMagmaRba ≡> \out -> do
              lift $ need [pathMagmaSetup]
              lift $ putNormal "# Running Magma v2 (C3)"
              Magma.runMagma pathMagmaProj out >>= lift . putNormal
            pathMagmaExport ≡> \out -> do
              lift $ need [pathMagmaMid, pathMagmaProj]
              lift $ putNormal "# Running Magma v2 to export MIDI"
              -- TODO: bypass Magma if it fails due to over 1MB midi
              Magma.runMagmaMIDI pathMagmaProj out >>= lift . putNormal
            let getRealSections :: StackTraceT Action (RTB.T U.Beats T.Text)
                getRealSections = do
                  raw <- shakeMIDI $ planDir </> "raw.mid"
                  let evts = discardPS $ RBFile.onyxEvents $ RBFile.s_tracks raw
                  return $ RTB.mapMaybe (\case Events.PracticeSection s -> Just s; _ -> Nothing) evts
            pathMagmaExport2 ≡> \out -> do
              -- Using Magma's "export MIDI" option overwrites all animations/venue
              -- with autogenerated ones, even if they were actually authored.
              -- So, we now need to readd them back from the user MIDI (if they exist).
              userMid <- shakeMIDI pathMagmaMid
              magmaMid <- shakeMIDI pathMagmaExport
              sects <- getRealSections
              let reauthor getTrack eventPredicates magmaTrack = let
                    authoredTrack = getTrack $ RBFile.s_tracks userMid
                    applyEventFn isEvent t = let
                      authoredEvents = RTB.filter isEvent authoredTrack
                      magmaNoEvents = RTB.filter (not . isEvent) t
                      in if RTB.null authoredEvents then t else RTB.merge authoredEvents magmaNoEvents
                    in foldr applyEventFn magmaTrack eventPredicates
                  fivePredicates =
                    [ \case RBFive.Mood{} -> True; _ -> False
                    , \case RBFive.HandMap{} -> True; _ -> False
                    , \case RBFive.StrumMap{} -> True; _ -> False
                    , \case RBFive.FretPosition{} -> True; _ -> False
                    ]
              lift $ saveMIDI out $ magmaMid
                { RBFile.s_tracks = let
                  orig = RBFile.s_tracks magmaMid
                  in orig
                    { RBFile.rb3PartDrums = let
                      isMood = \case RBDrums.Mood{} -> True; _ -> False
                      isAnim = \case RBDrums.Animation{} -> True; _ -> False
                      in reauthor RBFile.rb3PartDrums [isMood, isAnim] $ RBFile.rb3PartDrums orig
                    , RBFile.rb3PartGuitar =
                      reauthor RBFile.rb3PartGuitar fivePredicates $ RBFile.rb3PartGuitar orig
                    , RBFile.rb3PartBass =
                      reauthor RBFile.rb3PartBass fivePredicates $ RBFile.rb3PartBass orig
                    , RBFile.rb3PartKeys =
                      reauthor RBFile.rb3PartKeys fivePredicates $ RBFile.rb3PartKeys orig
                    , RBFile.rb3PartVocals = let
                      isMood = \case RBVox.Mood{} -> True; _ -> False
                      in reauthor RBFile.rb3PartVocals [isMood] $ RBFile.rb3PartVocals orig
                    , RBFile.rb3Venue = reauthor RBFile.rb3Venue [const True] $ RBFile.rb3Venue orig
                      -- TODO: split up camera and lighting so you can author just one
                    , RBFile.rb3Events = if RTB.null sects
                      then RBFile.rb3Events orig
                      else RTB.merge (fmap Events.PracticeSection sects)
                        $ RTB.filter (\case Events.PracticeSection _ -> False; _ -> True) $ RBFile.rb3Events orig
                    -- Stuff "export midi" doesn't overwrite:
                    -- PART KEYS_ANIM_LH/RH
                    -- Crowd stuff in EVENTS
                    }
                }

            [pathMagmaMid, pathMagmaPad] &%> \[out, outPad] -> shakeTrace $ do
              input <- shakeMIDI $ planDir </> "raw.mid"
              (_, mixMode) <- computeDrumsPart (rb3_Drums rb3) plan songYaml
              let adjustMIDISpeed mid = case rb3_Speed rb3 of
                    Just n | n /= 1 -> mid
                      { RBFile.s_tempos
                        = U.tempoMapFromBPS
                        $ fmap (* realToFrac n)
                        $ U.tempoMapToBPS
                        $ RBFile.s_tempos mid
                      }
                    _               -> mid
              (output, pad) <- RB3.processRB3Pad
                rb3
                songYaml
                (adjustMIDISpeed input)
                mixMode
                (getAudioLength planName)
              liftIO $ writeFile outPad $ show pad
              sects <- ATB.toPairList . RTB.toAbsoluteEventList 0 <$> getRealSections
              let (magmaSects, invalid) = makeRBN2Sections sects
                  magmaSects' = RTB.fromAbsoluteEventList $ ATB.fromPairList magmaSects
                  isSection (Events.PracticeSection _) = True
                  isSection _                          = False
                  adjustEvents trks = trks
                    { RBFile.rb3Events
                      = RTB.merge (fmap Events.PracticeSection magmaSects')
                      $ RTB.filter (not . isSection)
                      $ RBFile.rb3Events trks
                    }
              lift $ case invalid of
                [] -> return ()
                _  -> putNormal $ "The following sections were unrecognized and replaced: " ++ show invalid
              lift $ saveMIDI out output
                { RBFile.s_tracks = adjustEvents $ RBFile.s_tracks output
                }

            let pathDta = dir </> "stfs/songs/songs.dta"
                pathMid = dir </> "stfs/songs" </> pkg </> pkg <.> "mid"
                pathOgg = dir </> "audio.ogg"
                pathMogg = dir </> "stfs/songs" </> pkg </> pkg <.> "mogg"
                pathPng = dir </> "stfs/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
                pathMilo = dir </> "stfs/songs" </> pkg </> "gen" </> pkg <.> "milo_xbox"
                pathCon = dir </> "rb3con"

            pathDta ≡> \out -> do
              song <- shakeMIDI pathMid
              songPkg <- makeRB3DTA songYaml plan rb3 song pkg
              liftIO $ writeUtf8CRLF out $ prettyDTA pkg songPkg $ makeC3DTAComments (_metadata songYaml) plan $ rb3_2xBassPedal rb3
            pathMid %> copyFile' pathMagmaExport2
            pathOgg ≡> \out -> case plan of
              MoggPlan{..} -> lift $ do
                let speed = fromMaybe 1 $ rb3_Speed rb3
                pad <- read <$> readFile' (dir </> "magma/pad.txt")
                case (speed, pad) of
                  (1, 0) -> copyFile' (planDir </> "audio.ogg") out
                  _      -> do
                    input <- buildSource $ Input $ planDir </> "audio.ogg"
                    let src = padStart (Seconds $ realToFrac (pad :: Int))
                          $ stretchFullSmart _silent (1 / speed) 1 input
                    runAudio src out
              Plan{..}   -> do
                (_, mixMode) <- computeDrumsPart (rb3_Drums rb3) plan songYaml
                let parts = concat
                      [ [pathMagmaKick   | rb3DrumsRank  /= 0 && mixMode /= RBDrums.D0]
                      , [pathMagmaSnare  | rb3DrumsRank  /= 0 && notElem mixMode [RBDrums.D0, RBDrums.D4]]
                      , [pathMagmaDrums  | rb3DrumsRank  /= 0]
                      , [pathMagmaBass   | rb3BassRank   /= 0]
                      , [pathMagmaGuitar | rb3GuitarRank /= 0]
                      , [pathMagmaKeys   | rb3KeysRank   /= 0]
                      , [pathMagmaVocal  | rb3VocalRank  /= 0]
                      , [pathMagmaCrowd  | isJust _crowd]
                      , [pathMagmaSong]
                      ]
                lift $ do
                  src <- buildSource $ Merge $ map Input parts
                  runAudio src out
            pathMogg ≡> \out -> case plan of
              MoggPlan{} -> case rb3_Speed rb3 of
                Just n | n /= 1 -> do
                  lift $ need [pathOgg]
                  Magma.oggToMogg pathOgg out
                _ -> lift $ copyFile' (planDir </> "audio.mogg") out
              Plan{..}   -> do
                lift $ need [pathOgg]
                Magma.oggToMogg pathOgg out
            pathPng  %> copyFile' "gen/cover.png_xbox"
            pathMilo %> \out -> liftIO $ B.writeFile out emptyMilo
            pathCon ≡> \out -> do
              lift $ need [pathDta, pathMid, pathMogg, pathPng, pathMilo]
              lift $ putNormal "# Producing RB3 CON file via X360"
              rb3pkg
                (getArtist (_metadata songYaml) <> ": " <> title)
                ("Compiled by Onyx Music Game Toolkit")
                (dir </> "stfs")
                out

            -- Guitar rules
            dir </> "protar-hear.mid" ≡> \out -> do
              input <- shakeMIDI pathMagmaMid
              let goffs = case maybe [] pgTuning $ getPart (rb3_Guitar rb3) songYaml >>= partProGuitar of
                    []   -> [0, 0, 0, 0, 0, 0]
                    offs -> offs
                  boffs = case maybe [] pgTuning $ getPart (rb3_Bass   rb3) songYaml >>= partProGuitar of
                    []   -> [0, 0, 0, 0]
                    offs -> offs
              lift $ saveMIDI out $ RBFile.playGuitarFile goffs boffs input
            dir </> "protar-mpa.mid" ≡> \out -> do
              input <- shakeMIDI pathMagmaMid
              let gtr17   = discardPS $ RBFile.flexPartRealGuitar   $ RBFile.getFlexPart (rb3_Guitar rb3) $ RBFile.s_tracks input
                  gtr22   = discardPS $ RBFile.flexPartRealGuitar22 $ RBFile.getFlexPart (rb3_Guitar rb3) $ RBFile.s_tracks input
                  bass17  = discardPS $ RBFile.flexPartRealGuitar   $ RBFile.getFlexPart (rb3_Bass   rb3) $ RBFile.s_tracks input
                  bass22  = discardPS $ RBFile.flexPartRealGuitar22 $ RBFile.getFlexPart (rb3_Bass   rb3) $ RBFile.s_tracks input
                  pgThres = maybe 170 pgHopoThreshold $ getPart (rb3_Guitar rb3) songYaml >>= partProGuitar
                  pbThres = maybe 170 pgHopoThreshold $ getPart (rb3_Bass   rb3) songYaml >>= partProGuitar
                  playTrack thres cont name t = let
                    expert = flip RTB.mapMaybe t $ \case
                      ProGtr.DiffEvent Expert devt -> Just devt
                      _                            -> Nothing
                    auto = PGPlay.autoplay (fromIntegral thres / 480) expert
                    msgToSysEx msg
                      = E.SystemExclusive $ SysEx.Regular $ PGPlay.sendCommand (cont, msg) ++ [0xF7]
                    in U.setTrackName name $ msgToSysEx <$> auto
              lift $ saveMIDI out input
                { RBFile.s_tracks = RBFile.RawFile
                    [ playTrack pgThres PGPlay.Mustang "GTR17"  $ if RTB.null gtr17  then gtr22  else gtr17
                    , playTrack pgThres PGPlay.Squier  "GTR22"  $ if RTB.null gtr22  then gtr17  else gtr22
                    , playTrack pbThres PGPlay.Mustang "BASS17" $ if RTB.null bass17 then bass22 else bass17
                    , playTrack pbThres PGPlay.Squier  "BASS22" $ if RTB.null bass22 then bass17 else bass22
                    ]
                }

            case mrb2 of
              Nothing -> return ()
              Just rb2 -> do

                pathMagmaMidV1 ≡> \out -> shakeMIDI pathMagmaMid >>= lift . saveMIDI out . RB2.convertMIDI

                pathMagmaProjV1 ≡> \out -> do
                  p <- makeMagmaProj songYaml rb3 plan pkg pathMagmaMid $ return title
                  let makeDummy (Magma.Tracks dl dkt dk ds b g v k bck) = Magma.Tracks
                        dl
                        (makeDummyKeep dkt)
                        (makeDummyKeep dk)
                        (makeDummyKeep ds)
                        (makeDummyMono b)
                        (makeDummyMono g)
                        (makeDummyMono v)
                        (makeDummyMono k) -- doesn't matter
                        (makeDummyMono bck)
                      makeDummyMono af = af
                        { Magma.audioFile = "dummy-mono.wav"
                        , Magma.channels = 1
                        , Magma.pan = [0]
                        , Magma.vol = [0]
                        }
                      makeDummyKeep af = case Magma.channels af of
                        1 -> af
                          { Magma.audioFile = "dummy-mono.wav"
                          }
                        _ -> af
                          { Magma.audioFile = "dummy-stereo.wav"
                          , Magma.channels = 2
                          , Magma.pan = [-1, 1]
                          , Magma.vol = [0, 0]
                          }
                  liftIO $ D.writeFileDTA_latin1 out $ D.serialize D.stackChunks p
                    { Magma.project = (Magma.project p)
                      { Magma.albumArt = Magma.AlbumArt "cover-v1.bmp"
                      , Magma.midi = (Magma.midi $ Magma.project p)
                        { Magma.midiFile = "notes-v1.mid"
                        }
                      , Magma.projectVersion = 5
                      , Magma.languages = let
                          lang s = elem s $ _languages $ _metadata songYaml
                          eng = lang "English"
                          fre = lang "French"
                          ita = lang "Italian"
                          spa = lang "Spanish"
                          in Magma.Languages
                            { Magma.english  = Just $ eng || not (or [eng, fre, ita, spa])
                            , Magma.french   = Just fre
                            , Magma.italian  = Just ita
                            , Magma.spanish  = Just spa
                            , Magma.german   = Nothing
                            , Magma.japanese = Nothing
                            }
                      , Magma.dryVox = (Magma.dryVox $ Magma.project p)
                        { Magma.dryVoxFileRB2 = Just "dryvox-sine.wav"
                        }
                      , Magma.tracks = makeDummy $ Magma.tracks $ Magma.project p
                      , Magma.metadata = (Magma.metadata $ Magma.project p)
                        { Magma.genre = rbn1Genre fullGenre
                        , Magma.subGenre = "subgenre_" <> rbn1Subgenre fullGenre
                        }
                      , Magma.gamedata = (Magma.gamedata $ Magma.project p)
                        { Magma.previewStartMs = 0 -- for dummy audio. will reset after magma
                        }
                      }
                    }

                pathMagmaRbaV1 ≡> \out -> do
                  lift $ need [pathMagmaDummyMono, pathMagmaDummyStereo, pathMagmaDryvoxSine, pathMagmaCoverV1, pathMagmaMidV1, pathMagmaProjV1]
                  lift $ putNormal "# Running Magma v1 (without 10 min limit)"
                  errorToWarning (Magma.runMagmaV1 pathMagmaProjV1 out) >>= lift . \case
                    Just output -> putNormal output
                    Nothing     -> do
                      putNormal "Magma v1 failed; optimistically bypassing."
                      liftIO $ B.writeFile out B.empty

                -- Magma v1 rba to con
                do
                  let doesRBAExist = do
                        need [pathMagmaRbaV1]
                        liftIO $ (/= 0) <$> withBinaryFile pathMagmaRbaV1 ReadMode hFileSize
                      rb2CON = dir </> "rb2con"
                      rb2OriginalDTA = dir </> "rb2-original.dta"
                      rb2DTA = dir </> "rb2/songs/songs.dta"
                      rb2Mogg = dir </> "rb2/songs" </> pkg </> pkg <.> "mogg"
                      rb2Mid = dir </> "rb2/songs" </> pkg </> pkg <.> "mid"
                      rb2Art = dir </> "rb2/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
                      rb2Weights = dir </> "rb2/songs" </> pkg </> "gen" </> (pkg ++ "_weights.bin")
                      rb2Milo = dir </> "rb2/songs" </> pkg </> "gen" </> pkg <.> "milo_xbox"
                      rb2Pan = dir </> "rb2/songs" </> pkg </> pkg <.> "pan"
                      fixDict
                        = HM.fromList
                        . mapMaybe (\(k, v) -> case k of
                          "guitar" -> Just (k, v)
                          "bass"   -> Just (k, v)
                          "keys"   -> Nothing
                          "drum"   -> Just (k, v)
                          "vocals" -> Just (k, v)
                          "band"   -> Just (k, v)
                          _        -> Nothing
                        )
                        . HM.toList
                  rb2OriginalDTA ≡> \out -> do
                    ex <- lift doesRBAExist
                    if ex
                      then Magma.getRBAFile 0 pathMagmaRbaV1 out
                      else do
                        lift $ need [pathDta]
                        (_, rb3DTA, _) <- readRB3DTA pathDta
                        let newDTA :: D.SongPackage
                            newDTA = D.SongPackage
                              { D.name = D.name rb3DTA
                              , D.artist = D.artist rb3DTA
                              , D.master = not $ _cover $ _metadata songYaml
                              , D.song = D.Song
                                -- most of this gets rewritten later anyway
                                { D.songName = D.songName $ D.song rb3DTA
                                , D.tracksCount = Nothing
                                , D.tracks = D.tracks $ D.song rb3DTA
                                , D.pans = D.pans $ D.song rb3DTA
                                , D.vols = D.vols $ D.song rb3DTA
                                , D.cores = D.cores $ D.song rb3DTA
                                , D.drumSolo = D.drumSolo $ D.song rb3DTA -- needed
                                , D.drumFreestyle = D.drumFreestyle $ D.song rb3DTA -- needed
                                , D.midiFile = D.midiFile $ D.song rb3DTA
                                -- not used
                                , D.vocalParts = Nothing
                                , D.crowdChannels = Nothing
                                , D.hopoThreshold = Nothing
                                , D.muteVolume = Nothing
                                , D.muteVolumeVocals = Nothing
                                }
                              , D.songScrollSpeed = D.songScrollSpeed rb3DTA
                              , D.bank = D.bank rb3DTA
                              , D.animTempo = D.animTempo rb3DTA
                              , D.songLength = D.songLength rb3DTA
                              , D.preview = D.preview rb3DTA
                              , D.rank = fixDict $ D.rank rb3DTA
                              , D.genre = rbn1Genre fullGenre
                              , D.decade = Just $ let y = D.yearReleased rb3DTA in if
                                | 1960 <= y && y < 1970 -> "the60s"
                                | 1970 <= y && y < 1980 -> "the70s"
                                | 1980 <= y && y < 1990 -> "the80s"
                                | 1990 <= y && y < 2000 -> "the90s"
                                | 2000 <= y && y < 2010 -> "the00s"
                                | 2010 <= y && y < 2020 -> "the10s"
                                | otherwise -> "the10s"
                              , D.vocalGender = D.vocalGender rb3DTA
                              , D.version = 0
                              , D.fake = Nothing
                              , D.downloaded = Just True
                              , D.songFormat = 4
                              , D.albumArt = Just True
                              , D.yearReleased = D.yearReleased rb3DTA
                              , D.basePoints = Just 0
                              , D.rating = D.rating rb3DTA
                              , D.subGenre = Just $ "subgenre_" <> rbn1Subgenre fullGenre
                              , D.songId = D.songId rb3DTA
                              , D.tuningOffsetCents = D.tuningOffsetCents rb3DTA
                              , D.context = Just 2000
                              , D.gameOrigin = Just "rb2"
                              , D.ugc = Nothing
                              , D.albumName = D.albumName rb3DTA
                              , D.albumTrackNumber = D.albumTrackNumber rb3DTA
                              , D.packName = D.packName rb3DTA
                              -- not present
                              , D.drumBank = Nothing
                              , D.bandFailCue = Nothing
                              , D.solo = Nothing
                              , D.shortVersion = Nothing
                              , D.vocalTonicNote = Nothing
                              , D.songTonality = Nothing
                              , D.songKey = Nothing
                              , D.realGuitarTuning = Nothing
                              , D.realBassTuning = Nothing
                              , D.guidePitchVolume = Nothing
                              , D.encoding = Nothing
                              , D.extraAuthoring = Nothing
                              , D.alternatePath = Nothing
                              }
                        liftIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0 [D.Parens (D.Tree 0 (D.Key pkg : stackShow D.stackChunks newDTA))]
                  rb2DTA ≡> \out -> do
                    lift $ need [rb2OriginalDTA, pathDta]
                    (_, magmaDTA, _) <- readRB3DTA rb2OriginalDTA
                    (_, rb3DTA, _) <- readRB3DTA pathDta
                    let newDTA :: D.SongPackage
                        newDTA = magmaDTA
                          { D.name = targetTitle songYaml $ RB2 rb2
                          , D.artist = D.artist rb3DTA
                          , D.albumName = D.albumName rb3DTA
                          , D.master = not $ _cover $ _metadata songYaml
                          , D.version = 0
                          -- if version is not 0, you get a message
                          -- "can't play this song until all players in your session purchase it!"
                          , D.song = (D.song magmaDTA)
                            { D.tracksCount = Nothing
                            , D.tracks = fixDict $ D.tracks $ D.song rb3DTA
                            , D.midiFile = Just $ "songs/" <> pkg <> "/" <> pkg <> ".mid"
                            , D.songName = "songs/" <> pkg <> "/" <> pkg
                            , D.pans = D.pans $ D.song rb3DTA
                            , D.vols = D.vols $ D.song rb3DTA
                            , D.cores = D.cores $ D.song rb3DTA
                            , D.crowdChannels = D.crowdChannels $ D.song rb3DTA
                            }
                          , D.songId = Just $ fromMaybe (Right pkg) $ rb2_SongID rb2
                          , D.preview = D.preview rb3DTA -- because we told magma preview was at 0s earlier
                          , D.songLength = D.songLength rb3DTA -- magma v1 set this to 31s from the audio file lengths
                          }
                    liftIO $ writeLatin1CRLF out $ prettyDTA pkg newDTA $ makeC3DTAComments (_metadata songYaml) plan (rb2_2xBassPedal rb2)
                  rb2Mid ≡> \out -> do
                    ex <- lift doesRBAExist
                    lift $ need [pathMagmaMid]
                    mid <- liftIO $ if ex
                      then do
                        Magma.getRBAFile 1 pathMagmaRbaV1 out
                        Load.fromFile out
                      else Load.fromFile pathMagmaMidV1
                    let Left beatTracks = U.decodeFile mid
                    -- add back practice sections
                    sects <- getRealSections
                    let modifyTrack t = if U.trackName t == Just "EVENTS"
                          then RTB.merge (fmap makeRB2Section sects) $ flip RTB.filter t $ \e -> case readCommandList e of
                            Just ["section", _] -> False
                            _                   -> True
                          else t
                        defaultVenue = U.setTrackName "VENUE" $ U.trackJoin $ RTB.flatten $ RTB.singleton 0
                          [ unparseList ["lighting", "()"]
                          , unparseList ["verse"]
                          , unparseBlip 60
                          , unparseBlip 61
                          , unparseBlip 62
                          , unparseBlip 63
                          , unparseBlip 64
                          , unparseBlip 70
                          , unparseBlip 71
                          , unparseBlip 73
                          , unparseBlip 109
                          ]
                        addVenue = if any ((== Just "VENUE") . U.trackName) beatTracks
                          then id
                          else (++ [defaultVenue])
                    liftIO $ Save.toFile out $ U.encodeFileBeats F.Parallel 480 $
                      addVenue $ if RTB.null sects
                        then beatTracks
                        else map modifyTrack beatTracks
                  rb2Mogg %> copyFile' (planDir </> "audio.mogg")
                  rb2Milo %> \out -> do
                    ex <- doesRBAExist
                    liftIO $ if ex
                      then Magma.getRBAFile 3 pathMagmaRbaV1 out
                      else B.writeFile out emptyMiloRB2
                  rb2Weights %> \out -> do
                    ex <- doesRBAExist
                    liftIO $ if ex
                      then Magma.getRBAFile 5 pathMagmaRbaV1 out
                      else B.writeFile out emptyWeightsRB2
                  rb2Art %> copyFile' "gen/cover.png_xbox"
                  rb2Pan %> \out -> liftIO $ B.writeFile out B.empty
                  rb2CON ≡> \out -> do
                    lift $ need [rb2DTA, rb2Mogg, rb2Mid, rb2Art, rb2Weights, rb2Milo, rb2Pan]
                    lift $ putNormal "# Producing RB2 CON file via X360"
                    rb2pkg
                      (getArtist (_metadata songYaml) <> ": " <> getTitle (_metadata songYaml))
                      (getArtist (_metadata songYaml) <> ": " <> getTitle (_metadata songYaml))
                      (dir </> "rb2")
                      out

      forM_ (extraTargets ++ HM.toList (_targets songYaml)) $ \(targetName, target) -> do
        let dir = "gen/target" </> T.unpack targetName
        case target of
          RB3 rb3 -> rbRules dir rb3 Nothing
          RB2 rb2 -> let
            rb3 = TargetRB3
              { rb3_Speed = rb2_Speed rb2
              , rb3_Plan = rb2_Plan rb2
              , rb3_2xBassPedal = rb2_2xBassPedal rb2
              , rb3_SongID = rb2_SongID rb2
              , rb3_Label = rb2_Label rb2
              , rb3_Version = rb2_Version rb2
              , rb3_Guitar = rb2_Guitar rb2
              , rb3_Bass = rb2_Bass rb2
              , rb3_Drums = rb2_Drums rb2
              , rb3_Vocal = rb2_Vocal rb2
              , rb3_Keys = RBFile.FlexKeys
              }
            in rbRules dir rb3 $ Just rb2
          GH2 gh2 -> do

            (planName, plan) <- case getPlan (gh2_Plan gh2) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show gh2
              Just pair -> return pair
            let planDir = "gen/plan" </> T.unpack planName

            dir </> "gh2/notes.mid" ≡> \out -> do
              input <- shakeMIDI $ planDir </> "raw.mid"
              lift $ saveMIDI out $ midiRB3toGH2 songYaml gh2 input

            dir </> "gh2/audio.vgs" ≡> \out -> do
              let coopPart = case gh2_Coop gh2 of
                    GH2Bass   -> gh2_Bass   gh2
                    GH2Rhythm -> gh2_Rhythm gh2
              -- TODO support no coop part
              srcGtr  <- getPartSource [(-1, 0), (1, 0)] planName plan (gh2_Guitar gh2) 1
              srcCoop <- getPartSource [(-1, 0), (1, 0)] planName plan coopPart 1
              srcSong <- sourceSongCountin Nothing 0 True planName plan [(gh2_Guitar gh2, 1), (coopPart, 1)]
              stackIO $ runResourceT $ writeVGS out $ mapSamples integralSample $ merge (merge srcGtr srcCoop) srcSong

            forM_ ([90, 75, 60] :: [Int]) $ \speed -> do
              dir </> ("gh2/audio_p" ++ show speed ++ ".vgs") ≡> \out -> do
                let coopPart = case gh2_Coop gh2 of
                      GH2Bass   -> gh2_Bass   gh2
                      GH2Rhythm -> gh2_Rhythm gh2
                srcCoop <- applyVolsMono [0] <$> getPartSource [(-1, 0), (1, 0)] planName plan coopPart 1
                srcGtr  <- applyVolsMono [0] <$> getPartSource [(-1, 0), (1, 0)] planName plan (gh2_Guitar gh2) 1
                rate <- case speed of
                  60 -> return 19875
                  75 -> return 16125
                  90 -> return 13500
                  50 -> return 24000
                  65 -> return 18375
                  85 -> return 14250
                  _  -> fatal $ "No known rate for GH2 practice speed: " ++ show speed ++ "%"
                lift $ putNormal $ "Writing GH2 practice audio for " ++ show speed ++ "% speed"
                stackIO $ runResourceT $ writeVGS out
                  $ mapSamples integralSample
                  $ resampleTo rate SincMediumQuality
                  $ stretchFull 1 (100 / fromIntegral speed)
                  $ merge srcCoop srcGtr
                lift $ putNormal $ "Finished writing GH2 practice audio for " ++ show speed ++ "% speed"

            dir </> "gh2/songs.dta" ≡> \out -> do
              input <- shakeMIDI $ planDir </> "raw.mid"
              let dta = makeGH2DTA songYaml (previewBounds songYaml input) gh2
              stackIO $ D.writeFileDTA_latin1 out $ D.serialize D.stackChunks dta

            phony (dir </> "gh2") $ need
              [ dir </> "gh2/notes.mid"
              , dir </> "gh2/audio.vgs"
              , dir </> "gh2/audio_p90.vgs"
              , dir </> "gh2/audio_p75.vgs"
              , dir </> "gh2/audio_p60.vgs"
              , dir </> "gh2/songs.dta"
              ]

          PS ps -> do

            (planName, plan) <- case getPlan (ps_Plan ps) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show ps
              Just pair -> return pair
            let planDir = "gen/plan" </> T.unpack planName
                DifficultyPS{..} = difficultyPS ps songYaml
                DifficultyRB3{..} = psDifficultyRB3

            dir </> "ps/notes.mid" ≡> \out -> do
              input <- shakeMIDI $ planDir </> "raw.mid"
              (_, mixMode) <- computeDrumsPart (ps_Drums ps) plan songYaml
              output <- RB3.processPS
                ps
                songYaml
                input
                mixMode
                (getAudioLength planName)
              lift $ saveMIDI out output

            dir </> "ps/video.avi" ≡> \out -> case ps_FileVideo ps of
              Nothing  -> fatal "requested Phase Shift video background, but target doesn't have one"
              Just vid -> lift $ copyFile' vid out

            dir </> "ps/song.ini" ≡> \out -> do
              song <- shakeMIDI $ dir </> "ps/notes.mid"
              let (pstart, _) = previewBounds songYaml song
                  len = songLengthMS song
              FoF.saveSong out FoF.Song
                { FoF.artist           = _artist $ _metadata songYaml
                , FoF.name             = Just $ targetTitle songYaml target
                , FoF.album            = _album $ _metadata songYaml
                , FoF.charter          = _author $ _metadata songYaml
                , FoF.year             = _year $ _metadata songYaml
                , FoF.genre            = Just $ fofGenre fullGenre
                , FoF.proDrums         = fmap drumsPro $ getPart (ps_Drums ps) songYaml >>= partDrums
                , FoF.songLength       = Just len
                , FoF.previewStartTime = Just pstart
                -- difficulty tiers go from 0 to 6, or -1 for no part
                , FoF.diffBand         = Just $ fromIntegral $ rb3BandTier    - 1
                , FoF.diffGuitar       = Just $ fromIntegral $ rb3GuitarTier  - 1
                , FoF.diffBass         = Just $ fromIntegral $ rb3BassTier    - 1
                , FoF.diffDrums        = Just $ fromIntegral $ rb3DrumsTier   - 1
                , FoF.diffDrumsReal    = Just $ case fmap drumsPro $ getPart (ps_Drums ps) songYaml >>= partDrums of
                  Just True -> fromIntegral $ rb3DrumsTier - 1
                  _         -> -1
                , FoF.diffKeys         = Just $ fromIntegral $ rb3KeysTier    - 1
                , FoF.diffKeysReal     = Just $ fromIntegral $ rb3ProKeysTier - 1
                , FoF.diffVocals       = Just $ fromIntegral $ rb3VocalTier   - 1
                , FoF.diffVocalsHarm   = Just $ fromIntegral $ rb3VocalTier   - 1
                , FoF.diffDance        = Just (-1)
                , FoF.diffBassReal     = Just $ fromIntegral $ rb3ProBassTier - 1
                , FoF.diffGuitarReal   = Just $ fromIntegral $ rb3ProGuitarTier - 1
                -- TODO: are the 22-fret difficulties needed?
                , FoF.diffBassReal22   = Just $ fromIntegral $ rb3ProBassTier - 1
                , FoF.diffGuitarReal22 = Just $ fromIntegral $ rb3ProGuitarTier - 1
                , FoF.diffGuitarCoop   = Just $ fromIntegral $ psGuitarCoopTier - 1
                , FoF.diffRhythm       = Just $ fromIntegral $ psRhythmTier - 1
                , FoF.diffDrumsRealPS  = Just (-1)
                , FoF.diffKeysRealPS   = Just (-1)
                , FoF.delay            = Nothing
                , FoF.starPowerNote    = Just 116
                , FoF.track            = _trackNumber $ _metadata songYaml
                , FoF.sysexSlider      = Just $ let
                  isTap = \case
                    PSM.PS (PSM.PSMessage _ PSM.TapNotes _) -> True
                    _                                       -> False
                  in any (any isTap . RBFile.flexFiveButton)
                    $ RBFile.onyxFlexParts $ RBFile.s_tracks song
                , FoF.sysexOpenBass    = Just $ let
                  isOpen = \case
                    PSM.PS (PSM.PSMessage _ PSM.OpenStrum _) -> True
                    _                                        -> False
                  in any (any isOpen . RBFile.flexFiveButton)
                    $ RBFile.onyxFlexParts $ RBFile.s_tracks song
                , FoF.video            = const "video.avi" <$> ps_FileVideo ps
                }

            let psParts = map ($ ps) [ps_Drums, ps_Guitar, ps_Bass, ps_Keys, ps_Vocal, ps_Rhythm, ps_GuitarCoop]
            dir </> "ps/drums.ogg"   ≡> writeStereoParts psParts (ps_Speed ps) 0 planName plan [(ps_Drums  ps, rb3DrumsRank)]
            dir </> "ps/drums_1.ogg" ≡> writeKick  psParts (ps_Speed ps) 0 False planName plan  (ps_Drums  ps) rb3DrumsRank
            dir </> "ps/drums_2.ogg" ≡> writeSnare psParts (ps_Speed ps) 0 False planName plan  (ps_Drums  ps) rb3DrumsRank
            dir </> "ps/drums_3.ogg" ≡> writeKit   psParts (ps_Speed ps) 0 False planName plan  (ps_Drums  ps) rb3DrumsRank
            dir </> "ps/guitar.ogg"  ≡> writeStereoParts psParts (ps_Speed ps) 0 planName plan
              [(ps_Guitar ps, rb3GuitarRank), (ps_GuitarCoop ps, psGuitarCoopTier)]
            dir </> "ps/keys.ogg"    ≡> writeStereoParts psParts (ps_Speed ps) 0 planName plan [(ps_Keys   ps, rb3KeysRank)]
            dir </> "ps/rhythm.ogg"  ≡> writeStereoParts psParts (ps_Speed ps) 0 planName plan
              [(ps_Bass ps, rb3BassRank), (ps_Rhythm ps, psRhythmTier)]
            dir </> "ps/vocals.ogg"  ≡> writeStereoParts psParts (ps_Speed ps) 0 planName plan [(ps_Vocal  ps, rb3VocalRank)]
            dir </> "ps/crowd.ogg"   ≡> writeCrowd       (ps_Speed ps) 0 planName plan
            dir </> "ps/song.ogg"    ≡> writeSongCountin (ps_Speed ps) 0 True planName plan
              [ (ps_Drums      ps, rb3DrumsTier    )
              , (ps_Guitar     ps, rb3GuitarTier   )
              , (ps_GuitarCoop ps, psGuitarCoopTier)
              , (ps_Bass       ps, rb3BassTier     )
              , (ps_Rhythm     ps, psRhythmTier    )
              , (ps_Keys       ps, rb3KeysTier     )
              , (ps_Vocal      ps, rb3VocalTier    )
              ]
            dir </> "ps/album.png"   %> copyFile' "gen/cover.png"
            phony (dir </> "ps") $ shakeTrace $ do
              (_, mixMode) <- computeDrumsPart (ps_Drums ps) plan songYaml
              lift $ need $ map (\f -> dir </> "ps" </> f) $ concat
                -- TODO replace (/= def), should actually check whether the right PS play mode is present
                [ ["song.ini", "notes.mid", "song.ogg", "album.png"]
                , ["drums.ogg"   | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode == RBDrums.D0 && case plan of
                    Plan{..} -> HM.member (ps_Drums ps) $ getParts _planParts
                    _        -> True
                  ]
                , ["drums_1.ogg" | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode /= RBDrums.D0]
                , ["drums_2.ogg" | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode /= RBDrums.D0]
                , ["drums_3.ogg" | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode /= RBDrums.D0]
                -- TODO also check ps_GuitarCoop
                , ["guitar.ogg"  | maybe False (/= def) (getPart (ps_Guitar ps) songYaml) && case plan of
                    Plan{..} -> HM.member (ps_Guitar ps) $ getParts _planParts
                    _        -> True
                  ]
                , ["keys.ogg"    | maybe False (/= def) (getPart (ps_Keys ps) songYaml) && case plan of
                    Plan{..} -> HM.member (ps_Keys ps) $ getParts _planParts
                    _        -> True
                  ]
                -- TODO also check ps_Rhythm
                , ["rhythm.ogg"  | maybe False (/= def) (getPart (ps_Bass ps) songYaml) && case plan of
                    Plan{..} -> HM.member (ps_Bass ps) $ getParts _planParts
                    _        -> True
                  ]
                , ["vocals.ogg"  | maybe False (/= def) (getPart (ps_Vocal ps) songYaml) && case plan of
                    Plan{..} -> HM.member (ps_Vocal ps) $ getParts _planParts
                    _        -> True
                  ]
                , ["crowd.ogg"   | case plan of
                    Plan{..}     -> isJust _crowd
                    MoggPlan{..} -> not $ null _moggCrowd
                  ]
                , ["video.avi" | isJust $ ps_FileVideo ps]
                ]
            dir </> "ps.zip" %> \out -> do
              let d = dir </> "ps"
              need [d]
              files <- map (d </>) <$> getDirectoryContents d
              let folderInZip = T.unpack $ toValidFileName $ targetTitle songYaml target <> " (" <> getArtist (_metadata songYaml) <> ")"
              z <- liftIO $ Zip.addFilesToArchive [Zip.OptLocation folderInZip False] Zip.emptyArchive files
              liftIO $ BL.writeFile out $ Zip.fromArchive z

      forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do

        let dir = "gen/plan" </> T.unpack planName

        -- plan audio, currently only used for REAPER project
        let allPlanParts :: [(RBFile.FlexPartName, PartAudio ())]
            allPlanParts = case plan of
              Plan{..}     -> HM.toList $ getParts $ void <$> _planParts
              MoggPlan{..} -> do
                (fpart, pa) <- HM.toList $ getParts _moggParts
                guard $ not $ null $ concat $ toList pa
                return (fpart, void pa)
        dir </> "song.wav" ≡>
          writeSongCountin Nothing 0 False planName plan [ (fpart, 1) | (fpart, _) <- allPlanParts ]
        dir </> "crowd.wav" ≡> writeCrowd Nothing 0 planName plan
        forM_ allPlanParts $ \(fpart, pa) -> do
          let name = T.unpack $ RBFile.getPartName fpart
          case pa of
            PartSingle () -> do
              dir </> name <.> "wav" ≡> writeSimplePart [fpart] Nothing 0 False planName plan fpart 1
            PartDrumKit mkick msnare () -> do
              forM_ mkick $ \() -> do
                dir </> (name ++ "-kick") <.> "wav" ≡>
                  writeKick [fpart] Nothing 0 False planName plan fpart 1
              forM_ msnare $ \() -> do
                dir </> (name ++ "-snare") <.> "wav" ≡>
                  writeSnare [fpart] Nothing 0 False planName plan fpart 1
              dir </> (name ++ "-kit") <.> "wav" ≡>
                writeKit [fpart] Nothing 0 False planName plan fpart 1
        let allPlanAudio :: [FilePath]
            allPlanAudio = map (dir </>) $ concat
              [ [ "song.wav" ]
              , [ "crowd.wav"
                | case plan of Plan{..} -> isJust _crowd; MoggPlan{..} -> not $ null _moggCrowd
                ]
              , allPlanParts >>= \(fpart, pa) -> let
                name = T.unpack $ RBFile.getPartName fpart
                in case pa of
                  PartSingle () -> [name <.> "wav"]
                  PartDrumKit mkick msnare () -> concat
                    [ map (\() -> (name ++ "-kick") <.> "wav") $ toList mkick
                    , map (\() -> (name ++ "-snare") <.> "wav") $ toList msnare
                    , [(name ++ "-kit") <.> "wav"]
                    ]
              ]

        -- REAPER project
        "notes-" ++ T.unpack planName ++ ".RPP" %> \out -> do
          let extraTempo = "tempo-" ++ T.unpack planName ++ ".mid"
          b <- doesFileExist extraTempo
          let tempo = if b then extraTempo else "gen/notes.mid"
          makeReaper "gen/notes.mid" tempo allPlanAudio out

        dir </> "web/song.js" %> \out -> do
          let json = dir </> "display.json"
          s <- readFile' json
          let s' = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s
              js = "window.onyxSong = " ++ s' ++ ";\n"
          liftIO $ writeFile out js
        phony (dir </> "web") $ do
          liftIO $ forM_ webDisplay $ \(f, bs) -> do
            Dir.createDirectoryIfMissing True $ dir </> "web" </> takeDirectory f
            B.writeFile (dir </> "web" </> f) bs
          need
            [ dir </> "web/preview-audio.mp3"
            , dir </> "web/preview-audio.ogg"
            , dir </> "web/song.js"
            ]

        dir </> "everything.wav" ≡> \out -> case plan of
          MoggPlan{..} -> lift $ do
            src <- buildSource $ Input $ dir </> "audio.ogg"
            runAudio (applyPansVols (map realToFrac _pans) (map realToFrac _vols) src) out
          Plan{..} -> do
            let planAudios = concat
                  [ toList _song
                  , toList _crowd
                  , toList _planParts >>= toList
                  ]
            srcs <- mapM (buildAudioToSpec audioLib songYaml [(-1, 0), (1, 0)] . Just) planAudios
            count <- lift $ buildSource $ Input $ dir </> "countin.wav"
            lift $ runAudio (foldr mix count srcs) out
        dir </> "everything.ogg" %> buildAudio (Input $ dir </> "everything.wav")

        dir </> "everything-mono.wav" ≡> \out -> case plan of
          MoggPlan{..} -> lift $ do
            src <- buildSource $ Input $ dir </> "audio.ogg"
            runAudio (applyVolsMono (map realToFrac _vols) src) out
          Plan{..} -> do
            let planAudios = concat
                  [ toList _song
                  , toList _crowd
                  , toList _planParts >>= toList
                  ]
            srcs <- forM planAudios $ \pa -> let
              chans = computeChannelsPlan songYaml $ _planExpr pa
              vols = map realToFrac $ case _planVols pa of
                [] -> replicate chans 0
                xs -> xs
              in do
                src <- fmap join $ mapM (manualLeaf audioLib songYaml) $ _planExpr pa
                fmap (applyVolsMono vols) $ lift $ buildSource src
            count <- do
              csrc <- lift $ buildSource $ Input $ dir </> "countin.wav"
              return $ applyVolsMono [0, 0] csrc
            lift $ runAudio (foldr mix count srcs) out

        -- MIDI files

        let midprocessed = dir </> "processed.mid"
            midraw = dir </> "raw.mid"
            display = dir </> "display.json"
        midraw ≡> \out -> do
          lift $ putNormal "Loading the MIDI file..."
          input <- shakeMIDI "gen/notes.mid"
          let _ = input :: RBFile.Song (RBFile.RawFile U.Beats)
              extraTempo  = "tempo-" ++ T.unpack planName ++ ".mid"
          tempos <- fmap RBFile.s_tempos $ lift (doesFileExist extraTempo) >>= \b -> if b
            then shakeMIDI extraTempo
            else return input
          lift $ saveMIDI out input { RBFile.s_tempos = tempos }
        midprocessed ≡> \out -> do
          input <- shakeMIDI midraw
          let defTarget = def { rb3_2xBassPedal = True }
          output <- RB3.processRB3 defTarget songYaml input RBDrums.D0 $ getAudioLength planName
          lift $ saveMIDI out output

        display ≡> \out -> do
          song <- shakeMIDI midprocessed
          liftIO $ BL.writeFile out $ makeDisplay songYaml song

        -- count-in audio
        dir </> "countin.wav" ≡> \out -> do
          let hits = case plan of MoggPlan{} -> []; Plan{..} -> case _countin of Countin h -> h
          src <- buildAudioToSpec audioLib songYaml [(-1, 0), (1, 0)] =<< case hits of
            [] -> return Nothing
            _  -> Just . (\expr -> PlanAudio expr [] []) <$> do
              mid <- shakeMIDI $ dir </> "raw.mid"
              let _ = mid :: RBFile.Song (RBFile.RawFile U.Beats)
              return $ Mix $ flip map hits $ \(posn, aud) -> let
                time = Seconds $ realToFrac $ case posn of
                  Left  mb   -> U.applyTempoMap (RBFile.s_tempos mid) $ U.unapplyMeasureMap (RBFile.s_signatures mid) mb
                  Right secs -> secs
                in Pad Start time aud
          lift $ runAudio src out

        -- Getting MOGG/OGG from MoggPlan
        let ogg  = dir </> "audio.ogg"
            mogg = dir </> "audio.mogg"
        case plan of
          Plan{} -> return ()
          MoggPlan{..} -> do
            ogg ≡> \out -> do
              lift $ need [mogg]
              moggToOgg mogg out
            mogg ≡> \out -> do
              p <- inside "Searching for MOGG file" $ searchMOGG audioLib _moggMD5
              lift $ putNormal $ "Found the MOGG file: " ++ toFilePath p
              -- TODO: check if it's actually an OGG (starts with OggS)
              lift $ copyFile' (toFilePath p) out

        -- Audio files for the online preview app
        forM_ ["mp3", "ogg"] $ \ext -> do
          dir </> "web/preview-audio" <.> ext %>
            buildAudio (Input $ dir </> "everything.wav")

        -- Warn about notes that might hang off before a pro keys range shift
        -- TODO flex parts
        phony (dir </> "hanging") $ shakeTrace $ do
          song <- shakeMIDI midprocessed
          lift $ putNormal $ closeShiftsFile song

        -- Print out a summary of (non-vocal) overdrive and unison phrases
        -- TODO flex parts
        phony (dir </> "overdrive") $ shakeTrace $ printOverdrive midprocessed

        -- Melody's Escape customs
        let melodyAudio = dir </> "melody/audio.ogg"
            melodyChart = dir </> "melody/song.track"
        melodyAudio %> copyFile' (dir </> "everything.ogg")
        melodyChart ≡> \out -> do
          lift $ need [midraw, melodyAudio]
          mid <- shakeMIDI midraw
          melody <- liftIO $ MelodysEscape.randomNotes $ U.applyTempoTrack (RBFile.s_tempos mid)
            $ RBFile.onyxMelodysEscape $ RBFile.s_tracks mid
          info <- liftIO $ Snd.getFileInfo melodyAudio
          let secs = realToFrac (Snd.frames info) / realToFrac (Snd.samplerate info) :: U.Seconds
              str = unlines
                [ "1.02"
                , intercalate ";"
                  [ show (MelodysEscape.secondsToTicks secs)
                  , show (realToFrac secs :: Centi)
                  , "420"
                  , "4"
                  ]
                , MelodysEscape.writeTransitions melody
                , MelodysEscape.writeNotes melody
                ]
          liftIO $ writeFile out str
        phony (dir </> "melody") $ need [melodyAudio, melodyChart]

        {-
          -- Check for some extra problems that Magma doesn't catch.
          phony (pedalDir </> "problems") $ do
            song <- loadMIDI $ pedalDir </> "notes.mid"
            let problems = RB3.findProblems song
            mapM_ putNormal problems
            unless (null problems) $ fail "At least 1 problem was found in the MIDI."
        -}

      want buildables
