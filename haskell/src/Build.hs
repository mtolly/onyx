{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
module Build (shakeBuild, targetTitle) where

import           Audio
import qualified C3
import           Codec.Picture
import           Config                                hiding (Difficulty)
import           Control.Monad.Extra
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Lazy                  as BL
import           Data.Char                             (isSpace)
import           Data.Conduit.Audio
import           Data.Conduit.Audio.Sndfile
import qualified Data.DTA                              as D
import qualified Data.DTA.Serialize.Magma              as Magma
import qualified Data.DTA.Serialize.RB3                as D
import qualified Data.DTA.Serialize2                   as D2
import qualified Data.EventList.Absolute.TimeBody      as ATB
import qualified Data.EventList.Relative.TimeBody      as RTB
import           Data.Fixed                            (Centi)
import qualified Data.HashMap.Strict                   as HM
import           Data.List                             (intercalate, isPrefixOf)
import qualified Data.Map                              as Map
import           Data.Maybe                            (fromMaybe, isJust,
                                                        mapMaybe)
import           Data.Monoid                           ((<>))
import qualified Data.Set                              as Set
import           Data.String                           (IsString, fromString)
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as TIO
import           Development.Shake                     hiding (phony)
import qualified Development.Shake                     as Shake
import           Development.Shake.Classes             (hash)
import           Development.Shake.FilePath
import           Difficulty
import           DryVox                                (clipDryVox,
                                                        toDryVoxFormat,
                                                        vocalTubes)
import qualified FretsOnFire                           as FoF
import           Genre
import           Image
import           Import
import           JSONData                              (JSONEither (..),
                                                        traceJSON)
import qualified Magma
import qualified MelodysEscape
import           MoggDecrypt
import           OnyxiteDisplay.Process                (makeDisplay)
import           PrettyDTA
import           ProKeysRanges
import           Readme                                (makeReadme)
import           Reaper.Build                          (makeReaper)
import           RenderAudio
import           Resources                             (emptyMilo, emptyMiloRB2,
                                                        emptyWeightsRB2,
                                                        webDisplay)
import           RockBand.Common                       (Difficulty (..),
                                                        readCommandList)
import qualified RockBand.Drums                        as RBDrums
import qualified RockBand.Events                       as Events
import qualified RockBand.File                         as RBFile
import qualified RockBand.FiveButton                   as RBFive
import           RockBand.Parse                        (unparseBlip,
                                                        unparseList)
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
import           X360
import           YAMLTree

targetTitle :: SongYaml -> Target -> T.Text
targetTitle songYaml target = case target of
  RB3 rb3 -> case rb3_Label rb3 of
    Just lbl | not (T.all isSpace lbl) -> getTitle (_metadata songYaml) <> " " <> lbl
    _ -> if rb3_2xBassPedal rb3
      then getTitle (_metadata songYaml) <> " (2x Bass Pedal)"
      else getTitle (_metadata songYaml)
  RB2 rb2 -> case rb2_Label rb2 of
    Just lbl | not (T.all isSpace lbl) -> getTitle (_metadata songYaml) <> " " <> lbl
    _ -> if rb2_2xBassPedal rb2
      then getTitle (_metadata songYaml) <> " (2x Bass Pedal)"
      else getTitle (_metadata songYaml)
  _ -> undefined

hashRB3 :: SongYaml -> TargetRB3 -> Int
hashRB3 songYaml rb3 = let
  hashed =
    ( rb3_Plan rb3
    , rb3_2xBassPedal rb3
    , (\(JSONEither e) -> e) <$> rb3_SongID rb3
    , rb3_Label rb3
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

makeRB3DTA :: SongYaml -> Plan -> TargetRB3 -> RBFile.Song U.Beats -> T.Text -> D.SongPackage
makeRB3DTA songYaml plan rb3 song filename = let
  (pstart, pend) = previewBounds songYaml song
  PansVols{..} = computePansVols songYaml plan
  RanksTiers{..} = computeRanksTiers songYaml
  len = songLengthMS song
  perctype = getPercType song
  channels = concat [kickPV, snarePV, drumsPV, bassPV, guitarPV, keysPV, vocalPV, crowdPV, songPV]
  pans = case plan of
    MoggPlan{..} -> _pans
    _            -> map fst channels
  vols = case plan of
    MoggPlan{..} -> _vols
    _            -> map snd channels
  -- I still don't know what cores are...
  -- All I know is guitar channels are usually (not always) 1 and all others are -1
  cores = case plan of
    MoggPlan{..} -> map (\i -> if elem i _moggGuitar then 1 else -1) $ zipWith const [0..] _pans
    _ -> concat
      [ map (const (-1)) $ concat [kickPV, snarePV, drumsPV, bassPV]
      , map (const   1)    guitarPV
      , map (const (-1)) $ concat [keysPV, vocalPV, crowdPV, songPV]
      ]
  -- TODO: clean this up
  crowdChannels = case plan of
    MoggPlan{..} -> _moggCrowd
    EachPlan{} -> []
    Plan{..} -> let
      notCrowd = sum $ map length [kickPV, snarePV, drumsPV, bassPV, guitarPV, keysPV, vocalPV]
      in take (length crowdPV) [notCrowd ..]
  tracksAssocList = Map.fromList $ case plan of
    MoggPlan{..} -> let
      maybeChannelPair _   []    = []
      maybeChannelPair str chans = [(str, map fromIntegral chans)]
      in concat
        [ maybeChannelPair "drum" _moggDrums
        , maybeChannelPair "guitar" _moggGuitar
        , maybeChannelPair "bass" _moggBass
        , maybeChannelPair "keys" _moggKeys
        , maybeChannelPair "vocals" _moggVocal
        ]
    _ -> let
      counts =
        [ ("drum", concat [kickPV, snarePV, drumsPV])
        , ("bass", bassPV)
        , ("guitar", guitarPV)
        , ("keys", keysPV)
        , ("vocals", vocalPV)
        ]
      go _ [] = []
      go n ((inst, chans) : rest) = case length chans of
        0 -> go n rest
        c -> (inst, map fromIntegral $ take c [n..]) : go (n + c) rest
      in go 0 counts
  fullGenre = interpretGenre
    (_genre    $ _metadata songYaml)
    (_subgenre $ _metadata songYaml)
  songPkg = D.SongPackage
    { D.name = targetTitle songYaml $ RB3 rb3
    , D.artist = getArtist $ _metadata songYaml
    , D.master = not $ _cover $ _metadata songYaml
    , D.songId = case rb3_SongID rb3 of
      Nothing               -> Right filename
      Just (JSONEither sid) -> sid
    , D.song = D.Song
      { D.songName = "songs/" <> filename <> "/" <> filename
      , D.tracksCount = Nothing
      , D.tracks = D2.Dict tracksAssocList
      , D.vocalParts = Just $ case _hasVocal $ _instruments songYaml of
        Vocal0 -> 0
        Vocal1 -> 1
        Vocal2 -> 2
        Vocal3 -> 3
      , D.pans = map realToFrac pans
      , D.vols = map realToFrac vols
      , D.cores = cores
      , D.drumSolo = D.DrumSounds $ T.words $ case _drumLayout $ _metadata songYaml of
        StandardLayout -> "kick.cue snare.cue tom1.cue tom2.cue crash.cue"
        FlipYBToms     -> "kick.cue snare.cue tom2.cue tom1.cue crash.cue"
      , D.drumFreestyle = D.DrumSounds $ T.words
        "kick.cue snare.cue hat.cue ride.cue crash.cue"
      , D.crowdChannels = guard (not $ null crowdChannels) >> Just (map fromIntegral crowdChannels)
      , D.hopoThreshold = Just $ fromIntegral $ _hopoThreshold $ _options songYaml
      , D.muteVolume = Nothing
      , D.muteVolumeVocals = Nothing
      , D.midiFile = Nothing
      }
    , D.bank = Just $ case perctype of
      Nothing               -> "sfx/tambourine_bank.milo"
      Just RBVox.Tambourine -> "sfx/tambourine_bank.milo"
      Just RBVox.Cowbell    -> "sfx/cowbell_bank.milo"
      Just RBVox.Clap       -> "sfx/handclap_bank.milo"
    , D.drumBank = Just $ case _drumKit $ _metadata songYaml of
      HardRockKit   -> "sfx/kit01_bank.milo"
      ArenaKit      -> "sfx/kit02_bank.milo"
      VintageKit    -> "sfx/kit03_bank.milo"
      TrashyKit     -> "sfx/kit04_bank.milo"
      ElectronicKit -> "sfx/kit05_bank.milo"
    , D.animTempo = Left D.KTempoMedium
    , D.bandFailCue = Nothing
    , D.songScrollSpeed = 2300
    , D.preview = (fromIntegral pstart, fromIntegral pend)
    , D.songLength = Just $ fromIntegral len
    , D.rank = D2.Dict $ Map.fromList
      [ ("drum"       , drumsRank    )
      , ("bass"       , bassRank     )
      , ("guitar"     , guitarRank   )
      , ("vocals"     , vocalRank    )
      , ("keys"       , keysRank     )
      , ("real_keys"  , proKeysRank  )
      , ("real_guitar", proGuitarRank)
      , ("real_bass"  , proBassRank  )
      , ("band"       , bandRank     )
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
    , D.gameOrigin = "ugc_plus"
    , D.rating = fromIntegral $ fromEnum (_rating $ _metadata songYaml) + 1
    , D.genre = rbn2Genre fullGenre
    , D.subGenre = Just $ "subgenre_" <> rbn2Subgenre fullGenre
    , D.vocalGender = fromMaybe Magma.Female $ _vocalGender $ _metadata songYaml
    , D.shortVersion = Nothing
    , D.yearReleased = fromIntegral $ getYear $ _metadata songYaml
    , D.albumArt = Just True
    , D.albumName = Just $ getAlbum $ _metadata songYaml
    , D.albumTrackNumber = Just $ fromIntegral $ getTrackNumber $ _metadata songYaml
    , D.vocalTonicNote = toEnum . fromEnum <$> _key (_metadata songYaml)
    , D.songTonality = Nothing
    , D.songKey = Nothing
    , D.tuningOffsetCents = Just 0
    , D.realGuitarTuning = do
      guard $ _hasProGuitar $ _instruments songYaml
      Just $ map fromIntegral $ case _proGuitarTuning $ _options songYaml of
        []   -> [0, 0, 0, 0, 0, 0]
        tune -> tune
    , D.realBassTuning = do
      guard $ _hasProBass $ _instruments songYaml
      Just $ map fromIntegral $ case _proBassTuning $ _options songYaml of
        []   -> [0, 0, 0, 0]
        tune -> tune
    , D.guidePitchVolume = Just (-3)
    , D.encoding = Just "utf8"
    , D.context = Nothing
    , D.decade = Nothing
    , D.downloaded = Nothing
    , D.basePoints = Nothing
    }
  in songPkg

phony :: FilePath -> Action () -> Rules ()
phony fp act = Shake.phony fp act >> Shake.phony (fp ++ "/") act

allFiles :: FilePath -> Action [FilePath]
allFiles absolute = do
  entries <- getDirectoryContents absolute
  flip concatMapM entries $ \entry -> do
    let full = absolute </> entry
    isDir <- doesDirectoryExist full
    if  | entry `elem` [".", ".."] -> return []
        | isDir                    -> allFiles full
        | otherwise                -> return [full]

printOverdrive :: FilePath -> Action ()
printOverdrive mid = do
  song <- loadMIDI mid
  let trackTimes = Set.fromList . ATB.getTimes . RTB.toAbsoluteEventList 0
      getTrack f = foldr RTB.merge RTB.empty $ mapMaybe f $ RBFile.s_tracks song
      fiveOverdrive t = trackTimes $ RTB.filter (== RBFive.Overdrive True) t
      drumOverdrive t = trackTimes $ RTB.filter (== RBDrums.Overdrive True) t
      gtr = fiveOverdrive $ getTrack $ \case RBFile.PartGuitar t -> Just t; _ -> Nothing
      bass = fiveOverdrive $ getTrack $ \case RBFile.PartBass t -> Just t; _ -> Nothing
      keys = fiveOverdrive $ getTrack $ \case RBFile.PartKeys t -> Just t; _ -> Nothing
      drums = drumOverdrive $ getTrack $ \case RBFile.PartDrums t -> Just t; _ -> Nothing
  forM_ (Set.toAscList $ Set.unions [gtr, bass, keys, drums]) $ \t -> let
    insts = intercalate "," $ concat
      [ ["guitar" | Set.member t gtr]
      , ["bass" | Set.member t bass]
      , ["keys" | Set.member t keys]
      , ["drums" | Set.member t drums]
      ]
    posn = RBFile.showPosition $ U.applyMeasureMap (RBFile.s_signatures song) t
    in putNormal $ posn ++ ": " ++ insts
  return ()

makeC3 :: SongYaml -> Plan -> TargetRB3 -> RBFile.Song U.Beats -> T.Text -> C3.C3
makeC3 songYaml plan rb3 midi pkg = let
  (pstart, _) = previewBounds songYaml midi
  PansVols{..} = computePansVols songYaml plan
  RanksTiers{..} = computeRanksTiers songYaml
  title = targetTitle songYaml $ RB3 rb3
  crowdVol = case map snd crowdPV of
    [] -> Nothing
    v : vs -> if all (== v) vs
      then Just v
      else error $ "C3 doesn't support separate crowd volumes: " ++ show (v : vs)
  numSongID = case rb3_SongID rb3 of
    Just (JSONEither (Left i)) -> Just i
    _                          -> Nothing
  in C3.C3
    { C3.song = getTitle $ _metadata songYaml
    , C3.artist = getArtist $ _metadata songYaml
    , C3.album = getAlbum $ _metadata songYaml
    , C3.customID = pkg
    , C3.version = fromIntegral $ fromMaybe 1 $ rb3_Version rb3
    , C3.isMaster = not $ _cover $ _metadata songYaml
    , C3.encodingQuality = 5
    , C3.crowdAudio = guard (isJust crowdVol) >> Just "crowd.wav"
    , C3.crowdVol = crowdVol
    , C3.is2xBass = rb3_2xBassPedal rb3
    , C3.rhythmKeys = _rhythmKeys $ _metadata songYaml
    , C3.rhythmBass = _rhythmBass $ _metadata songYaml
    , C3.karaoke = getKaraoke plan
    , C3.multitrack = getMultitrack plan
    , C3.convert = _convert $ _metadata songYaml
    , C3.expertOnly = _expertOnly $ _metadata songYaml
    , C3.proBassDiff = guard (_hasProBass $ _instruments songYaml) >> Just (fromIntegral proBassTier)
    , C3.proBassTuning = if _hasProBass $ _instruments songYaml
      then Just $ case _proBassTuning $ _options songYaml of
        []   -> "(real_bass_tuning (0 0 0 0))"
        tune -> "(real_bass_tuning (" <> T.unwords (map (T.pack . show) tune) <> "))"
      else Nothing
    , C3.proGuitarDiff = guard (_hasProGuitar $ _instruments songYaml) >> Just (fromIntegral proGuitarTier)
    , C3.proGuitarTuning = if _hasProGuitar $ _instruments songYaml
      then Just $ case _proGuitarTuning $ _options songYaml of
        []   -> "(real_guitar_tuning (0 0 0 0 0 0))"
        tune -> "(real_guitar_tuning (" <> T.unwords (map (T.pack . show) tune) <> "))"
      else Nothing
    , C3.disableProKeys =
        _hasKeys (_instruments songYaml) && not (_hasProKeys $ _instruments songYaml)
    , C3.tonicNote = _key $ _metadata songYaml
    , C3.tuningCents = 0
    , C3.songRating = fromEnum (_rating $ _metadata songYaml) + 1
    , C3.drumKitSFX = fromEnum $ _drumKit $ _metadata songYaml
    , C3.hopoThresholdIndex = case _hopoThreshold $ _options songYaml of
      90  -> 0
      130 -> 1
      170 -> 2
      250 -> 3
      ht  -> error $ "C3 Magma does not support the HOPO threshold " ++ show ht
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
    , C3.packageDescription = "Created with Magma: C3 Roks Edition (forums.customscreators.com) and Onyxite's Build Tool."
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
makeMagmaProj :: SongYaml -> Plan -> T.Text -> FilePath -> Action T.Text -> Action Magma.RBProj
makeMagmaProj songYaml plan pkg mid thisTitle = do
  song <- loadMIDI mid
  let (pstart, _) = previewBounds songYaml song
      RanksTiers{..} = computeRanksTiers songYaml
      PansVols{..} = computePansVols songYaml plan
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
      pvFile [] _ = disabledFile
      pvFile pv f = Magma.AudioFile
        { Magma.audioEnabled = True
        , Magma.channels = fromIntegral $ length pv
        , Magma.pan = map (realToFrac . fst) pv
        , Magma.vol = map (realToFrac . snd) pv
        , Magma.audioFile = f
        }
      replaceŸ = T.map $ \case
        'ÿ' -> 'y'
        'Ÿ' -> 'Y'
        c   -> c
  title <- T.map (\case '"' -> '\''; c -> c) <$> thisTitle
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
        { Magma.previewStartMs = fromIntegral pstart
        , Magma.rankDrum    = max 1 drumsTier
        , Magma.rankBass    = max 1 bassTier
        , Magma.rankGuitar  = max 1 guitarTier
        , Magma.rankVocals  = max 1 vocalTier
        , Magma.rankKeys    = max 1 keysTier
        , Magma.rankProKeys = max 1 proKeysTier
        , Magma.rankBand    = max 1 bandTier
        , Magma.vocalScrollSpeed = 2300
        , Magma.animTempo = 32
        , Magma.vocalGender = fromMaybe Magma.Female $ _vocalGender $ _metadata songYaml
        , Magma.vocalPercussion = case perctype of
          Nothing               -> Magma.Tambourine
          Just RBVox.Tambourine -> Magma.Tambourine
          Just RBVox.Cowbell    -> Magma.Cowbell
          Just RBVox.Clap       -> Magma.Handclap
        , Magma.vocalParts = case _hasVocal $ _instruments songYaml of
          Vocal0 -> 0
          Vocal1 -> 1
          Vocal2 -> 2
          Vocal3 -> 3
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
        , Magma.autogenTheme = Right $ case _autogenTheme $ _metadata songYaml of
          AutogenDefault -> "Default.rbtheme"
          theme          -> T.pack (show theme) <> ".rbtheme"
        }
      , Magma.dryVox = Magma.DryVox
        { Magma.part0 = case _hasVocal $ _instruments songYaml of
          Vocal0 -> emptyDryVox
          Vocal1 -> silentDryVox 0
          _      -> silentDryVox 1
        , Magma.part1 = if _hasVocal (_instruments songYaml) >= Vocal2
          then silentDryVox 2
          else emptyDryVox
        , Magma.part2 = if _hasVocal (_instruments songYaml) >= Vocal3
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
        , Magma.drumKit = pvFile drumsPV "drums.wav"
        , Magma.drumKick = pvFile kickPV "kick.wav"
        , Magma.drumSnare = pvFile snarePV "snare.wav"
        , Magma.bass = pvFile bassPV "bass.wav"
        , Magma.guitar = pvFile guitarPV "guitar.wav"
        , Magma.vocals = pvFile vocalPV "vocal.wav"
        , Magma.keys = pvFile keysPV "keys.wav"
        , Magma.backing = pvFile songPV "song-countin.wav"
        }
      }
    }

shakeBuild :: [FilePath] -> FilePath -> [FilePath] -> IO ()
shakeBuild audioDirs yamlPath buildables = do

  songYaml
    <-  readYAMLTree yamlPath
    >>= runReaderT (printStackTraceIO traceJSON)

  let fullGenre = interpretGenre
        (_genre    $ _metadata songYaml)
        (_subgenre $ _metadata songYaml)

  checkDefined songYaml

  exeTime <- getExecutablePath >>= Dir.getModificationTime
  yamlTime <- Dir.getModificationTime yamlPath
  let version = show exeTime ++ "," ++ show yamlTime

  Dir.withCurrentDirectory (takeDirectory yamlPath) $ do

    shake shakeOptions{ shakeThreads = 0, shakeFiles = "gen", shakeVersion = version } $ do

      allFilesInAudioDirs <- newCache $ \() -> do
        genAbsolute <- liftIO $ Dir.canonicalizePath "gen/"
        filter (\f -> not $ genAbsolute `isPrefixOf` f)
          <$> concatMapM allFiles audioDirs
      allJammitInAudioDirs <- newCache $ \() -> liftIO $ concatMapM J.loadLibrary audioDirs

      _            <- addOracle $ \(AudioSearch  s) -> allFilesInAudioDirs () >>= audioSearch (read s)
      jammitOracle <- addOracle $ \(JammitSearch s) -> fmap show $ allJammitInAudioDirs () >>= jammitSearch songYaml (read s)
      moggOracle   <- addOracle $ \(MoggSearch   s) -> allFilesInAudioDirs () >>= moggSearch s

      forM_ (HM.elems $ _audio songYaml) $ \case
        AudioFile{ _filePath = Just fp, _commands = cmds } | not $ null cmds -> do
          fp %> \_ -> mapM_ (Shake.unit . Shake.cmd . T.unpack) cmds
        _ -> return ()

      phony "yaml"  $ liftIO $ print songYaml
      phony "audio" $ liftIO $ print audioDirs
      phony "clean" $ cmd ("rm -rf gen" :: String)

      let RanksTiers{..} = computeRanksTiers songYaml

      -- Find and convert all Jammit audio into the work directory
      let jammitAudioParts = map J.Only    [minBound .. maxBound]
                          ++ map J.Without [minBound .. maxBound]
      forM_ (HM.toList $ _jammit songYaml) $ \(jammitName, jammitQuery) ->
        forM_ jammitAudioParts $ \audpart ->
          jammitPath jammitName audpart %> \out -> do
            putNormal $ "Looking for the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart
            result <- fmap read $ jammitOracle $ JammitSearch $ show jammitQuery
            case [ jcfx | (audpart', jcfx) <- result, audpart == audpart' ] of
              jcfx : _ -> do
                putNormal $ "Found the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart
                liftIO $ J.runAudio [jcfx] [] out
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
            Nothing -> return $ generateImage (\_ _ -> PixelRGB8 0 0 255) 256 256
      "gen/cover.bmp" %> \out -> loadRGB8 >>= liftIO . writeBitmap out . scaleSTBIR 256 256
      "gen/cover.png" %> \out -> loadRGB8 >>= liftIO . writePng    out . scaleSTBIR 256 256
      "gen/cover.png_xbox" %> \out -> case _fileAlbumArt $ _metadata songYaml of
        Just f | takeExtension f == ".png_xbox" -> copyFile' f out
        _      -> loadRGB8 >>= liftIO . BL.writeFile out . toPNG_XBOX

      -- The Markdown README file, for GitHub purposes
      phony "update-readme" $ if _published songYaml
        then need ["README.md"]
        else removeFilesAfter "." ["README.md"]
      "README.md" %> \out -> do
        yamlAbs <- liftIO $ Dir.canonicalizePath yamlPath
        liftIO $ writeFile out $ makeReadme songYaml yamlAbs

      "gen/notes.mid" %> \out -> do
        doesFileExist "notes.mid" >>= \b -> if b
          then copyFile' "notes.mid" out
          else saveMIDI out RBFile.Song
            { RBFile.s_tempos = U.tempoMapFromBPS RTB.empty
            , RBFile.s_signatures = U.measureMapFromTimeSigs U.Error RTB.empty
            , RBFile.s_tracks =
              [ RBFile.PartDrums RTB.empty
              , RBFile.PartGuitar RTB.empty
              , RBFile.PartBass RTB.empty
              , RBFile.PartKeys RTB.empty
              , RBFile.PartRealKeys Expert RTB.empty
              , RBFile.PartRealKeys Hard RTB.empty
              , RBFile.PartRealKeys Medium RTB.empty
              , RBFile.PartRealKeys Easy RTB.empty
              , RBFile.PartVocals RTB.empty
              , RBFile.Harm1 RTB.empty
              , RBFile.Harm2 RTB.empty
              , RBFile.Harm3 RTB.empty
              , RBFile.Events RTB.empty
              , RBFile.Beat RTB.empty
              , RBFile.Venue RTB.empty
              ]
            }

      let getAudioLength :: T.Text -> Action U.Seconds
          getAudioLength planName = do
            let allSourceAudio =
                  [ "gen/plan" </> T.unpack planName </> "kick.wav"
                  , "gen/plan" </> T.unpack planName </> "snare.wav"
                  , "gen/plan" </> T.unpack planName </> "drums.wav"
                  , "gen/plan" </> T.unpack planName </> "guitar.wav"
                  , "gen/plan" </> T.unpack planName </> "bass.wav"
                  , "gen/plan" </> T.unpack planName </> "keys.wav"
                  , "gen/plan" </> T.unpack planName </> "vocal.wav"
                  , "gen/plan" </> T.unpack planName </> "crowd.wav"
                  , "gen/plan" </> T.unpack planName </> "song.wav"
                  ]
            need allSourceAudio
            liftIO $ maximum <$> mapM audioSeconds allSourceAudio

      let rbRules :: FilePath -> T.Text -> TargetRB3 -> Maybe TargetRB2 -> Rules ()
          rbRules dir targetName rb3 mrb2 = do
            let pkg :: (IsString a) => a
                pkg = fromString $ "onyx" <> show (hashRB3 songYaml rb3)
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

            pathMagmaKick   %> copyFile' (planDir </> "kick.wav"  )
            pathMagmaSnare  %> copyFile' (planDir </> "snare.wav" )
            pathMagmaDrums  %> copyFile' (planDir </> "drums.wav" )
            pathMagmaBass   %> copyFile' (planDir </> "bass.wav"  )
            pathMagmaGuitar %> copyFile' (planDir </> "guitar.wav")
            pathMagmaKeys   %> copyFile' (planDir </> "keys.wav"  )
            pathMagmaVocal  %> copyFile' (planDir </> "vocal.wav" )
            pathMagmaCrowd  %> copyFile' (planDir </> "crowd.wav" )
            pathMagmaSong   %> copyFile' (planDir </> "song-countin.wav")
            let saveClip m out vox = do
                  let fmt = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
                      clip = clipDryVox $ U.applyTempoTrack (RBFile.s_tempos m) $ vocalTubes vox
                  need [pathMagmaVocal]
                  unclippedVox <- liftIO $ sourceSnd pathMagmaVocal
                  unclipped <- case frames unclippedVox of
                    0 -> do
                      need [pathMagmaSong]
                      liftIO $ sourceSnd pathMagmaSong
                    _ -> return unclippedVox
                  putNormal $ "Writing a clipped dry vocals file to " ++ out
                  liftIO $ runResourceT $ sinkSnd out fmt $ toDryVoxFormat $ clip unclipped
                  putNormal $ "Finished writing dry vocals to " ++ out
            pathMagmaDryvox0 %> \out -> do
              m <- loadMIDI pathMagmaMid
              saveClip m out $ foldr RTB.merge RTB.empty [ trk | RBFile.PartVocals trk <- RBFile.s_tracks m ]
            pathMagmaDryvox1 %> \out -> do
              m <- loadMIDI pathMagmaMid
              saveClip m out $ foldr RTB.merge RTB.empty [ trk | RBFile.Harm1 trk <- RBFile.s_tracks m ]
            pathMagmaDryvox2 %> \out -> do
              m <- loadMIDI pathMagmaMid
              saveClip m out $ foldr RTB.merge RTB.empty [ trk | RBFile.Harm2 trk <- RBFile.s_tracks m ]
            pathMagmaDryvox3 %> \out -> do
              m <- loadMIDI pathMagmaMid
              saveClip m out $ foldr RTB.merge RTB.empty [ trk | RBFile.Harm3 trk <- RBFile.s_tracks m ]
            pathMagmaDryvoxSine %> \out -> do
              m <- loadMIDI pathMagmaMid
              let fmt = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
              liftIO $ runResourceT $ sinkSnd out fmt $ RB2.dryVoxAudio m
            pathMagmaDummyMono   %> buildAudio (Silence 1 $ Seconds 31) -- we set preview start to 0:00 so these can be short
            pathMagmaDummyStereo %> buildAudio (Silence 2 $ Seconds 31)
            pathMagmaCover %> copyFile' "gen/cover.bmp"
            pathMagmaCoverV1 %> \out -> liftIO $ writeBitmap out $ generateImage (\_ _ -> PixelRGB8 0 0 255) 256 256
            let title = targetTitle songYaml $ RB3 rb3
            pathMagmaProj %> \out -> do
              p <- makeMagmaProj songYaml plan pkg pathMagmaMid $ return title
              liftIO $ D.writeFileDTA_latin1 out $ D2.serialize D2.format p
            pathMagmaC3 %> \out -> do
              midi <- loadMIDI pathMagmaMid
              liftIO $ TIO.writeFile out $ C3.showC3 $ makeC3 songYaml plan rb3 midi pkg
            phony pathMagmaSetup $ need $ concat
              -- Just make all the Magma prereqs, but don't actually run Magma
              [ guard (_hasDrums    $ _instruments songYaml) >> [pathMagmaDrums, pathMagmaKick, pathMagmaSnare]
              , guard (hasAnyBass   $ _instruments songYaml) >> [pathMagmaBass  ]
              , guard (hasAnyGuitar $ _instruments songYaml) >> [pathMagmaGuitar]
              , guard (hasAnyKeys   $ _instruments songYaml) >> [pathMagmaKeys  ]
              , case _hasVocal $ _instruments songYaml of
                Vocal0 -> []
                Vocal1 -> [pathMagmaVocal, pathMagmaDryvox0]
                Vocal2 -> [pathMagmaVocal, pathMagmaDryvox1, pathMagmaDryvox2]
                Vocal3 -> [pathMagmaVocal, pathMagmaDryvox1, pathMagmaDryvox2, pathMagmaDryvox3]
              , [pathMagmaSong, pathMagmaCrowd, pathMagmaCover, pathMagmaMid, pathMagmaProj, pathMagmaC3]
              ]
            pathMagmaRba %> \out -> do
              need [pathMagmaSetup]
              putNormal "# Running Magma v2 (C3)"
              liftIO (Magma.runMagma pathMagmaProj out) >>= putNormal
            pathMagmaExport %> \out -> do
              need [pathMagmaMid, pathMagmaProj]
              putNormal "# Running Magma v2 to export MIDI"
              liftIO (Magma.runMagmaMIDI pathMagmaProj out) >>= putNormal
            let getRealSections :: Action (RTB.T U.Beats T.Text)
                getRealSections = do
                  raw <- loadMIDI $ planDir </> "raw.mid"
                  let evts = foldr RTB.merge RTB.empty [ trk | RBFile.Events trk <- RBFile.s_tracks raw ]
                  return $ RTB.mapMaybe (\case Events.PracticeSection s -> Just s; _ -> Nothing) evts
            pathMagmaExport2 %> \out -> do
              -- Using Magma's "export MIDI" option overwrites all animations/venue
              -- with autogenerated ones, even if they were actually authored.
              -- So, we now need to readd them back from the user MIDI (if they exist).
              userMid <- loadMIDI pathMagmaMid
              magmaMid <- loadMIDI pathMagmaExport
              sects <- getRealSections
              let reauthor getTrack eventPredicates magmaTrack = let
                    authoredTrack = foldr RTB.merge RTB.empty $ mapMaybe getTrack $ RBFile.s_tracks userMid
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
              saveMIDI out $ magmaMid
                { RBFile.s_tracks = flip map (RBFile.s_tracks magmaMid) $ \case
                  RBFile.PartDrums t -> RBFile.PartDrums $ let
                    getTrack = \case RBFile.PartDrums trk -> Just trk; _ -> Nothing
                    isMood = \case RBDrums.Mood{} -> True; _ -> False
                    isAnim = \case RBDrums.Animation{} -> True; _ -> False
                    in reauthor getTrack [isMood, isAnim] t
                  RBFile.PartGuitar t -> RBFile.PartGuitar $ let
                    getTrack = \case RBFile.PartGuitar trk -> Just trk; _ -> Nothing
                    in reauthor getTrack fivePredicates t
                  RBFile.PartBass t -> RBFile.PartBass $ let
                    getTrack = \case RBFile.PartBass trk -> Just trk; _ -> Nothing
                    in reauthor getTrack fivePredicates t
                  RBFile.PartKeys       t -> RBFile.PartKeys $ let
                    getTrack = \case RBFile.PartKeys trk -> Just trk; _ -> Nothing
                    in reauthor getTrack fivePredicates t
                  RBFile.PartVocals t -> RBFile.PartVocals $ let
                    getTrack = \case RBFile.PartVocals trk -> Just trk; _ -> Nothing
                    isMood = \case RBVox.Mood{} -> True; _ -> False
                    in reauthor getTrack [isMood] t
                  RBFile.Venue t -> RBFile.Venue $ let
                    getTrack = \case RBFile.Venue trk -> Just trk; _ -> Nothing
                    -- TODO: split up camera and lighting so you can author just one
                    in reauthor getTrack [const True] t
                  RBFile.Events t -> RBFile.Events $ if RTB.null sects
                    then t
                    else RTB.merge (fmap Events.PracticeSection sects)
                      $ RTB.filter (\case Events.PracticeSection _ -> False; _ -> True) t
                  -- Stuff "export midi" doesn't overwrite:
                  -- PART KEYS_ANIM_LH/RH
                  -- Crowd stuff in EVENTS
                  t -> t
                }

            pathMagmaMid %> \out -> do
              input <- loadMIDI $ planDir </> "raw.mid"
              let pv = computePansVols songYaml plan
              output <- RB3.processMIDI
                songYaml
                input
                (if rb3_2xBassPedal rb3 then RB3.Kicks2x else RB3.Kicks1x)
                (mixMode pv)
                (getAudioLength planName)
              sects <- ATB.toPairList . RTB.toAbsoluteEventList 0 <$> getRealSections
              let (magmaSects, invalid) = makeRBN2Sections sects
                  magmaSects' = RTB.fromAbsoluteEventList $ ATB.fromPairList magmaSects
                  isSection (Events.PracticeSection _) = True
                  isSection _                          = False
                  adjustTrack = \case
                    RBFile.Events t -> RBFile.Events
                      $ RTB.merge (fmap Events.PracticeSection magmaSects')
                      $ RTB.filter (not . isSection) t
                    trk             -> trk
              case invalid of
                [] -> return ()
                _  -> putNormal $ "The following sections were unrecognized and replaced: " ++ show invalid
              saveMIDI out output
                { RBFile.s_tracks = map adjustTrack $ RBFile.s_tracks output
                }

            let pathDta = dir </> "stfs/songs/songs.dta"
                pathMid = dir </> "stfs/songs" </> pkg </> pkg <.> "mid"
                pathMogg = dir </> "stfs/songs" </> pkg </> pkg <.> "mogg"
                pathPng = dir </> "stfs/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
                pathMilo = dir </> "stfs/songs" </> pkg </> "gen" </> pkg <.> "milo_xbox"
                pathCon = dir </> "rb3con"

            pathDta %> \out -> do
              song <- loadMIDI pathMid
              let songPkg = makeRB3DTA songYaml plan rb3 song pkg
              liftIO $ writeUtf8CRLF out $ prettyDTA pkg songPkg $ makeC3DTAComments (_metadata songYaml) plan $ rb3_2xBassPedal rb3
            pathMid %> copyFile' pathMagmaExport2
            pathMogg %> copyFile' (planDir </> "audio.mogg")
            pathPng  %> copyFile' "gen/cover.png_xbox"
            pathMilo %> \out -> liftIO $ B.writeFile out emptyMilo
            pathCon %> \out -> do
              need [pathDta, pathMid, pathMogg, pathPng, pathMilo]
              putNormal "# Calling rb3pkg to make RB3 CON file"
              s <- liftIO $ rb3pkg
                (getArtist (_metadata songYaml) <> ": " <> getTitle (_metadata songYaml))
                ("Version: " <> targetName)
                (dir </> "stfs")
                out
              putNormal s

            case mrb2 of
              Nothing -> return ()
              Just rb2 -> do

                pathMagmaMidV1 %> \out -> loadMIDI pathMagmaMid >>= saveMIDI out . RB2.convertMIDI
                  (rb2_Keys rb2)
                  (fromIntegral (_hopoThreshold $ _options songYaml) / 480)

                pathMagmaProjV1 %> \out -> do
                  p <- makeMagmaProj songYaml plan pkg pathMagmaMid $ return title
                  let makeDummy (Magma.Tracks dl dkt dk ds b g v k bck) = Magma.Tracks
                        dl
                        (makeDummyKeep dkt)
                        (makeDummyKeep dk)
                        (makeDummyKeep ds)
                        (makeDummyMono $ if rb2_Keys rb2 == KeysBass   then k else b)
                        (makeDummyMono $ if rb2_Keys rb2 == KeysGuitar then k else g)
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
                      swapRanks gd = case rb2_Keys rb2 of
                        NoKeys     -> gd
                        KeysBass   -> gd { Magma.rankBass   = Magma.rankKeys gd }
                        KeysGuitar -> gd { Magma.rankGuitar = Magma.rankKeys gd }
                  liftIO $ D.writeFileDTA_latin1 out $ D2.serialize D2.format p
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
                      , Magma.gamedata = swapRanks $ (Magma.gamedata $ Magma.project p)
                        { Magma.previewStartMs = 0 -- for dummy audio. will reset after magma
                        }
                      }
                    }

                pathMagmaRbaV1 %> \out -> do
                  need [pathMagmaDummyMono, pathMagmaDummyStereo, pathMagmaDryvoxSine, pathMagmaCoverV1, pathMagmaMidV1, pathMagmaProjV1]
                  putNormal "# Running Magma v1 (without 10 min limit)"
                  (str, good) <- liftIO $ Magma.runMagmaV1 pathMagmaProjV1 out
                  putNormal str
                  unless good $ do
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
                        = D2.Dict
                        . Map.fromList
                        . mapMaybe (\(k, v) -> case k of
                          "guitar" -> case rb2_Keys rb2 of
                            KeysGuitar -> Nothing
                            _          -> Just (k, v)
                          "bass" -> case rb2_Keys rb2 of
                            KeysBass -> Nothing
                            _        -> Just (k, v)
                          "keys" -> case rb2_Keys rb2 of
                            NoKeys     -> Nothing
                            KeysGuitar -> Just ("guitar", v)
                            KeysBass   -> Just ("bass", v)
                          "drum" -> Just (k, v)
                          "vocals" -> Just (k, v)
                          "band" -> Just (k, v)
                          _ -> Nothing
                        )
                        . Map.toList
                        . D2.fromDict
                  rb2OriginalDTA %> \out -> do
                    ex <- doesRBAExist
                    if ex
                      then liftIO $ getRBAFile 0 pathMagmaRbaV1 out
                      else do
                        need [pathDta]
                        (_, rb3DTA, _) <- liftIO $ readRB3DTA pathDta
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
                              , D.gameOrigin = "rb2"
                              , D.albumName = D.albumName rb3DTA
                              , D.albumTrackNumber = D.albumTrackNumber rb3DTA
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
                              }
                        liftIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0 [D.Parens (D.Tree 0 (D.Key pkg : D2.toChunks D2.format newDTA))]
                  rb2DTA %> \out -> do
                    need [rb2OriginalDTA, pathDta]
                    (_, magmaDTA, _) <- liftIO $ readRB3DTA rb2OriginalDTA
                    (_, rb3DTA, _) <- liftIO $ readRB3DTA pathDta
                    let newDTA :: D.SongPackage
                        newDTA = magmaDTA
                          { D.name = D.name rb3DTA
                          , D.artist = D.artist rb3DTA
                          , D.albumName = D.albumName rb3DTA
                          , D.master = not $ _cover $ _metadata songYaml
                          , D.version = fromMaybe 1 $ rb2_Version rb2
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
                          , D.songId = case rb2_SongID rb2 of
                            Nothing               -> Right pkg
                            Just (JSONEither eis) -> eis
                          , D.preview = D.preview rb3DTA -- because we told magma preview was at 0s earlier
                          , D.songLength = D.songLength rb3DTA -- magma v1 set this to 31s from the audio file lengths
                          }
                    liftIO $ writeLatin1CRLF out $ prettyDTA pkg newDTA $ makeC3DTAComments (_metadata songYaml) plan (rb2_2xBassPedal rb2)
                  rb2Mid %> \out -> do
                    ex <- doesRBAExist
                    need [pathMagmaMid]
                    mid <- liftIO $ if ex
                      then do
                        getRBAFile 1 pathMagmaRbaV1 out
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
                      then getRBAFile 3 pathMagmaRbaV1 out
                      else B.writeFile out emptyMiloRB2
                  rb2Weights %> \out -> do
                    ex <- doesRBAExist
                    liftIO $ if ex
                      then getRBAFile 5 pathMagmaRbaV1 out
                      else B.writeFile out emptyWeightsRB2
                  rb2Art %> copyFile' "gen/cover.png_xbox"
                  rb2Pan %> \out -> liftIO $ B.writeFile out B.empty
                  rb2CON %> \out -> do
                    need [rb2DTA, rb2Mogg, rb2Mid, rb2Art, rb2Weights, rb2Milo, rb2Pan]
                    putNormal "# Calling rb3pkg to make RB2 CON file"
                    s <- liftIO $ rb2pkg
                      (getArtist (_metadata songYaml) <> ": " <> getTitle (_metadata songYaml))
                      (getArtist (_metadata songYaml) <> ": " <> getTitle (_metadata songYaml))
                      (dir </> "rb2")
                      out
                    putNormal s

      let defaultTargets = HM.fromList $ do
            maybePlan <- map Just (HM.keys $ _plans songYaml) ++ case HM.keys $ _plans songYaml of
              [_] -> [Nothing]
              _   -> []
            let addPlan t = maybe t (\plan -> t <> "-" <> plan) maybePlan
            [   (addPlan "rb3", RB3 TargetRB3
                { rb3_Plan = maybePlan
                , rb3_2xBassPedal = False
                , rb3_SongID = Nothing
                , rb3_Label = Nothing
                , rb3_Version = Nothing
                })
              , (addPlan "rb2", RB2 TargetRB2
                { rb2_Plan = maybePlan
                , rb2_2xBassPedal = False
                , rb2_SongID = Nothing
                , rb2_Label = Nothing
                , rb2_Keys = NoKeys
                , rb2_Version = Nothing
                })
              , (addPlan "rb3-2x", RB3 TargetRB3
                { rb3_Plan = maybePlan
                , rb3_2xBassPedal = True
                , rb3_SongID = Nothing
                , rb3_Label = Nothing
                , rb3_Version = Nothing
                })
              , (addPlan "rb2-2x", RB2 TargetRB2
                { rb2_Plan = maybePlan
                , rb2_2xBassPedal = True
                , rb2_SongID = Nothing
                , rb2_Label = Nothing
                , rb2_Keys = NoKeys
                , rb2_Version = Nothing
                })
              , (addPlan "ps", PS TargetPS
                { ps_Plan = maybePlan
                , ps_Label = Nothing
                })
              ]

      forM_ (HM.toList $ HM.union (_targets songYaml) defaultTargets) $ \(targetName, target) -> do
        let dir = "gen/target" </> T.unpack targetName
        case target of
          RB3 rb3 -> rbRules dir targetName rb3 Nothing
          RB2 rb2 -> let
            rb3 = TargetRB3
              { rb3_Plan = rb2_Plan rb2
              , rb3_2xBassPedal = rb2_2xBassPedal rb2
              , rb3_SongID = rb2_SongID rb2
              , rb3_Label = rb2_Label rb2
              , rb3_Version = rb2_Version rb2
              }
            in rbRules dir targetName rb3 $ Just rb2
          PS ps -> do

            (planName, plan) <- case getPlan (ps_Plan ps) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show ps
              Just pair -> return pair
            let planDir = "gen/plan" </> T.unpack planName
                pv = computePansVols songYaml plan

            dir </> "ps/notes.mid" %> \out -> do
              input <- loadMIDI $ planDir </> "raw.mid"
              output <- RB3.processMIDI
                songYaml
                input
                RB3.KicksPS
                (mixMode pv)
                (getAudioLength planName)
              saveMIDI out output

            dir </> "ps/song.ini" %> \out -> do
              song <- loadMIDI $ dir </> "ps/notes.mid"
              let (pstart, _) = previewBounds songYaml song
                  len = songLengthMS song
              liftIO $ FoF.saveSong out FoF.Song
                { FoF.artist           = _artist $ _metadata songYaml
                , FoF.name             = _title $ _metadata songYaml
                , FoF.album            = _album $ _metadata songYaml
                , FoF.charter          = _author $ _metadata songYaml
                , FoF.year             = _year $ _metadata songYaml
                , FoF.genre            = Just $ fofGenre fullGenre
                , FoF.proDrums         = guard (_hasDrums $ _instruments songYaml) >> Just (_proDrums $ _options songYaml)
                , FoF.songLength       = Just len
                , FoF.previewStartTime = Just pstart
                -- difficulty tiers go from 0 to 6, or -1 for no part
                , FoF.diffBand         = Just $ fromIntegral $ bandTier    - 1
                , FoF.diffGuitar       = Just $ fromIntegral $ guitarTier  - 1
                , FoF.diffBass         = Just $ fromIntegral $ bassTier    - 1
                , FoF.diffDrums        = Just $ fromIntegral $ drumsTier   - 1
                , FoF.diffDrumsReal    = Just $ if _proDrums $ _options songYaml
                  then fromIntegral $ drumsTier - 1
                  else -1
                , FoF.diffKeys         = Just $ fromIntegral $ keysTier    - 1
                , FoF.diffKeysReal     = Just $ fromIntegral $ proKeysTier - 1
                , FoF.diffVocals       = Just $ fromIntegral $ vocalTier   - 1
                , FoF.diffVocalsHarm   = Just $ fromIntegral $ vocalTier   - 1
                , FoF.diffDance        = Just (-1)
                , FoF.diffBassReal     = Just $ fromIntegral $ proBassTier - 1
                , FoF.diffGuitarReal   = Just $ fromIntegral $ proGuitarTier - 1
                -- TODO: are the 22-fret difficulties needed?
                , FoF.diffBassReal22   = Just $ fromIntegral $ proBassTier - 1
                , FoF.diffGuitarReal22 = Just $ fromIntegral $ proGuitarTier - 1
                , FoF.diffGuitarCoop   = Just (-1)
                , FoF.diffRhythm       = Just (-1)
                , FoF.diffDrumsRealPS  = Just (-1)
                , FoF.diffKeysRealPS   = Just (-1)
                , FoF.delay            = Nothing
                , FoF.starPowerNote    = Just 116
                , FoF.track            = _trackNumber $ _metadata songYaml
                }
            dir </> "ps/drums.ogg"   %> buildAudio (Input $ planDir </> "drums.wav"       )
            dir </> "ps/drums_1.ogg" %> buildAudio (Input $ planDir </> "kick.wav"        )
            dir </> "ps/drums_2.ogg" %> buildAudio (Input $ planDir </> "snare.wav"       )
            dir </> "ps/drums_3.ogg" %> buildAudio (Input $ planDir </> "drums.wav"       )
            dir </> "ps/guitar.ogg"  %> buildAudio (Input $ planDir </> "guitar.wav"      )
            dir </> "ps/keys.ogg"    %> buildAudio (Input $ planDir </> "keys.wav"        )
            dir </> "ps/rhythm.ogg"  %> buildAudio (Input $ planDir </> "bass.wav"        )
            dir </> "ps/vocals.ogg"  %> buildAudio (Input $ planDir </> "vocal.wav"       )
            dir </> "ps/crowd.ogg"   %> buildAudio (Input $ planDir </> "crowd.wav"       )
            dir </> "ps/song.ogg"    %> buildAudio (Input $ planDir </> "song-countin.wav")
            dir </> "ps/album.png"   %> copyFile' "gen/cover.png"
            phony (dir </> "ps") $ need $ map (\f -> dir </> "ps" </> f) $ concat
              [ ["song.ini", "notes.mid", "song.ogg", "album.png"]
              , ["drums.ogg"   | _hasDrums     (_instruments songYaml) && mixMode pv == RBDrums.D0]
              , ["drums_1.ogg" | _hasDrums     (_instruments songYaml) && mixMode pv /= RBDrums.D0]
              , ["drums_2.ogg" | _hasDrums     (_instruments songYaml) && mixMode pv /= RBDrums.D0]
              , ["drums_3.ogg" | _hasDrums     (_instruments songYaml) && mixMode pv /= RBDrums.D0]
              , ["guitar.ogg"  | hasAnyGuitar $ _instruments songYaml]
              , ["keys.ogg"    | hasAnyKeys   $ _instruments songYaml]
              , ["rhythm.ogg"  | hasAnyBass   $ _instruments songYaml]
              , ["vocals.ogg"  | hasAnyVocal  $ _instruments songYaml]
              , ["crowd.ogg"   | case plan of Plan{..} -> isJust _crowd; _ -> False]
              ]

      forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do

        let dir = "gen/plan" </> T.unpack planName
            PansVols{..} = computePansVols songYaml plan

        -- REAPER project
        "notes-" ++ T.unpack planName ++ ".RPP" %> \out -> do
          let audios = map (\x -> "gen/plan" </> T.unpack planName </> x <.> "wav")
                $ ["guitar", "bass", "drums", "kick", "snare", "keys", "vocal", "crowd"] ++ case plan of
                  MoggPlan{} -> ["song-countin"]
                  _          -> ["song"]
                  -- Previously this relied on countin,
                  -- but it's better to not have to generate gen/plan/foo/xp/notes.mid
              extraTempo = "tempo-" ++ T.unpack planName ++ ".mid"
          b <- doesFileExist extraTempo
          let tempo = if b then extraTempo else "gen/notes.mid"
          makeReaper "gen/notes.mid" tempo audios out

        -- Audio files
        makeAudioFiles songYaml plan dir mixMode

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

        let allAudioWithPV =
              [ (kickPV, dir </> "kick.wav")
              , (snarePV, dir </> "snare.wav")
              , (drumsPV, dir </> "drums.wav")
              , (guitarPV, dir </> "guitar.wav")
              , (bassPV, dir </> "bass.wav")
              , (keysPV, dir </> "keys.wav")
              , (vocalPV, dir </> "vocal.wav")
              , (crowdPV, dir </> "crowd.wav")
              , (songPV, dir </> "song-countin.wav")
              ]

        dir </> "everything.wav" %> \out -> case plan of
          MoggPlan{..} -> do
            let ogg = dir </> "audio.ogg"
            need [ogg]
            src <- liftIO $ sourceSnd ogg
            runAudio (applyPansVols (map realToFrac _pans) (map realToFrac _vols) src) out
          _ -> do
            need $ map snd allAudioWithPV
            srcs <- fmap concat $ forM allAudioWithPV $ \(pv, wav) -> case pv of
              [] -> return []
              _  -> do
                src <- liftIO $ sourceSnd wav
                return [applyPansVols (map (realToFrac . fst) pv) (map (realToFrac . snd) pv) src]
            let mixed = case srcs of
                  []     -> silent (Frames 0) 44100 2
                  s : ss -> foldr mix s ss
            runAudio mixed out
        dir </> "everything.ogg" %> buildAudio (Input $ dir </> "everything.wav")

        dir </> "everything-mono.wav" %> \out -> case plan of
          MoggPlan{..} -> do
            let ogg = dir </> "audio.ogg"
            need [ogg]
            src <- liftIO $ sourceSnd ogg
            runAudio (applyVolsMono (map realToFrac _vols) src) out
          _ -> do
            need $ map snd allAudioWithPV
            srcs <- fmap concat $ forM allAudioWithPV $ \(pv, wav) -> case pv of
              [] -> return []
              _  -> do
                src <- liftIO $ sourceSnd wav
                return [applyVolsMono (map (realToFrac . snd) pv) src]
            let mixed = case srcs of
                  []     -> silent (Frames 0) 44100 1
                  s : ss -> foldr mix s ss
            runAudio mixed out

        -- MIDI files

        let midprocessed = dir </> "processed.mid"
            midraw = dir </> "raw.mid"
            display = dir </> "display.json"
        midraw %> \out -> do
          putNormal "Loading the MIDI file..."
          input <- loadMIDI "gen/notes.mid"
          let extraTempo  = "tempo-" ++ T.unpack planName ++ ".mid"
          tempos <- fmap RBFile.s_tempos $ doesFileExist extraTempo >>= \b -> if b
            then loadMIDI extraTempo
            else return input
          saveMIDI out input { RBFile.s_tempos = tempos }
        midprocessed %> \out -> do
          input <- loadMIDI midraw
          output <- RB3.processMIDI songYaml input RB3.Kicks2x mixMode $ getAudioLength planName
          saveMIDI out output

        display %> \out -> do
          song <- loadMIDI midprocessed
          liftIO $ BL.writeFile out $ makeDisplay songYaml song

        -- Guitar rules
        dir </> "protar-hear.mid" %> \out -> do
          input <- loadMIDI midprocessed
          let goffs = case _proGuitarTuning $ _options songYaml of
                []   -> [0, 0, 0, 0, 0, 0]
                offs -> offs
              boffs = case _proBassTuning $ _options songYaml of
                []   -> [0, 0, 0, 0]
                offs -> offs
          saveMIDI out $ RBFile.playGuitarFile goffs boffs input
        dir </> "protar-mpa.mid" %> \out -> do
          input <- loadMIDI midprocessed
          let gtr17   = foldr RTB.merge RTB.empty [ t | RBFile.PartRealGuitar   t <- RBFile.s_tracks input ]
              gtr22   = foldr RTB.merge RTB.empty [ t | RBFile.PartRealGuitar22 t <- RBFile.s_tracks input ]
              bass17  = foldr RTB.merge RTB.empty [ t | RBFile.PartRealBass     t <- RBFile.s_tracks input ]
              bass22  = foldr RTB.merge RTB.empty [ t | RBFile.PartRealBass22   t <- RBFile.s_tracks input ]
              playTrack cont name t = let
                expert = flip RTB.mapMaybe t $ \case
                  ProGtr.DiffEvent Expert devt -> Just devt
                  _                            -> Nothing
                thres = fromIntegral (_hopoThreshold $ _options songYaml) / 480
                auto = PGPlay.autoplay thres expert
                msgToSysEx msg
                  = E.SystemExclusive $ SysEx.Regular $ PGPlay.sendCommand (cont, msg) ++ [0xF7]
                in RBFile.RawTrack $ U.setTrackName name $ msgToSysEx <$> auto
          saveMIDI out input
            { RBFile.s_tracks =
                [ playTrack PGPlay.Mustang "GTR17"  $ if RTB.null gtr17  then gtr22  else gtr17
                , playTrack PGPlay.Squier  "GTR22"  $ if RTB.null gtr22  then gtr17  else gtr22
                , playTrack PGPlay.Mustang "BASS17" $ if RTB.null bass17 then bass22 else bass17
                , playTrack PGPlay.Squier  "BASS22" $ if RTB.null bass22 then bass17 else bass22
                ]
            }

        -- Countin audio, and song+countin files
        let useCountin (Countin hits) = do
              dir </> "countin.wav" %> \out -> case hits of
                [] -> buildAudio (Silence 1 $ Frames 0) out
                _  -> do
                  mid <- loadMIDI $ dir </> "raw.mid"
                  hits' <- forM hits $ \(posn, aud) -> do
                    let time = case posn of
                          Left  mb   -> Seconds $ realToFrac $ U.applyTempoMap (RBFile.s_tempos mid) $ U.unapplyMeasureMap (RBFile.s_signatures mid) mb
                          Right secs -> Seconds $ realToFrac secs
                    aud' <- fmap join $ mapM (manualLeaf songYaml) aud
                    return $ Pad Start time aud'
                  buildAudio (Mix hits') out
              dir </> "song-countin.wav" %> \out -> do
                let song = Input $ dir </> "song.wav"
                    countin = Input $ dir </> "countin.wav"
                buildAudio (Mix [song, countin]) out
        case plan of
          MoggPlan{}   -> return () -- handled by makeAudioFiles
          Plan{..}     -> useCountin _countin
          EachPlan{..} -> useCountin _countin
        dir </> "song-countin.ogg" %> \out ->
          buildAudio (Input $ out -<.> "wav") out

        -- Rock Band OGG and MOGG
        let ogg  = dir </> "audio.ogg"
            mogg = dir </> "audio.mogg"
        ogg %> \out -> case plan of
          MoggPlan{} -> do
            need [mogg]
            liftIO $ moggToOgg mogg out
          _ -> let
            hasCrowd = case plan of
              Plan{..} -> isJust _crowd
              _        -> False
            parts = map Input $ concat
              [ [dir </> "kick.wav"   | _hasDrums     (_instruments songYaml) && mixMode /= RBDrums.D0]
              , [dir </> "snare.wav"  | _hasDrums     (_instruments songYaml) && elem mixMode [RBDrums.D1, RBDrums.D2, RBDrums.D3]]
              , [dir </> "drums.wav"  | _hasDrums    $ _instruments songYaml]
              , [dir </> "bass.wav"   | hasAnyBass   $ _instruments songYaml]
              , [dir </> "guitar.wav" | hasAnyGuitar $ _instruments songYaml]
              , [dir </> "keys.wav"   | hasAnyKeys   $ _instruments songYaml]
              , [dir </> "vocal.wav"  | hasAnyVocal  $ _instruments songYaml]
              , [dir </> "crowd.wav"  | hasCrowd                            ]
              , [dir </> "song-countin.wav"]
              ]
            in buildAudio (Merge parts) out
        mogg %> \out -> case plan of
          MoggPlan{..} -> moggOracle (MoggSearch _moggMD5) >>= \case
            Nothing -> fail "Couldn't find the MOGG file"
            Just f -> do
              putNormal $ "Found the MOGG file: " ++ f
              copyFile' f out
          _ -> do
            need [ogg]
            putNormal "# Wrapping OGG into unencrypted MOGG with Magma hack"
            liftIO $ Magma.oggToMogg ogg out

        -- Low-quality audio files for the online preview app
        forM_ [("mp3", crapMP3), ("ogg", crapVorbis)] $ \(ext, crap) -> do
          dir </> "web/preview-audio" <.> ext %> \out -> do
            need [dir </> "everything-mono.wav"]
            src <- liftIO $ sourceSnd $ dir </> "everything-mono.wav"
            putNormal $ "Writing a crappy audio file to " ++ out
            liftIO $ runResourceT $ crap out src
            putNormal $ "Finished writing a crappy audio file to " ++ out

        -- Warn about notes that might hang off before a pro keys range shift
        phony (dir </> "hanging") $ do
          song <- loadMIDI midprocessed
          putNormal $ closeShiftsFile song

        -- Print out a summary of (non-vocal) overdrive and unison phrases
        phony (dir </> "overdrive") $ printOverdrive midprocessed

        -- Melody's Escape customs
        let melodyAudio = dir </> "melody/audio.ogg"
            melodyChart = dir </> "melody/song.track"
        melodyAudio %> copyFile' (dir </> "everything.ogg")
        melodyChart %> \out -> do
          need [midraw, melodyAudio]
          mid <- loadMIDI midraw
          melody <- liftIO $ MelodysEscape.randomNotes $ U.applyTempoTrack (RBFile.s_tempos mid)
            $ foldr RTB.merge RTB.empty [ trk | RBFile.MelodysEscape trk <- RBFile.s_tracks mid ]
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
