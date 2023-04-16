{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.GuitarHero2 where

import           Control.Arrow                    (second)
import           Control.Concurrent.Async         (forConcurrently)
import           Control.Monad                    (forM, guard, void, when)
import           Control.Monad.Extra              (concatMapM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource     (MonadResource)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.Int
import           Data.List.Extra                  (firstJust, (\\))
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeLatin1)
import           Onyx.Audio                       (Audio (..))
import           Onyx.Audio.VGS                   (splitOutVGSChannels,
                                                   vgsChannelCount)
import           Onyx.Harmonix.Ark
import           Onyx.Harmonix.Ark.GH2            (GH2DXExtra (..),
                                                   readSongListGH2,
                                                   readSongListGH2Extra)
import qualified Onyx.Harmonix.DTA                as D
import           Onyx.Harmonix.DTA.Crypt          (decrypt, oldCrypt)
import qualified Onyx.Harmonix.DTA.Serialize      as D
import           Onyx.Harmonix.DTA.Serialize.GH2
import           Onyx.Harmonix.GH2.Events
import           Onyx.Harmonix.GH2.File
import           Onyx.Harmonix.GH2.PartDrum
import           Onyx.Harmonix.GH2.PartGuitar
import           Onyx.Harmonix.GH2.Triggers
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..),
                                                   pattern RNil, pattern Wait)
import qualified Onyx.MIDI.Track.Drums            as Drums
import qualified Onyx.MIDI.Track.Events           as RB
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as RB
import           Onyx.Project
import           Onyx.Sections                    (fromGH2Section)
import           Onyx.StackTrace
import           Onyx.Util.Handle
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  ((<.>))
import           Text.Read                        (readMaybe)

getSongList :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> StackTraceT m [(T.Text, SongPackage)]
getSongList gen = do
  (hdr, arks) <- stackIO $ loadGEN gen
  dtb <- case filter (\fe -> fe.folder == Just "config/gen" && fe.name == "songs.dtb") hdr.files of
    entry : _ -> do
      r <- readFileEntry hdr arks entry
      stackIO $ useHandle r handleToByteString
    []        -> fatal "Couldn't find songs.dtb"
  readSongListGH2 $ D.decodeDTB $ decrypt oldCrypt dtb

getImports :: [(T.Text, SongPackage)] -> [(T.Text, (ImportMode, SongPackage))]
getImports = concatMap $ \(t, pkg) -> case songCoop pkg of
  Nothing -> [(t, (ImportSolo, pkg))]
  Just _  -> [(t, (ImportSolo, pkg)), (t, (ImportCoop, pkg))]

data ImportMode = ImportSolo | ImportCoop
  deriving (Eq)

importGH2 :: (SendMessage m, MonadResource m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH2 genPath gen = map (\(_, (mode, pkg)) -> importGH2Song mode pkg genPath gen) . getImports <$> getSongList gen

importGH2MIDI :: ImportMode -> Song -> F.Song (GH2File U.Beats) -> F.Song (F.OnyxFile U.Beats)
importGH2MIDI mode songChunk (F.Song tmap mmap gh2) = F.Song tmap mmap $ let
  convertPart :: PartTrack U.Beats -> RB.FiveTrack U.Beats
  convertPart part = RB.FiveTrack
    { RB.fiveDifficulties = flip fmap (partDifficulties part) $ \diff -> mempty
      { RB.fiveGems       = partGems       diff
      -- stuff that we may have written for future gh2dx support
      , RB.fiveForceHOPO  = partForceHOPO  diff
      , RB.fiveForceStrum = partForceStrum diff
      , RB.fiveTap        = partForceTap   diff
      }
    , RB.fiveMood         = RTB.empty -- TODO
    , RB.fiveHandMap      = flip fmap (partHandMap part) $ \case
      HandMap_Default   -> RB.HandMap_Default
      HandMap_DropD2    -> RB.HandMap_DropD2
      HandMap_Solo      -> RB.HandMap_Solo
      HandMap_NoChords  -> RB.HandMap_NoChords
      HandMap_AllChords -> RB.HandMap_AllChords
    , RB.fiveStrumMap     = flip fmap (partStrumMap part) $ \case
      StrumMap_SlapBass -> RB.StrumMap_SlapBass
    , RB.fiveFretPosition = partFretPosition part
    , RB.fiveTremolo      = RTB.empty
    , RB.fiveTrill        = RTB.empty
    , RB.fiveOverdrive    = maybe RTB.empty partStarPower $ Map.lookup Expert $ partDifficulties part
    , RB.fiveBRE          = RTB.empty
    , RB.fiveSolo         = importSolo $ partSoloEdge part
    , RB.fivePlayer1      = maybe RTB.empty partPlayer1 $ Map.lookup Expert $ partDifficulties part
    , RB.fivePlayer2      = maybe RTB.empty partPlayer2 $ Map.lookup Expert $ partDifficulties part
    }
  -- import from gh2dx "on/off" solo format
  importSolo :: RTB.T t () -> RTB.T t Bool
  importSolo = go True where
    go True (Wait _ _  RNil) = RNil -- don't start a solo that doesn't end
    go b    (Wait t () rest) = Wait t b $ go (not b) rest
    go _    RNil             = RNil
  in F.fixedToOnyx mempty
    { F.fixedEvents = RB.EventsTrack
      { RB.eventsMusicStart = void $ RTB.filter (== MusicStart) $ eventsOther $ gh2Events gh2
      , RB.eventsMusicEnd   = RTB.empty
      , RB.eventsEnd        = void $ RTB.filter (== End) $ eventsOther $ gh2Events gh2
      , RB.eventsCoda       = RTB.empty
      , RB.eventsCodaResume = RTB.empty
      , RB.eventsCrowd      = RTB.empty -- TODO
      , RB.eventsCrowdClap  = RTB.empty -- TODO
      , RB.eventsSections   = (\s -> (RB.SectionRB2, fromGH2Section s)) <$> eventsSections (gh2Events gh2)
      , RB.eventsBacking    = triggersBacking $ gh2Triggers gh2
      }
    , F.fixedPartGuitar = convertPart $ case mode of
      ImportSolo -> gh2PartGuitar gh2
      ImportCoop -> if nullPart $ gh2PartGuitarCoop gh2
        then gh2PartGuitar gh2
        else gh2PartGuitarCoop gh2
    , F.fixedPartBass = if isJust $ lookup "bass" $ D.fromDictList $ tracks songChunk
      then convertPart $ gh2PartBass gh2
      else mempty
    , F.fixedPartRhythm = if isJust $ lookup "rhythm" $ D.fromDictList $ tracks songChunk
      then convertPart $ gh2PartRhythm gh2
      else mempty
    , F.fixedPartDrums = if isJust $ lookup "drum" $ D.fromDictList $ tracks songChunk
      then let
        drums = gh2PartDrum gh2
        expert = fromMaybe mempty $ Map.lookup Expert $ gh2drumDifficulties drums
        in Drums.DrumTrack
          { drumDifficulties = flip fmap (gh2drumDifficulties drums) $ \diff -> Drums.DrumDifficulty
            { Drums.drumMix = RTB.empty
            , Drums.drumPSModifiers = RTB.empty
            , Drums.drumGems = (, Drums.VelocityNormal) <$> gh2drumGems diff
            }
          , drumMood           = RTB.empty -- TODO import from band drummer
          , drumToms           = RTB.empty
          , drumSingleRoll     = RTB.empty
          , drumDoubleRoll     = RTB.empty
          , drumOverdrive      = gh2drumStarPower expert
          , drumActivation     = RTB.empty
          , drumSolo           = importSolo $ gh2drumSoloEdge drums
          , drumPlayer1        = gh2drumPlayer1 expert
          , drumPlayer2        = gh2drumPlayer2 expert
          , drumKick2x         = RTB.empty
          , drumAnimation      = RTB.empty -- TODO import from band drummer
          , drumEnableDynamics = RTB.empty
          }
      else mempty
    }

gh2SongYaml :: ImportMode -> SongPackage -> Maybe GH2DXExtra -> Song -> F.Song (F.OnyxFile U.Beats) -> SongYaml SoftFile
gh2SongYaml mode pkg extra songChunk onyxMidi = SongYaml
  { metadata = def'
    { title  = Just $ name pkg <> case mode of
      ImportSolo -> ""
      ImportCoop -> " (Co-op)"
    , artist = Just $ artist pkg
    , cover = caption pkg /= Just "performed_by"
    , fileAlbumArt = Nothing
    , previewStart = Just $ PreviewSeconds $ fromIntegral (fst $ preview pkg) / 1000
    , previewEnd = Just $ PreviewSeconds $ fromIntegral (snd $ preview pkg) / 1000
    , album = extra >>= (.songalbum)
    , author = extra >>= (.author)
    , year = extra >>= (.songyear) >>= readMaybe . T.unpack
    , genre = extra >>= (.songgenre)
    }
  , global = def'
    { fileMidi            = SoftFile "notes.mid" $ SoftChart onyxMidi
    , fileSongAnim        = Nothing
    , backgroundVideo     = Nothing
    , fileBackgroundImage = Nothing
    }
  , audio = HM.empty
  , jammit = HM.empty
  , plans = HM.empty
  , targets = HM.empty -- TODO add gh2 target
  , parts = Parts $ HM.fromList $ catMaybes
    [ do
      guard $ maybe False (not . null) $ lookup "guitar" $ D.fromDictList $ tracks songChunk
      return (F.FlexGuitar, emptyPart
        { grybo = Just (def :: PartGRYBO)
          { hopoThreshold = maybe 170 fromIntegral $ hopoThreshold songChunk
          , difficulty = Tier $ min 1 $ maybe 1 fromIntegral $ extra >>= (.songguitarrank)
          }
        })
    , do
      guard $ maybe False (not . null) $ lookup "bass" $ D.fromDictList $ tracks songChunk
      return (F.FlexBass, emptyPart
        { grybo = Just (def :: PartGRYBO)
          { hopoThreshold = maybe 170 fromIntegral $ hopoThreshold songChunk
          , difficulty = Tier $ min 1 $ maybe 1 fromIntegral $ extra >>= (.songbassrank)
          }
        })
    , do
      guard $ maybe False (not . null) $ lookup "rhythm" $ D.fromDictList $ tracks songChunk
      return (F.FlexExtra "rhythm", emptyPart
        { grybo = Just (def :: PartGRYBO)
          { hopoThreshold = maybe 170 fromIntegral $ hopoThreshold songChunk
          , difficulty = Tier $ min 1 $ maybe 1 fromIntegral $ extra >>= (.songrhythmrank)
          }
        })
    , do
      guard $ maybe False (not . null) $ lookup "drum" $ D.fromDictList $ tracks songChunk
      return (F.FlexDrums, emptyPart
        { drums = Just (emptyPartDrums Drums4 Kicks1x :: PartDrums SoftFile)
          { difficulty = Tier $ min 1 $ maybe 1 fromIntegral $ extra >>= (.songdrumrank)
          -- TODO should probably save kicks 1x/2x so we can reimport here
          }
        })
    ]
  }

importGH2Song :: (SendMessage m, MonadResource m) => ImportMode -> SongPackage -> FilePath -> Folder T.Text Readable -> Import m
importGH2Song mode pkg genPath gen level = do
  (hdr, arks) <- stackIO $ loadGEN gen
  folder <- loadArkFolder hdr arks
  let encLatin1 = B8.pack . T.unpack
      split s = case splitPath s of
        Nothing -> fatal $ "Internal error, couldn't parse path: " <> show s
        Just p  -> return p
      need p = case findFile p folder of
        Just r  -> return r
        Nothing -> fatal $ "Required file not found: " <> B8.unpack (B8.intercalate "/" $ toList p)
  songChunk <- case mode of
    ImportSolo -> return $ song pkg
    ImportCoop -> case songCoop pkg of
      Nothing -> fatal "Tried to import coop version from a song that doesn't have one"
      Just coop -> return coop
  when (level == ImportFull) $ do
    lg $ "Importing GH2 song [" <> T.unpack (songName songChunk) <> "] from: " <> genPath
  midi <- split (midiFile songChunk) >>= need . fmap encLatin1
  vgs <- split (songName songChunk <> ".vgs") >>= need . fmap encLatin1
  onyxMidi <- case level of
    ImportFull  -> importGH2MIDI mode songChunk <$> F.loadMIDIReadable midi
    ImportQuick -> return emptyChart
  numChannels <- stackIO $ vgsChannelCount vgs
  namedChans <- stackIO $ forConcurrently [0 .. numChannels - 1] $ \i -> do
    bs <- case level of
      ImportFull  -> splitOutVGSChannels [i] vgs
      ImportQuick -> return BL.empty
    return ("vgs-" <> show i, bs)
  return (gh2SongYaml mode pkg Nothing songChunk onyxMidi)
    { audio = HM.fromList $ do
      (name, bs) <- namedChans
      return $ (T.pack name ,) $ AudioFile AudioInfo
        { md5 = Nothing
        , frames = Nothing
        , commands = []
        , filePath = Just $ SoftFile (name <.> "vgs") $ SoftReadable $ makeHandle name $ byteStringSimpleHandle bs
        , rate = Nothing
        , channels = 1
        }
    , plans = HM.singleton "vgs" $ let
      guitarChans = map fromIntegral $ fromMaybe [] $ lookup "guitar" $ D.fromDictList $ tracks songChunk
      bassChans = map fromIntegral $ fromMaybe [] $ lookup "bass" $ D.fromDictList $ tracks songChunk
      rhythmChans = map fromIntegral $ fromMaybe [] $ lookup "rhythm" $ D.fromDictList $ tracks songChunk
      songChans = zipWith const [0..] (pans songChunk)
        \\ (guitarChans ++ bassChans ++ rhythmChans)
      mixChans v cs = do
        cs' <- NE.nonEmpty cs
        Just $ case cs' of
          c :| [] -> PlanAudio
            { expr = Input $ Named $ T.pack $ fst $ namedChans !! c
            , pans = map realToFrac [pans songChunk !! c]
            , vols = map realToFrac [(vols songChunk !! c) + v]
            }
          _ -> PlanAudio
            { expr = Merge $ fmap (Input . Named . T.pack . fst . (namedChans !!)) cs'
            , pans = map realToFrac [ pans songChunk !! c | c <- cs ]
            , vols = map realToFrac [ (vols songChunk !! c) + v | c <- cs ]
            }
      in StandardPlan StandardPlanInfo
        { song = mixChans 0 songChans
        , parts = Parts $ HM.fromList $ catMaybes
          -- I made up these volume adjustments but they seem to work
          [ (F.FlexGuitar ,) . PartSingle <$> mixChans (-0.5) guitarChans
          , (F.FlexBass ,) . PartSingle <$> mixChans (-3) bassChans
          , (F.FlexExtra "rhythm" ,) . PartSingle <$> mixChans (-1.5) rhythmChans
          ]
        , crowd = Nothing
        , comments = []
        , tuningCents = 0
        , fileTempo = Nothing
        }
    }

importGH2DLC :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH2DLC src folder = do
  packSongs <- stackIO (findByteString ("config" :| ["songs.dta"]) folder) >>= \case
    Nothing -> fatal "config/songs.dta not found"
    Just bs -> do
      dta <- D.readDTA_latin1 $ BL.toStrict bs
      readSongListGH2Extra dta
  flip concatMapM packSongs $ \(_top, pkg, extra) -> do
    let base = T.unpack $ songName $ song pkg
        split s = case splitPath $ T.pack s of
          Nothing -> fatal $ "Internal error, couldn't parse path: " <> show s
          Just p  -> return p
        need p = case findFileCI p folder of
          Just r  -> return r
          Nothing -> fatal $ "Required file not found: " <> T.unpack (T.intercalate "/" $ toList p)
    moggPath <- split $ base <.> "mogg"
    mogg <- SoftReadable <$> need moggPath
    let modes = case songCoop pkg of
          Nothing -> [ImportSolo]
          Just _  -> [ImportSolo, ImportCoop]
    forM modes $ \mode -> do
      return $ \level -> do
        songChunk <- case mode of
          ImportSolo -> return $ song pkg
          ImportCoop -> case songCoop pkg of
            Nothing -> fatal "Tried to import coop version from a song that doesn't have one"
            Just coop -> return coop
        when (level == ImportFull) $ do
          lg $ "Importing GH2 DLC song [" <> T.unpack (songName songChunk) <> "] from: " <> src
        midi <- split (T.unpack $ midiFile songChunk) >>= need
        onyxMidi <- case level of
          ImportFull  -> importGH2MIDI mode songChunk <$> F.loadMIDIReadable midi
          ImportQuick -> return emptyChart
        let instChans :: [(T.Text, [Int])]
            instChans = map (second $ map fromIntegral) $ D.fromDictList $ tracks songChunk
        return (gh2SongYaml mode pkg (Just extra) songChunk onyxMidi)
          { plans = HM.singleton "mogg" $ MoggPlan MoggPlanInfo
            { fileMOGG = Just $ SoftFile "audio.mogg" mogg
            , moggMD5 = Nothing
            , parts = Parts $ HM.fromList $ concat
              [ [ (F.FlexGuitar        , PartSingle ns) | ns <- toList $ lookup "guitar" instChans ]
              , [ (F.FlexBass          , PartSingle ns) | ns <- toList $ lookup "bass"   instChans ]
              , [ (F.FlexExtra "rhythm", PartSingle ns) | ns <- toList $ lookup "rhythm" instChans ]
              ]
            , crowd = []
            , pans = map realToFrac $ pans songChunk
            , vols = do
              let i `channelFor` part = maybe False (elem i) $ lookup part instChans
              (i, vol) <- zip [0..] $ vols songChunk
              -- again, made up these volume adjustments so things sound ok in RB context
              return $ realToFrac $ if
                | i `channelFor` "guitar" -> vol - 0.5
                | i `channelFor` "bass"   -> vol - 3
                | i `channelFor` "rhythm" -> vol - 1.5
                | otherwise               -> vol
            , comments = []
            , tuningCents = 0
            , fileTempo = Nothing
            , karaoke = False
            , multitrack = False
            , decryptSilent = False
            }
          }

data Setlist a = Setlist
  { campaign :: [(B.ByteString, [a])]
  , bonus    :: [(a, Int32)]
  } deriving (Show, Functor, Foldable, Traversable)

loadSetlist :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m (Setlist B.ByteString)
loadSetlist gen = do
  (hdr, arks) <- stackIO $ crawlFolder gen >>= loadGEN
  let loadDTB name = case filter (\fe -> fe.folder == Just "config/gen" && fe.name == name) hdr.files of
        entry : _ -> do
          r <- readFileEntry hdr arks entry
          dtb <- stackIO $ useHandle r handleToByteString
          return $ D.decodeDTB (decrypt oldCrypt dtb)
        []        -> fatal $ "Couldn't find " <> show name
  dtbCampaign <- loadDTB "campaign.dtb"
  dtbBonus    <- loadDTB "store.dtb"
  let isKeyMatch k = \case
        D.Parens (D.Tree _ (D.Sym k' : rest)) | k == k' -> Just rest
        _                                               -> Nothing
      findKey k = firstJust (isKeyMatch k) . D.treeChunks
      parseTier (D.Parens (D.Tree _ (D.Sym x : xs))) = return (x, [song | D.Sym song <- xs])
      parseTier _                              = fatal "Couldn't extract info from a tier in campaign.dtb"
      parseBonusSong (D.Parens (D.Tree _ [D.Sym x, isKeyMatch "price" -> Just [D.Int n]])) = return (x, n)
      parseBonusSong _ = fatal "Couldn't extract info from a bonus song in store.dtb"
  campaign <- case findKey "order" $ D.topTree dtbCampaign of
    Nothing    -> fatal "Couldn't find 'order' in campaign.dtb"
    Just order -> mapM parseTier order
  bonus <- case findKey "song" $ D.topTree dtbBonus of
    Nothing    -> fatal "Couldn't find bonus songs in store.dtb"
    Just songs -> mapM parseBonusSong songs
  return Setlist{..}

loadSetlistFull :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m (Setlist (B.ByteString, SongPackage))
loadSetlistFull gen = do
  setlist <- loadSetlist gen
  dir <- stackIO $ crawlFolder gen
  songs <- HM.fromList <$> getSongList dir
  forM setlist $ \k -> case HM.lookup (decodeLatin1 k) songs of
    Just pkg -> return (k, pkg)
    Nothing  -> fatal $ "Couldn't locate setlist song " <> show k

{-
Note for whenever we import .voc files.
Arterial Black has:

 (song
  (name songs/arterialblack/arterialblack_sp)
  ...
  (midi_file songs/arterialblack/arterialblack.mid))

And the lipsync file is named `arterialblack.voc`. So maybe the `midi_file` is
the template to find it, instead of `name`? Or maybe it just doesn't load and
nobody noticed...
-}
