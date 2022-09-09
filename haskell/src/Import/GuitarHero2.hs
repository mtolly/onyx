{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module Import.GuitarHero2 where

import           Audio                            (Audio (..))
import           Config
import           Control.Arrow                    (second)
import           Control.Concurrent.Async         (forConcurrently)
import           Control.Monad                    (forM, guard, void, when)
import           Control.Monad.Extra              (concatMapM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Default.Class               (def)
import qualified Data.DTA                         as D
import           Data.DTA.Crypt                   (decrypt, oldCrypt)
import qualified Data.DTA.Serialize               as D
import           Data.DTA.Serialize.GH2
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.Int
import           Data.List.Extra                  (firstJust, (\\))
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust)
import           Data.SimpleHandle
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeLatin1)
import           GuitarHeroII.Ark                 (readSongListGH2)
import           GuitarHeroII.Audio               (splitOutVGSChannels,
                                                   vgsChannelCount)
import           GuitarHeroII.Events
import           GuitarHeroII.File
import           GuitarHeroII.PartGuitar
import           GuitarHeroII.Triggers
import           Harmonix.Ark
import           Import.Base
import qualified RockBand.Codec.Events            as RB
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as RB
import           RockBand.Common                  (Difficulty (..))
import           RockBand.Sections                (fromGH2Section)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  ((<.>))

getSongList :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> StackTraceT m [(T.Text, SongPackage)]
getSongList gen = do
  (hdr, arks) <- stackIO $ loadGEN gen
  dtb <- case filter (\fe -> fe_folder fe == Just "config/gen" && fe_name fe == "songs.dtb") $ hdr_Files hdr of
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

importGH2MIDI :: ImportMode -> Song -> RBFile.Song (GH2File U.Beats) -> RBFile.Song (RBFile.OnyxFile U.Beats)
importGH2MIDI mode songChunk (RBFile.Song tmap mmap gh2) = RBFile.Song tmap mmap $ let
  convertPart :: PartTrack U.Beats -> RB.FiveTrack U.Beats
  convertPart part = RB.FiveTrack
    { RB.fiveDifficulties = flip fmap (partDifficulties part) $ \diff -> mempty
      { RB.fiveGems = partGems diff
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
    , RB.fiveSolo         = RTB.empty
    , RB.fivePlayer1      = maybe RTB.empty partPlayer1 $ Map.lookup Expert $ partDifficulties part
    , RB.fivePlayer2      = maybe RTB.empty partPlayer2 $ Map.lookup Expert $ partDifficulties part
    }
  in RBFile.fixedToOnyx mempty
    { RBFile.fixedEvents = RB.EventsTrack
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
    , RBFile.fixedPartGuitar = convertPart $ case mode of
      ImportSolo -> gh2PartGuitar gh2
      ImportCoop -> if nullPart $ gh2PartGuitarCoop gh2
        then gh2PartGuitar gh2
        else gh2PartGuitarCoop gh2
    , RBFile.fixedPartBass = if isJust $ lookup "bass" $ D.fromDictList $ tracks songChunk
      then convertPart $ gh2PartBass gh2
      else mempty
    , RBFile.fixedPartRhythm = if isJust $ lookup "rhythm" $ D.fromDictList $ tracks songChunk
      then convertPart $ gh2PartRhythm gh2
      else mempty
    }

gh2SongYaml :: ImportMode -> SongPackage -> Song -> RBFile.Song (RBFile.OnyxFile U.Beats) -> SongYaml SoftFile
gh2SongYaml mode pkg songChunk onyxMidi = SongYaml
  { _metadata = def'
    { _title  = Just $ name pkg <> case mode of
      ImportSolo -> ""
      ImportCoop -> " (Co-op)"
    , _artist = Just $ artist pkg
    , _cover = caption pkg /= Just "performed_by"
    , _fileAlbumArt = Nothing
    , _previewStart = Just $ PreviewSeconds $ fromIntegral (fst $ preview pkg) / 1000
    , _previewEnd = Just $ PreviewSeconds $ fromIntegral (snd $ preview pkg) / 1000
    }
  , _global = def'
    { _fileMidi            = SoftFile "notes.mid" $ SoftChart onyxMidi
    , _fileSongAnim        = Nothing
    , _backgroundVideo     = Nothing
    , _fileBackgroundImage = Nothing
    }
  , _audio = HM.empty
  , _jammit = HM.empty
  , _plans = HM.empty
  , _targets = HM.empty -- TODO add gh2 target
  , _parts = Parts $ HM.fromList $ catMaybes
    [ do
      guard $ maybe False (not . null) $ lookup "guitar" $ D.fromDictList $ tracks songChunk
      return $ (RBFile.FlexGuitar ,) def
        { partGRYBO = Just def { gryboHopoThreshold = maybe 170 fromIntegral $ hopoThreshold songChunk }
        }
    , do
      guard $ maybe False (not . null) $ lookup "bass" $ D.fromDictList $ tracks songChunk
      return $ (RBFile.FlexBass ,) def
        { partGRYBO = Just def { gryboHopoThreshold = maybe 170 fromIntegral $ hopoThreshold songChunk }
        }
    , do
      guard $ maybe False (not . null) $ lookup "rhythm" $ D.fromDictList $ tracks songChunk
      return $ (RBFile.FlexExtra "rhythm" ,) def
        { partGRYBO = Just def { gryboHopoThreshold = maybe 170 fromIntegral $ hopoThreshold songChunk }
        }
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
    ImportFull  -> importGH2MIDI mode songChunk <$> RBFile.loadMIDIReadable midi
    ImportQuick -> return emptyChart
  numChannels <- stackIO $ vgsChannelCount vgs
  namedChans <- stackIO $ forConcurrently [0 .. numChannels - 1] $ \i -> do
    bs <- case level of
      ImportFull  -> splitOutVGSChannels [i] vgs
      ImportQuick -> return BL.empty
    return ("vgs-" <> show i, bs)
  return (gh2SongYaml mode pkg songChunk onyxMidi)
    { _audio = HM.fromList $ do
      (name, bs) <- namedChans
      return $ (T.pack name ,) $ AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just $ SoftFile (name <.> "vgs") $ SoftReadable $ makeHandle name $ byteStringSimpleHandle bs
        , _rate = Nothing
        , _channels = 1
        }
    , _plans = HM.singleton "vgs" $ let
      guitarChans = map fromIntegral $ fromMaybe [] $ lookup "guitar" $ D.fromDictList $ tracks songChunk
      bassChans = map fromIntegral $ fromMaybe [] $ lookup "bass" $ D.fromDictList $ tracks songChunk
      rhythmChans = map fromIntegral $ fromMaybe [] $ lookup "rhythm" $ D.fromDictList $ tracks songChunk
      songChans = zipWith const [0..] (pans songChunk)
        \\ (guitarChans ++ bassChans ++ rhythmChans)
      mixChans v cs = do
        cs' <- NE.nonEmpty cs
        Just $ case cs' of
          c :| [] -> PlanAudio
            { _planExpr = Input $ Named $ T.pack $ fst $ namedChans !! c
            , _planPans = map realToFrac [pans songChunk !! c]
            , _planVols = map realToFrac [(vols songChunk !! c) + v]
            }
          _ -> PlanAudio
            { _planExpr = Merge $ fmap (Input . Named . T.pack . fst . (namedChans !!)) cs'
            , _planPans = map realToFrac [ pans songChunk !! c | c <- cs ]
            , _planVols = map realToFrac [ (vols songChunk !! c) + v | c <- cs ]
            }
      in Plan
        { _song = mixChans 0 songChans
        , _countin = Countin []
        , _planParts = Parts $ HM.fromList $ catMaybes
          -- I made up these volume adjustments but they seem to work
          [ (RBFile.FlexGuitar ,) . PartSingle <$> mixChans (-0.5) guitarChans
          , (RBFile.FlexBass ,) . PartSingle <$> mixChans (-3) bassChans
          , (RBFile.FlexExtra "rhythm" ,) . PartSingle <$> mixChans (-1.5) rhythmChans
          ]
        , _crowd = Nothing
        , _planComments = []
        , _tuningCents = 0
        , _fileTempo = Nothing
        }
    }

importGH2DLC :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH2DLC src folder = do
  packSongs <- stackIO (findByteString ("config" :| ["songs.dta"]) folder) >>= \case
    Nothing -> fatal "config/songs.dta not found"
    Just bs -> do
      dta <- D.readDTA_latin1 $ BL.toStrict bs
      D.fromDictList <$> D.unserialize D.stackChunks dta
  let _ = packSongs :: [(T.Text, SongPackage)]
  flip concatMapM packSongs $ \(_top, pkg) -> do
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
          ImportFull  -> importGH2MIDI mode songChunk <$> RBFile.loadMIDIReadable midi
          ImportQuick -> return emptyChart
        let instChans :: [(T.Text, [Int])]
            instChans = map (second $ map fromIntegral) $ D.fromDictList $ tracks songChunk
        return (gh2SongYaml mode pkg songChunk onyxMidi)
          { _plans = HM.singleton "mogg" MoggPlan
            { _fileMOGG = Just $ SoftFile "audio.mogg" mogg
            , _moggMD5 = Nothing
            , _moggParts = Parts $ HM.fromList $ concat
              [ [ (RBFile.FlexGuitar        , PartSingle ns) | ns <- toList $ lookup "guitar" instChans ]
              , [ (RBFile.FlexBass          , PartSingle ns) | ns <- toList $ lookup "bass"   instChans ]
              , [ (RBFile.FlexExtra "rhythm", PartSingle ns) | ns <- toList $ lookup "rhythm" instChans ]
              ]
            , _moggCrowd = []
            , _pans = map realToFrac $ pans songChunk
            , _vols = do
              let i `channelFor` part = maybe False (elem i) $ lookup part instChans
              (i, vol) <- zip [0..] $ vols songChunk
              -- again, made up these volume adjustments so things sound ok in RB context
              return $ realToFrac $ if
                | i `channelFor` "guitar" -> vol - 0.5
                | i `channelFor` "bass"   -> vol - 3
                | i `channelFor` "rhythm" -> vol - 1.5
                | otherwise               -> vol
            , _planComments = []
            , _tuningCents = 0
            , _fileTempo = Nothing
            , _karaoke = False
            , _multitrack = False
            , _decryptSilent = False
            }
          }

data Setlist a = Setlist
  { set_campaign :: [(B.ByteString, [a])]
  , set_bonus    :: [(a, Int32)]
  } deriving (Show, Functor, Foldable, Traversable)

loadSetlist :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m (Setlist B.ByteString)
loadSetlist gen = do
  (hdr, arks) <- stackIO $ crawlFolder gen >>= loadGEN
  let loadDTB name = case filter (\fe -> fe_folder fe == Just "config/gen" && fe_name fe == name) $ hdr_Files hdr of
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
  set_campaign <- case findKey "order" $ D.topTree dtbCampaign of
    Nothing    -> fatal "Couldn't find 'order' in campaign.dtb"
    Just order -> mapM parseTier order
  set_bonus <- case findKey "song" $ D.topTree dtbBonus of
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
