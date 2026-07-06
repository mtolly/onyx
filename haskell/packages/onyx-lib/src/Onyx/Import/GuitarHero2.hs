{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
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
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
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
import           Onyx.Audio.VGS                   (VGS (..), VGSHeader (..),
                                                   decryptReadVGS,
                                                   splitOutVGSChannels)
import           Onyx.Harmonix.Ark
import           Onyx.Harmonix.Ark.GH2            (GH2DXExtra (..),
                                                   readSongListGH2,
                                                   readSongListGH2Extra)
import qualified Onyx.Harmonix.DTA                as D
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
import           Onyx.Sections                    (fromGH2Section,
                                                   simpleSection)
import           Onyx.StackTrace
import           Onyx.Util.Handle
import           Optics                           (set, (.~))
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
  D.decodeDecryptDTB dtb >>= readSongListGH2

data ImportMode = ImportSolo | ImportCoop
  deriving (Eq)

importGH2 :: (SendMessage m, MonadResource m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH2 displayPath gen = do
  (hdr, arks) <- stackIO $ loadGEN gen
  folder <- loadArkFolder hdr arks
  getSongList gen <&> concatMap \(_, pkg) ->
    importGH2Song ImportSolo pkg.song pkg displayPath folder
    : case pkg.songCoop of
      Nothing   -> []
      Just coop -> [importGH2Song ImportCoop coop pkg displayPath folder]

importGH2MIDI :: ImportMode -> Song -> F.Song (GH2File U.Beats) -> F.Song (F.OnyxFile U.Beats)
importGH2MIDI mode songChunk (F.Song tmap mmap gh2) = F.Song tmap mmap let
  convertPart :: PartTrack U.Beats -> RB.FiveTrack U.Beats
  convertPart part = RB.FiveTrack
    { RB.difficulties = part.partDifficulties <&> \diff -> mempty
      { RB.gems       = fmap (fmap Just) diff.partGems
      -- stuff that we may have written for future gh2dx support
      , RB.forceHOPO  = diff.partForceHOPO
      , RB.forceStrum = diff.partForceStrum
      , RB.tap        = diff.partForceTap
      }
    , RB.mood         = RTB.empty -- TODO
    , RB.handMap      = part.partHandMap <&> \case
      HandMap_Default   -> RB.HandMap_Default
      HandMap_DropD2    -> RB.HandMap_DropD2
      HandMap_Solo      -> RB.HandMap_Solo
      HandMap_NoChords  -> RB.HandMap_NoChords
      HandMap_AllChords -> RB.HandMap_AllChords
    , RB.strumMap     = part.partStrumMap <&> \case
      StrumMap_SlapBass -> RB.StrumMap_SlapBass
    , RB.fretPosition = part.partFretPosition
    , RB.tremolo      = RTB.empty
    , RB.trill        = RTB.empty
    , RB.overdrive    = maybe RTB.empty (.partStarPower) $ Map.lookup Expert part.partDifficulties
    , RB.bre          = RTB.empty
    , RB.solo         = importSolo part.partSoloEdge
    , RB.player1      = maybe RTB.empty (.partPlayer1) $ Map.lookup Expert part.partDifficulties
    , RB.player2      = maybe RTB.empty (.partPlayer2) $ Map.lookup Expert part.partDifficulties
    }
  -- import from gh2dx "on/off" solo format
  importSolo :: RTB.T t () -> RTB.T t Bool
  importSolo = go True where
    go True (Wait _ _  RNil) = RNil -- don't start a solo that doesn't end
    go b    (Wait t () rest) = Wait t b $ go (not b) rest
    go _    RNil             = RNil
  in F.fixedToOnyx mempty
    { F.fixedEvents = mempty
      { RB.musicStart = void $ RTB.filter (== MusicStart) gh2.gh2Events.eventsOther
      , RB.musicEnd   = RTB.empty
      , RB.end        = void $ RTB.filter (== End) gh2.gh2Events.eventsOther
      , RB.coda       = RTB.empty
      , RB.codaResume = RTB.empty
      , RB.crowd      = RTB.empty -- TODO
      , RB.crowdClap  = RTB.empty -- TODO
      , RB.sections   = (\s -> simpleSection $ fromGH2Section s) <$> gh2.gh2Events.eventsSections
      , RB.backing    = gh2.gh2Triggers.triggersBacking
      }
    , F.fixedPartGuitar = convertPart case mode of
      ImportSolo -> gh2.gh2PartGuitar
      ImportCoop -> if nullPart gh2.gh2PartGuitarCoop
        then gh2.gh2PartGuitar
        else gh2.gh2PartGuitarCoop
    , F.fixedPartBass = if isJust $ lookup "bass" songChunk.tracks.fromDictList
      then convertPart gh2.gh2PartBass
      else mempty
    , F.fixedPartRhythm = if isJust $ lookup "rhythm" songChunk.tracks.fromDictList
      then convertPart gh2.gh2PartRhythm
      else mempty
    , F.fixedPartDrums = if isJust $ lookup "drum" songChunk.tracks.fromDictList
      then let
        drums = gh2.gh2PartDrum
        expert = fromMaybe mempty $ Map.lookup Expert drums.gh2drumDifficulties
        in Drums.DrumTrack
          { difficulties = drums.gh2drumDifficulties <&> \diff -> Drums.DrumDifficulty
            { Drums.mix = RTB.empty
            , Drums.psModifiers = RTB.empty
            , Drums.gems = (, Drums.VelocityNormal) <$> diff.gh2drumGems
            }
          , mood           = RTB.empty -- TODO import from band drummer
          , toms           = RTB.empty
          , singleRoll     = RTB.empty
          , doubleRoll     = RTB.empty
          , overdrive      = expert.gh2drumStarPower
          , activation     = RTB.empty
          , solo           = importSolo drums.gh2drumSoloEdge
          , player1        = expert.gh2drumPlayer1
          , player2        = expert.gh2drumPlayer2
          , kick2x         = RTB.empty
          , animation      = RTB.empty -- TODO import from band drummer
          , enableDynamics = RTB.empty
          }
      else mempty
    }

gh2SongYaml :: ImportMode -> SongPackage -> Maybe GH2DXExtra -> Song -> Readable -> F.Song (F.OnyxFile U.Beats) -> SongYaml SoftFile
gh2SongYaml mode pkg extra songChunk origMidi onyxMidi = SongYaml
  { metadata = def'
    { title  = Just $ pkg.name <> case mode of
      ImportSolo -> ""
      ImportCoop -> " (Co-op)"
    , artist = Just pkg.artist
    , cover = pkg.caption /= Just "performed_by"
    , fileAlbumArt = Nothing
    , previewStart = Just $ PreviewSeconds $ fromIntegral (fst pkg.preview) / 1000
    , previewEnd = Just $ PreviewSeconds $ fromIntegral (snd pkg.preview) / 1000
    , album = extra >>= (.songalbum)
    , author = extra >>= (.author)
    , year = extra >>= (.songyear) >>= readMaybe . T.unpack
    , genre = extra >>= (.songgenre)
    -- TODO import loadingPhrase
    }
  , global = def'
    { fileMidi            = SoftFile "notes.mid" $ SoftChart onyxMidi
    , fileMidiOriginal    = Just $ SoftFile "original.mid" $ SoftReadable origMidi
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
      guard $ maybe False (not . null) $ lookup "guitar" songChunk.tracks.fromDictList
      return (F.PartGuitar, emptyPart
        { grybo = Just $ def
          & do set #hopoThreshold $ maybe 170 fromIntegral songChunk.hopoThreshold
          & do set #difficulty    $ Tier $ min 1 $ maybe 1 fromIntegral $ extra >>= (.songguitarrank)
        })
    , do
      guard $ maybe False (not . null) $ lookup "bass" songChunk.tracks.fromDictList
      return (F.PartBass, emptyPart
        { grybo = Just $ def
          & do set #hopoThreshold $ maybe 170 fromIntegral songChunk.hopoThreshold
          & do set #difficulty    $ Tier $ min 1 $ maybe 1 fromIntegral $ extra >>= (.songbassrank)
        })
    , do
      guard $ maybe False (not . null) $ lookup "rhythm" songChunk.tracks.fromDictList
      return (F.PartName "rhythm", emptyPart
        { grybo = Just $ def
          & do set #hopoThreshold $ maybe 170 fromIntegral songChunk.hopoThreshold
          & do set #difficulty    $ Tier $ min 1 $ maybe 1 fromIntegral $ extra >>= (.songrhythmrank)
        })
    , do
      guard $ maybe False (not . null) $ lookup "drum" songChunk.tracks.fromDictList
      -- TODO should probably save kicks 1x/2x so we can reimport here
      return (F.PartDrums, emptyPart & #drums .~ Just do
        emptyPartDrums Drums4 Kicks1x & #difficulty .~ do
          Tier $ min 1 $ maybe 1 fromIntegral $ extra >>= (.songdrumrank)
        )
    ]
  }

importGH2Song :: (SendMessage m, MonadResource m) => ImportMode -> Song -> SongPackage -> FilePath -> Folder B.ByteString Readable -> Import m
importGH2Song mode songChunk pkg displayPath folder level = do
  let encLatin1 = B8.pack . T.unpack
      split s = case splitPath s of
        Nothing -> fatal $ "Internal error, couldn't parse path: " <> show s
        Just p  -> return p
      need p = case findFile p folder of
        Just r  -> return r
        Nothing -> fatal $ "Required file not found: " <> B8.unpack (B8.intercalate "/" $ toList p)
  when (level == ImportFull) do
    lg $ "Importing GH2 song [" <> T.unpack songChunk.songName <> "] from: " <> displayPath
  midi <- split songChunk.midiFile >>= need . fmap encLatin1
  onyxMidi <- case level of
    ImportFull  -> importGH2MIDI mode songChunk <$> F.loadMIDIReadable midi
    ImportQuick -> return emptyChart
  (audio, plans) <- do
    pathVGS  <- fmap (fmap encLatin1) $ split $ songChunk.songName <> ".vgs"
    pathMogg <- fmap (fmap encLatin1) $ split $ songChunk.songName <> ".mogg"
    case (findFile pathVGS folder, findFile pathMogg folder) of
      (Just vgsData, _) -> do
        vgs <- stackIO $ decryptReadVGS vgsData
        namedChans <- stackIO $ forConcurrently [0 .. length vgs.header.channels - 1] \i -> do
          bs <- case level of
            ImportFull  -> splitOutVGSChannels [i] vgs
            ImportQuick -> return BL.empty
          return ("vgs-" <> show i, bs)
        let audio = HM.fromList do
              (name, bs) <- namedChans
              return $ (T.pack name ,) $ AudioFile AudioInfo
                { md5 = Nothing
                , frames = Nothing
                , commands = []
                , filePath = Just $ SoftFile (name <.> "vgs") $ SoftReadable $ makeHandle name $ byteStringSimpleHandle bs
                , rate = Nothing
                , channels = 1
                }
            plans = HM.singleton "vgs" let
              guitarChans = map fromIntegral $ fromMaybe [] $ lookup "guitar" songChunk.tracks.fromDictList
              bassChans   = map fromIntegral $ fromMaybe [] $ lookup "bass"   songChunk.tracks.fromDictList
              rhythmChans = map fromIntegral $ fromMaybe [] $ lookup "rhythm" songChunk.tracks.fromDictList
              songChans = zipWith const [0..] songChunk.pans
                \\ (guitarChans ++ bassChans ++ rhythmChans)
              mixChans v cs = do
                presentChannels <- NE.nonEmpty do
                  c <- cs
                  -- in some custom isos, vgs can have fewer channels than claimed by dta
                  case drop c namedChans of
                    (name, _) : _ -> [(c, T.pack name)]
                    []            -> []
                Just case presentChannels of
                  (c, name) :| [] -> PansVols
                    (map realToFrac [songChunk.pans !! c])
                    (map realToFrac [(songChunk.vols !! c) + v])
                    (Input $ Named name)
                  _ -> PansVols
                    (map realToFrac [ songChunk.pans !! c | (c, _) <- toList presentChannels ])
                    (map realToFrac [ (songChunk.vols !! c) + v | (c, _) <- toList presentChannels ])
                    (Merge $ fmap (Input . Named . snd) presentChannels)
              in StandardPlan StandardPlanInfo
                { song = mixChans 0 songChans
                , parts = Parts $ HM.fromList $ catMaybes
                  -- I made up these volume adjustments but they seem to work
                  [ (F.PartGuitar ,) . PartSingle <$> mixChans (-0.5) guitarChans
                  , (F.PartBass ,) . PartSingle <$> mixChans (-3) bassChans
                  , (F.PartName "rhythm" ,) . PartSingle <$> mixChans (-1.5) rhythmChans
                  ]
                , crowd = Nothing
                , comments = []
                , tuningCents = 0
                , fileTempo = Nothing
                }
        return (audio, plans)
      (Nothing, Just mogg) -> let
        audio = HM.empty
        plans = case level of
          ImportQuick -> HM.empty
          ImportFull  -> HM.singleton "mogg" $ MoggPlan $ getGH2MoggPlan mogg songChunk
        in return (audio, plans)
      (Nothing, Nothing) -> fail $ "Couldn't find .vgs or .mogg audio for " <> show songChunk.songName
  return (gh2SongYaml mode pkg Nothing songChunk midi onyxMidi)
    { audio = audio
    , plans = plans
    }

getGH2MoggPlan :: Readable -> Song -> MoggPlanInfo SoftFile
getGH2MoggPlan mogg songChunk = let
  instChans :: [(T.Text, [Int])]
  instChans = map (second $ map fromIntegral) songChunk.tracks.fromDictList
  in MoggPlanInfo
    { fileMOGG = Just $ SoftFile "audio.mogg" $ SoftReadable mogg
    , moggMD5 = Nothing
    , parts = Parts $ HM.fromList $ concat
      [ [ (F.PartGuitar        , PartSingle ns) | ns <- toList $ lookup "guitar" instChans ]
      , [ (F.PartBass          , PartSingle ns) | ns <- toList $ lookup "bass"   instChans ]
      , [ (F.PartName "rhythm", PartSingle ns) | ns <- toList $ lookup "rhythm" instChans ]
      ]
    , crowd = []
    , pans = map realToFrac songChunk.pans
    , vols = do
      let i `channelFor` part = maybe False (elem i) $ lookup part instChans
      (i, vol) <- zip [0..] songChunk.vols
      -- again, made up these volume adjustments so things sound ok in RB context
      return $ realToFrac if
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

importGH2DLC :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH2DLC src folder = do
  packSongs <- stackIO (findByteString ("config" :| ["songs.dta"]) folder) >>= \case
    Nothing -> fatal "config/songs.dta not found"
    Just bs -> do
      dta <- D.readDTA_latin1 $ BL.toStrict bs
      readSongListGH2Extra dta
  flip concatMapM packSongs \(_top, pkg, extra) -> do
    let base = T.unpack pkg.song.songName
        split s = case splitPath $ T.pack s of
          Nothing -> fatal $ "Internal error, couldn't parse path: " <> show s
          Just p  -> return p
        need p = case findFileCI p folder of
          Just r  -> return r
          Nothing -> fatal $ "Required file not found: " <> T.unpack (T.intercalate "/" $ toList p)
    moggPath <- split $ base <.> "mogg"
    mogg <- need moggPath
    art <- case T.splitOn "/" pkg.song.songName of
      [x, y, z] -> do
        let artPath = x :| [y, "gen", z <> ".bmp_xbox"]
        return $ fmap (SoftFile "cover.png_xbox" . SoftReadable) $ findFileCI artPath folder
      _ -> do
        warn "Couldn't recognize song path format to search for album art"
        return Nothing
    let modes = (ImportSolo, pkg.song) : case pkg.songCoop of
          Nothing   -> []
          Just coop -> [(ImportCoop, coop)]
    forM modes \(mode, songChunk) -> do
      return \level -> do
        when (level == ImportFull) do
          lg $ "Importing GH2 DLC song [" <> T.unpack songChunk.songName <> "] from: " <> src
        midi <- split (T.unpack songChunk.midiFile) >>= need
        onyxMidi <- case level of
          ImportFull  -> importGH2MIDI mode songChunk <$> F.loadMIDIReadable midi
          ImportQuick -> return emptyChart
        let yaml = gh2SongYaml mode pkg (Just extra) songChunk midi onyxMidi
        return yaml
          { plans = HM.singleton "mogg" $ MoggPlan $ getGH2MoggPlan mogg songChunk
          , metadata = yaml.metadata
            { fileAlbumArt = art
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
          D.decodeDecryptDTB dtb
        []        -> fatal $ "Couldn't find " <> show name
  dtbCampaign <- loadDTB "campaign.dtb"
  dtbBonus    <- loadDTB "store.dtb"
  let isKeyMatch k = \case
        D.Parens (D.Tree _ (D.Sym k' : rest)) | k == k' -> Just rest
        _                                               -> Nothing
      findKey :: B.ByteString -> D.Tree B.ByteString -> Maybe [D.Chunk B.ByteString]
      findKey k = firstJust (isKeyMatch k) . (.treeChunks)
      parseTier (D.Parens (D.Tree _ (D.Sym x : xs))) = return (x, [song | D.Sym song <- xs])
      parseTier _                              = fatal "Couldn't extract info from a tier in campaign.dtb"
      parseBonusSong (D.Parens (D.Tree _ [D.Sym x, isKeyMatch "price" -> Just [D.Int n]])) = return (x, n)
      parseBonusSong _ = fatal "Couldn't extract info from a bonus song in store.dtb"
  campaign <- case findKey "order" dtbCampaign.topTree of
    Nothing    -> fatal "Couldn't find 'order' in campaign.dtb"
    Just order -> mapM parseTier order
  bonus <- case findKey "song" dtbBonus.topTree of
    Nothing    -> fatal "Couldn't find bonus songs in store.dtb"
    Just songs -> mapM parseBonusSong songs
  return Setlist{..}

loadSetlistFull :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m (Setlist (B.ByteString, SongPackage))
loadSetlistFull gen = do
  setlist <- loadSetlist gen
  dir <- stackIO $ crawlFolder gen
  songs <- HM.fromList <$> getSongList dir
  forM setlist \k -> case HM.lookup (decodeLatin1 k) songs of
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
