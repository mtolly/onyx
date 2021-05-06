{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module Import.GuitarHero2 where

import           Amplitude.PS2.Ark                (FileEntry (..), entryFolder,
                                                   readFileEntry)
import           Audio                            (Audio (..))
import           Config
import           Control.Monad                    (forM, guard, void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.DTA                         as D
import           Data.DTA.Crypt                   (decrypt, oldCrypt)
import qualified Data.DTA.Serialize               as D
import           Data.DTA.Serialize.GH2
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.Int
import           Data.List.Extra                  (firstJust, (\\))
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe)
import           Data.SimpleHandle                (findFile, handleToByteString,
                                                   splitPath, useHandle)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeLatin1)
import           GuitarHeroII.Ark                 (readFileEntries)
import           GuitarHeroII.Audio               (readVGSReadable)
import           GuitarHeroII.Events
import           GuitarHeroII.File
import           GuitarHeroII.PartGuitar
import           GuitarHeroII.Triggers
import           Import.Base
import qualified RockBand.Codec.Events            as RB
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as RB
import           RockBand.Common                  (Difficulty (..))
import           RockBand.Sections                (fromGH2Section)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  ((<.>), (</>))

getSongList :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m [(T.Text, SongPackage)]
getSongList gen = do
  entries <- stackIO $ readFileEntries $ gen </> "MAIN.HDR"
  dtb <- case filter (\fe -> fe_folder fe == Just "config/gen" && fe_name fe == "songs.dtb") entries of
    entry : _ -> stackIO $ useHandle (readFileEntry entry $ gen </> "MAIN_0.ARK") handleToByteString
    []        -> fatal "Couldn't find songs.dtb"
  let editDTB d = d { D.topTree = editTree $ D.topTree d }
      editTree t = t { D.treeChunks = filter keepChunk $ D.treeChunks t }
      keepChunk = \case
        D.Parens tree -> not $ any isIgnore $ D.treeChunks tree
        _             -> False
      isIgnore = \case
        D.Parens (D.Tree _ [D.Sym "validate_ignore", D.Sym "TRUE"]) -> True
        _                                                           -> False
  fmap D.fromDictList
    $ D.unserialize (D.chunksDictList D.chunkSym D.stackChunks)
    $ editDTB $ decodeLatin1 <$> D.decodeDTB (decrypt oldCrypt dtb)

getImports :: [(T.Text, SongPackage)] -> [(T.Text, (ImportMode, SongPackage))]
getImports = concatMap $ \(t, pkg) -> case songCoop pkg of
  Nothing -> [(t, (ImportSolo, pkg))]
  Just _  -> [(t, (ImportSolo, pkg)), (t, (ImportCoop, pkg))]

data ImportMode = ImportSolo | ImportCoop

importGH2 :: (SendMessage m, MonadResource m) => FilePath -> StackTraceT m [Import m]
importGH2 gen = map (\(_, (mode, pkg)) -> importGH2Song mode pkg gen) . getImports <$> getSongList gen

importGH2Song :: (SendMessage m, MonadResource m) => ImportMode -> SongPackage -> FilePath -> Import m
importGH2Song mode pkg gen level = do
  entries <- stackIO $ readFileEntries $ gen </> "MAIN.HDR"
  let folder = fmap (\entry -> readFileEntry entry $ gen </> "MAIN_0.ARK") $ entryFolder entries
      encLatin1 = B8.pack . T.unpack
      split s = case splitPath s of
        Nothing -> fatal $ "Internal error, couldn't parse path: " <> show s
        Just p  -> return p
      need p = case findFile p folder of
        Just r  -> return r
        Nothing -> fatal $ "Required file not found: " <> show p
  songChunk <- case mode of
    ImportSolo -> return $ song pkg
    ImportCoop -> case songCoop pkg of
      Nothing -> fatal "Tried to import coop version from a song that doesn't have one"
      Just coop -> return coop
  midi <- split (midiFile songChunk) >>= need . fmap encLatin1
  vgs <- split (songName songChunk <> ".vgs") >>= need . fmap encLatin1
  RBFile.Song tmap mmap gh2 <- case level of
    ImportFull  -> RBFile.loadMIDIReadable midi
    ImportQuick -> return emptyChart
  let convmid :: RBFile.Song (RBFile.OnyxFile U.Beats)
      convmid = RBFile.Song tmap mmap $ RBFile.fixedToOnyx mempty
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
          ImportSolo -> gh2PartGuitar     gh2
          ImportCoop -> gh2PartGuitarCoop gh2
        , RBFile.fixedPartBass = if HM.member "bass" $ tracks songChunk
          then convertPart $ gh2PartBass gh2
          else mempty
        , RBFile.fixedPartRhythm = if HM.member "rhythm" $ tracks songChunk
          then convertPart $ gh2PartRhythm gh2
          else mempty
        }
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
  srcs <- stackIO $ readVGSReadable vgs
  let namedSrcs = zip (map (\i -> "vgs-" <> show (i :: Int)) [0..]) srcs
  return SongYaml
    { _metadata = def'
      { _title  = Just $ name pkg <> case mode of
        ImportSolo -> ""
        ImportCoop -> " (Co-op)"
      , _artist = Just $ artist pkg
      , _cover = caption pkg /= Just "performed_by"
      , _fileAlbumArt = Nothing
      }
    , _global = Global
      { _fileMidi            = SoftFile "notes.mid" $ SoftChart convmid
      , _animTempo           = _animTempo def'
      , _fileSongAnim        = Nothing
      , _autogenTheme        = Nothing
      , _backgroundVideo     = Nothing
      , _fileBackgroundImage = Nothing
      }
    , _audio = HM.fromList $ do
      (srcName, src) <- namedSrcs
      return $ (T.pack srcName ,) $ AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just $ SoftFile (srcName <.> "wav") $ SoftAudio $ CA.mapSamples CA.fractionalSample src
        , _rate = Nothing
        , _channels = 1
        }
    , _jammit = HM.empty
    , _plans = HM.singleton "vgs" $ let
      guitarChans = map fromIntegral $ fromMaybe [] $ HM.lookup "guitar" $ tracks songChunk
      bassChans = map fromIntegral $ fromMaybe [] $ HM.lookup "bass" $ tracks songChunk
      rhythmChans = map fromIntegral $ fromMaybe [] $ HM.lookup "rhythm" $ tracks songChunk
      songChans = zipWith const [0..] (pans songChunk)
        \\ (guitarChans ++ bassChans ++ rhythmChans)
      mixChans _ [] = Nothing
      mixChans v [c] = Just PlanAudio
        { _planExpr = Input $ Named $ T.pack $ fst $ namedSrcs !! c
        , _planPans = map realToFrac [pans songChunk !! c]
        , _planVols = map realToFrac [(vols songChunk !! c) + v]
        }
      mixChans v cs = Just PlanAudio
        { _planExpr = Merge $ map (Input . Named . T.pack . fst . (namedSrcs !!)) cs
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
    , _targets = HM.empty -- TODO add gh2 target
    , _parts = Parts $ HM.fromList $ catMaybes
      [ do
        guard $ maybe False (not . null) $ HM.lookup "guitar" $ tracks songChunk
        return $ (RBFile.FlexGuitar ,) def
          { partGRYBO = Just def { gryboHopoThreshold = maybe 170 fromIntegral $ hopoThreshold songChunk }
          }
      , do
        guard $ maybe False (not . null) $ HM.lookup "bass" $ tracks songChunk
        return $ (RBFile.FlexBass ,) def
          { partGRYBO = Just def { gryboHopoThreshold = maybe 170 fromIntegral $ hopoThreshold songChunk }
          }
      , do
        guard $ maybe False (not . null) $ HM.lookup "rhythm" $ tracks songChunk
        return $ (RBFile.FlexExtra "rhythm" ,) def
          { partGRYBO = Just def { gryboHopoThreshold = maybe 170 fromIntegral $ hopoThreshold songChunk }
          }
      ]
    }

data Setlist a = Setlist
  { set_campaign :: [(B.ByteString, [a])]
  , set_bonus    :: [(a, Int32)]
  } deriving (Show, Functor, Foldable, Traversable)

loadSetlist :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m (Setlist B.ByteString)
loadSetlist gen = do
  entries <- stackIO $ readFileEntries $ gen </> "MAIN.HDR"
  let loadDTB name = case filter (\fe -> fe_folder fe == Just "config/gen" && fe_name fe == name) entries of
        entry : _ -> do
          dtb <- stackIO $ useHandle (readFileEntry entry $ gen </> "MAIN_0.ARK") handleToByteString
          return $ D.decodeDTB (decrypt oldCrypt dtb)
        []        -> fatal $ "Couldn't find " <> show name
  dtbCampaign <- loadDTB "campaign.dtb"
  dtbBonus    <- loadDTB "store.dtb"
  let isKeyMatch k = \case
        D.Parens (D.Tree _ (D.Sym k' : rest)) | k == k' -> Just rest
        _                                     -> Nothing
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
  songs <- HM.fromList <$> getSongList gen
  forM setlist $ \k -> case HM.lookup (decodeLatin1 k) songs of
    Just pkg -> return (k, pkg)
    Nothing  -> fatal $ "Couldn't locate setlist song " <> show k
