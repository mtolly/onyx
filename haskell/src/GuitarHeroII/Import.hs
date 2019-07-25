{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module GuitarHeroII.Import where

import           ArkTool
import           Audio                            (Audio (..), applyPansVols,
                                                   runAudio)
import           Config
import           Control.Monad                    (forM, guard, void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Char8            as B8
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.DTA                         as D
import qualified Data.DTA.Serialize               as D
import           Data.DTA.Serialize.GH2
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List                        ((\\))
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeLatin1)
import qualified Data.Yaml                        as Y
import           GuitarHeroII.Audio               (readVGS)
import           GuitarHeroII.Events
import           GuitarHeroII.File
import           GuitarHeroII.PartGuitar
import           GuitarHeroII.Triggers
import           JSONData                         (toJSON)
import qualified RockBand.Codec.Events            as RB
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as RB
import           RockBand.Common                  (Difficulty (..))
import           RockBand.Sections                (fromGH2Section)
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  ((</>))
import qualified System.IO                        as IO
import           System.IO.Temp                   (withSystemTempFile)

getSongList :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m [(T.Text, SongPackage)]
getSongList gen = do
  dtb <- stackIO $ withArk gen $ \ark -> do
    withSystemTempFile "songs.dtb" $ \fdtb hdl -> do
      IO.hClose hdl
      ark_GetFile' ark fdtb "config/gen/songs.dtb" True
      D.readFileDTB fdtb
  let editDTB d = d { D.topTree = editTree $ D.topTree d }
      editTree t = t { D.treeChunks = filter keepChunk $ D.treeChunks t }
      keepChunk = \case
        D.Parens tree -> not $ any isIgnore $ D.treeChunks tree
        _             -> False
      isIgnore = \case
        D.Parens (D.Tree _ [D.Key "validate_ignore", D.Key "TRUE"]) -> True
        _                                                           -> False
  fmap D.fromDictList
    $ D.unserialize (D.chunksDictList D.chunkKey D.stackChunks)
    $ editDTB $ fmap decodeLatin1 $ dtb

getImports :: [(T.Text, SongPackage)] -> [(T.Text, (ImportMode, SongPackage))]
getImports = concatMap $ \(t, pkg) -> case songCoop pkg of
  Nothing -> [(t, (ImportSolo, pkg))]
  Just _  -> [(t, (ImportSolo, pkg)), (t, (ImportCoop, pkg))]

data ImportMode = ImportSolo | ImportCoop

importGH2 :: (SendMessage m, MonadResource m) => ImportMode -> SongPackage -> FilePath -> FilePath -> StackTraceT m ()
importGH2 mode pkg gen dout = do
  songChunk <- case mode of
    ImportSolo -> return $ song pkg
    ImportCoop -> case songCoop pkg of
      Nothing -> fatal "Tried to import coop version from a song that doesn't have one"
      Just coop -> return coop
  stackIO $ withArk gen $ \ark -> do
    let encLatin1 = B8.pack . T.unpack
    ark_GetFile' ark (dout </> "gh2.mid") (encLatin1 $ midiFile songChunk) True
    ark_GetFile' ark (dout </> "audio.vgs") (encLatin1 (songName songChunk) <> ".vgs") True
  RBFile.Song tmap mmap gh2 <- stackIO (Load.fromFile $ dout </> "gh2.mid") >>= RBFile.readMIDIFile'
  let convmid :: RBFile.Song (RBFile.FixedFile U.Beats)
      convmid = RBFile.Song tmap mmap mempty
        { RBFile.fixedEvents = RB.EventsTrack
          { RB.eventsMusicStart = void $ RTB.filter (== MusicStart) $ eventsOther $ gh2Events gh2
          , RB.eventsMusicEnd   = RTB.empty
          , RB.eventsEnd        = void $ RTB.filter (== End) $ eventsOther $ gh2Events gh2
          , RB.eventsCoda       = RTB.empty
          , RB.eventsCrowd      = RTB.empty -- TODO
          , RB.eventsCrowdClap  = RTB.empty -- TODO
          , RB.eventsSections   = (\s -> (RB.SectionRB2, fromGH2Section s)) <$> eventsSections (gh2Events gh2)
          , RB.eventsBacking    = triggersBacking $ gh2Triggers gh2
          }
        , RBFile.fixedPartGuitar = convertPart $ case mode of
          ImportSolo -> gh2PartGuitar     gh2
          ImportCoop -> gh2PartGuitarCoop gh2
        , RBFile.fixedPartBass = convertPart $ gh2PartBass gh2
        , RBFile.fixedPartRhythm = convertPart $ gh2PartRhythm gh2
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
  stackIO $ Save.toFile (dout </> "notes.mid") $ RBFile.showMIDIFile' convmid
  srcs <- stackIO $ readVGS $ dout </> "audio.vgs"
  wavs <- forM (zip [0..] srcs) $ \(i, src) -> do
    let f = "vgs-" <> show (i :: Int) <> ".wav"
        src' = applyPansVols [pans songChunk !! i] [vols songChunk !! i]
          $ CA.mapSamples CA.fractionalSample src
    runAudio src' $ dout </> f
    return f
  stackIO $ Y.encodeFile (dout </> "song.yml") $ toJSON SongYaml
    { _metadata = def
      { _title  = Just $ name pkg <> case mode of
        ImportSolo -> ""
        ImportCoop -> " (Co-op)"
      , _artist = Just $ artist pkg
      , _cover = caption pkg /= Just "performed_by"
      }
    , _audio = HM.fromList $ do
      wav <- wavs
      return $ (T.pack wav ,) $ AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just wav
        , _rate = Nothing
        , _channels = 2 -- because we did applyPansVols
        }
    , _jammit = HM.empty
    , _plans = HM.singleton "vgs" $ let
      guitarChans = fromMaybe [] $ HM.lookup "guitar" $ tracks songChunk
      bassChans = fromMaybe [] $ HM.lookup "bass" $ tracks songChunk
      rhythmChans = fromMaybe [] $ HM.lookup "rhythm" $ tracks songChunk
      songChans = zipWith const [0..] (pans songChunk)
        \\ (guitarChans ++ bassChans ++ rhythmChans)
      mixChans [] = Nothing
      mixChans [c] = Just PlanAudio
        { _planExpr = Input $ Named $ T.pack $ wavs !! fromIntegral c
        , _planPans = []
        , _planVols = []
        }
      mixChans cs = Just PlanAudio
        { _planExpr = Mix $ map (Input . Named . T.pack . (wavs !!) . fromIntegral) cs
        , _planPans = []
        , _planVols = []
        }
      in Plan
        { _song = mixChans songChans
        , _countin = Countin []
        , _planParts = Parts $ HM.fromList $ catMaybes
          [ (RBFile.FlexGuitar ,) . PartSingle <$> mixChans guitarChans
          , (RBFile.FlexBass ,) . PartSingle <$> mixChans bassChans
          , (RBFile.FlexExtra "rhythm" ,) . PartSingle <$> mixChans rhythmChans
          ]
        , _crowd = Nothing
        , _planComments = []
        , _tuningCents = 0
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
