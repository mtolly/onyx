{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module GuitarHeroI.Import where

import           ArkTool
import           Audio                            (Audio (..), runAudio)
import           Config
import           Control.Monad                    (forM, void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Char8            as B8
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.DTA                         as D
import qualified Data.DTA.Serialize               as D
import           Data.DTA.Serialize.GH1
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List                        ((\\))
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeLatin1)
import           GuitarHeroI.File
import           GuitarHeroII.Audio               (readVGS)
import           GuitarHeroII.PartGuitar
import           JSONData                         (toJSON, yamlEncodeFile)
import qualified RockBand.Codec.Events            as RB
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as RB
import           RockBand.Common                  (Difficulty (..), Mood (..))
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
        D.Parens _ -> True
        _          -> False
  fmap D.fromDictList
    $ D.unserialize (D.chunksDictList D.chunkKey D.stackChunks)
    $ editDTB $ fmap decodeLatin1 $ dtb

importGH1 :: (SendMessage m, MonadResource m) => SongPackage -> FilePath -> FilePath -> StackTraceT m ()
importGH1 pkg gen dout = do
  stackIO $ withArk gen $ \ark -> do
    let encLatin1 = B8.pack . T.unpack
    ark_GetFile' ark (dout </> "gh1.mid") (encLatin1 $ midiFile pkg) True
    ark_GetFile' ark (dout </> "audio.vgs") (encLatin1 (songName $ song pkg) <> ".vgs") True
  RBFile.Song tmap mmap gh1 <- stackIO (Load.fromFile $ dout </> "gh1.mid") >>= RBFile.readMIDIFile'
  let convmid :: RBFile.Song (RBFile.FixedFile U.Beats)
      convmid = RBFile.Song tmap mmap mempty
        { RBFile.fixedPartGuitar = mempty
          { RB.fiveDifficulties = flip fmap diffs $ \diff -> mempty
            { RB.fiveGems = partGems diff
            }
          , RB.fiveOverdrive    = maybe RTB.empty partStarPower $ Map.lookup Expert diffs
          , RB.fivePlayer1      = maybe RTB.empty partPlayer1   $ Map.lookup Expert diffs
          , RB.fivePlayer2      = maybe RTB.empty partPlayer2   $ Map.lookup Expert diffs
          , RB.fiveFretPosition = flip fmap (animFretPosition $ gh1Anim gh1) $ \case
            (FretPosition p, b) -> (p        , b)
            (Fret60        , b) -> (RB.Fret59, b)
          , RB.fiveHandMap      = flip fmap (animHandMap $ gh1Anim gh1) $ \case
            HandMap_Default   -> RB.HandMap_Default
            HandMap_DropD2    -> RB.HandMap_DropD2
            HandMap_Solo      -> RB.HandMap_Solo
            HandMap_NoChords  -> RB.HandMap_NoChords
            HandMap_AllChords -> RB.HandMap_AllChords
          , RB.fiveMood         = flip RTB.mapMaybe (eventsList $ gh1Events gh1) $ \case
            Event_gtr_on  -> Just Mood_play
            Event_gtr_off -> Just Mood_idle
            _             -> Nothing
          }
        , RBFile.fixedEvents = mempty
          { RB.eventsEnd = void $ RTB.filter (== Event_end) $ eventsList $ gh1Events gh1
          }
        }
      diffs = gemsDifficulties $ gh1T1Gems gh1
  stackIO $ Save.toFile (dout </> "notes.mid") $ RBFile.showMIDIFile' convmid
  srcs <- stackIO $ readVGS $ dout </> "audio.vgs"
  wavs <- forM (zip [0..] srcs) $ \(i, src) -> do
    let f = "vgs-" <> show (i :: Int) <> ".wav"
    runAudio (CA.mapSamples CA.fractionalSample src) $ dout </> f
    return f
  stackIO $ yamlEncodeFile (dout </> "song.yml") $ toJSON SongYaml
    { _metadata = def
      { _title  = Just $ name pkg
      , _artist = Just $ artist pkg
      , _cover = False -- TODO this doesn't appear to be in songs.dta, where is it?
      }
    , _audio = HM.fromList $ do
      wav <- wavs
      return $ (T.pack wav ,) $ AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just wav
        , _rate = Nothing
        , _channels = 1
        }
    , _jammit = HM.empty
    , _plans = HM.singleton "vgs" $ let
      guitarChans = map fromIntegral $ concat $ take 1 $ slip_tracks $ song pkg
      songChans = zipWith const [0..] (pans $ song pkg) \\ guitarChans
      mixChans [] = Nothing
      mixChans [c] = Just PlanAudio
        { _planExpr = Input $ Named $ T.pack $ wavs !! c
        , _planPans = map realToFrac [pans (song pkg) !! c]
        , _planVols = map realToFrac [vols (song pkg) !! c]
        }
      mixChans cs = Just PlanAudio
        { _planExpr = Merge $ map (Input . Named . T.pack . (wavs !!)) cs
        , _planPans = map realToFrac [ pans (song pkg) !! c | c <- cs ]
        , _planVols = map realToFrac [ vols (song pkg) !! c | c <- cs ]
        }
      in Plan
        { _song = mixChans songChans
        , _countin = Countin []
        , _planParts = Parts $ HM.fromList $ catMaybes
          [ (RBFile.FlexGuitar ,) . PartSingle <$> mixChans guitarChans
          ]
        , _crowd = Nothing
        , _planComments = []
        , _tuningCents = 0
        }
    , _targets = HM.empty
    , _parts = Parts $ HM.singleton RBFile.FlexGuitar $ def
      { partGRYBO = Just def
      }
    }
