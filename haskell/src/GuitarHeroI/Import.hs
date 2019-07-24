{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module GuitarHeroI.Import where

import           ArkTool
import           Audio                            (Audio (..), applyPansVols,
                                                   runAudio)
import           Config
import           Control.Exception                (bracket, bracket_)
import           Control.Monad                    (forM, unless)
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
import qualified Data.Yaml                        as Y
import           GuitarHeroI.File
import           GuitarHeroII.Audio               (readVGS)
import           GuitarHeroII.PartGuitar
import           JSONData                         (toJSON)
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as RB
import           RockBand.Common                  (Difficulty (..))
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  ((</>))
import qualified System.IO                        as IO
import           System.IO.Temp                   (withSystemTempFile)

getSongList :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m [(T.Text, SongPackage)]
getSongList gen = do
  let wrap msg f = f >>= \b -> unless b $ fail msg
  dtb <- stackIO $ bracket ark_new ark_delete $ \ark -> do
    let open = wrap "Couldn't open the ARK file" $ ark_Open ark gen
    bracket_ open (ark_Close ark) $ do
      withSystemTempFile "songs.dtb" $ \fdtb hdl -> do
        IO.hClose hdl
        wrap "Couldn't find songs.dtb in the ARK." $
          ark_GetFile ark fdtb "config/gen/songs.dtb" True
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
  let wrap msg f = f >>= \b -> unless b $ fail msg
  stackIO $ bracket ark_new ark_delete $ \ark -> do
    let open = wrap "Couldn't open the ARK file" $ ark_Open ark gen
    bracket_ open (ark_Close ark) $ do
      let encLatin1 = B8.pack . T.unpack
      wrap "Couldn't load MIDI file" $
        ark_GetFile ark (dout </> "gh1.mid") (encLatin1 $ midiFile pkg) True
      wrap "Couldn't load audio (VGS) file" $
        ark_GetFile ark (dout </> "audio.vgs") (encLatin1 (songName $ song pkg) <> ".vgs") True
  RBFile.Song tmap mmap gh1 <- stackIO (Load.fromFile $ dout </> "gh1.mid") >>= RBFile.readMIDIFile'
  let convmid :: RBFile.Song (RBFile.FixedFile U.Beats)
      convmid = RBFile.Song tmap mmap mempty
        { RBFile.fixedPartGuitar = convertPart $ gh1T1Gems gh1
        }
      convertPart :: GemsTrack U.Beats -> RB.FiveTrack U.Beats
      convertPart part = mempty
        { RB.fiveDifficulties = flip fmap (gemsDifficulties part) $ \diff -> mempty
          { RB.fiveGems = partGems diff
          }
        , RB.fiveOverdrive    = maybe RTB.empty partStarPower $ Map.lookup Expert $ gemsDifficulties part
        , RB.fivePlayer1      = maybe RTB.empty partPlayer1   $ Map.lookup Expert $ gemsDifficulties part
        , RB.fivePlayer2      = maybe RTB.empty partPlayer2   $ Map.lookup Expert $ gemsDifficulties part
        }
  stackIO $ Save.toFile (dout </> "notes.mid") $ RBFile.showMIDIFile' convmid
  srcs <- stackIO $ readVGS $ dout </> "audio.vgs"
  wavs <- forM (zip [0..] srcs) $ \(i, src) -> do
    let f = "vgs-" <> show (i :: Int) <> ".wav"
        src' = applyPansVols [pans (song pkg) !! i] [vols (song pkg) !! i]
          $ CA.mapSamples CA.fractionalSample src
    runAudio src' $ dout </> f
    return f
  stackIO $ Y.encodeFile (dout </> "song.yml") $ toJSON SongYaml
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
        , _channels = 2 -- because we did applyPansVols
        }
    , _jammit = HM.empty
    , _plans = HM.singleton "vgs" $ let
      guitarChans = concat $ take 1 $ slip_tracks $ song pkg
      songChans = zipWith const [0..] (pans $ song pkg) \\ guitarChans
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
