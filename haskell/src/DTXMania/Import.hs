{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module DTXMania.Import where

import           Audio
import           Config
import           Control.Monad                    (guard)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.StackTrace
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import qualified Data.Text                        as T
import qualified Data.Yaml                        as Y
import           DTXMania.DTX
import           JSONData                         (toJSON)
import qualified RockBand.Codec.Drums             as D
import           RockBand.Codec.File              (FlexPartName (..))
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.Six
import           RockBand.Common                  (Difficulty (..))
import qualified Sound.MIDI.File.Save             as Save
import           System.FilePath                  ((</>))

importDTX :: (SendMessage m, MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
importDTX fin dout = do
  dtx <- stackIO $ readDTXLines <$> loadDTXLines fin
  let writeAudio f chips = if RTB.null chips
        then return Nothing
        else do
          src <- getAudio chips fin dtx
          runAudio src $ dout </> f
          return $ Just (f, CA.channels src)
  song   <- writeAudio "song.wav"   $         dtx_BGM    dtx
  drums  <- writeAudio "drums.wav"  $ snd <$> dtx_Drums  dtx
  guitar <- writeAudio "guitar.wav" $ snd <$> dtx_Guitar dtx
  bass   <- writeAudio "bass.wav"   $ snd <$> dtx_Bass   dtx
  let audioExpr (aud, _) = PlanAudio
        { _planExpr = Input $ Named $ T.pack aud
        , _planPans = []
        , _planVols = []
        }
      importDrums notes = mempty
        { D.drumDifficulties = Map.singleton Expert mempty
          { D.drumGems = RTB.flatten $ fmap drumsInstant $ RTB.collectCoincident notes
          }
        }
      drumsInstant xs = let
        normal = flip map xs $ \case
          HihatClose -> D.Pro D.Yellow ()
          Snare      -> D.Red
          BassDrum   -> D.Kick
          HighTom    -> D.Pro D.Yellow ()
          LowTom     -> D.Pro D.Blue ()
          Cymbal     -> D.Pro D.Green ()
          FloorTom   -> D.Pro D.Green ()
          HihatOpen  -> D.Pro D.Yellow ()
          RideCymbal -> D.Pro D.Blue ()
          LeftCymbal -> D.Pro D.Yellow ()
        in case normal of
          [D.Pro D.Yellow (), D.Pro D.Yellow ()] -> [D.Pro D.Yellow (), D.Pro D.Blue  ()]
          [D.Pro D.Blue   (), D.Pro D.Blue   ()] -> [D.Pro D.Yellow (), D.Pro D.Blue  ()]
          [D.Pro D.Green  (), D.Pro D.Green  ()] -> [D.Pro D.Blue   (), D.Pro D.Green ()]
          _                                      -> normal
      guitarToSix notes = mempty
        { sixDifficulties = Map.singleton Expert mempty
          { sixGems = fmap (, Nothing) $ RTB.flatten $ flip fmap notes $ \case
            Open -> [Nothing]
            R    -> [Just Black1]
            G    -> [Just Black2]
            B    -> [Just Black3]
            RG   -> [Just Black1, Just Black2]
            RB   -> [Just Black1, Just Black3]
            GB   -> [Just Black2, Just Black3]
            RGB  -> [Just Black1, Just Black2, Just Black3]
          }
        }

  stackIO
    $ Save.toFile (dout </> "notes.mid")
    $ RBFile.showMIDIFile'
    $ RBFile.Song (dtx_TempoMap dtx) (dtx_MeasureMap dtx) mempty
      { RBFile.fixedPartRealDrumsPS = importDrums $ fmap fst $ dtx_Drums dtx
      , RBFile.fixedPartGuitarGHL = guitarToSix $ fmap fst $ dtx_Guitar dtx
      , RBFile.fixedPartBassGHL = guitarToSix $ fmap fst $ dtx_Bass dtx
      }
  stackIO $ Y.encodeFile (dout </> "song.yml") $ toJSON SongYaml
    { _metadata = def
      { _title        = dtx_TITLE dtx
      , _artist       = dtx_ARTIST dtx
      }
    , _audio = HM.fromList $ do
      (f, chans) <- catMaybes [drums, guitar, bass, song]
      return $ (T.pack f ,) $ AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just f
        , _rate = Nothing
        , _channels = chans
        }
    , _jammit = HM.empty
    , _plans = HM.singleton "dtx" Plan
      { _song         = fmap audioExpr song
      , _countin      = Countin []
      , _planParts    = Parts $ HM.fromList $ catMaybes
        [ (FlexDrums  ,) . PartSingle . audioExpr <$> drums
        , (FlexGuitar ,) . PartSingle . audioExpr <$> guitar
        , (FlexBass   ,) . PartSingle . audioExpr <$> bass
        ]
      , _crowd = Nothing
      , _planComments = []
      , _tuningCents = 0
      }
    , _targets = HM.empty
    , _parts = Parts $ HM.fromList $ catMaybes
      [ do
        guard $ not $ RTB.null $ dtx_Drums dtx
        return $ (FlexDrums ,) def
          { partDrums = Just PartDrums
            { drumsDifficulty  = Tier 1
            , drumsMode        = Drums4 -- TODO pro or real
            , drumsKicks       = Kicks1x
            , drumsFixFreeform = False
            , drumsKit         = HardRockKit
            , drumsLayout      = StandardLayout
            , drumsFallback    = FallbackGreen
            }
          }
      , do
        guard $ not $ RTB.null $ dtx_Guitar dtx
        return $ (FlexGuitar ,) def
          { partGHL = Just PartGHL
            { ghlDifficulty    = Tier 1
            , ghlHopoThreshold = 170
            }
          }
      , do
        guard $ not $ RTB.null $ dtx_Bass dtx
        return $ (FlexBass ,) def
          { partGHL = Just PartGHL
            { ghlDifficulty    = Tier 1
            , ghlHopoThreshold = 170
            }
          }
      ]
    }
