{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module DTXMania.Import where

import           Audio
import           Config
import           Control.Monad.Extra              (forM, guard, mapMaybeM)
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
import           RockBand.Common                  (Difficulty (..),
                                                   pattern RNil, pattern Wait)
import qualified Sound.MIDI.File.Save             as Save
import           System.FilePath                  (dropExtension, (</>))

importDTX :: (SendMessage m, MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
importDTX fin dout = do
  dtx <- stackIO $ readDTXLines <$> loadDTXLines fin
  let simpleAudio f overlap chips = if RTB.null chips
        then return Nothing
        else do
          src <- getAudio overlap chips fin dtx
          writeAudio f [src]
      writeAudio f = \case
        []         -> return Nothing
        xs@(x : _) -> do
          let src = mixMany True (CA.rate x) (CA.channels x) $ foldr (Wait 0) RNil xs
          runAudio src $ dout </> f
          return $ Just (f, CA.channels src)
      drumSrc lanes = let
        chips = RTB.mapMaybe (\(l, chip) -> guard (elem l lanes) >> Just chip) $ dtx_Drums dtx
        in if RTB.null chips
          then return Nothing
          else Just <$> getAudio False chips fin dtx
      writeDrums f lanes = mapMaybeM drumSrc lanes >>= writeAudio f
  song   <- simpleAudio "song.wav" True $ dtx_BGM dtx
  kick   <- writeDrums "kick.wav" [[BassDrum]]
  snare  <- writeDrums "snare.wav" [[Snare]]
  kit    <- writeDrums "kit.wav"
    [ [HihatClose, HihatOpen]
    , [HighTom]
    , [LowTom]
    , [Cymbal]
    , [FloorTom]
    , [HihatOpen]
    , [RideCymbal]
    , [LeftCymbal]
    ]
  guitar <- simpleAudio "guitar.wav" False $ RTB.mapMaybe id $ snd <$> dtx_Guitar dtx
  bass   <- simpleAudio "bass.wav"   False $ RTB.mapMaybe id $ snd <$> dtx_Bass   dtx
  extra  <- forM (HM.toList $ dtx_BGMExtra dtx) $ \(chan, chips) -> do
    let f = "song-" <> T.unpack chan <> ".wav"
    simpleAudio f False chips
  let audioExpr (aud, _) = PlanAudio
        { _planExpr = Input $ Named $ T.pack aud
        , _planPans = []
        , _planVols = []
        }
      audiosExpr pairs = PlanAudio
        { _planExpr = Mix $ map (Input . Named . T.pack . fst) pairs
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
            Open    -> [Nothing]
            R       -> [Just Black1]
            G       -> [Just Black2]
            B       -> [Just Black3]
            RG      -> [Just Black1, Just Black2]
            RB      -> [Just Black1, Just Black3]
            GB      -> [Just Black2, Just Black3]
            RGB     -> [Just Black1, Just Black2, Just Black3]
            Wailing -> []
          }
        }

  stackIO
    $ Save.toFile (dout </> "notes.mid")
    $ RBFile.showMIDIFile'
    $ RBFile.Song (dtx_TempoMap dtx) (dtx_MeasureMap dtx) mempty
      { RBFile.fixedPartDrums = importDrums $ fmap fst $ dtx_Drums dtx
      , RBFile.fixedPartGuitarGHL = guitarToSix $ fmap fst $ dtx_Guitar dtx
      , RBFile.fixedPartBassGHL = guitarToSix $ fmap fst $ dtx_Bass dtx
      }
  stackIO $ Y.encodeFile (dout </> "song.yml") $ toJSON SongYaml
    { _metadata = def
      { _title        = dtx_TITLE dtx
      , _artist       = dtx_ARTIST dtx
      }
    , _audio = HM.fromList $ do
      (f, chans) <- catMaybes $ [kick, snare, kit, guitar, bass, song] ++ extra
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
      , _planParts    = Parts $ HM.fromList $ catMaybes $
        [ (FlexDrums ,) <$> case kit of
          Just k -> Just PartDrumKit
            { drumsSplitKick  = audioExpr <$> kick
            , drumsSplitSnare = audioExpr <$> snare
            , drumsSplitKit   = audioExpr k
            }
          Nothing -> case catMaybes [kick, snare, kit] of
            []  -> Nothing
            [x] -> Just $ PartSingle $ audioExpr x
            xs  -> Just $ PartSingle $ audiosExpr xs
        , (FlexGuitar ,) . PartSingle . audioExpr <$> guitar
        , (FlexBass   ,) . PartSingle . audioExpr <$> bass
        ] ++ do
          ex <- catMaybes extra
          return $ Just (FlexExtra (T.pack $ dropExtension $ fst ex), PartSingle $ audioExpr ex)
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
