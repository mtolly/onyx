{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module Import.DTXMania where

import           Audio
import           Config
import           Control.Monad.Extra              (forM, guard, mapMaybeM,
                                                   unless)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.StackTrace
import           Data.Char                        (toLower)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   mapMaybe)
import           Data.SimpleHandle                (fileReadable)
import qualified Data.Text                        as T
import           Data.Tuple.Extra                 (fst3)
import           DTXMania.DTX
import           DTXMania.Set
import           Guitars                          (emit5')
import           Import.Base
import           RockBand.Codec.File              (FlexPartName (..))
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.Five
import qualified RockBand.Codec.FullDrums         as FD
import           RockBand.Common                  (Difficulty (..),
                                                   pattern RNil,
                                                   StrumHOPOTap (..),
                                                   pattern Wait)
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.FilePath                  (dropExtension, takeDirectory,
                                                   takeExtension, (<.>), (</>))

dtxConvertDrums, dtxConvertGuitar, dtxConvertBass
  :: DTX -> RBFile.Song (RBFile.OnyxFile U.Beats) -> RBFile.Song (RBFile.OnyxFile U.Beats)
dtxConvertDrums dtx (RBFile.Song tmap mmap onyx) = let
  importFullDrums notes = mempty
    { FD.fdDifficulties = Map.singleton Expert FD.FullDrumDifficulty
      { FD.fdGems = flip RTB.mapMaybe notes $ \case
        HihatClose -> Just (FD.Hihat    , FD.GemHihatClosed, FD.VelocityNormal)
        Snare      -> Just (FD.Snare    , FD.GemNormal     , FD.VelocityNormal)
        BassDrum   -> Just (FD.Kick     , FD.GemNormal     , FD.VelocityNormal)
        HighTom    -> Just (FD.Tom1     , FD.GemNormal     , FD.VelocityNormal)
        LowTom     -> Just (FD.Tom2     , FD.GemNormal     , FD.VelocityNormal)
        Cymbal     -> Just (FD.CrashR   , FD.GemNormal     , FD.VelocityNormal)
        FloorTom   -> Just (FD.Tom3     , FD.GemNormal     , FD.VelocityNormal)
        HihatOpen  -> Just (FD.Hihat    , FD.GemHihatOpen  , FD.VelocityNormal)
        RideCymbal -> Just (FD.Ride     , FD.GemNormal     , FD.VelocityNormal)
        LeftCymbal -> Just (FD.CrashL   , FD.GemNormal     , FD.VelocityNormal)
        LeftPedal  -> Just (FD.HihatFoot, FD.GemNormal     , FD.VelocityNormal)
        LeftBass   -> Nothing
      , FD.fdFlam = RTB.empty
      }
    , FD.fdKick2 = RTB.mapMaybe (\case LeftBass -> Just (); _ -> Nothing) notes
    }
  in RBFile.Song tmap mmap $ RBFile.editOnyxPart FlexDrums
    (\opart -> opart { RBFile.onyxPartFullDrums = importFullDrums $ fmap fst $ dtx_Drums dtx })
    onyx
dtxConvertGuitar = dtxConvertGB dtx_Guitar $ \onyx five -> RBFile.editOnyxPart
  FlexGuitar
  (\opart -> opart { RBFile.onyxPartGuitar = five })
  onyx
dtxConvertBass   = dtxConvertGB dtx_Bass   $ \onyx five -> RBFile.editOnyxPart
  FlexBass
  (\opart -> opart { RBFile.onyxPartGuitar = five })
  onyx

dtxConvertGB
  :: (DTX -> RTB.T U.Beats ([Color], Chip))
  -> (f U.Beats -> FiveTrack U.Beats -> f U.Beats)
  -> DTX
  -> RBFile.Song (f U.Beats)
  -> RBFile.Song (f U.Beats)
dtxConvertGB getter setter dtx (RBFile.Song tmap mmap fixed) = let
  -- TODO we should be able to add sustains by looking at the length of audio chips
  guitarToFive notes = mempty
    { fiveDifficulties = Map.singleton Expert $ emit5' $ RTB.flatten $ flip fmap notes $ \case
      [] -> [((Nothing, Strum), Nothing)]
      xs -> [((Just x , Strum), Nothing) | x <- xs ]
    }
  in RBFile.Song tmap mmap $ setter fixed $ guitarToFive $ fmap fst $ getter dtx

dtxBaseMIDI :: DTX -> RBFile.Song (RBFile.OnyxFile U.Beats)
dtxBaseMIDI dtx = RBFile.Song (dtx_TempoMap dtx) (dtx_MeasureMap dtx) mempty

dtxToAudio :: (SendMessage m, MonadIO m) => DTX -> FilePath -> StackTraceT m (SongYaml SoftFile -> SongYaml SoftFile)
dtxToAudio dtx fin = do
  let simpleAudio f overlap chips = if RTB.null chips
        then return Nothing
        else do
          src <- getAudio (if overlap then Nothing else Just (1, 0.002)) chips fin dtx
          writeAudio f [src]
      writeAudio f = \case
        []         -> return Nothing
        xs@(x : _) -> do
          let src = mixMany (CA.rate x) (CA.channels x) Nothing $ foldr (Wait 0) RNil xs
          return $ Just (f, src, CA.channels src)
      drumSrc lanes = let
        chips = RTB.mapMaybe (\(l, chip) -> guard (elem l lanes) >> Just chip) $ dtx_Drums dtx
        in if RTB.null chips
          then return Nothing
          else Just <$> getAudio (Just (1, 0.2)) chips fin dtx
      writeDrums f lanes = mapMaybeM drumSrc lanes >>= writeAudio f
  song   <- simpleAudio "song.wav" True $ dtx_BGM dtx
  kick   <- writeDrums "kick.wav" [[BassDrum, LeftBass]]
  snare  <- writeDrums "snare.wav" [[Snare]]
  kit    <- writeDrums "kit.wav"
    [ [HihatClose, HihatOpen, LeftPedal]
    , [HighTom]
    , [LowTom]
    , [Cymbal]
    , [FloorTom]
    , [RideCymbal]
    , [LeftCymbal]
    ]
  guitar <- simpleAudio "guitar.wav" False $ snd <$> dtx_Guitar dtx
  bass   <- simpleAudio "bass.wav"   False $ snd <$> dtx_Bass   dtx
  extra  <- forM (HM.toList $ dtx_BGMExtra dtx) $ \(chan, chips) -> do
    let f = "song-" <> T.unpack chan <> ".wav"
    simpleAudio f False chips
  let audioExpr (aud, _, _) = PlanAudio
        { _planExpr = Input $ Named $ T.pack aud
        , _planPans = []
        , _planVols = []
        }
      audiosExpr pairs = PlanAudio
        { _planExpr = Mix $ map (Input . Named . T.pack . fst3) pairs
        , _planPans = []
        , _planVols = []
        }
  return $ \songYaml -> songYaml
    { _audio = HM.fromList $ do
      (f, src, chans) <- catMaybes $ [kick, snare, kit, guitar, bass, song] ++ extra
      return $ (T.pack f ,) $ AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just $ SoftFile f $ SoftAudio src
        , _rate = Nothing
        , _channels = chans
        }
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
          return $ Just (FlexExtra (T.pack $ dropExtension $ fst3 ex), PartSingle $ audioExpr ex)
      , _crowd = Nothing
      , _planComments = []
      , _tuningCents = 0
      , _fileTempo = Nothing
      }
    }

importSet :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m [Import m]
importSet setDefPath = inside ("Loading DTX set.def from: " <> setDefPath) $ do
  songs <- stackIO $ loadSet setDefPath
  return $ map (importSetDef $ Just setDefPath) songs

importSetDef :: (SendMessage m, MonadIO m) => Maybe FilePath -> SetDef -> Import m
importSetDef setDefPath song _level = do
  -- TODO need to fix path separators (both backslash and yen)
  let relToSet = maybe id (\sdp -> (takeDirectory sdp </>)) setDefPath
      fs = map (relToSet . T.unpack)
        $ mapMaybe ($ song) [setL5File, setL4File, setL3File, setL2File, setL1File]
  diffs <- fmap catMaybes $ forM fs $ \f -> let
    fmt = case map toLower $ takeExtension f of
      ".gda" -> FormatGDA
      _      -> FormatDTX
    in stackIO (Dir.doesFileExist f) >>= \case
      True -> stackIO $ do
        dtx <- readDTXLines fmt <$> loadDTXLines f
        return $ Just (f, dtx)
      False -> do
        warn $ "Couldn't find difficulty from set.def: " <> f
        return Nothing
  (topDiffPath, topDiffDTX) <- case diffs of
    []    -> fatal "No difficulties found for song"
    d : _ -> return d
  addAudio <- dtxToAudio topDiffDTX topDiffPath
  let hasDrums  = not . RTB.null . dtx_Drums
      hasGuitar = not . RTB.null . dtx_Guitar
      hasBass   = not . RTB.null . dtx_Bass
  topDrumDiff <- case filter (hasDrums . snd) diffs of
    []       -> return Nothing
    (path, diff) : _ -> do
      unless (path == topDiffPath) $ warn
        "Highest difficulty does not contain drums, but a lower one does. Audio might not be separated"
      return $ Just diff
  topGuitarDiff <- case filter (hasGuitar . snd) diffs of
    []       -> return Nothing
    (path, diff) : _ -> do
      unless (path == topDiffPath) $ warn
        "Highest difficulty does not contain guitar, but a lower one does. Audio might not be separated"
      return $ Just diff
  topBassDiff <- case filter (hasBass . snd) diffs of
    [] -> return Nothing
    (path, diff) : _ -> do
      unless (path == topDiffPath) $ warn
        "Highest difficulty does not contain bass, but a lower one does. Audio might not be separated"
      return $ Just diff
  let midiOnyx
        = maybe id dtxConvertDrums topDrumDiff
        $ maybe id dtxConvertGuitar topGuitarDiff
        $ maybe id dtxConvertBass topBassDiff
        $ dtxBaseMIDI topDiffDTX
  art <- case (takeDirectory topDiffPath </>) <$> dtx_PREIMAGE topDiffDTX of
    Nothing -> return Nothing
    Just f  -> stackIO (Dir.doesFileExist f) >>= \case
      False -> do
        warn $ "Couldn't find preview image: " <> f
        return Nothing
      True -> do
        let loc = "cover" <.> takeExtension f
        return $ Just $ SoftFile loc $ SoftReadable $ fileReadable f
  let translateDifficulty Nothing    _   = Rank 1
      translateDifficulty (Just lvl) dec = let
        lvl' = (fromIntegral lvl + maybe 0 ((/ 10) . fromIntegral) dec) / 100 :: Rational
        in Rank $ max 1 $ round $ lvl' * 525 -- arbitrary scaling factor
  return $ addAudio SongYaml
    { _metadata = def'
      { _title        = case setTitle song of
        ""    -> dtx_TITLE topDiffDTX
        title -> Just title
      , _artist       = dtx_ARTIST topDiffDTX
      , _comments     = toList $ dtx_COMMENT topDiffDTX
      , _genre        = dtx_GENRE topDiffDTX
      , _fileAlbumArt = art
      }
    , _global = def'
      { _backgroundVideo = Nothing
      , _fileBackgroundImage = Nothing
      , _fileMidi = SoftFile "notes.mid" $ SoftChart midiOnyx
      , _fileSongAnim = Nothing
      }
    , _audio = HM.empty
    , _jammit = HM.empty
    , _plans = HM.empty
    , _targets = HM.empty
    , _parts = Parts $ HM.fromList $ catMaybes
      [ flip fmap topDrumDiff $ \diff -> (FlexDrums ,) def
        { partDrums = Just PartDrums
          { drumsDifficulty  = translateDifficulty (dtx_DLEVEL diff) (dtx_DLVDEC diff)
          , drumsMode        = DrumsFull
          , drumsKicks       = if any ((== LeftBass) . fst) $ dtx_Drums diff
            then KicksBoth
            else Kicks1x
          , drumsFixFreeform = False
          , drumsKit         = HardRockKit
          , drumsLayout      = StandardLayout
          , drumsFallback    = FallbackGreen
          }
        }
      , flip fmap topGuitarDiff $ \diff -> (FlexGuitar ,) def
        { partGRYBO = Just def
          { gryboDifficulty = translateDifficulty (dtx_GLEVEL diff) (dtx_GLVDEC diff)
          }
        }
      , flip fmap topBassDiff $ \diff -> (FlexBass ,) def
        { partGRYBO = Just def
          { gryboDifficulty = translateDifficulty (dtx_BLEVEL diff) (dtx_BLVDEC diff)
          }
        }
      ]
    }

importDTX :: (SendMessage m, MonadIO m) => FilePath -> Import m
importDTX fin level = do
  dtxLines <- stackIO $ loadDTXLines fin
  let setDef = SetDef
        { setTitle   = fromMaybe "" $ lookup "TITLE" dtxLines
        , setL1Label = Nothing
        , setL1File  = Just $ T.pack fin
        , setL2Label = Nothing
        , setL2File  = Nothing
        , setL3Label = Nothing
        , setL3File  = Nothing
        , setL4Label = Nothing
        , setL4File  = Nothing
        , setL5Label = Nothing
        , setL5File  = Nothing
        }
  importSetDef Nothing setDef level
