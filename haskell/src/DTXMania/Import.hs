{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module DTXMania.Import where

import           Audio
import           Config
import           Control.Monad.Codec.Onyx.JSON    (toJSON, yamlEncodeFile)
import           Control.Monad.Extra              (forM, guard, mapMaybeM,
                                                   unless, void, when)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State
import           Data.Char                        (toLower)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import           DTXMania.DTX
import           DTXMania.Set
import           Guitars                          (emit5')
import qualified Numeric.NonNegative.Class        as NNC
import qualified RockBand.Codec.Drums             as D
import           RockBand.Codec.File              (FlexPartName (..))
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.Five
import           RockBand.Common                  (Difficulty (..),
                                                   pattern RNil,
                                                   StrumHOPOTap (..),
                                                   pattern Wait)
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.FilePath                  (dropExtension, takeDirectory,
                                                   takeExtension, (<.>), (</>))

data CymbalInstant lc rd = CymbalInstant
  { instantHH :: Maybe D.PSGem -- always yellow (with ps real mods)
  , instantLC :: Maybe lc -- default yellow, can be pushed to blue
  , instantRD :: Maybe rd -- default blue, can be pushed to green
  , instantRC :: Bool -- always green
  } deriving (Show)

placeCymbals :: (NNC.C t) => RTB.T t DrumLane -> RTB.T t D.RealDrum
placeCymbals = RTB.flatten . fmap emitCymbals . assignFull . RTB.filter hasCymbals . fmap makeInstant . RTB.collectCoincident where
  makeInstant notes = CymbalInstant
    { instantHH = if
      | elem HihatClose notes -> Just D.HHSizzle
      | elem HihatOpen notes  -> Just D.HHOpen
      | elem LeftPedal notes  -> Just D.HHPedal
      | otherwise             -> Nothing
    , instantLC = guard (elem LeftCymbal notes) >> Just Nothing
    , instantRD = guard (elem RideCymbal notes) >> Just Nothing
    , instantRC = elem Cymbal notes
    }
  hasCymbals = \case
    CymbalInstant Nothing Nothing Nothing False -> False
    _                                           -> True

  -- first, assign all the cymbals that only have one option due to other simultaneous cymbals.
  -- any LC with HH is blue, any RD with RC is blue (or drop it if also LC)
  assignInstant
    :: RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
    -> RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
  assignInstant = fmap $ \now -> let
    newLC = case (instantHH now, instantLC now) of
      (Just _, Just Nothing) -> Just $ Just D.Blue
      _                      -> instantLC now
    newRD = case (newLC, instantRD now, instantRC now) of
      (Just (Just D.Blue), Just _      , True ) -> Nothing -- already blue and green taken
      (Just (Just D.Blue), Just Nothing, False) -> Just $ Just D.Green -- green because LC is blue
      (_                 , Just Nothing, True ) -> Just $ Just D.Blue -- blue because RC is green
      _                                         -> instantRD now
    in now { instantLC = newLC, instantRD = newRD }

  -- then, propagate info forward to do more cymbal assignments.
  -- this gets run both forward and backward
  propagatePass _    []           = []
  propagatePass prev (now : rest) = let
    newLC = case (prev >>= instantHH, prev >>= instantLC, instantLC now) of
      (_, Just (Just prevLC), Just Nothing) -> Just $ Just prevLC -- match previous LC color
      (Just _, _, Just Nothing)             -> Just $ Just D.Blue -- make LC blue because there's a hihat previously
      _                                     -> instantLC now
    newRD = case (newLC, instantRD now) of
      (Just (Just D.Blue), Just Nothing) -> Just $ Just D.Green -- green because now there's a blue LC
      _ -> case (prev >>= instantLC, prev >>= instantRD, instantRD now) of
        (_, Just (Just prevRD), Just Nothing) -> Just $ Just prevRD -- match previous RD color
        (Just (Just D.Blue), _, Just Nothing) -> Just $ Just D.Green -- green because there's a blue LC previously
        _                                     -> instantRD now
    newNow = now { instantLC = newLC, instantRD = newRD }
    in newNow : propagatePass (Just newNow) rest
  assignPropagate
    :: RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
    -> RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
  assignPropagate rtb = let
    times = RTB.getTimes rtb
    bodies = reverse $ propagatePass Nothing $ reverse $ propagatePass Nothing $ RTB.getBodies rtb
    in RTB.fromPairList $ zip times bodies

  -- finally, default to putting LC on yellow and RD on blue
  assignFinal
    :: RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
    -> RTB.T t (CymbalInstant D.ProColor D.ProColor)
  assignFinal = fmap $ \now -> now
    { instantLC = fromMaybe D.Yellow <$> instantLC now
    , instantRD = fromMaybe D.Blue   <$> instantRD now
    }

  assignFull
    :: RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
    -> RTB.T t (CymbalInstant D.ProColor D.ProColor)
  assignFull = assignFinal . assignPropagate . assignInstant

  emitCymbals now = catMaybes
    [ Left <$> instantHH now
    , (\color -> Right $ D.Pro color D.Cymbal) <$> instantLC now
    , (\color -> Right $ D.Pro color D.Cymbal) <$> instantRD now
    , guard (instantRC now) >> Just (Right $ D.Pro D.Green D.Cymbal)
    ]

placeToms :: (NNC.C t) => RTB.T t D.RealDrum -> RTB.T t DrumLane -> RTB.T t D.RealDrum
placeToms cymbals notes = let
  basicGem = \case
    Left D.Rimshot -> D.Red
    Left _         -> D.Pro D.Yellow ()
    Right rb       -> void rb
  conflict x y = basicGem x == basicGem y
  tomsInstant stuff = let
    thisCymbals = lefts stuff
    thisNotes = rights stuff
    add x choices = when (elem x thisNotes) $ modify $ \cur ->
      case filter (\y -> all (not . conflict y) $ thisCymbals <> cur) choices of
        []    -> cur -- no options!
        y : _ -> y : cur
    in flip execState [] $ do
      add HighTom $ map (\c -> Right $ D.Pro c D.Tom) [D.Yellow, D.Blue, D.Green]
      add LowTom $ map (\c -> Right $ D.Pro c D.Tom) [D.Blue, D.Yellow, D.Green]
      add FloorTom $ map (\c -> Right $ D.Pro c D.Tom) [D.Green, D.Blue, D.Yellow]
  in RTB.flatten $ fmap tomsInstant $ RTB.collectCoincident $ RTB.merge (Left <$> cymbals) (Right <$> notes)

dtxConvertDrums, dtxConvertGuitar, dtxConvertBass
  :: DTX -> RBFile.Song (RBFile.FixedFile U.Beats) -> RBFile.Song (RBFile.FixedFile U.Beats)
dtxConvertDrums dtx (RBFile.Song tmap mmap fixed) = let
  importDrums notes = let
    cymbals = placeCymbals notes
    kicksSnares = flip RTB.mapMaybe notes $ \case
      BassDrum -> Just $ Right D.Kick
      Snare    -> Just $ Right D.Red
      _        -> Nothing
    toms = placeToms cymbals notes
    real = D.encodePSReal (1/32) Expert $ RTB.merge cymbals $ RTB.merge kicksSnares toms
    in real
      { D.drumKick2x = RTB.mapMaybe (\case LeftBass -> Just (); _ -> Nothing) notes
      }
  in RBFile.Song tmap mmap fixed
    { RBFile.fixedPartRealDrumsPS = importDrums $ fmap fst $ dtx_Drums dtx
    , RBFile.fixedPartDrums = importDrums $ RTB.filter (/= LeftPedal) $ fmap fst $ dtx_Drums dtx
    }
dtxConvertGuitar = dtxConvertGB dtx_Guitar $ \fixed five -> fixed { RBFile.fixedPartGuitar = five }
dtxConvertBass   = dtxConvertGB dtx_Bass   $ \fixed five -> fixed { RBFile.fixedPartBass   = five }

dtxConvertGB
  :: (DTX -> RTB.T U.Beats ([Color], Chip))
  -> (RBFile.FixedFile U.Beats -> FiveTrack U.Beats -> RBFile.FixedFile U.Beats)
  -> DTX
  -> RBFile.Song (RBFile.FixedFile U.Beats)
  -> RBFile.Song (RBFile.FixedFile U.Beats)
dtxConvertGB getter setter dtx (RBFile.Song tmap mmap fixed) = let
  -- TODO we should be able to add sustains by looking at the length of audio chips
  guitarToFive notes = mempty
    { fiveDifficulties = Map.singleton Expert $ emit5' $ RTB.flatten $ flip fmap notes $ \case
      [] -> [((Nothing, Strum), Nothing)]
      xs -> [((Just x , Strum), Nothing) | x <- xs ]
    }
  in RBFile.Song tmap mmap $ setter fixed $ guitarToFive $ fmap fst $ getter dtx

dtxBaseMIDI :: DTX -> RBFile.Song (RBFile.FixedFile U.Beats)
dtxBaseMIDI dtx = RBFile.Song (dtx_TempoMap dtx) (dtx_MeasureMap dtx) mempty

dtxToAudio :: (SendMessage m, MonadIO m) => DTX -> FilePath -> FilePath -> StackTraceT m (SongYaml FilePath -> SongYaml FilePath)
dtxToAudio dtx fin dout = do
  let simpleAudio f overlap chips = if RTB.null chips
        then return Nothing
        else do
          src <- getAudio (if overlap then Nothing else Just (1, 0.002)) chips fin dtx
          writeAudio f [src]
      writeAudio f = \case
        []         -> return Nothing
        xs@(x : _) -> do
          let src = mixMany (CA.rate x) (CA.channels x) Nothing $ foldr (Wait 0) RNil xs
          runAudio src $ dout </> f
          return $ Just (f, CA.channels src)
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
  return $ \songYaml -> songYaml
    { _audio = HM.fromList $ do
      (f, chans) <- catMaybes $ [kick, snare, kit, guitar, bass, song] ++ extra
      return $ (T.pack f ,) $ AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just f
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
          return $ Just (FlexExtra (T.pack $ dropExtension $ fst ex), PartSingle $ audioExpr ex)
      , _crowd = Nothing
      , _planComments = []
      , _tuningCents = 0
      , _fileTempo = Nothing
      }
    }

importSet :: (SendMessage m, MonadIO m) => Int -> FilePath -> FilePath -> StackTraceT m ()
importSet i setDefPath dout = inside ("Loading DTX set.def from: " <> setDefPath) $ do
  song <- stackIO (loadSet setDefPath) >>= \songs -> case drop i songs of
    []       -> fatal $ "Couldn't find song index " <> show i <> " in set.def"
    song : _ -> return song
  importSetDef (Just setDefPath) song dout

importSetDef :: (SendMessage m, MonadIO m) => Maybe FilePath -> SetDef -> FilePath -> StackTraceT m ()
importSetDef setDefPath song dout = do
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
  addAudio <- dtxToAudio topDiffDTX topDiffPath dout
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
  stackIO
    $ Save.toFile (dout </> "notes.mid")
    $ RBFile.showMIDIFile'
    $ maybe id dtxConvertDrums topDrumDiff
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
        stackIO $ Dir.copyFile f $ dout </> loc
        return $ Just loc
  let translateDifficulty Nothing    _   = Rank 1
      translateDifficulty (Just lvl) dec = let
        lvl' = (fromIntegral lvl + maybe 0 ((/ 10) . fromIntegral) dec) / 100 :: Rational
        in Rank $ max 1 $ round $ lvl' * 525 -- arbitrary scaling factor
  stackIO $ yamlEncodeFile (dout </> "song.yml") $ toJSON $ addAudio SongYaml
    { _metadata = (def :: Metadata FilePath)
      { _title        = case setTitle song of
        ""    -> dtx_TITLE topDiffDTX
        title -> Just title
      , _artist       = dtx_ARTIST topDiffDTX
      , _comments     = toList $ dtx_COMMENT topDiffDTX
      , _genre        = dtx_GENRE topDiffDTX
      , _fileAlbumArt = art
      }
    , _global = def
    , _audio = HM.empty
    , _jammit = HM.empty
    , _plans = HM.empty
    , _targets = HM.empty
    , _parts = Parts $ HM.fromList $ catMaybes
      [ flip fmap topDrumDiff $ \diff -> (FlexDrums ,) def
        { partDrums = Just PartDrums
          { drumsDifficulty  = translateDifficulty (dtx_DLEVEL diff) (dtx_DLVDEC diff)
          , drumsMode        = DrumsReal
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

importDTX :: (SendMessage m, MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
importDTX fin dout = do
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
  importSetDef Nothing setDef dout
