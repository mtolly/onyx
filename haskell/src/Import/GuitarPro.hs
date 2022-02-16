{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
module Import.GuitarPro where

import           Config
import           Control.Monad                    (forM, guard)
import           Control.Monad.Trans.StackTrace
import           Data.Bifunctor                   (first)
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   listToMaybe)
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           GuitarPro
import           Import.Base
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.ProGuitar         (GtrBase (..), GtrTuning (..),
                                                   getStringIndex,
                                                   tuningPitches)
import           RockBand.Common
import           Rocksmith.MIDI
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

rhythmLength :: Rhythm -> Maybe U.Beats
rhythmLength r = do
  base <- case rhythm_NoteValue r of
    "Whole"   -> return 4
    "Half"    -> return 2
    "Quarter" -> return 1
    "Eighth"  -> return 0.5
    "16th"    -> return 0.25
    "32nd"    -> return 0.125
    _         -> Nothing
  let dots = maybe 0 aug_count $ rhythm_AugmentationDot r
      dotRatio = 2 - 2 ^^ (negate dots)
      tupleRatio = maybe 1 (\tup -> fromIntegral (tup_den tup) / fromIntegral (tup_num tup)) $ rhythm_PrimaryTuplet r
  return $ base * dotRatio * tupleRatio

readTimeSig :: T.Text -> Maybe U.TimeSig
readTimeSig ts = case T.splitOn "/" ts of
  [n, d] -> do
    num  <- fmap fromInteger               $ readMaybe $ T.unpack n
    unit <- fmap (\x -> 4 / fromInteger x) $ readMaybe $ T.unpack d
    return $ U.TimeSig (num * unit) unit
  _ -> Nothing

readTempo :: T.Text -> Maybe U.BPS
readTempo t = case T.words t of
  [x, y] -> do
    base  <- readMaybe $ T.unpack x :: Maybe Double
    ratio <- readMaybe $ T.unpack y :: Maybe Int
    -- 1 means base 8th notes per minute, 2 means base quarter notes per minute?
    return $ (realToFrac base / 60) / (4 / (2 ^^ ratio))
  _ -> Nothing

timingGPIF :: (Monad m) => GPIF -> StackTraceT m (U.TempoMap, U.MeasureMap)
timingGPIF gpif = do
  timeSigs <- forM (zip (V.toList $ gp_MasterBars gpif) [0..]) \(mb, mbIndex) -> do
    inside ("MasterBar index " <> show (mbIndex :: Int)) do
    case readTimeSig $ mb_Time mb of
      Nothing -> fatal $ "Couldn't understand time signature: " <> show (mb_Time mb)
      Just ts -> return ts
  let assembleMMap sigs = foldr ($) RNil
        $ zipWith Wait (0 : map U.timeSigLength sigs) sigs
      mmap = U.measureMapFromTimeSigs U.Error $ assembleMMap timeSigs
  tempoChanges <- catMaybes <$> forM (V.toList $ mt_Automations $ gp_MasterTrack gpif) \auto -> do
    if auto_Type auto == "Tempo"
      then case readTempo $ auto_Value auto of
        Nothing  -> fatal $ "Couldn't understand tempo change: " <> show (auto_Value auto)
        Just bps -> let
          posn = U.unapplyMeasureMap mmap (auto_Bar auto, realToFrac $ auto_Position auto)
          in return $ Just (posn, bps)
      else return Nothing
  let tmap = U.tempoMapFromBPS $ RTB.fromAbsoluteEventList $ ATB.fromPairList tempoChanges
  return (tmap, mmap)

fromGPIF :: (SendMessage m) => GPIF -> StackTraceT m [(T.Text, GtrTuning, RocksmithTrack U.Beats)]
fromGPIF gpif = do

  let lookupTable :: (Monad m) => String -> (a -> Int) -> (GPIF -> V.Vector a) -> Int -> StackTraceT m a
      lookupTable name getID getVect = let
        table = HM.fromList do
          x <- V.toList $ getVect gpif
          return (getID x, x)
        in \wantID -> case HM.lookup wantID table of
          Just x  -> return x
          Nothing -> fatal $ "Couldn't find " <> name <> " with id = " <> show wantID
      getTrack  = lookupTable "Track"  trk_id    gp_Tracks
      getBar    = lookupTable "Bar"    bar_id    gp_Bars
      getVoice  = lookupTable "Voice"  voice_id  gp_Voices
      getBeat   = lookupTable "Beat"   beat_id   gp_Beats
      getNote   = lookupTable "Note"   note_id   gp_Notes
      getRhythm = lookupTable "Rhythm" rhythm_id gp_Rhythms

  forM (zip (mt_Tracks $ gp_MasterTrack gpif) [0..]) \(trackID, trackBarIndex) -> do
    inside ("<Track id=\"" <> show trackID <> "\">") do
    track <- getTrack trackID
    let tunings = do
          staff <- V.toList $ trk_Staves track
          Property "Tuning" (PropertyTuning t) <- V.toList $ props_Properties $ staff_Properties staff
          return t
    tuning <- case tunings of
      []    -> fatal "No tuning found for any of track's staves"
      t : _ -> return GtrTuning
        -- TODO maybe identify normal tunings, also use tuning_Instrument ("Guitar"/"Bass")
        { gtrBase = GtrCustom $ tuning_Pitches t
        , gtrOffsets = []
        , gtrGlobal = 0
        , gtrCapo = 0 -- TODO
        }
    let stringLookup = do
          str <- [minBound .. maxBound]
          return (getStringIndex (length $ tuningPitches tuning) str, str)
    gotBars <- forM (zip (V.toList $ gp_MasterBars gpif) [0..]) \(mb, mbIndex) -> do
      inside ("MasterBar index " <> show (mbIndex :: Int)) do
      bar <- case drop trackBarIndex $ mb_Bars mb of
        []    -> fatal "No bar found for the track in this master bar"
        i : _ -> getBar i
      voices <- mapM getVoice $ filter (>= 0) $ bar_Voices bar
      gotVoices <- forM voices \voice -> do
        beats <- mapM getBeat $ voice_Beats voice
        funcs <- forM beats \beat -> do
          notes <- mapM getNote $ fromMaybe [] $ beat_Notes beat
          rhythm <- getRhythm $ rhythm_ref $ beat_Rhythm beat
          gotNotes <- forM notes \note -> do
            let props = V.toList $ props_Properties $ note_Properties note
            string <- case [n | Property "String" (PropertyString n) <- props] of
              []    -> fatal $ "No String set for note id = " <> show (note_id note)
              n : _ -> return n
            midiString <- case lookup string stringLookup of
              Nothing -> fatal $ "Tuning doesn't contain string with index " <> show string
              Just s  -> return s
            fret <- case [n | Property "Fret" (PropertyFret n) <- props] of
              []    -> fatal $ "No Fret set for note id = " <> show (note_id note)
              n : _ -> return n
            let enabled = [x | Property x PropertyEnable <- props]
                htype = listToMaybe [x | Property "HarmonicType" (PropertyHType x) <- props]
                slide = listToMaybe [x | Property "Slide" (PropertyFlags x) <- props]
                mods = concat
                  [ [ModPalmMute      | elem "PalmMuted" enabled]
                  , [ModMute          | elem "Muted"     enabled]
                  , [ModAccent        | isJust $ note_Accent note]
                  -- see tap harmonics note below
                  , [ModHarmonic      | elem "Harmonic"  enabled && elem htype [Just "Natural", Just "Tap"]]
                  , [ModHarmonicPinch | elem "Harmonic"  enabled && htype == Just "Pinch"                  ]
                  , [ModTap           | elem "Tapped"    enabled || htype == Just "Tap"                    ]
                  , [ModSlap          | elem "Slapped"   enabled]
                  , [ModPluck         | elem "Popped"    enabled]
                  , [ModHammerOn      | elem "LeftHandTapped" enabled] -- TODO normal hammerons
                  ]
                {-
                  remaining stuff to import:
                  ModVibrato, ModHammerOn, ModPullOff, ModSlide, ModSlideUnpitch, ModLink,
                  left hand info, ModRightHand, ModTremolo, ModPickUp, ModPickDown

                  hopos:
                  first note has HopoOrigin, second note has HopoDestination.
                  need to compute whether to put hammeron or pulloff on the second note

                  slides:
                  slide down to undefined endpoint has <Property name="Slide"><Flags>4</Flags></Property>
                  slide to following note (restrum) has <Property name="Slide"><Flags>1</Flags></Property>
                  slide to following note (no restrum) has <Property name="Slide"><Flags>2</Flags></Property>
                  also seen: 8, 16, 32, 18 (16 + 2), 20 (16 + 4)

                  tap harmonics:
                  in gp, example note (fusion collusion bass) has Fret 7, Harmonic enabled, HType Tap, and HFret 5.
                  so this means hold fret 7 and tap on fret 12 (7 + 5).
                  for rs, this should generate a hand position on 7, a note on fret 12, with tap + harmonic mods.
                -}
            return ((fret, mods), midiString, note)
          duration <- case rhythmLength rhythm of
            Nothing  -> fatal $ "Couldn't understand note duration: " <> show rhythm
            Just bts -> return bts
          let ons = do
                (fretMods, str, note) <- gotNotes
                guard $ maybe True (not . tie_destination) $ note_Tie note
                return $ EdgeOn fretMods str
              offs = do
                (_fretMods, str, note) <- gotNotes
                guard $ maybe True (not . tie_origin) $ note_Tie note
                return $ EdgeOff str
          return \rest -> case beat_GraceNotes beat of
            Nothing -> Wait 0 ons $ Wait duration offs rest
            Just _ -> case rest of
              Wait 0 nextOns (Wait nextDuration nextOffs rest') -> let
                (graceDuration, nextDuration') = if nextDuration > (1/2)
                  then (1/6, nextDuration - (1/6))
                  else (nextDuration * (1/3), nextDuration * (2/3))
                in Wait 0 ons
                  $ Wait graceDuration offs
                  $ Wait 0 nextOns
                  $ Wait nextDuration' nextOffs rest'
              _ -> rest -- TODO warn or something
        return $ foldr ($) RTB.empty funcs
      measureLength <- case readTimeSig $ mb_Time mb of
        Nothing -> fatal $ "Couldn't understand time signature: " <> show (mb_Time mb)
        Just ts -> return $ U.timeSigLength ts
      return \rest -> foldr RTB.merge (RTB.delay measureLength rest) gotVoices

    let flat = RTB.flatten $ foldr ($) RTB.empty gotBars
        rs = mempty
          { rsNotes = first fst <$> flat
          , rsModifiers = flip RTB.mapMaybe flat \case
            EdgeOn (_fret, []  ) _   -> Nothing
            EdgeOn (_fret, mods) str -> Just ([str], mods)
            EdgeOff _                -> Nothing
          }
    return (trk_Name track, tuning, rs)

importGPIF :: (SendMessage m) => GPIF -> Import m
importGPIF gpif level = do
  imported <- case level of
    ImportQuick -> return []
    ImportFull  -> fromGPIF gpif
  (tmap, mmap) <- timingGPIF gpif
  let mid = RBFile.Song tmap mmap mempty
        { RBFile.onyxParts = Map.fromList do
          (name, _, trk) <- imported
          let opart = mempty { RBFile.onyxPartRSGuitar = trk }
          return (RBFile.FlexExtra name, opart)
        }
  return SongYaml
    { _metadata = def'
      { _title        = Just $ score_Title  $ gp_Score gpif
      , _artist       = Just $ score_Artist $ gp_Score gpif
      , _album        = Just $ score_Album  $ gp_Score gpif
      , _fileAlbumArt = Nothing
      }
    , _jammit = HM.empty
    , _targets = HM.empty
    , _global = def'
      { _fileMidi            = SoftFile "notes.mid" $ SoftChart mid
      , _fileSongAnim        = Nothing
      , _backgroundVideo     = Nothing
      , _fileBackgroundImage = Nothing
      }
    , _audio = HM.empty
    , _plans = HM.singleton "dummy" $ Plan
      { _song         = Nothing
      , _countin      = Countin []
      , _planParts    = Parts HM.empty
      , _crowd        = Nothing
      , _planComments = []
      , _tuningCents  = 0
      , _fileTempo    = Nothing
      }
    , _parts = Parts $ HM.fromList $ do
      (name, tuning, _) <- imported
      let part = def
            { partProGuitar = Just PartProGuitar
              { pgDifficulty    = Tier 1
              , pgHopoThreshold = 170
              , pgTuning        = tuning
              , pgTuningRSBass  = Nothing
              , pgFixFreeform   = False
              , pgTones         = Nothing
              , pgPickedBass    = False
              }
            }
      return (RBFile.FlexExtra name, part)
    }

