{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
module Onyx.Import.GuitarPro where

import           Control.Monad                    (forM, guard)
import           Data.Bifunctor                   (first)
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   listToMaybe)
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           Onyx.GuitarPro
import           Onyx.Import.Base
import           Onyx.MIDI.Common
import qualified Onyx.MIDI.Track.File             as RBFile
import           Onyx.MIDI.Track.ProGuitar        (GtrBase (..), GtrTuning (..),
                                                   getStringIndex,
                                                   tuningPitches)
import           Onyx.MIDI.Track.Rocksmith
import           Onyx.Project
import           Onyx.StackTrace
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

rhythmLength :: Rhythm -> Maybe U.Beats
rhythmLength r = do
  base <- case r.noteValue of
    "Whole"   -> return 4
    "Half"    -> return 2
    "Quarter" -> return 1
    "Eighth"  -> return 0.5
    "16th"    -> return 0.25
    "32nd"    -> return 0.125
    _         -> Nothing
  let dots = maybe 0 (.count) r.augmentationDot
      dotRatio = 2 - 2 ^^ (negate dots)
      tupleRatio = maybe 1 (\tup -> fromIntegral tup.den / fromIntegral tup.num) r.primaryTuplet
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
  timeSigs <- forM (zip (V.toList gpif.masterBars) [0..]) \(mb, mbIndex) -> do
    inside ("MasterBar index " <> show (mbIndex :: Int)) do
    case readTimeSig mb.time of
      Nothing -> fatal $ "Couldn't understand time signature: " <> show mb.time
      Just ts -> return ts
  let assembleMMap sigs = foldr ($) RNil
        $ zipWith Wait (0 : map U.timeSigLength sigs) sigs
      mmap = U.measureMapFromTimeSigs U.Error $ assembleMMap timeSigs
  tempoChanges <- catMaybes <$> forM (V.toList gpif.masterTrack.automations) \auto -> do
    if auto.type_ == "Tempo"
      then case readTempo auto.value of
        Nothing  -> fatal $ "Couldn't understand tempo change: " <> show auto.value
        Just bps -> let
          posn = U.unapplyMeasureMap mmap (auto.bar, realToFrac auto.position)
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
      getTrack  = lookupTable "Track"  (.id_) (.tracks )
      getBar    = lookupTable "Bar"    (.id_) (.bars   )
      getVoice  = lookupTable "Voice"  (.id_) (.voices )
      getBeat   = lookupTable "Beat"   (.id_) (.beats  )
      getNote   = lookupTable "Note"   (.id_) (.notes  )
      getRhythm = lookupTable "Rhythm" (.id_) (.rhythms)

  forM (zip gpif.masterTrack.tracks [0..]) \(trackID, trackBarIndex) -> do
    inside ("<Track id=\"" <> show trackID <> "\">") do
    track <- getTrack trackID
    let tuningsGP = do
          staff <- toList track.staves >>= V.toList
          Property "Tuning" (PropertyTuning t) <- V.toList staff.properties.properties
          return t
        tuningsGPX = do
          Property "Tuning" (PropertyTuning t) <- toList track.properties >>= V.toList . (.properties)
          return t
    tuning <- case tuningsGP <> tuningsGPX of
      []    -> fatal "No tuning found for any of track's staves"
      t : _ -> return GtrTuning
        -- TODO maybe identify normal tunings, also use tuning_Instrument ("Guitar"/"Bass")
        { gtrBase = GtrCustom t.pitches
        , gtrOffsets = []
        , gtrGlobal = 0
        , gtrCapo = 0 -- TODO
        }
    let stringLookup = do
          str <- [minBound .. maxBound]
          return (getStringIndex (length $ tuningPitches tuning) str, str)
    gotBars <- forM (zip (V.toList gpif.masterBars) [0..]) \(mb, mbIndex) -> do
      inside ("MasterBar index " <> show (mbIndex :: Int)) do
      bar <- case drop trackBarIndex mb.bars of
        []    -> fatal "No bar found for the track in this master bar"
        i : _ -> getBar i
      voices <- mapM getVoice $ filter (>= 0) bar.voices
      gotVoices <- forM voices \voice -> do
        beats <- mapM getBeat voice.beats
        funcs <- forM beats \beat -> do
          notes <- mapM getNote $ fromMaybe [] beat.notes
          -- TODO don't sustain if staccato
          rhythm <- getRhythm beat.rhythm.ref
          gotNotes <- forM notes \note -> do
            let props = V.toList note.properties.properties
            string <- case [n | Property "String" (PropertyString n) <- props] of
              []    -> fatal $ "No String set for note id = " <> show note.id_
              n : _ -> return n
            midiString <- case lookup string stringLookup of
              Nothing -> fatal $ "Tuning doesn't contain string with index " <> show string
              Just s  -> return s
            fret <- case [n | Property "Fret" (PropertyFret n) <- props] of
              []    -> fatal $ "No Fret set for note id = " <> show note.id_
              n : _ -> return n
            let enabled = [x | Property x PropertyEnable <- props]
                htype = listToMaybe [x | Property "HarmonicType" (PropertyHType x) <- props]
                _slide = listToMaybe [x | Property "Slide" (PropertyFlags x) <- props]
                mods = concat
                  [ [ModPalmMute      | elem "PalmMuted" enabled]
                  , [ModMute          | elem "Muted"     enabled]
                  , [ModAccent        | isJust note.accent]
                  -- see tap harmonics note below
                  , [ModHarmonic      | elem "Harmonic"  enabled && elem htype [Just "Natural", Just "Tap"]]
                  , [ModHarmonicPinch | elem "Harmonic"  enabled && htype == Just "Pinch"                  ]
                  , [ModTap           | elem "Tapped"    enabled || htype == Just "Tap"                    ]
                  , [ModSlap          | elem "Slapped"   enabled]
                  , [ModPluck         | elem "Popped"    enabled]
                  , [ModHammerOn      | elem "LeftHandTapped" enabled] -- TODO normal hammerons
                  ]
                -- for RS readability, make all fret hand mutes "open"
                fret' = if elem ModMute mods then 0 else fret
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
            return ((fret', mods), midiString, note)
          duration <- case rhythmLength rhythm of
            Nothing  -> fatal $ "Couldn't understand note duration: " <> show rhythm
            Just bts -> return bts
          let ons = do
                (fretMods, str, note) <- gotNotes
                guard $ maybe True (not . (.destination)) note.tie
                return $ EdgeOn fretMods str
              offs = do
                (_fretMods, str, note) <- gotNotes
                guard $ maybe True (not . (.origin)) note.tie
                return $ EdgeOff str
          return \rest -> case beat.graceNotes of
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
      measureLength <- case readTimeSig mb.time of
        Nothing -> fatal $ "Couldn't understand time signature: " <> show mb.time
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
    return (track.name, tuning, rs)

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
    { metadata = def'
      { title        = Just gpif.score.title
      , artist       = Just gpif.score.artist
      , album        = Just gpif.score.album
      , fileAlbumArt = Nothing
      }
    , jammit = HM.empty
    , targets = HM.empty
    , global = def'
      { _fileMidi            = SoftFile "notes.mid" $ SoftChart mid
      , _fileSongAnim        = Nothing
      , _backgroundVideo     = Nothing
      , _fileBackgroundImage = Nothing
      }
    , audio = HM.empty
    , plans = HM.singleton "dummy" $ StandardPlan StandardPlanInfo
      { song        = Nothing
      , countin     = Countin []
      , parts       = Parts HM.empty
      , crowd       = Nothing
      , comments    = []
      , tuningCents = 0
      , fileTempo   = Nothing
      }
    , parts = Parts $ HM.fromList $ do
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

