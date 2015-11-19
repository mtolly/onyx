{-# LANGUAGE LambdaCase #-}
module OnyxiteDisplay.Process where

import           Control.Monad                    (guard)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (listToMaybe, mapMaybe)
import qualified RockBand.Beat                    as Beat
import           RockBand.Common                  (Difficulty (..))
import qualified RockBand.Drums                   as Drums
import qualified RockBand.FiveButton              as Five
import qualified RockBand.ProKeys                 as PK
import qualified RockBand.Vocals                  as Vox
import qualified Sound.MIDI.Util                  as U

data Sustainable a
  = SustainEnd
  | Note a
  | Sustain a
  deriving (Eq, Ord, Show, Read)

data GuitarNoteType = Strum | HOPO
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Five = Five
  { fiveNotes  :: Map.Map Five.Color (Map.Map U.Seconds (Sustainable GuitarNoteType))
  , fiveSolo   :: Map.Map U.Seconds Bool
  , fiveEnergy :: Map.Map U.Seconds Bool
  } deriving (Eq, Ord, Show)

data Drums = Drums
  { drumNotes  :: Map.Map U.Seconds [Drums.Gem Drums.ProType]
  , drumSolo   :: Map.Map U.Seconds Bool
  , drumEnergy :: Map.Map U.Seconds Bool
  } deriving (Eq, Ord, Show)

data ProKeys = ProKeys
  { proKeysNotes  :: Map.Map PK.Pitch (Map.Map U.Seconds (Sustainable ()))
  , proKeysRanges :: Map.Map U.Seconds PK.LaneRange
  , proKeysSolo   :: Map.Map U.Seconds Bool
  , proKeysEnergy :: Map.Map U.Seconds Bool
  } deriving (Eq, Ord, Show)

removeStubs :: (Ord a) => RTB.T U.Beats (Sustainable a) -> RTB.T U.Beats (Sustainable a)
removeStubs = go . RTB.normalize where
  go rtb = case RTB.viewL rtb of
    Nothing              -> RTB.empty
    Just ((dt, e), rtb') -> case e of
      Note    nt -> RTB.cons dt (Note nt) $ go rtb'
      Sustain nt -> case RTB.viewL rtb' of
        Nothing                         -> RTB.empty
        Just ((dt', SustainEnd), rtb'') -> if dt' <= 1/4
          then RTB.cons dt (Note    nt) $ RTB.delay dt' $ go rtb''
          else RTB.cons dt (Sustain nt) $ RTB.cons  dt' SustainEnd $ go rtb''
        _                               -> error "removeStubs: double note-on"
      SustainEnd -> RTB.delay dt $ go rtb'

trackToMap :: (Ord a) => U.TempoMap -> RTB.T U.Beats a -> Map.Map U.Seconds a
trackToMap tmap = Map.fromList . ATB.toPairList . RTB.toAbsoluteEventList 0 . U.applyTempoTrack tmap . RTB.normalize

processFive :: Maybe U.Beats -> U.TempoMap -> RTB.T U.Beats Five.Event -> Five
processFive hopoThreshold tmap trk = let
  expert = flip RTB.mapMaybe trk $ \case Five.DiffEvent Expert e -> Just e; _ -> Nothing
  assigned = case hopoThreshold of
    Just threshold -> Five.assignHOPO threshold expert
    Nothing -> flip RTB.mapMaybe expert $ \case
      Five.Note True  color -> Just $ Five.Strum   color
      Five.Note False color -> Just $ Five.NoteOff color
      _ -> Nothing
  getColor color = trackToMap tmap $ removeStubs $ flip RTB.mapMaybe assigned $ \case
    Five.NoteOff c -> guard (c == color) >> Just SustainEnd
    Five.Strum   c -> guard (c == color) >> Just (Sustain Strum)
    Five.HOPO    c -> guard (c == color) >> Just (Sustain HOPO )
  notes = Map.fromList $ do
    color <- [minBound .. maxBound]
    return (color, getColor color)
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Five.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Five.Overdrive b -> Just b; _ -> Nothing
  in Five notes solo energy

processDrums :: U.TempoMap -> RTB.T U.Beats Drums.Event -> Drums
processDrums tmap trk = let
  notes = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $
    U.applyTempoTrack tmap $ RTB.collectCoincident $ flip RTB.mapMaybe (Drums.assignToms trk) $ \case
      (Expert, gem) -> Just gem
      _             -> Nothing
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Drums.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Drums.Overdrive b -> Just b; _ -> Nothing
  in Drums notes solo energy

processProKeys :: U.TempoMap -> RTB.T U.Beats PK.Event -> ProKeys
processProKeys tmap trk = let
  notesForPitch p = trackToMap tmap $ removeStubs $ flip RTB.mapMaybe trk $ \case
    PK.Note b p' | p == p' -> Just $ if b then Sustain () else SustainEnd
    _                      -> Nothing
  notes = Map.fromList [ (p, notesForPitch p) | p <- [minBound .. maxBound] ]
  ranges = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.LaneShift r -> Just r; _ -> Nothing
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.Overdrive b -> Just b; _ -> Nothing
  in ProKeys notes ranges solo energy

type Beats = Map.Map U.Seconds Beat

data Beat
  = Bar
  | Beat
  | HalfBeat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

processBeat :: U.TempoMap -> RTB.T U.Beats Beat.Event -> Beats
processBeat tmap rtb = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0
  $ U.applyTempoTrack tmap $ flip fmap rtb $ \case
    Beat.Bar -> Bar
    Beat.Beat -> Beat
    -- TODO: add half-beats

data VocalNote
  = Pitched WordPart String Vox.Pitch
  | Slide                   Vox.Pitch
  | Talky   WordPart String
  | VocalEnd
  deriving (Eq, Ord, Show, Read)

data WordPart = WordStart | WordContinue
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data VocalPhrase
  = PhraseStart
  | EnergyPhraseStart
  | PhraseEnd
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Vocals = Vocals
  { vocalNotes   :: Map.Map U.Seconds VocalNote
  , vocalPhrases :: Map.Map U.Seconds VocalPhrase
  , vocalRange   :: Maybe (Vox.Pitch, Vox.Pitch)
  -- TODO: support range shifts
  } deriving (Eq, Ord, Show)

processVocals :: U.TempoMap -> RTB.T U.Beats Vox.Event -> Vocals
processVocals tmap trk = let
  notes = trackToMap tmap $ flip RTB.mapMaybe (RTB.collectCoincident trk) $ \evts ->
    if flip any evts $ \case Vox.Note _ False -> True; _ -> False
      then Just VocalEnd
      else let
        mpitch = listToMaybe $ flip mapMaybe evts $ \case Vox.Note p True -> Just p; _ -> Nothing
        mlyric = listToMaybe $ flip mapMaybe evts $ \case Vox.Lyric s -> Just s; _ -> Nothing
        part p lyric = case reverse lyric of
          '#' : rev -> Just $ Talky p $ reverse rev
          '^' : rev -> Just $ Talky p $ reverse rev
          _         -> mpitch >>= Just . Pitched p lyric
        in mlyric >>= \case
          "+"         -> mpitch >>= Just . Slide
          '-' : lyric -> part WordContinue lyric
          lyric       -> part WordStart    lyric
  phrases = trackToMap tmap $ flip RTB.mapMaybe (RTB.collectCoincident trk) $ \evts ->
    if any isPhraseStart evts
      then Just $ if any (== Vox.Overdrive True) evts
        then EnergyPhraseStart
        else PhraseStart
      else if any isPhraseEnd evts
        then Just PhraseEnd
        else Nothing
  isPhraseStart = \case Vox.Phrase True  -> True; Vox.Phrase2 True  -> True; _ -> False
  isPhraseEnd   = \case Vox.Phrase False -> True; Vox.Phrase2 False -> True; _ -> False
  pitches = flip mapMaybe (Map.elems notes) $ \case
    Pitched _ _ p -> Just p
    Slide       p -> Just p
    _             -> Nothing
  range = case pitches of
    p : ps -> Just (foldr min p ps, foldr max p ps)
    []     -> Just undefined
  in Vocals notes phrases range
