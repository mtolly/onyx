{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module OnyxiteDisplay.Process where

import           Control.Monad                    (guard)
import qualified Data.Aeson                       as A
import           Data.Char                        (toLower)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map.Strict                  as Map
import qualified RockBand.Beat                    as Beat
import           RockBand.Common                  (Difficulty (..))
import qualified RockBand.Drums                   as Drums
import qualified RockBand.FiveButton              as Five
import qualified RockBand.ProKeys                 as PK
import qualified Sound.MIDI.Util                  as U
import qualified Data.Text                        as T
import Data.Monoid ((<>))

class TimeFunctor f where
  mapTime :: (Real u) => (t -> u) -> f t -> f u

data Sustainable a
  = SustainEnd
  | Note a
  | Sustain a
  deriving (Eq, Ord, Show, Read)

data GuitarNoteType = Strum | HOPO
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Five t = Five
  { fiveNotes  :: Map.Map Five.Color (Map.Map t (Sustainable GuitarNoteType))
  , fiveSolo   :: Map.Map t Bool
  , fiveEnergy :: Map.Map t Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor Five where
  mapTime f (Five x y z) = Five (Map.map (Map.mapKeys f) x) (Map.mapKeys f y) (Map.mapKeys f z)

eventList :: (Real t) => Map.Map t a -> (a -> A.Value) -> A.Value
eventList evts f = A.toJSON $ map g $ Map.toAscList evts where
  g (secs, evt) = let
    secs' = A.Number $ realToFrac secs
    evt' = f evt
    in A.toJSON [secs', evt']

instance (Real t) => A.ToJSON (Five t) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ fiveNotes x) $ \(color, notes) ->
      (,) (T.pack $ map toLower $ show color) $ eventList notes $ \case
        SustainEnd -> "end"
        Note Strum -> "strum"
        Note HOPO -> "hopo"
        Sustain Strum -> "strum-sust"
        Sustain HOPO -> "hopo-sust"
    , (,) "solo" $ eventList (fiveSolo x) A.toJSON
    , (,) "energy" $ eventList (fiveEnergy x) A.toJSON
    ]

data Drums t = Drums
  { drumNotes  :: Map.Map t [Drums.Gem Drums.ProType]
  , drumSolo   :: Map.Map t Bool
  , drumEnergy :: Map.Map t Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor Drums where
  mapTime f (Drums x y z) = Drums (Map.mapKeys f x) (Map.mapKeys f y) (Map.mapKeys f z)

instance (Real t) => A.ToJSON (Drums t) where
  toJSON x = A.object
    [ (,) "notes" $ eventList (drumNotes x) $ let
      gem = A.String . \case
        Drums.Kick -> "kick"
        Drums.Red -> "red"
        Drums.Pro Drums.Yellow Drums.Cymbal -> "y-cym"
        Drums.Pro Drums.Yellow Drums.Tom -> "y-tom"
        Drums.Pro Drums.Blue Drums.Cymbal -> "b-cym"
        Drums.Pro Drums.Blue Drums.Tom -> "b-tom"
        Drums.Pro Drums.Green Drums.Cymbal -> "g-cym"
        Drums.Pro Drums.Green Drums.Tom -> "g-tom"
      in A.toJSON . map gem
    , (,) "solo" $ eventList (drumSolo x) A.toJSON
    , (,) "energy" $ eventList (drumEnergy x) A.toJSON
    ]

data ProKeys t = ProKeys
  { proKeysNotes  :: Map.Map PK.Pitch (Map.Map t (Sustainable ()))
  , proKeysRanges :: Map.Map t PK.LaneRange
  , proKeysSolo   :: Map.Map t Bool
  , proKeysEnergy :: Map.Map t Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor ProKeys where
  mapTime f (ProKeys w x y z) = ProKeys (Map.map (Map.mapKeys f) w) (Map.mapKeys f x) (Map.mapKeys f y) (Map.mapKeys f z)

showPitch :: PK.Pitch -> T.Text
showPitch = \case
  PK.RedYellow k -> "ry-" <> showKey k
  PK.BlueGreen k -> "bg-" <> showKey k
  PK.OrangeC -> "o-c"
  where showKey = T.pack . map toLower . show

instance (Real t) => A.ToJSON (ProKeys t) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ proKeysNotes x) $ \(p, notes) ->
      (,) (showPitch p) $ eventList notes $ \case
        SustainEnd -> "end"
        Note () -> "note"
        Sustain () -> "sust"
    , (,) "ranges" $ eventList (proKeysRanges x) $ A.toJSON . map toLower . drop 5 . show
    , (,) "solo" $ eventList (proKeysSolo x) A.toJSON
    , (,) "energy" $ eventList (proKeysEnergy x) A.toJSON
    ]

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

processFive :: Maybe U.Beats -> U.TempoMap -> RTB.T U.Beats Five.Event -> Five U.Seconds
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

processDrums :: U.TempoMap -> RTB.T U.Beats Drums.Event -> Drums U.Seconds
processDrums tmap trk = let
  notes = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $
    U.applyTempoTrack tmap $ RTB.collectCoincident $ flip RTB.mapMaybe (Drums.assignToms trk) $ \case
      (Expert, gem) -> Just gem
      _             -> Nothing
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Drums.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Drums.Overdrive b -> Just b; _ -> Nothing
  in Drums notes solo energy

processProKeys :: U.TempoMap -> RTB.T U.Beats PK.Event -> ProKeys U.Seconds
processProKeys tmap trk = let
  notesForPitch p = trackToMap tmap $ removeStubs $ flip RTB.mapMaybe trk $ \case
    PK.Note b p' | p == p' -> Just $ if b then Sustain () else SustainEnd
    _                      -> Nothing
  notes = Map.fromList [ (p, notesForPitch p) | p <- [minBound .. maxBound] ]
  ranges = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.LaneShift r -> Just r; _ -> Nothing
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.Overdrive b -> Just b; _ -> Nothing
  in ProKeys notes ranges solo energy

data Beats t = Beats
  { beatLines :: Map.Map t Beat
  } deriving (Eq, Ord, Show)

instance TimeFunctor Beats where
  mapTime f (Beats x) = Beats $ Map.mapKeys f x

instance (Real t) => A.ToJSON (Beats t) where
  toJSON x = A.object
    [ (,) "lines" $ eventList (beatLines x) $ A.toJSON . map toLower . show
    ]

data Beat
  = Bar
  | Beat
  | HalfBeat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

processBeat :: U.TempoMap -> RTB.T U.Beats Beat.Event -> Beats U.Seconds
processBeat tmap rtb = Beats $ Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0
  $ U.applyTempoTrack tmap $ flip fmap rtb $ \case
    Beat.Bar -> Bar
    Beat.Beat -> Beat
    -- TODO: add half-beats
