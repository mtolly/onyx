{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Build.RockBand2 (convertMidiRB2, stripMidiMagmaV1, dryVoxAudio) where

import           Control.Monad                    (guard)
import           Data.Conduit.Audio               (AudioSource)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import           Data.List                        (inits, tails)
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (mapMaybe)
import qualified Data.Set                         as Set
import           Onyx.Guitar                      (guitarify')
import           Onyx.MIDI.Common                 (Difficulty (..), Edge (..),
                                                   blipEdgesRB_, edgeBlips_,
                                                   minSustainLengthRB)
import           Onyx.MIDI.Read                   (mapTrack)
import           Onyx.MIDI.Track.Drums            as Drums
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.MIDI.Track.Venue
import           Onyx.MIDI.Track.Vocal
import           Onyx.Overdrive                   (fixPartialUnisons)
import           Onyx.StackTrace
import           Onyx.Vocal.DryVox                (sineDryVox)
import           Optics                           ((%~), (.~))
import qualified Sound.MIDI.Util                  as U

dryVoxAudio :: (Monad m) => F.Song (F.FixedFile U.Beats) -> AudioSource m Float
dryVoxAudio f = sineDryVox $ mapTrack (U.applyTempoTrack f.tempos) f.tracks.fixedPartVocals

-- Should be given a valid RB3 .mid
convertMidiRB2 :: (SendMessage m) => F.Song (F.FixedFile U.Beats) -> StackTraceT m (F.Song (F.FixedFile U.Beats))
convertMidiRB2 mid = fixUnisons mid
  { F.tracks = mempty
    { F.fixedPartDrums = fixDrumColors let
      pd = mid.tracks.fixedPartDrums
      in pd
        -- note: we don't have to remove tom markers, Magma v1 is fine with them
        { kick2x = RTB.empty
        , difficulties = pd.difficulties <&> \dd -> dd
          { mix = dd.mix <&> \case
            (aud, DiscoNoFlip) -> (aud, NoDisco)
            x                  -> x
          }
        , animation = let
          anims = pd.animation <&> \case
            -- these were added in RB3
            Snare SoftHit hand -> Snare HardHit hand
            Ride LH            -> Hihat LH
            Crash2 hit LH      -> Crash1 hit LH
            x                  -> x
          in RTB.flatten $ fmap nubOrd $ RTB.collectCoincident anims
          -- we do nub for when a song, inexplicably,
          -- has simultaneous "soft snare LH" and "hard snare LH"
        }
    , F.fixedPartGuitar = fixFiveColors mid.tracks.fixedPartGuitar
    , F.fixedPartBass   = fixFiveColors
      $ mid.tracks.fixedPartBass & #solo .~ RTB.empty
    , F.fixedPartVocals = mid.tracks.fixedPartVocals
      { lyricShift = RTB.empty
      , rangeShift = RTB.empty
      }
    , F.fixedEvents = mid.tracks.fixedEvents { sections = RTB.empty }
    , F.fixedBeat = mid.tracks.fixedBeat
    -- We now compile venue for RB2 already in Onyx.Build.RB3CH
    , F.fixedVenue = mid.tracks.fixedVenue
    -- include these for RB2 but remove for Magma (stripMidiMagmaV1)
    , F.fixedHarm1 = mid.tracks.fixedHarm1
    , F.fixedHarm2 = mid.tracks.fixedHarm2
    , F.fixedHarm3 = mid.tracks.fixedHarm3
    }
  } where
    fixUnisons :: (SendMessage m) => F.Song (F.FixedFile U.Beats) -> StackTraceT m (F.Song (F.FixedFile U.Beats))
    fixUnisons song = let
      gtr  = song.tracks.fixedPartGuitar
      bass = song.tracks.fixedPartBass
      drum = song.tracks.fixedPartDrums
      in if not $ nullFive gtr || nullFive bass || nullDrums drum
        then fixPartialUnisons [F.PartGuitar, F.PartBass, F.PartDrums] song
        else return song

-- Should be given the output of convertMidiRB2
stripMidiMagmaV1 :: F.Song (F.FixedFile U.Beats) -> F.Song (F.FixedFile U.Beats)
stripMidiMagmaV1 mid = mid
  { F.tracks = mid.tracks
    { F.fixedPartDrums  = noDrumLanes mid.tracks.fixedPartDrums
    , F.fixedPartGuitar = noFiveLanes mid.tracks.fixedPartGuitar
    , F.fixedPartBass   = noFiveLanes mid.tracks.fixedPartBass
    , F.fixedHarm1      = mempty
    , F.fixedHarm2      = mempty
    , F.fixedHarm3      = mempty
    , F.fixedVenue      = mid.tracks.fixedVenue
      -- Magma v2 is ok with these but not v1
      { fog = RTB.empty
      }
    }
  } where
    noDrumLanes dt = dt
      { singleRoll = RTB.empty
      , doubleRoll = RTB.empty
      }
    noFiveLanes ft = ft
      { trill   = RTB.empty
      , tremolo = RTB.empty
      }

fixFiveColors :: FiveTrack U.Beats -> FiveTrack U.Beats
fixFiveColors trk = let
  expert = maybe RTB.empty (.gems) $ Map.lookup Expert trk.difficulties
  usedColors = Set.fromList $ flip mapMaybe (RTB.getBodies expert) \case
    EdgeOn _ color -> Just color
    EdgeOff _      -> Nothing
  in trk & #difficulties .~ do
    flip Map.mapWithKey trk.difficulties \diff fd -> case diff of
      Expert -> fd
      _      -> fd & #gems %~ useColorsFive usedColors

useColorsFive :: (Ord color) => Set.Set color -> RTB.T U.Beats (Edge () color) -> RTB.T U.Beats (Edge () color)
useColorsFive cols rtb = let
  gtr = guitarify' $ edgeBlips_ minSustainLengthRB rtb
  present = Set.fromList $ flip mapMaybe (RTB.getBodies rtb) \case
    EdgeOn _ color -> Just color
    EdgeOff _      -> Nothing
  missing = Set.difference cols present
  good = foldl (>>=) [gtr] $ map useColorFive $ Set.toDescList missing
  in if Set.null missing then rtb else case good of
    []    -> rtb
    g : _ -> blipEdgesRB_ $ RTB.flatten $ fmap (\(colors, len) -> map (, len) colors) g

focuses :: [a] -> [([a], a, [a])]
focuses [] = []
focuses xs = zip3 (inits xs) xs (tail $ tails xs)

useColorFive
  :: (Ord color)
  =>                  color
  ->  RTB.T U.Beats ([color], Maybe U.Beats)
  -> [RTB.T U.Beats ([color], Maybe U.Beats)]
useColorFive newColor rtb = do
  -- TODO sort this better (move closer colors first)
  (before, (t, (oldColors, len)), after) <- focuses $ reverse $ RTB.toPairList rtb
  oldColor <- oldColors
  let newColors = map (\c -> if c == oldColor then newColor else c) oldColors
  guard $ elem oldColor $ concatMap (\(_, (cols, _)) -> cols) $ before ++ after
  return $ RTB.fromPairList $ reverse $ before ++ [(t, (newColors, len))] ++ after

fixDrumColors :: DrumTrack U.Beats -> DrumTrack U.Beats
fixDrumColors trk = let
  expert = fmap fst $ maybe RTB.empty (.gems) $ Map.lookup Expert trk.difficulties
  usedColors = Set.fromList $ RTB.getBodies expert
  in trk & #difficulties .~ do
    flip Map.mapWithKey trk.difficulties \diff dd -> case diff of
      Expert -> dd
      _      -> dd & #gems %~ fmap (, VelocityNormal) . useColorsDrums usedColors expert . fmap fst

useColorsDrums :: Set.Set (Drums.Gem ()) -> RTB.T U.Beats (Drums.Gem ()) -> RTB.T U.Beats (Drums.Gem ()) -> RTB.T U.Beats (Drums.Gem ())
useColorsDrums cols expert rtb = let
  drums = RTB.collectCoincident rtb
  present = Set.fromList $ RTB.getBodies rtb
  missing = Set.difference cols present
  expert' = RTB.collectCoincident expert
  good = foldl (>>=) [drums] $ map (useColorDrums expert') $ Set.toDescList missing
  in if Set.null missing then rtb else case good of
    []    -> rtb
    g : _ -> RTB.flatten g

useColorDrums
  ::  RTB.T U.Beats [Drums.Gem ()]
  ->                 Drums.Gem ()
  ->  RTB.T U.Beats [Drums.Gem ()]
  -> [RTB.T U.Beats [Drums.Gem ()]]
useColorDrums expert gem rtb = let
  annotated = RTB.mapMaybe annotate $ RTB.collectCoincident $ RTB.merge (fmap Left expert) (fmap Right rtb)
  annotate = \case
    [Left x, Right y] -> Just ( x, y)
    [Right y]         -> Just ([], y)
    [Left x]          -> Just (x, [])
    _                 -> error "RockBand2.useColorDrums: panic! impossible case while fixing drums reductions"
  removeX (t, (_, gems)) = (t, gems)
  in do
    (before, (t, (xgems, gems)), after) <- focuses $ reverse $ RTB.toPairList annotated
    let otherGems = concatMap (snd . snd) $ before ++ after
    guard $ elem gem xgems
    guard $ all (`elem` otherGems) gems
    return $ RTB.fromPairList $ reverse $ map removeX before ++ [(t, [gem])] ++ map removeX after
