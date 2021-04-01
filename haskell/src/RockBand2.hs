{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module RockBand2 (convertMIDI, dryVoxAudio) where

import           Control.Monad                    (guard)
import           Control.Monad.Trans.StackTrace
import           Data.Conduit.Audio               (AudioSource)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List                        (inits, tails)
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (listToMaybe, mapMaybe)
import qualified Data.Set                         as Set
import           DryVox                           (sineDryVox)
import           Guitars                          (guitarify')
import           Overdrive                        (fixPartialUnisons)
import           RockBand.Codec                   (mapTrack)
import           RockBand.Codec.Drums             as Drums
import           RockBand.Codec.Events
import qualified RockBand.Codec.File              as F
import           RockBand.Codec.Five              as Five
import           RockBand.Codec.Venue
import           RockBand.Codec.Vocal
import           RockBand.Common                  (Difficulty (..), Edge (..),
                                                   blipEdgesRB_, edgeBlipsRB_)
import qualified Sound.MIDI.Util                  as U

dryVoxAudio :: (Monad m) => F.Song (F.FixedFile U.Beats) -> AudioSource m Float
dryVoxAudio f = sineDryVox $ mapTrack (U.applyTempoTrack $ F.s_tempos f)
  $ F.fixedPartVocals $ F.s_tracks f

convertMIDI :: (SendMessage m) => F.Song (F.FixedFile U.Beats) -> StackTraceT m (F.Song (F.FixedFile U.Beats))
convertMIDI mid = fixUnisons mid
  { F.s_tracks = mempty
    { F.fixedPartDrums = fixDrumColors $ let
      pd = F.fixedPartDrums $ F.s_tracks mid
      in pd
        -- note: we don't have to remove tom markers, Magma v1 is fine with them
        { drumSingleRoll = RTB.empty
        , drumDoubleRoll = RTB.empty
        , drumKick2x = RTB.empty
        , drumDifficulties = flip fmap (drumDifficulties pd) $ \dd -> dd
          { drumMix = flip fmap (drumMix dd) $ \case
            (aud, DiscoNoFlip) -> (aud, NoDisco)
            x                        -> x
          }
        , drumAnimation = let
          anims = flip fmap (drumAnimation pd) $ \case
            -- these were added in RB3
            Snare SoftHit hand -> Snare HardHit hand
            Ride LH            -> Hihat LH
            Crash2 hit LH      -> Crash1 hit LH
            x                  -> x
          in RTB.flatten $ fmap nubOrd $ RTB.collectCoincident anims
          -- we do nub for when a song, inexplicably,
          -- has simultaneous "soft snare LH" and "hard snare LH"
        }
    , F.fixedPartGuitar = fixFiveColors $ fixGB True $ F.fixedPartGuitar $ F.s_tracks mid
    , F.fixedPartBass = fixFiveColors $ fixGB False $ F.fixedPartBass $ F.s_tracks mid
    , F.fixedPartVocals = (F.fixedPartVocals $ F.s_tracks mid)
      { vocalLyricShift = RTB.empty
      , vocalRangeShift = RTB.empty
      }
    , F.fixedEvents = (F.fixedEvents $ F.s_tracks mid) { eventsSections = RTB.empty }
    , F.fixedBeat = F.fixedBeat $ F.s_tracks mid
    , F.fixedVenue = let
      v2 = compileVenueRB2 $ F.fixedVenue $ F.s_tracks mid
      in case endPosn of
        Nothing  -> v2 -- shouldn't happen
        Just end -> mapTrack (U.trackTake end) v2
        -- the trackTake is because otherwise new blips
        -- introduced in rb3->rb2 can go past the end event
    }
  } where
    endPosn :: Maybe U.Beats
    endPosn = listToMaybe $ toList $ fmap (fst . fst) $ RTB.viewL
      $ eventsEnd $ F.fixedEvents $ F.s_tracks mid
    fixGB hasSolos t = t
      { fiveTremolo = RTB.empty
      , fiveTrill = RTB.empty
      , fiveSolo = if hasSolos then fiveSolo t else RTB.empty
      }
    fixUnisons song = let
      gtr  = F.fixedPartGuitar $ F.s_tracks song
      bass = F.fixedPartBass   $ F.s_tracks song
      drum = F.fixedPartDrums  $ F.s_tracks song
      in if not $ nullFive gtr || nullFive bass || nullDrums drum
        then fixPartialUnisons [F.FlexGuitar, F.FlexBass, F.FlexDrums] song
        else return song

fixFiveColors :: FiveTrack U.Beats -> FiveTrack U.Beats
fixFiveColors trk = let
  expert = maybe RTB.empty fiveGems $ Map.lookup Expert $ fiveDifficulties trk
  usedColors = Set.fromList $ flip mapMaybe (RTB.getBodies expert) $ \case
    EdgeOn _ color -> Just color
    EdgeOff _      -> Nothing
  in trk
    { fiveDifficulties = flip Map.mapWithKey (fiveDifficulties trk) $ \diff fd -> case diff of
      Expert -> fd
      _      -> fd { fiveGems = useColorsFive usedColors $ fiveGems fd }
    }

useColorsFive :: Set.Set Five.Color -> RTB.T U.Beats (Edge () Five.Color) -> RTB.T U.Beats (Edge () Five.Color)
useColorsFive cols rtb = let
  gtr = guitarify' $ edgeBlipsRB_ rtb
  present = Set.fromList $ flip mapMaybe (RTB.getBodies rtb) $ \case
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
  ::                  Five.Color
  ->  RTB.T U.Beats ([Five.Color], Maybe U.Beats)
  -> [RTB.T U.Beats ([Five.Color], Maybe U.Beats)]
useColorFive newColor rtb = do
  -- TODO sort this better (move closer colors first)
  (before, (t, (oldColors, len)), after) <- focuses $ reverse $ RTB.toPairList rtb
  oldColor <- oldColors
  let newColors = map (\c -> if c == oldColor then newColor else c) oldColors
  guard $ elem oldColor $ concatMap (\(_, (cols, _)) -> cols) $ before ++ after
  return $ RTB.fromPairList $ reverse $ before ++ [(t, (newColors, len))] ++ after

fixDrumColors :: DrumTrack U.Beats -> DrumTrack U.Beats
fixDrumColors trk = let
  expert = fmap fst $ maybe RTB.empty drumGems $ Map.lookup Expert $ drumDifficulties trk
  usedColors = Set.fromList $ RTB.getBodies expert
  in trk
    { drumDifficulties = flip Map.mapWithKey (drumDifficulties trk) $ \diff dd -> case diff of
      Expert -> dd
      _      -> dd { drumGems = fmap (, VelocityNormal) $ useColorsDrums usedColors expert $ fmap fst $ drumGems dd }
    }

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
