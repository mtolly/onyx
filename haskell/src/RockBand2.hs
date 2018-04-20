{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module RockBand2 (convertMIDI, dryVoxAudio) where

import           Control.Monad                    (guard)
import           Control.Monad.Trans.StackTrace
import           Data.Conduit.Audio               (AudioSource)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List                        (inits, nub, tails)
import           Data.Maybe                       (listToMaybe)
import qualified Data.Set                         as Set
import           DryVox                           (sineDryVox)
import           Guitars                          (guitarify)
import           Overdrive                        (fixPartialUnisons)
import           RockBand.Codec.Drums             (nullDrums)
import           RockBand.Codec.Events
import qualified RockBand.Codec.File              as F
import           RockBand.Codec.Five              (nullFive)
import           RockBand.Codec.Venue
import           RockBand.Codec.Vocal
import           RockBand.Common                  (Difficulty (..),
                                                   LongNote (..), joinEdges,
                                                   splitEdges)
import qualified RockBand.Drums                   as Drums
import qualified RockBand.FiveButton              as Five
import qualified RockBand.Vocals                  as Vox
import qualified Sound.MIDI.Util                  as U

dryVoxAudio :: (Monad m) => F.Song (F.FixedFile U.Beats) -> AudioSource m Float
dryVoxAudio f = sineDryVox $ U.applyTempoTrack (F.s_tempos f)
  $ Vox.vocalToLegacy $ F.fixedPartVocals $ F.s_tracks f

convertMIDI :: (SendMessage m) => F.Song (F.FixedFile U.Beats) -> StackTraceT m (F.Song (F.FixedFile U.Beats))
convertMIDI mid = fixUnisons mid
  { F.s_tracks = mempty
    { F.fixedPartDrums = Drums.drumsFromLegacy $ fixDrumColors $ fixDoubleEvents $
      flip RTB.mapMaybe (Drums.drumsToLegacy $ F.fixedPartDrums $ F.s_tracks mid) $ \case
        -- Drums.ProType{} -> Nothing -- Magma is fine with pro markers
        Drums.SingleRoll{} -> Nothing
        Drums.DoubleRoll{} -> Nothing
        Drums.Kick2x -> Nothing
        Drums.DiffEvent diff (Drums.Mix aud Drums.DiscoNoFlip) ->
          Just $ Drums.DiffEvent diff $ Drums.Mix aud Drums.NoDisco
        Drums.Animation a -> Just $ Drums.Animation $ case a of
          -- these were added in RB3
          Drums.Snare Drums.SoftHit hand -> Drums.Snare Drums.HardHit hand
          Drums.Ride Drums.LH            -> Drums.Hihat Drums.LH
          Drums.Crash2 hit Drums.LH      -> Drums.Crash1 hit Drums.LH
          _                              -> a
        x -> Just x
    , F.fixedPartGuitar = Five.fiveFromLegacy $ fixFiveColors $ fixGB True $ Five.fiveToLegacy $ F.fixedPartGuitar $ F.s_tracks mid
    , F.fixedPartBass = Five.fiveFromLegacy $ fixFiveColors $ fixGB False $ Five.fiveToLegacy $ F.fixedPartBass $ F.s_tracks mid
    , F.fixedPartVocals = (F.fixedPartVocals $ F.s_tracks mid)
      { vocalLyricShift = RTB.empty
      , vocalRangeShift = RTB.empty
      }
    , F.fixedEvents = (F.fixedEvents $ F.s_tracks mid) { eventsSections = RTB.empty }
    , F.fixedBeat = F.fixedBeat $ F.s_tracks mid
    , F.fixedVenue = compileVenueRB2 $ F.fixedVenue $ F.s_tracks mid
    -- TODO replace lighting at posn 0 with [verse]
    -- and cut off blips starting a beat before endPosn
    }
  } where
    endPosn :: Maybe U.Beats
    endPosn = listToMaybe $ toList $ fmap (fst . fst) $ RTB.viewL
      $ eventsEnd $ F.fixedEvents $ F.s_tracks mid
    fixGB hasSolos t = flip RTB.mapMaybe t $ \case
      Five.Tremolo{} -> Nothing
      Five.Trill{} -> Nothing
      Five.Solo{} | not hasSolos -> Nothing
      e -> Just e
    -- this fixes when a song, inexplicably, has simultaneous "soft snare LH" and "hard snare LH"
    fixDoubleEvents = RTB.flatten . fmap nub . RTB.collectCoincident
    fixUnisons song = let
      gtr  = F.fixedPartGuitar $ F.s_tracks song
      bass = F.fixedPartBass   $ F.s_tracks song
      drum = F.fixedPartDrums  $ F.s_tracks song
      in if not $ nullFive gtr || nullFive bass || nullDrums drum
        then fixPartialUnisons [F.FlexGuitar, F.FlexBass, F.FlexDrums] song
        else return song

fixFiveColors :: RTB.T U.Beats Five.Event -> RTB.T U.Beats Five.Event
fixFiveColors rtb = let
  getDiff d = RTB.partitionMaybe $ \case
    Five.DiffEvent d' (Five.Note ln) | d == d' -> Just ln
    _                                          -> Nothing
  (easy  , notEasy  ) = getDiff Easy rtb
  (medium, notMedium) = getDiff Medium notEasy
  (hard  , notHard  ) = getDiff Hard notMedium
  (expert, _        ) = getDiff Expert notHard
  usedColors = Set.fromList $ concatMap toList $ RTB.getBodies expert
  easy'   = makeDiff Easy   $ useColorsFive usedColors easy
  medium' = makeDiff Medium $ useColorsFive usedColors medium
  hard'   = makeDiff Hard   $ useColorsFive usedColors hard
  makeDiff d = fmap $ Five.DiffEvent d . Five.Note
  in foldr RTB.merge notHard [easy', medium', hard']

useColorsFive :: Set.Set Five.Color -> RTB.T U.Beats (LongNote () Five.Color) -> RTB.T U.Beats (LongNote () Five.Color)
useColorsFive cols rtb = let
  gtr = joinEdges $ guitarify rtb
  present = Set.fromList $ concatMap toList $ RTB.getBodies rtb
  missing = Set.difference cols present
  good = foldl (>>=) [gtr] $ map useColorFive $ Set.toDescList missing
  in if Set.null missing then rtb else case good of
    []    -> rtb
    g : _ -> RTB.flatten $ fmap (traverse toList) $ splitEdges g

focuses :: [a] -> [([a], a, [a])]
focuses [] = []
focuses xs = zip3 (inits xs) xs (tail $ tails xs)

useColorFive
  ::                      Five.Color
  ->  RTB.T U.Beats ((), [Five.Color], Maybe U.Beats)
  -> [RTB.T U.Beats ((), [Five.Color], Maybe U.Beats)]
useColorFive newColor rtb = do
  -- TODO sort this better (move closer colors first)
  (before, (t, ((), oldColors, len)), after) <- focuses $ reverse $ RTB.toPairList rtb
  oldColor <- oldColors
  let newColors = map (\c -> if c == oldColor then newColor else c) oldColors
  guard $ elem oldColor $ concatMap (\(_, (_, cols, _)) -> cols) $ before ++ after
  return $ RTB.fromPairList $ reverse $ before ++ [(t, ((), newColors, len))] ++ after

fixDrumColors :: RTB.T U.Beats Drums.Event -> RTB.T U.Beats Drums.Event
fixDrumColors rtb = let
  getDiff d = RTB.partitionMaybe $ \case
    Drums.DiffEvent d' (Drums.Note gem) | d == d' -> Just gem
    _                                             -> Nothing
  (easy  , notEasy  ) = getDiff Easy rtb
  (medium, notMedium) = getDiff Medium notEasy
  (hard  , notHard  ) = getDiff Hard notMedium
  (expert, _        ) = getDiff Expert notHard
  usedColors = Set.fromList $ RTB.getBodies expert
  easy'   = makeDiff Easy   $ useColorsDrums usedColors expert easy
  medium' = makeDiff Medium $ useColorsDrums usedColors expert medium
  hard'   = makeDiff Hard   $ useColorsDrums usedColors expert hard
  makeDiff d = fmap $ Drums.DiffEvent d . Drums.Note
  in foldr RTB.merge notHard [easy', medium', hard']

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
