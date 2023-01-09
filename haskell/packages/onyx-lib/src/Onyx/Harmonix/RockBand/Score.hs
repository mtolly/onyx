{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module Onyx.Harmonix.RockBand.Score where

import           Control.Applicative              (liftA2)
import           Control.Monad                    (guard, void)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.Guitar                      (applyStatus, fixSloppyNotes)
import           Onyx.Harmonix.GH2.PartGuitar
import           Onyx.MIDI.Common                 (Difficulty (..), Edge,
                                                   edgeBlips, edgeBlips_,
                                                   minSustainLengthRB,
                                                   pattern RNil, pattern Wait,
                                                   trackGlue)
import qualified Onyx.MIDI.Track.Drums            as Drums
import qualified Onyx.MIDI.Track.Events           as E
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import qualified Onyx.MIDI.Track.ProGuitar        as PG
import qualified Onyx.MIDI.Track.ProKeys          as PK
import qualified Onyx.MIDI.Track.Vocal            as Vox
import qualified Sound.MIDI.Util                  as U

data ScoreTrack
  = ScoreGuitar
  | ScoreBass
  | ScoreDrums
  | ScoreKeys
  | ScoreVocals
  | ScoreProGuitar
  | ScoreProBass
  | ScoreProDrums
  | ScoreProKeys
  | ScoreHarmonies
  deriving (Eq, Ord, Show, Enum, Bounded)

scoreTrackName :: ScoreTrack -> T.Text
scoreTrackName = \case
  ScoreGuitar    -> "Guitar"
  ScoreBass      -> "Bass"
  ScoreDrums     -> "Drums"
  ScoreKeys      -> "Keys"
  ScoreVocals    -> "Vocals"
  ScoreProGuitar -> "Pro Guitar"
  ScoreProBass   -> "Pro Bass"
  ScoreProDrums  -> "Pro Drums"
  ScoreProKeys   -> "Pro Keys"
  ScoreHarmonies -> "Harmonies"

data Stars a = Stars
  { stars1    :: a
  , stars2    :: a
  , stars3    :: a
  , stars4    :: a
  , stars5    :: a
  , starsGold :: a
  } deriving (Eq, Show, Functor, Foldable)

instance Applicative Stars where
  pure x = Stars x x x x x x
  Stars f1 f2 f3 f4 f5 fg <*> Stars x1 x2 x3 x4 x5 xg
    = Stars (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (fg xg)

new_instrument_thresholds :: ScoreTrack -> Stars Float
new_instrument_thresholds = \case
  ScoreGuitar    -> Stars 0.06 0.12 0.2  0.47 0.78 1.15
  ScoreBass      -> Stars 0.05 0.1  0.19 0.47 0.78 1.15
  ScoreDrums     -> Stars 0.06 0.12 0.2  0.45 0.75 1.09
  ScoreKeys      -> Stars 0.06 0.12 0.2  0.47 0.78 1.15
  ScoreVocals    -> Stars 0.05 0.11 0.19 0.46 0.77 1.06
  ScoreProGuitar -> Stars 0.06 0.12 0.2  0.47 0.78 1.15
  ScoreProBass   -> Stars 0.05 0.1  0.19 0.47 0.78 1.15
  -- dtb also has separate lines for 22-fret but they are the same as 17
  ScoreProDrums  -> Stars 0.06 0.12 0.2  0.45 0.75 1.09 -- not a separate line in dtb
  ScoreProKeys   -> Stars 0.06 0.12 0.2  0.47 0.78 1.15
  ScoreHarmonies -> Stars 0.05 0.11 0.19 0.46 0.77 1.06 -- not a separate line in dtb

new_bonus_thresholds :: Stars Float
new_bonus_thresholds = Stars 0.05 0.1 0.2 0.3 0.4 0.95

-- | This could be done with 'annotateMultiplier' but drums are way simpler
drumBase :: Int -> RTB.T t a -> Int
drumBase gem rtb = let
  len = length rtb
  gems1x = min 9 len
  gems2x = min 10 $ len - gems1x
  gems3x = min 10 $ len - gems1x - gems2x
  gems4x = len - gems1x - gems2x - gems3x
  in sum
    [ gems1x * gem
    , gems2x * gem * 2
    , gems3x * gem * 3
    , gems4x * gem * 4
    ]

perfectSoloBonus :: (NNC.C t) => Int -> RTB.T t Bool -> RTB.T t a -> Int
perfectSoloBonus perNote solo gems = sum $ fmap score $ applyStatus (fmap ((),) solo) $ void gems where
  score ([], _) = 0
  score _       = perNote

data VoxScoreEvent = PhraseEnd | PercNote
  deriving (Eq, Ord)

voxBaseAndSolo :: (NNC.C t) => Difficulty -> Vox.VocalTrack t -> (Int, Int)
voxBaseAndSolo diff vt = let
  phraseEnds = fmap (const PhraseEnd) $ RTB.collectCoincident $ RTB.merge
    (RTB.filter not $ Vox.vocalPhrase1 vt)
    (RTB.filter not $ Vox.vocalPhrase2 vt)
  perc = fmap (const PercNote) $ Vox.vocalPerc vt
  phrasePoints = case diff of
    Easy   -> 200
    Medium -> 400
    Hard   -> 800
    Expert -> 1000
  go _ [] = []
  go curMult (PhraseEnd : rest)
    -- normal phrase
    = Left (phrasePoints * curMult) : go (min 4 $ curMult + 1) rest
  go curMult xs@(PercNote : _)
    -- percussion phrase
    = case span (== PercNote) xs of
      (percs, after) -> Right (length percs * 100) : go curMult (drop 1 after)
  pieces = go 1 $ RTB.getBodies $ RTB.merge phraseEnds perc
  in (sum $ lefts pieces, sum $ rights pieces)

baseAndSolo :: F.FixedFile U.Beats -> (ScoreTrack, Difficulty) -> (Int, Int)
baseAndSolo mid (scoreTrack, diff) = let
  adjustGems breLanes = ignoreBRE breLanes . fixSloppyNotes (10 / 480)
  -- have to ignore notes under the BRE lanes, but not the ones after
  ignoreBRE breLanes = case E.eventsCoda $ F.fixedEvents mid of
    Wait breStart () _ -> case RTB.filter not $ U.trackDrop breStart breLanes of
      Wait breLength _ _ -> \gems -> let
        breEnd = breStart <> breLength
        in trackGlue breEnd (U.trackTake breStart gems) (U.trackDrop breEnd gems)
      RNil -> U.trackTake breStart -- shouldn't happen
    _            -> id
  getDrums gem = let
    trk = F.fixedPartDrums mid
    gems = adjustGems (Drums.drumActivation trk) $ Drums.computePro (Just diff) trk
    base = drumBase gem gems
    solo = perfectSoloBonus 100 (Drums.drumSolo trk) gems
    in (base, solo)
  getFive maxStreak getTrack = let
    trk = getTrack mid
    gems = adjustGems (Five.fiveBRE trk)
      $ edgeBlips_ minSustainLengthRB
      $ maybe RTB.empty Five.fiveGems
      $ Map.lookup diff $ Five.fiveDifficulties trk
    base = gbkBase 25 12 maxStreak $ fmap snd gems
    solo = perfectSoloBonus 100 (Five.fiveSolo trk) $ RTB.collectCoincident gems
    in (base, solo)
  -- From what I can tell 17 and 22 are same cutoff, and it uses the 22 chart
  getPG maxStreak get22 get17 = let
    trk = case get22 mid of
      trk22 | not $ PG.nullPG trk22 -> trk22
      _                             -> get17 mid
    gems
      = adjustGems (snd <$> PG.pgBRE trk)
      $ edgeBlips minSustainLengthRB
      $ maybe RTB.empty PG.pgNotes
      $ Map.lookup diff $ PG.pgDifficulties trk
    base = gbkBase 60 30 maxStreak $ maxChord2 $ fmap (\(_, (_, _), mlen) -> mlen) gems
    solo = perfectSoloBonus 150 (PG.pgSolo trk) $ RTB.collectCoincident gems
    in (base, solo)
  maxChord2 = RTB.flatten . fmap (take 2) . RTB.collectCoincident
  getVox getTrack = voxBaseAndSolo diff $ getTrack mid
  getPK = let
    trk = case diff of
      Easy   -> F.fixedPartRealKeysE mid
      Medium -> F.fixedPartRealKeysM mid
      Hard   -> F.fixedPartRealKeysH mid
      Expert -> F.fixedPartRealKeysX mid
    expert = F.fixedPartRealKeysX mid
    gems = adjustGems (PK.pkBRE expert) $ edgeBlips_ minSustainLengthRB $ PK.pkNotes trk
    base = gbkBase 60 30 4 $ fmap snd gems
    solo = perfectSoloBonus 100 (PK.pkSolo expert) $ RTB.collectCoincident gems
    in (base, solo)
  in case scoreTrack of
    ScoreDrums     -> getDrums 25
    ScoreProDrums  -> getDrums 30
    ScoreGuitar    -> getFive 4 F.fixedPartGuitar
    ScoreBass      -> getFive 6 F.fixedPartBass
    ScoreKeys      -> getFive 4 F.fixedPartKeys
    ScoreProGuitar -> getPG 4 F.fixedPartRealGuitar22 F.fixedPartRealGuitar
    ScoreProBass   -> getPG 6 F.fixedPartRealBass22 F.fixedPartRealBass
    ScoreVocals    -> getVox F.fixedPartVocals
    ScoreHarmonies -> getVox F.fixedHarm1
    ScoreProKeys   -> getPK

annotateMultiplier :: Int -> RTB.T t a -> RTB.T t (a, Int)
annotateMultiplier maxMult = RTB.fromPairList . go 1 9 . RTB.toPairList where
  go _       _       []   = []
  go curMult multLen evts = if curMult >= maxMult
    then map (addMult curMult) evts
    else case splitAt multLen evts of
      (xs, ys) -> map (addMult curMult) xs ++ go (curMult + 1) 10 ys
  addMult mult (dt, x) = (dt, (x, mult))

gbkBase :: Int -> Int -> Int -> RTB.T U.Beats (Maybe U.Beats) -> Int
gbkBase headPoints tailPoints maxStreak evts = let
  annotated = annotateMultiplier maxStreak $ RTB.collectCoincident evts
  in sum $ RTB.getBodies annotated >>= \(mlens, mult) -> do
    mlen <- mlens
    let tailTicks = maybe 0 (\bts -> floor $ toRational bts * toRational tailPoints) mlen
    return $ mult * (headPoints + tailTicks)

getScoreTracks :: F.FixedFile U.Beats -> [(ScoreTrack, Difficulty, (Int, Int))]
getScoreTracks mid = do
  strack <- [minBound .. maxBound]
  diff   <- [minBound .. maxBound]
  let (base, solo) = baseAndSolo mid (strack, diff)
  guard $ base /= 0
  return (strack, diff, (base, solo))

tracksToStars :: [(ScoreTrack, Difficulty, (Int, Int))] -> Stars (Maybe Int)
tracksToStars trks = let
  new_num_instruments_multiplier = case length trks of
    1 -> 1.0
    2 -> 1.26
    3 -> 1.52
    _ -> 1.8
  sumOfBases :: Stars Float
  sumOfBases = foldr (liftA2 (+)) (pure 0) $ flip map trks $ \(scoreTrack, _, (base, solo)) -> liftA2 (+)
    (((fromIntegral base * new_num_instruments_multiplier) *) <$> new_instrument_thresholds scoreTrack)
    ((fromIntegral solo *) <$> new_bonus_thresholds)
  allCutoffs = Just . (floor :: Float -> Int) <$> sumOfBases
  allExpert = all (\(_, diff, _) -> diff == Expert) trks
  in allCutoffs { starsGold = guard allExpert >> starsGold allCutoffs }

starCutoffs :: F.FixedFile U.Beats -> [(ScoreTrack, Difficulty)] -> Stars (Maybe Int)
starCutoffs mid trks = tracksToStars $ do
  pair@(strack, diff) <- trks
  let (base, solo) = baseAndSolo mid pair
  return (strack, diff, (base, solo))

-- GH2 stuff

gh2BaseGems :: RTB.T U.Beats (Edge () Five.Color) -> Int
gh2BaseGems edges = let
  gems = fixSloppyNotes (10 / 480) $ edgeBlips_ minSustainLengthRB edges
  in gbkBase 50 25 1 $ fmap snd gems

gh2Base :: Difficulty -> PartTrack U.Beats -> Int
gh2Base diff pt = case Map.lookup diff $ partDifficulties pt of
  Nothing -> 0
  Just pd -> gh2BaseGems $ partGems pd

gh2BaseFixed :: Difficulty -> Five.FiveTrack U.Beats -> Int
gh2BaseFixed diff ft = case Map.lookup diff $ Five.fiveDifficulties ft of
  Nothing -> 0
  Just fd -> gh2BaseGems $ Five.fiveGems fd

-- In GH2 coop_max_scores (either a .dtb on disc or .dta in 360 DLC)
-- each song has an entry:
--   (the_song_key (easy medium hard expert))
-- where each difficulty is an int of "gh2Base(lead) + gh2Base(bass/rhythm)"

data ScoreTrackGH2
  = ScoreGH2Guitar
  | ScoreGH2Bass
  | ScoreGH2Rhythm
  deriving (Eq, Ord, Show, Enum, Bounded)

scoreTrackNameGH2 :: ScoreTrackGH2 -> T.Text
scoreTrackNameGH2 = \case
  ScoreGH2Guitar -> "Guitar"
  ScoreGH2Bass   -> "Bass"
  ScoreGH2Rhythm -> "Rhythm"

getScoreTracksGH2 :: F.FixedFile U.Beats -> [(ScoreTrackGH2, Difficulty, Int)]
getScoreTracksGH2 mid = do
  strack <- [minBound .. maxBound]
  diff   <- [minBound .. maxBound]
  let base = gh2BaseFixed diff $ case strack of
        ScoreGH2Guitar -> F.fixedPartGuitar mid
        ScoreGH2Bass   -> F.fixedPartBass   mid
        ScoreGH2Rhythm -> F.fixedPartRhythm mid
  guard $ base /= 0
  return (strack, diff, base)
