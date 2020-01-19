{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}
module RockBand.Score where

import           Control.Applicative              (liftA2)
import           Control.Monad                    (guard, void)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           Guitars                          (applyStatus, fixSloppyNotes)
import qualified Numeric.NonNegative.Class        as NNC
import qualified RockBand.Codec.Drums             as RBDrums
import qualified RockBand.Codec.Events            as E
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as RBFive
import qualified RockBand.Codec.ProGuitar         as PG
import qualified RockBand.Codec.ProKeys           as PK
import qualified RockBand.Codec.Vocal             as Vox
import           RockBand.Common                  (Difficulty (..),
                                                   pattern RNil, pattern Wait)
import           Scripts                          (trackGlue)
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
  deriving (Eq, Show, Enum, Bounded)

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
  ScoreGuitar      -> Stars 0.06 0.12 0.2  0.47 0.78 1.15
  ScoreBass        -> Stars 0.05 0.1  0.19 0.47 0.78 1.15
  ScoreDrums       -> Stars 0.06 0.12 0.2  0.45 0.75 1.09
  ScoreKeys        -> Stars 0.06 0.12 0.2  0.47 0.78 1.15
  ScoreVocals      -> Stars 0.05 0.11 0.19 0.46 0.77 1.06
  ScoreProGuitar   -> Stars 0.06 0.12 0.2  0.47 0.78 1.15
  ScoreProBass     -> Stars 0.05 0.1  0.19 0.47 0.78 1.15
  -- dtb also has separate lines for 22-fret but they are the same as 17
  ScoreProDrums    -> Stars 0.06 0.12 0.2  0.45 0.75 1.09 -- not a separate line in dtb
  ScoreProKeys     -> Stars 0.06 0.12 0.2  0.47 0.78 1.15
  ScoreHarmonies   -> Stars 0.05 0.11 0.19 0.46 0.77 1.06 -- not a separate line in dtb

new_bonus_thresholds :: Stars Float
new_bonus_thresholds = Stars 0.05 0.1 0.2 0.3 0.4 0.95

drumBase :: Int -> RTB.T t a -> Int
drumBase gem rtb = let
  len = length rtb
  gems1x = min 9 len
  gems2x = min 10 $ len - gems1x
  gems3x = min 10 $ len - gems1x - gems2x
  gems4x = len - gems1x - gems2x - gems3x
  in sum
    [ gems1x * gem
    + gems2x * gem * 2
    + gems3x * gem * 3
    + gems4x * gem * 4
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

baseAndSolo :: RBFile.FixedFile U.Beats -> (ScoreTrack, Difficulty) -> (Int, Int)
baseAndSolo mid (scoreTrack, diff) = let
  adjustGems breLanes = ignoreBRE breLanes . fixSloppyNotes (10 / 480)
  -- have to ignore notes under the BRE lanes, but not the ones after
  ignoreBRE breLanes = case E.eventsCoda $ RBFile.fixedEvents mid of
    Wait breStart () _ -> case RTB.filter not $ U.trackDrop breStart breLanes of
      Wait breLength _ _ -> \gems -> let
        breEnd = breStart <> breLength
        in trackGlue breEnd (U.trackTake breStart gems) (U.trackDrop breEnd gems)
      RNil -> U.trackTake breStart -- shouldn't happen
    _            -> id
  getDrums gem = let
    trk = RBFile.fixedPartDrums mid
    gems = adjustGems (RBDrums.drumActivation trk) $ RBDrums.computePro (Just diff) trk
    base = drumBase gem gems
    solo = perfectSoloBonus 100 (RBDrums.drumSolo trk) gems
    in (base, solo)
  getFive maxStreak getTrack = let
    trk = getTrack mid
    gems = adjustGems (RBFive.fiveBRE trk) $ maybe RTB.empty RBFive.fiveGems
      $ Map.lookup diff $ RBFive.fiveDifficulties trk
    base = gbkBase 25 12 maxStreak $ fmap snd gems
    solo = perfectSoloBonus 100 (RBFive.fiveSolo trk) $ RTB.collectCoincident gems
    in (base, solo)
  -- From what I can tell 17 and 22 are same cutoff, and it uses the 22 chart
  getPG maxStreak get22 get17 = let
    trk = case get22 mid of
      trk22 | not $ PG.nullPG trk22 -> trk22
      _                             -> get17 mid
    gems
      = adjustGems (snd <$> PG.pgBRE trk)
      $ maybe RTB.empty PG.pgNotes
      $ Map.lookup diff $ PG.pgDifficulties trk
    base = gbkBase 60 30 maxStreak $ maxChord2 $ fmap (\(_, (_, _, mlen)) -> mlen) gems
    solo = perfectSoloBonus 150 (PG.pgSolo trk) $ RTB.collectCoincident gems
    in (base, solo)
  maxChord2 = RTB.flatten . fmap (take 2) . RTB.collectCoincident
  getVox getTrack = voxBaseAndSolo diff $ getTrack mid
  getPK = let
    trk = case diff of
      Easy   -> RBFile.fixedPartRealKeysE mid
      Medium -> RBFile.fixedPartRealKeysM mid
      Hard   -> RBFile.fixedPartRealKeysH mid
      Expert -> RBFile.fixedPartRealKeysX mid
    expert = RBFile.fixedPartRealKeysX mid
    gems = adjustGems (PK.pkBRE expert) $ PK.pkNotes trk
    base = gbkBase 60 30 4 $ fmap snd gems
    solo = perfectSoloBonus 100 (PK.pkSolo expert) $ RTB.collectCoincident gems
    in (base, solo)
  in case scoreTrack of
    ScoreDrums       -> getDrums 25
    ScoreProDrums    -> getDrums 30
    ScoreGuitar      -> getFive 4 RBFile.fixedPartGuitar
    ScoreBass        -> getFive 6 RBFile.fixedPartBass
    ScoreKeys        -> getFive 4 RBFile.fixedPartKeys
    ScoreProGuitar   -> getPG 4 RBFile.fixedPartRealGuitar22 RBFile.fixedPartRealGuitar
    ScoreProBass     -> getPG 6 RBFile.fixedPartRealBass22 RBFile.fixedPartRealBass
    ScoreVocals      -> getVox RBFile.fixedPartVocals
    ScoreHarmonies   -> getVox RBFile.fixedHarm1
    ScoreProKeys     -> getPK

annotateMultiplier :: Int -> RTB.T t a -> RTB.T t (a, Int)
annotateMultiplier maxMult = RTB.fromPairList . go 1 9 . RTB.toPairList where
  go _         _         []   = []
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

starCutoffs :: RBFile.FixedFile U.Beats -> [(ScoreTrack, Difficulty)] -> Stars (Maybe Int)
starCutoffs mid trks = let
  new_num_instruments_multiplier = case length trks of
    1 -> 1.0
    2 -> 1.26
    3 -> 1.52
    _ -> 1.8
  sumOfBases :: Stars Float
  sumOfBases = foldr (liftA2 (+)) (pure 0) $ flip map trks $ \trk@(scoreTrack, _) -> let
    (base, solo) = baseAndSolo mid trk
    in liftA2 (+)
      (((fromIntegral base * new_num_instruments_multiplier) *) <$> new_instrument_thresholds scoreTrack)
      ((fromIntegral solo *) <$> new_bonus_thresholds)
  allCutoffs = Just . (floor :: Float -> Int) <$> sumOfBases
  allExpert = all ((== Expert) . snd) trks
  in allCutoffs { starsGold = guard allExpert >> starsGold allCutoffs }
