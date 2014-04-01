module MIDITime where

import Data.Ratio (denominator)

import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map as Map
import qualified Numeric.NonNegative.Wrapper as NN
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta

type Resolution = NN.Int
type Ticks      = NN.Integer
type Beats      = NN.Rational
type Seconds    = NN.Rational
type BPS        = NN.Rational

ticksToBeats :: Resolution -> RTB.T Ticks a -> RTB.T Beats a
ticksToBeats res = RTB.mapTime $ \t ->
  NN.fromNumberUnsafe $ fromIntegral t / fromIntegral res

beatsToTicks :: Resolution -> RTB.T Beats a -> RTB.T Ticks a
beatsToTicks res = RTB.discretize . RTB.mapTime (* fromIntegral res)

minResolution :: RTB.T Beats a -> Resolution
minResolution =
  fromIntegral . foldr lcm 1 . map (denominator . NN.toNumber) . RTB.getTimes

type TempoMap = Map.Map Beats (Seconds, BPS)

rtbToMap :: (Eq t, Num t) => RTB.T t a -> Map.Map t a
rtbToMap = Map.fromAscList . ATB.toPairList . RTB.toAbsoluteEventList 0

tempoMap :: RTB.T Beats E.T -> TempoMap
tempoMap = rtbToMap . go 0 2 where
  go :: Seconds -> BPS -> RTB.T Beats E.T -> RTB.T Beats (Seconds, BPS)
  go s bps rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((db, x), rtb') -> case x of
      E.MetaEvent (Meta.SetTempo uspb) -> let
        bps' = 1000000 / fromIntegral uspb
        in RTB.cons db (s', bps') $ go s' bps' rtb'
      _ -> go s' bps rtb'
      where s' = s + db / bps

beatsToSeconds :: TempoMap -> Beats -> Seconds
beatsToSeconds tm b = case Map.lookupLE b tm of
  Nothing -> error $
    "beatsToSeconds: no tempo event before " ++ show b ++ " beats"
  Just (b', (s, bps)) -> s + (b - b') / bps
