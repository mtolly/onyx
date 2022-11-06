{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Onyx.Vocal.DryVox where

import           Control.Monad                    (guard)
import           Control.Monad.Trans.Resource     (MonadResource)
import           Data.Conduit                     ((.|))
import qualified Data.Conduit                     as C
import           Data.Conduit.Audio
import           Data.Conduit.Audio.SampleRate
import qualified Data.Conduit.List                as CL
import           Data.Either                      (rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Maybe                       (fromMaybe, isJust,
                                                   listToMaybe)
import qualified Data.Text                        as T
import qualified Data.Vector.Storable             as V
import qualified Numeric.NonNegative.Class        as NNC
import qualified Numeric.NonNegative.Wrapper      as NN
import           Onyx.Audio                       (applyVolsMono)
import           Onyx.MIDI.Common                 (pattern RNil, pattern Wait)
import           Onyx.MIDI.Track.Vocal
import qualified Sound.MIDI.Util                  as U

vocalTubes :: (NNC.C t) => VocalTrack t -> RTB.T t (Maybe T.Text)
vocalTubes vox = let
  edges = fmap (Left . snd) $ vocalNotes vox
  lyrics = fmap Right $ vocalLyrics vox
  vox' = RTB.mapMaybe instant $ RTB.collectCoincident $ RTB.merge edges lyrics
  instant evts = let
    on    = elem (Left True) evts
    off   = elem (Left False) evts
    lyric = listToMaybe $ rights evts
    in guard (on || off || isJust lyric) >> Just (on, off, lyric)
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, (on, off, lyric)), rtb')
      | on && off -> RTB.cons dt Nothing $ RTB.cons NNC.zero (Just $ fromMaybe "" lyric) $ go rtb'
      | on        -> RTB.cons dt (Just $ fromMaybe "" lyric) $ go rtb'
      | off       -> RTB.cons  dt Nothing $ go rtb'
      | otherwise -> RTB.delay dt $ go rtb'
  isPlus s = elem s [Just "+", Just "+$"]
  removePlus = \case
    RNil -> RNil
    Wait t1 Nothing (Wait t2 lyric rest) | isPlus lyric -> removePlus $ RTB.delay (t1 <> t2) rest
    Wait t lyric rest | isPlus lyric -> removePlus $ RTB.delay t rest -- probably shouldn't happen
    Wait t x rest -> Wait t x $ removePlus rest
  in removePlus $ go vox'

clipDryVox :: (Monad m, Num a, V.Storable a) => RTB.T U.Seconds Bool -> AudioSource m a -> AudioSource m a
clipDryVox vox src = let
  vox' :: RTB.T NN.Integer Bool
  vox' = RTB.discretize $ RTB.mapTime (* realToFrac (rate src)) vox
  sink :: (Monad m, Num a, V.Storable a) => Bool -> RTB.T NN.Integer Bool -> C.ConduitT (V.Vector a) (V.Vector a) m ()
  sink b bools = case RTB.viewL bools of
    Nothing -> CL.map $ applyBool b
    Just ((dt, b'), bools') -> if dt == NNC.zero
      then sink b' bools'
      else C.await >>= \case
        Nothing -> return ()
        Just v -> let
          len = fromIntegral $ vectorFrames v (channels src)
          in case compare dt len of
            EQ -> do
              C.yield $ applyBool b v
              sink b' bools'
            LT -> do
              let (v1, v2) = V.splitAt (fromIntegral dt * channels src) v
              C.yield $ applyBool b v1
              C.leftover v2
              sink b' bools'
            GT -> do
              C.yield $ applyBool b v
              sink b $ RTB.cons (dt - len) b' bools'
  applyBool :: (Num a, V.Storable a) => Bool -> V.Vector a -> V.Vector a
  applyBool True  = id
  applyBool False = V.map $ const 0
  in AudioSource
    { rate     = rate src
    , frames   = frames src
    , channels = channels src
    , source   = source src .| sink False vox'
    }

toDryVoxFormat :: (MonadResource m) => AudioSource m Float -> AudioSource m Float
toDryVoxFormat = resampleTo 16000 SincMediumQuality . applyVolsMono []

sineDryVox :: (Monad m) => VocalTrack U.Seconds -> AudioSource m Float
sineDryVox vox = let
  notes = RTB.normalize $ fmap (\(p, b) -> guard b >> Just p) $ vocalNotes vox
  go p rtb = case RTB.viewL rtb of
    Nothing -> silent (Seconds 1) 16000 1
    Just ((dt, p'), rtb') -> let
      chunk = case p of
        Nothing -> silent (Seconds $ realToFrac dt) 16000 1
        Just pitch -> sine (midiPitchToFreq $ fromEnum pitch + 36) (Seconds $ realToFrac dt) 16000
      in concatenate chunk $ go p' rtb'
  midiPitchToFreq p = (2 ** ((fromIntegral p - 69) / 12)) * 440
  in go Nothing notes
