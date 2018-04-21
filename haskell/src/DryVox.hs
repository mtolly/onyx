{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module DryVox where

import           Audio                            (applyVolsMono)
import           Control.Monad                    (guard)
import           Control.Monad.Trans.Resource     (MonadResource)
import           Data.Conduit                     ((=$=))
import qualified Data.Conduit                     as C
import           Data.Conduit.Audio
import           Data.Conduit.Audio.SampleRate
import qualified Data.Conduit.List                as CL
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Vector.Storable             as V
import qualified Numeric.NonNegative.Class        as NNC
import qualified Numeric.NonNegative.Wrapper      as NN
import qualified RockBand.Legacy.Vocal            as Vox
import qualified Sound.MIDI.Util                  as U

vocalTubes :: (NNC.C t) => RTB.T t Vox.Event -> RTB.T t Bool
vocalTubes vox = let
  vox' = RTB.mapMaybe instant $ RTB.collectCoincident vox
  instant evts = let
    on   = not $ null [ () | Vox.Note True  _ <- evts ]
    off  = not $ null [ () | Vox.Note False _ <- evts ]
    plus = not $ null [ () | Vox.Lyric "+"    <- evts ]
    in guard (on || off || plus) >> Just (on, off, plus)
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, (on, off, _)), rtb')
      | on -> RTB.cons dt True $ go rtb'
      | off -> case RTB.viewL rtb' of
        Just ((_, (_, _, True)), _) -> RTB.delay dt       $ go rtb'
        _                           -> RTB.cons  dt False $ go rtb'
      | otherwise -> RTB.delay dt $ go rtb'
  in go vox'

clipDryVox :: (Monad m, Num a, V.Storable a) => RTB.T U.Seconds Bool -> AudioSource m a -> AudioSource m a
clipDryVox vox src = let
  vox' :: RTB.T NN.Integer Bool
  vox' = RTB.discretize $ RTB.mapTime (* realToFrac (rate src)) vox
  sink :: (Monad m, Num a, V.Storable a) => Bool -> RTB.T NN.Integer Bool -> C.Conduit (V.Vector a) m (V.Vector a)
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
    , source   = source src =$= sink False vox'
    }

toDryVoxFormat :: (MonadResource m) => AudioSource m Float -> AudioSource m Float
toDryVoxFormat = resampleTo 16000 SincMediumQuality . applyVolsMono []

sineDryVox :: (Monad m) => RTB.T U.Seconds Vox.Event -> AudioSource m Float
sineDryVox vox = let
  notes = RTB.normalize $ flip RTB.mapMaybe vox $ \case
    Vox.Note True  p -> Just (Just p)
    Vox.Note False _ -> Just Nothing
    _                -> Nothing
  go p rtb = case RTB.viewL rtb of
    Nothing -> silent (Seconds 1) 16000 1
    Just ((dt, p'), rtb') -> let
      chunk = case p of
        Nothing -> silent (Seconds $ realToFrac dt) 16000 1
        Just pitch -> sine (midiPitchToFreq $ fromEnum pitch + 36) (Seconds $ realToFrac dt) 16000
      in concatenate chunk $ go p' rtb'
  midiPitchToFreq p = (2 ** ((fromIntegral p - 69) / 12)) * 440
  in go Nothing notes
