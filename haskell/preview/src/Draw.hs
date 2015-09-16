{-# LANGUAGE LambdaCase #-}
module Draw
( ImageID(..)
, Opacity, Rect, Draw(..)
, draw
) where

import           Control.Monad              (forM_)
import           Control.Monad.Trans.Writer (execWriter, tell)
import           Data.List                  (sort, transpose)
import qualified Data.Map                   as Map
import           Midi
import           Numeric.NonNegative.Class  ((-|))
import           RockBand.Drums
import qualified Sound.MIDI.Util            as U
import           Text.Printf                (printf)

data ImageID
  = Image_RBN_background1
  | Image_beat_marker
  | Image_gem_blue
  | Image_gem_cym_blue
  | Image_gem_cym_green
  | Image_gem_cym_style
  | Image_gem_cym_yellow
  | Image_gem_green
  | Image_gem_hopo_blue
  | Image_gem_hopo_green
  | Image_gem_hopo_orange
  | Image_gem_hopo_red
  | Image_gem_hopo_style
  | Image_gem_hopo_yellow
  | Image_gem_kick
  | Image_gem_kick_style
  | Image_gem_orange
  | Image_gem_red
  | Image_gem_style
  | Image_gem_yellow
  | Image_half_beat_marker
  | Image_kick_flash_1
  | Image_kick_flash_2
  | Image_kick_flash_3
  | Image_kick_flash_4
  | Image_kick_flash_5
  | Image_kick_flash_6
  | Image_kick_flash_7
  | Image_measure
  | Image_smash_1
  | Image_smash_2
  | Image_smash_3
  | Image_smash_4
  | Image_smash_5
  | Image_smash_6
  | Image_smash_7
  | Image_smash_8
  | Image_smash_9
  | Image_smash_10
  | Image_smash_flare_blue
  | Image_smash_flare_green
  | Image_smash_flare_orange
  | Image_smash_flare_red
  | Image_smash_flare_style
  | Image_smash_flare_yellow
  | Image_sustain_blue
  | Image_sustain_blue_hi
  | Image_sustain_green
  | Image_sustain_green_hi
  | Image_sustain_orange
  | Image_sustain_orange_hi
  | Image_sustain_red
  | Image_sustain_red_hi
  | Image_sustain_style
  | Image_sustain_style_hi
  | Image_sustain_yellow
  | Image_sustain_yellow_hi
  | Image_track_drum
  | Image_track_guitar
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Opacity = Double
type Rect = (Double, Double, Double, Double) -- (x, y, w, h)

data Draw
  = DrawImage ImageID Rect Opacity
  | Text String Double Double
  deriving (Eq, Ord, Show, Read)

zoom :: (Ord k) => k -> k -> Map.Map k a -> Map.Map k a
zoom lb ub m = let
  (_, upper) = Map.split lb m
  (lower, _) = Map.split ub upper
  in lower

draw :: U.Seconds -> Preview -> [Draw]
draw posn preview = execWriter $ do
  let gems'      = zoom (posn -| 1) (posn + 1) $ gems      preview
      beatLines' = zoom (posn -| 1) (posn + 1) $ beatLines preview
      add x      = tell [x]
  add $ DrawImage Image_RBN_background1 (0 , 0 , 640, 480) 1
  add $ DrawImage Image_track_drum      (50, 50, 540, 430) 1
  let beatPosnNow, beatPosnFuture :: (Double, Double, Double, Double)
      beatPosnNow    = (320, 373.735, 431.201, 28.529)
      beatPosnFuture = (320, 175.58 , 234    , 17.888)
      futureTime = 0.75 :: Double
      getIllustratorPx now4 future4 eventPosn = let
        secOffset = realToFrac eventPosn - realToFrac posn :: Double
        nowToFuturePosn = case secOffset / futureTime of
          r | r >= 0    -> r ** 0.8
            | otherwise -> r * 1.25
        (xn, yn, wn, hn) = now4
        (xf, yf, wf, hf) = future4
        in  ( xn + nowToFuturePosn * (xf - xn)
            , yn + nowToFuturePosn * (yf - yn)
            , wn + nowToFuturePosn * (wf - wn)
            , hn + nowToFuturePosn * (hf - hn)
            )
      beatIllustratorPx    = getIllustratorPx beatPosnNow      beatPosnFuture
      gemIllustratorPx gem = getIllustratorPx (gemPosnNow gem) (gemPosnFuture gem)
      gemPosnNow, gemPosnFuture :: Gem ProType -> (Double, Double, Double, Double)
      gemPosnNow = \case
        Kick         -> (320  , 370.5  , 457, 27    )
        Red          -> (170.5, 362.5  , 91 , 45    )
        Pro Yellow _ -> (270.5, 360.47 , 91 , 45    )
        Pro Blue   _ -> (369.5, 360.47 , 86 , 46.289)
        Pro Green  _ -> (468  , 362.596, 86 , 46.289)
      gemPosnFuture = \case
        Kick         -> (320    , 175.577, 256   , 19.292)
        Red          -> (235.215, 169.926, 50.976, 32.137)
        Pro Yellow _ -> (291.233, 168.409, 50.976, 32.137)
        Pro Blue   _ -> (346.69 , 168.412, 48.175, 33.057)
        Pro Green  _ -> (401.869, 169.929, 48.175, 33.057)
      -- scales illustrator dims given a reference image and an actual image
      scaleTo :: (Double, Double) -> (Double, Double) ->
        (Double, Double, Double, Double) -> (Double, Double, Double, Double)
      (w1, h1) `scaleTo` (w2, h2) = \(x, y, w, h) ->
        (x, y, w * w2 / w1, h * h2 / h1)
      scaleSmash = (128, 64) `scaleTo` (256, 256)
      scaleSmashFlare = (128, 64) `scaleTo` (128, 128)
      scaleKickFlash = (1024, 64) `scaleTo` (1224, 512)
      getGemImages :: Gem ProType -> U.Seconds ->
        [(ImageID, (Double, Double, Double, Double))]
      getGemImages gem gemSecs = let
        secOffset = realToFrac gemSecs - realToFrac posn :: Double
        brokenDims = gemIllustratorPx gem posn
        movingDims = gemIllustratorPx gem gemSecs
        image = case gem of
          Kick              -> Image_gem_kick
          Red               -> Image_gem_red
          Pro Yellow Tom    -> Image_gem_yellow
          Pro Blue   Tom    -> Image_gem_blue
          Pro Green  Tom    -> Image_gem_green
          Pro Yellow Cymbal -> Image_gem_cym_yellow
          Pro Blue   Cymbal -> Image_gem_cym_blue
          Pro Green  Cymbal -> Image_gem_cym_green
        brokenAnim = case gem of
          Kick           -> transpose [kickFlashes]
          Pro ybg Cymbal -> transpose [flaresPro ybg]
          Pro ybg Tom    -> transpose [smashes, flaresPro ybg]
          Red            -> transpose [smashes, flaresRed]
          where flaresRed = do
                  flare <- replicate 3 Image_smash_flare_red
                  return (flare, scaleSmashFlare brokenDims)
                flaresPro ybg = do
                  flare <- replicate 3 $ case ybg of
                    Yellow -> Image_smash_flare_yellow
                    Blue   -> Image_smash_flare_blue
                    Green  -> Image_smash_flare_green
                  return (flare, scaleSmashFlare brokenDims)
                smashes = do
                  smash <- [Image_smash_1 .. Image_smash_10]
                  return (smash, scaleSmash brokenDims)
                kickFlashes = do
                  flash <- [Image_kick_flash_1 .. Image_kick_flash_7]
                  return (flash, scaleKickFlash brokenDims)
        in if secOffset <= 0
          then concat $ take 1 $ drop (floor $ secOffset * (-50)) brokenAnim
          else [(image, movingDims)]
      -- converts from adobe illustrator xywh to canvas xywh
      -- (illustrator uses rect center for x/y instead of rect top-left)
      getRealPx (x, y, w, h) = (x - 0.5 * w, y - 0.5 * h, w, h)
      opacity eventPosn = let
        secOffset = realToFrac eventPosn - realToFrac posn :: Double
        in if secOffset > 0.8 then 1 - (secOffset - 0.8) * 5 else 1
  forM_ (reverse $ Map.assocs beatLines') $ \(beatSecs, beat) -> let
    rect = getRealPx $ beatIllustratorPx beatSecs
    image = case beat of
      Bar      -> Image_measure
      Beat     -> Image_beat_marker
      HalfBeat -> Image_half_beat_marker
    in add $ DrawImage image rect $ opacity beatSecs
  forM_ (reverse $ Map.assocs gems') $ \(gemSecs, gemList) ->
    forM_ (sort gemList) $ \gem -> -- sort puts Kick first
      forM_ (getGemImages gem gemSecs) $ \(iid, illdims) ->
        add $ DrawImage iid (getRealPx illdims) $ opacity gemSecs
  let dposn = realToFrac posn :: Double
      mins = floor $ dposn / 60 :: Int
      secs = dposn - fromIntegral mins * 60 :: Double
      (msr, bts) = timeToMeasure preview posn
      timestamp = printf "Time: %02d:%06.3f | Measure: %03d | Beat: %06.3f"
        mins secs (msr + 1) (realToFrac bts + 1 :: Double) :: String
  add $ Text timestamp 10 0
