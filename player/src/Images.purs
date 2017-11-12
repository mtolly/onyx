module Images (ImageID(..), withImages) where

import Prelude
import Data.String (split, joinWith, drop, Pattern(..))
import Graphics.Canvas (CanvasImageSource(), tryLoadImage, CANVAS)
import Control.Monad.Eff (Eff())
import Data.Tuple (Tuple(..))
import Control.Parallel (parTraverse)
import Data.Maybe (Maybe(..))
import OnyxMap as Map
import Data.Generic (class Generic, gShow, gEq, gCompare)
import Control.Monad.Eff.Exception (throwException, EXCEPTION, error)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Cont (ContT(..), runContT)

data ImageID
  = Image_button_bass_off
  | Image_button_bass
  | Image_button_drums_off
  | Image_button_drums
  | Image_button_guitar_off
  | Image_button_guitar
  | Image_button_keys_off
  | Image_button_keys
  | Image_button_pause
  | Image_button_play
  | Image_button_probass_off
  | Image_button_probass
  | Image_button_proguitar_off
  | Image_button_proguitar
  | Image_button_prokeys_off
  | Image_button_prokeys
  | Image_button_vocal_off
  | Image_button_vocal
  | Image_gem_blackkey_energy
  | Image_gem_blackkey
  | Image_gem_blue_cymbal
  | Image_gem_blue_hopo
  | Image_gem_blue_pro_hopo
  | Image_gem_blue_pro
  | Image_gem_blue_tap
  | Image_gem_blue
  | Image_gem_energy_cymbal
  | Image_gem_energy_hopo
  | Image_gem_energy_mute_hopo
  | Image_gem_energy_mute
  | Image_gem_energy_pro_hopo
  | Image_gem_energy_pro
  | Image_gem_energy_tap
  | Image_gem_energy
  | Image_gem_green_cymbal
  | Image_gem_green_hopo
  | Image_gem_green_pro_hopo
  | Image_gem_green_pro
  | Image_gem_green_tap
  | Image_gem_green
  | Image_gem_kick_energy
  | Image_gem_kick
  | Image_gem_mute_hopo
  | Image_gem_mute
  | Image_gem_open_energy_hopo
  | Image_gem_open_energy_tap
  | Image_gem_open_energy
  | Image_gem_open_hopo
  | Image_gem_open_tap
  | Image_gem_open
  | Image_gem_orange_hopo
  | Image_gem_orange_pro_hopo
  | Image_gem_orange_pro
  | Image_gem_orange_tap
  | Image_gem_orange
  | Image_gem_purple_pro_hopo
  | Image_gem_purple_pro
  | Image_gem_red_cymbal
  | Image_gem_red_hopo
  | Image_gem_red_pro_hopo
  | Image_gem_red_pro
  | Image_gem_red_tap
  | Image_gem_red
  | Image_gem_whitekey_energy
  | Image_gem_whitekey
  | Image_gem_yellow_cymbal
  | Image_gem_yellow_hopo
  | Image_gem_yellow_pro_hopo
  | Image_gem_yellow_pro
  | Image_gem_yellow_tap
  | Image_gem_yellow
  | Image_highway_drums_bar
  | Image_highway_drums_beat
  | Image_highway_drums_halfbeat
  | Image_highway_drums_solo_edge
  | Image_highway_drums_target
  | Image_highway_grybo_bar
  | Image_highway_grybo_beat
  | Image_highway_grybo_halfbeat
  | Image_highway_grybo_solo_edge
  | Image_highway_grybo_target
  | Image_highway_prokeys_bar
  | Image_highway_prokeys_beat
  | Image_highway_prokeys_halfbeat
  | Image_highway_prokeys_solo_edge
  | Image_highway_prokeys_target
  | Image_highway_protar_beat
  | Image_highway_protar_halfbeat
  | Image_highway_protar_target
  | Image_pro_fret_00
  | Image_pro_fret_01
  | Image_pro_fret_02
  | Image_pro_fret_03
  | Image_pro_fret_04
  | Image_pro_fret_05
  | Image_pro_fret_06
  | Image_pro_fret_07
  | Image_pro_fret_08
  | Image_pro_fret_09
  | Image_pro_fret_10
  | Image_pro_fret_11
  | Image_pro_fret_12
  | Image_pro_fret_13
  | Image_pro_fret_14
  | Image_pro_fret_15
  | Image_pro_fret_16
  | Image_pro_fret_17
  | Image_pro_fret_18
  | Image_pro_fret_19
  | Image_pro_fret_20
  | Image_pro_fret_21
  | Image_pro_fret_22
  | Image_sustain_end
  | Image_sustain_key_end

derive instance genImageID :: Generic ImageID

instance showImageID :: Show ImageID where
  show = gShow

instance eqImageID :: Eq ImageID where
  eq = gEq

instance ordImageID :: Ord ImageID where
  compare = gCompare

allImageIDs :: Array ImageID
allImageIDs = [Image_button_bass_off, Image_button_bass, Image_button_drums_off, Image_button_drums, Image_button_guitar_off, Image_button_guitar, Image_button_keys_off, Image_button_keys, Image_button_pause, Image_button_play, Image_button_probass_off, Image_button_probass, Image_button_proguitar_off, Image_button_proguitar, Image_button_prokeys_off, Image_button_prokeys, Image_button_vocal_off, Image_button_vocal, Image_gem_blackkey_energy, Image_gem_blackkey, Image_gem_blue_cymbal, Image_gem_blue_hopo, Image_gem_blue_pro_hopo, Image_gem_blue_pro, Image_gem_blue_tap, Image_gem_blue, Image_gem_energy_cymbal, Image_gem_energy_hopo, Image_gem_energy_mute_hopo, Image_gem_energy_mute, Image_gem_energy_pro_hopo, Image_gem_energy_pro, Image_gem_energy_tap, Image_gem_energy, Image_gem_green_cymbal, Image_gem_green_hopo, Image_gem_green_pro_hopo, Image_gem_green_pro, Image_gem_green_tap, Image_gem_green, Image_gem_kick_energy, Image_gem_kick, Image_gem_mute_hopo, Image_gem_mute, Image_gem_open_energy_hopo, Image_gem_open_energy_tap, Image_gem_open_energy, Image_gem_open_hopo, Image_gem_open_tap, Image_gem_open, Image_gem_orange_hopo, Image_gem_orange_pro_hopo, Image_gem_orange_pro, Image_gem_orange_tap, Image_gem_orange, Image_gem_purple_pro_hopo, Image_gem_purple_pro, Image_gem_red_cymbal, Image_gem_red_hopo, Image_gem_red_pro_hopo, Image_gem_red_pro, Image_gem_red_tap, Image_gem_red, Image_gem_whitekey_energy, Image_gem_whitekey, Image_gem_yellow_cymbal, Image_gem_yellow_hopo, Image_gem_yellow_pro_hopo, Image_gem_yellow_pro, Image_gem_yellow_tap, Image_gem_yellow, Image_highway_drums_bar, Image_highway_drums_beat, Image_highway_drums_halfbeat, Image_highway_drums_solo_edge, Image_highway_drums_target, Image_highway_grybo_bar, Image_highway_grybo_beat, Image_highway_grybo_halfbeat, Image_highway_grybo_solo_edge, Image_highway_grybo_target, Image_highway_prokeys_bar, Image_highway_prokeys_beat, Image_highway_prokeys_halfbeat, Image_highway_prokeys_solo_edge, Image_highway_prokeys_target, Image_highway_protar_beat, Image_highway_protar_halfbeat, Image_highway_protar_target, Image_pro_fret_00, Image_pro_fret_01, Image_pro_fret_02, Image_pro_fret_03, Image_pro_fret_04, Image_pro_fret_05, Image_pro_fret_06, Image_pro_fret_07, Image_pro_fret_08, Image_pro_fret_09, Image_pro_fret_10, Image_pro_fret_11, Image_pro_fret_12, Image_pro_fret_13, Image_pro_fret_14, Image_pro_fret_15, Image_pro_fret_16, Image_pro_fret_17, Image_pro_fret_18, Image_pro_fret_19, Image_pro_fret_20, Image_pro_fret_21, Image_pro_fret_22, Image_sustain_end, Image_sustain_key_end]

withImages
  :: forall e
  .  ((ImageID -> CanvasImageSource) -> Eff (canvas :: CANVAS, exception :: EXCEPTION | e) Unit)
  -> Eff (canvas :: CANVAS, exception :: EXCEPTION | e) Unit
withImages = let
  loadTuple iid = let
    path = "images/" <> joinWith "-" (split (Pattern "_") $ drop 13 $ show iid) <> ".png"
    -- TODO: the 13 is the length of "Images.Image_". do this better somehow
    in map (Tuple iid) $ ContT $ withImage path
  withImage path f = tryLoadImage path $ \mimg -> case mimg of
    Just img -> f img
    Nothing  -> throwException $ error $ "panic! could not load image from path: " <> path
  pairsToFn :: Array (Tuple ImageID CanvasImageSource) -> ImageID -> CanvasImageSource
  pairsToFn pairs = let
    imageMap :: Map.Map ImageID CanvasImageSource
    imageMap = Map.fromFoldable pairs
    in \iid -> case Map.lookup iid imageMap of
      Just img -> img
      Nothing  -> unsafeThrow $ "panic! loaded image not found for image ID " <> show iid
  in runContT $ map pairsToFn $ parTraverse loadTuple allImageIDs
