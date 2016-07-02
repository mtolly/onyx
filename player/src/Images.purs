module Images (ImageID(..), withImages) where

import Prelude
import Data.Traversable (traverse)
import Data.String (split, joinWith, drop)
import Graphics.Canvas (CanvasImageSource(), withImage)
import Control.Monad.Eff (Eff())
import Data.Tuple (Tuple(..))
import Control.Parallel.Class (parallel, runParallel, Parallel())
import Data.Maybe (Maybe(..))
import OnyxMap as Map
import Data.Generic (class Generic, gShow, gEq, gCompare)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Cont (ContT(..), runContT)

data ImageID
  = Image_gem_blackkey
  | Image_gem_blackkey_energy
  | Image_gem_blue
  | Image_gem_blue_cymbal
  | Image_gem_blue_hopo
  | Image_gem_energy
  | Image_gem_energy_cymbal
  | Image_gem_energy_hopo
  | Image_gem_green
  | Image_gem_green_cymbal
  | Image_gem_green_hopo
  | Image_gem_kick
  | Image_gem_kick_energy
  | Image_gem_orange
  | Image_gem_orange_hopo
  | Image_gem_red
  | Image_gem_red_cymbal
  | Image_gem_red_hopo
  | Image_gem_whitekey
  | Image_gem_whitekey_energy
  | Image_gem_yellow
  | Image_gem_yellow_cymbal
  | Image_gem_yellow_hopo
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
  | Image_sustain_key_end
  | Image_sustain_end
  | Image_button_bass
  | Image_button_bass_off
  | Image_button_drums
  | Image_button_drums_off
  | Image_button_guitar
  | Image_button_guitar_off
  | Image_button_keys
  | Image_button_keys_off
  | Image_button_pause
  | Image_button_play
  | Image_button_prokeys
  | Image_button_prokeys_off
  | Image_button_vocal
  | Image_button_vocal_off

derive instance genImageID :: Generic ImageID

instance showImageID :: Show ImageID where
  show = gShow

instance eqImageID :: Eq ImageID where
  eq = gEq

instance ordImageID :: Ord ImageID where
  compare = gCompare

allImageIDs :: Array ImageID
allImageIDs = [Image_gem_blackkey, Image_gem_blackkey_energy, Image_gem_blue, Image_gem_blue_cymbal, Image_gem_blue_hopo, Image_gem_energy, Image_gem_energy_cymbal, Image_gem_energy_hopo, Image_gem_green, Image_gem_green_cymbal, Image_gem_green_hopo, Image_gem_kick, Image_gem_kick_energy, Image_gem_orange, Image_gem_orange_hopo, Image_gem_red, Image_gem_red_cymbal, Image_gem_red_hopo, Image_gem_whitekey, Image_gem_whitekey_energy, Image_gem_yellow, Image_gem_yellow_cymbal, Image_gem_yellow_hopo, Image_highway_drums_bar, Image_highway_drums_beat, Image_highway_drums_halfbeat, Image_highway_drums_solo_edge, Image_highway_drums_target, Image_highway_grybo_bar, Image_highway_grybo_beat, Image_highway_grybo_halfbeat, Image_highway_grybo_solo_edge, Image_highway_grybo_target, Image_highway_prokeys_bar, Image_highway_prokeys_beat, Image_highway_prokeys_halfbeat, Image_highway_prokeys_solo_edge, Image_highway_prokeys_target, Image_sustain_key_end, Image_sustain_end, Image_button_bass, Image_button_bass_off, Image_button_drums, Image_button_drums_off, Image_button_guitar, Image_button_guitar_off, Image_button_keys, Image_button_keys_off, Image_button_pause, Image_button_play, Image_button_prokeys, Image_button_prokeys_off, Image_button_vocal, Image_button_vocal_off]

withImages :: forall e. ((ImageID -> CanvasImageSource) -> Eff e Unit) -> Eff e Unit
withImages = let
  loadTuple :: forall e2. ImageID -> Parallel (ContT Unit (Eff e2)) (Tuple ImageID CanvasImageSource)
  loadTuple iid = let
    path = "images/" <> joinWith "-" (split "_" $ drop 13 $ show iid) <> ".png"
    -- TODO: the 13 is the length of "Images.Image_". do this better somehow
    in map (Tuple iid) $ parallel <<< ContT $ withImage path
  pairsToFn :: Array (Tuple ImageID CanvasImageSource) -> ImageID -> CanvasImageSource
  pairsToFn pairs = let
    imageMap :: Map.Map ImageID CanvasImageSource
    imageMap = Map.fromFoldable pairs
    in \iid -> case Map.lookup iid imageMap of
      Just img -> img
      Nothing  -> unsafeThrow $ "panic! loaded image not found for image ID " <> show iid
  in runContT $ runParallel $ map pairsToFn $ traverse loadTuple allImageIDs
