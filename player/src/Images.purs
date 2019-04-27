module Images where

import           Prelude

import           Control.Monad.Cont      (ContT (..), runContT)
import           Control.Parallel        (parTraverse)
import           Data.Array              as Array
import           Data.Maybe              (Maybe (..))
import           Effect                  (Effect)
import           Effect.Exception        (error, throwException)
import           Effect.Exception.Unsafe (unsafeThrow)
import           Graphics.Canvas         (CanvasImageSource, tryLoadImage)

type ImageID = { index :: Int, name :: String }

-- starting from here, generated with images.rb

image_button_gear :: ImageID
image_button_gear = { index: 0, name: "button-gear" }
image_button_pause :: ImageID
image_button_pause = { index: 1, name: "button-pause" }
image_button_play :: ImageID
image_button_play = { index: 2, name: "button-play" }
image_gem_black :: ImageID
image_gem_black = { index: 3, name: "gem-black" }
image_gem_black_hopo :: ImageID
image_gem_black_hopo = { index: 4, name: "gem-black-hopo" }
image_gem_black_tap :: ImageID
image_gem_black_tap = { index: 5, name: "gem-black-tap" }
image_gem_blackkey :: ImageID
image_gem_blackkey = { index: 6, name: "gem-blackkey" }
image_gem_blackkey_energy :: ImageID
image_gem_blackkey_energy = { index: 7, name: "gem-blackkey-energy" }
image_gem_blackwhite :: ImageID
image_gem_blackwhite = { index: 8, name: "gem-blackwhite" }
image_gem_blackwhite_hopo :: ImageID
image_gem_blackwhite_hopo = { index: 9, name: "gem-blackwhite-hopo" }
image_gem_blackwhite_tap :: ImageID
image_gem_blackwhite_tap = { index: 10, name: "gem-blackwhite-tap" }
image_gem_blue :: ImageID
image_gem_blue = { index: 11, name: "gem-blue" }
image_gem_blue_cymbal :: ImageID
image_gem_blue_cymbal = { index: 12, name: "gem-blue-cymbal" }
image_gem_blue_hhclosed :: ImageID
image_gem_blue_hhclosed = { index: 13, name: "gem-blue-hhclosed" }
image_gem_blue_hhopen :: ImageID
image_gem_blue_hhopen = { index: 14, name: "gem-blue-hhopen" }
image_gem_blue_hhpedal :: ImageID
image_gem_blue_hhpedal = { index: 15, name: "gem-blue-hhpedal" }
image_gem_blue_hopo :: ImageID
image_gem_blue_hopo = { index: 16, name: "gem-blue-hopo" }
image_gem_blue_pro :: ImageID
image_gem_blue_pro = { index: 17, name: "gem-blue-pro" }
image_gem_blue_pro_hopo :: ImageID
image_gem_blue_pro_hopo = { index: 18, name: "gem-blue-pro-hopo" }
image_gem_blue_pro_tap :: ImageID
image_gem_blue_pro_tap = { index: 19, name: "gem-blue-pro-tap" }
image_gem_blue_tap :: ImageID
image_gem_blue_tap = { index: 20, name: "gem-blue-tap" }
image_gem_catch :: ImageID
image_gem_catch = { index: 21, name: "gem-catch" }
image_gem_energy :: ImageID
image_gem_energy = { index: 22, name: "gem-energy" }
image_gem_energy_cymbal :: ImageID
image_gem_energy_cymbal = { index: 23, name: "gem-energy-cymbal" }
image_gem_energy_hhclosed :: ImageID
image_gem_energy_hhclosed = { index: 24, name: "gem-energy-hhclosed" }
image_gem_energy_hhopen :: ImageID
image_gem_energy_hhopen = { index: 25, name: "gem-energy-hhopen" }
image_gem_energy_hhpedal :: ImageID
image_gem_energy_hhpedal = { index: 26, name: "gem-energy-hhpedal" }
image_gem_energy_hopo :: ImageID
image_gem_energy_hopo = { index: 27, name: "gem-energy-hopo" }
image_gem_energy_mute :: ImageID
image_gem_energy_mute = { index: 28, name: "gem-energy-mute" }
image_gem_energy_mute_hopo :: ImageID
image_gem_energy_mute_hopo = { index: 29, name: "gem-energy-mute-hopo" }
image_gem_energy_mute_tap :: ImageID
image_gem_energy_mute_tap = { index: 30, name: "gem-energy-mute-tap" }
image_gem_energy_pro :: ImageID
image_gem_energy_pro = { index: 31, name: "gem-energy-pro" }
image_gem_energy_pro_hopo :: ImageID
image_gem_energy_pro_hopo = { index: 32, name: "gem-energy-pro-hopo" }
image_gem_energy_pro_tap :: ImageID
image_gem_energy_pro_tap = { index: 33, name: "gem-energy-pro-tap" }
image_gem_energy_rimshot :: ImageID
image_gem_energy_rimshot = { index: 34, name: "gem-energy-rimshot" }
image_gem_energy_tap :: ImageID
image_gem_energy_tap = { index: 35, name: "gem-energy-tap" }
image_gem_ghl_energy :: ImageID
image_gem_ghl_energy = { index: 36, name: "gem-ghl-energy" }
image_gem_green :: ImageID
image_gem_green = { index: 37, name: "gem-green" }
image_gem_green_cymbal :: ImageID
image_gem_green_cymbal = { index: 38, name: "gem-green-cymbal" }
image_gem_green_hopo :: ImageID
image_gem_green_hopo = { index: 39, name: "gem-green-hopo" }
image_gem_green_pro :: ImageID
image_gem_green_pro = { index: 40, name: "gem-green-pro" }
image_gem_green_pro_hopo :: ImageID
image_gem_green_pro_hopo = { index: 41, name: "gem-green-pro-hopo" }
image_gem_green_pro_tap :: ImageID
image_gem_green_pro_tap = { index: 42, name: "gem-green-pro-tap" }
image_gem_green_rimshot :: ImageID
image_gem_green_rimshot = { index: 43, name: "gem-green-rimshot" }
image_gem_green_tap :: ImageID
image_gem_green_tap = { index: 44, name: "gem-green-tap" }
image_gem_kick :: ImageID
image_gem_kick = { index: 45, name: "gem-kick" }
image_gem_kick_energy :: ImageID
image_gem_kick_energy = { index: 46, name: "gem-kick-energy" }
image_gem_mute :: ImageID
image_gem_mute = { index: 47, name: "gem-mute" }
image_gem_mute_hopo :: ImageID
image_gem_mute_hopo = { index: 48, name: "gem-mute-hopo" }
image_gem_mute_tap :: ImageID
image_gem_mute_tap = { index: 49, name: "gem-mute-tap" }
image_gem_open :: ImageID
image_gem_open = { index: 50, name: "gem-open" }
image_gem_open_energy :: ImageID
image_gem_open_energy = { index: 51, name: "gem-open-energy" }
image_gem_open_energy_hopo :: ImageID
image_gem_open_energy_hopo = { index: 52, name: "gem-open-energy-hopo" }
image_gem_open_energy_tap :: ImageID
image_gem_open_energy_tap = { index: 53, name: "gem-open-energy-tap" }
image_gem_open_hopo :: ImageID
image_gem_open_hopo = { index: 54, name: "gem-open-hopo" }
image_gem_open_tap :: ImageID
image_gem_open_tap = { index: 55, name: "gem-open-tap" }
image_gem_openghl :: ImageID
image_gem_openghl = { index: 56, name: "gem-openghl" }
image_gem_openghl_energy :: ImageID
image_gem_openghl_energy = { index: 57, name: "gem-openghl-energy" }
image_gem_openghl_hopo :: ImageID
image_gem_openghl_hopo = { index: 58, name: "gem-openghl-hopo" }
image_gem_openghl_tap :: ImageID
image_gem_openghl_tap = { index: 59, name: "gem-openghl-tap" }
image_gem_orange :: ImageID
image_gem_orange = { index: 60, name: "gem-orange" }
image_gem_orange_cymbal :: ImageID
image_gem_orange_cymbal = { index: 61, name: "gem-orange-cymbal" }
image_gem_orange_hopo :: ImageID
image_gem_orange_hopo = { index: 62, name: "gem-orange-hopo" }
image_gem_orange_pro :: ImageID
image_gem_orange_pro = { index: 63, name: "gem-orange-pro" }
image_gem_orange_pro_hopo :: ImageID
image_gem_orange_pro_hopo = { index: 64, name: "gem-orange-pro-hopo" }
image_gem_orange_pro_tap :: ImageID
image_gem_orange_pro_tap = { index: 65, name: "gem-orange-pro-tap" }
image_gem_orange_tap :: ImageID
image_gem_orange_tap = { index: 66, name: "gem-orange-tap" }
image_gem_purple_pro :: ImageID
image_gem_purple_pro = { index: 67, name: "gem-purple-pro" }
image_gem_purple_pro_hopo :: ImageID
image_gem_purple_pro_hopo = { index: 68, name: "gem-purple-pro-hopo" }
image_gem_purple_pro_tap :: ImageID
image_gem_purple_pro_tap = { index: 69, name: "gem-purple-pro-tap" }
image_gem_red :: ImageID
image_gem_red = { index: 70, name: "gem-red" }
image_gem_red_cymbal :: ImageID
image_gem_red_cymbal = { index: 71, name: "gem-red-cymbal" }
image_gem_red_hopo :: ImageID
image_gem_red_hopo = { index: 72, name: "gem-red-hopo" }
image_gem_red_pro :: ImageID
image_gem_red_pro = { index: 73, name: "gem-red-pro" }
image_gem_red_pro_hopo :: ImageID
image_gem_red_pro_hopo = { index: 74, name: "gem-red-pro-hopo" }
image_gem_red_pro_tap :: ImageID
image_gem_red_pro_tap = { index: 75, name: "gem-red-pro-tap" }
image_gem_red_rimshot :: ImageID
image_gem_red_rimshot = { index: 76, name: "gem-red-rimshot" }
image_gem_red_tap :: ImageID
image_gem_red_tap = { index: 77, name: "gem-red-tap" }
image_gem_white :: ImageID
image_gem_white = { index: 78, name: "gem-white" }
image_gem_white_hopo :: ImageID
image_gem_white_hopo = { index: 79, name: "gem-white-hopo" }
image_gem_white_tap :: ImageID
image_gem_white_tap = { index: 80, name: "gem-white-tap" }
image_gem_whiteblack :: ImageID
image_gem_whiteblack = { index: 81, name: "gem-whiteblack" }
image_gem_whiteblack_hopo :: ImageID
image_gem_whiteblack_hopo = { index: 82, name: "gem-whiteblack-hopo" }
image_gem_whiteblack_tap :: ImageID
image_gem_whiteblack_tap = { index: 83, name: "gem-whiteblack-tap" }
image_gem_whitekey :: ImageID
image_gem_whitekey = { index: 84, name: "gem-whitekey" }
image_gem_whitekey_energy :: ImageID
image_gem_whitekey_energy = { index: 85, name: "gem-whitekey-energy" }
image_gem_yellow :: ImageID
image_gem_yellow = { index: 86, name: "gem-yellow" }
image_gem_yellow_cymbal :: ImageID
image_gem_yellow_cymbal = { index: 87, name: "gem-yellow-cymbal" }
image_gem_yellow_hhclosed :: ImageID
image_gem_yellow_hhclosed = { index: 88, name: "gem-yellow-hhclosed" }
image_gem_yellow_hhopen :: ImageID
image_gem_yellow_hhopen = { index: 89, name: "gem-yellow-hhopen" }
image_gem_yellow_hhpedal :: ImageID
image_gem_yellow_hhpedal = { index: 90, name: "gem-yellow-hhpedal" }
image_gem_yellow_hopo :: ImageID
image_gem_yellow_hopo = { index: 91, name: "gem-yellow-hopo" }
image_gem_yellow_pro :: ImageID
image_gem_yellow_pro = { index: 92, name: "gem-yellow-pro" }
image_gem_yellow_pro_hopo :: ImageID
image_gem_yellow_pro_hopo = { index: 93, name: "gem-yellow-pro-hopo" }
image_gem_yellow_pro_tap :: ImageID
image_gem_yellow_pro_tap = { index: 94, name: "gem-yellow-pro-tap" }
image_gem_yellow_tap :: ImageID
image_gem_yellow_tap = { index: 95, name: "gem-yellow-tap" }
image_highway_catch_target :: ImageID
image_highway_catch_target = { index: 96, name: "highway-catch-target" }
image_highway_ghl_target :: ImageID
image_highway_ghl_target = { index: 97, name: "highway-ghl-target" }
image_highway_prokeys_target :: ImageID
image_highway_prokeys_target = { index: 98, name: "highway-prokeys-target" }
image_highway_protar_target_blue :: ImageID
image_highway_protar_target_blue = { index: 99, name: "highway-protar-target-blue" }
image_highway_protar_target_green :: ImageID
image_highway_protar_target_green = { index: 100, name: "highway-protar-target-green" }
image_highway_protar_target_orange :: ImageID
image_highway_protar_target_orange = { index: 101, name: "highway-protar-target-orange" }
image_highway_protar_target_purple :: ImageID
image_highway_protar_target_purple = { index: 102, name: "highway-protar-target-purple" }
image_highway_protar_target_red :: ImageID
image_highway_protar_target_red = { index: 103, name: "highway-protar-target-red" }
image_highway_protar_target_yellow :: ImageID
image_highway_protar_target_yellow = { index: 104, name: "highway-protar-target-yellow" }
image_highway_target_blue :: ImageID
image_highway_target_blue = { index: 105, name: "highway-target-blue" }
image_highway_target_green :: ImageID
image_highway_target_green = { index: 106, name: "highway-target-green" }
image_highway_target_orange :: ImageID
image_highway_target_orange = { index: 107, name: "highway-target-orange" }
image_highway_target_red :: ImageID
image_highway_target_red = { index: 108, name: "highway-target-red" }
image_highway_target_yellow :: ImageID
image_highway_target_yellow = { index: 109, name: "highway-target-yellow" }
image_icon_amplitude :: ImageID
image_icon_amplitude = { index: 110, name: "icon-amplitude" }
image_icon_bass :: ImageID
image_icon_bass = { index: 111, name: "icon-bass" }
image_icon_drums :: ImageID
image_icon_drums = { index: 112, name: "icon-drums" }
image_icon_ghl :: ImageID
image_icon_ghl = { index: 113, name: "icon-ghl" }
image_icon_guitar :: ImageID
image_icon_guitar = { index: 114, name: "icon-guitar" }
image_icon_keys :: ImageID
image_icon_keys = { index: 115, name: "icon-keys" }
image_icon_pro_bass :: ImageID
image_icon_pro_bass = { index: 116, name: "icon-pro-bass" }
image_icon_pro_drums :: ImageID
image_icon_pro_drums = { index: 117, name: "icon-pro-drums" }
image_icon_pro_guitar :: ImageID
image_icon_pro_guitar = { index: 118, name: "icon-pro-guitar" }
image_icon_pro_keys :: ImageID
image_icon_pro_keys = { index: 119, name: "icon-pro-keys" }
image_icon_vocal_1 :: ImageID
image_icon_vocal_1 = { index: 120, name: "icon-vocal-1" }
image_icon_vocal_2 :: ImageID
image_icon_vocal_2 = { index: 121, name: "icon-vocal-2" }
image_icon_vocal_3 :: ImageID
image_icon_vocal_3 = { index: 122, name: "icon-vocal-3" }
image_pro_fret_00 :: ImageID
image_pro_fret_00 = { index: 123, name: "pro-fret-00" }
image_pro_fret_01 :: ImageID
image_pro_fret_01 = { index: 124, name: "pro-fret-01" }
image_pro_fret_02 :: ImageID
image_pro_fret_02 = { index: 125, name: "pro-fret-02" }
image_pro_fret_03 :: ImageID
image_pro_fret_03 = { index: 126, name: "pro-fret-03" }
image_pro_fret_04 :: ImageID
image_pro_fret_04 = { index: 127, name: "pro-fret-04" }
image_pro_fret_05 :: ImageID
image_pro_fret_05 = { index: 128, name: "pro-fret-05" }
image_pro_fret_06 :: ImageID
image_pro_fret_06 = { index: 129, name: "pro-fret-06" }
image_pro_fret_07 :: ImageID
image_pro_fret_07 = { index: 130, name: "pro-fret-07" }
image_pro_fret_08 :: ImageID
image_pro_fret_08 = { index: 131, name: "pro-fret-08" }
image_pro_fret_09 :: ImageID
image_pro_fret_09 = { index: 132, name: "pro-fret-09" }
image_pro_fret_10 :: ImageID
image_pro_fret_10 = { index: 133, name: "pro-fret-10" }
image_pro_fret_11 :: ImageID
image_pro_fret_11 = { index: 134, name: "pro-fret-11" }
image_pro_fret_12 :: ImageID
image_pro_fret_12 = { index: 135, name: "pro-fret-12" }
image_pro_fret_13 :: ImageID
image_pro_fret_13 = { index: 136, name: "pro-fret-13" }
image_pro_fret_14 :: ImageID
image_pro_fret_14 = { index: 137, name: "pro-fret-14" }
image_pro_fret_15 :: ImageID
image_pro_fret_15 = { index: 138, name: "pro-fret-15" }
image_pro_fret_16 :: ImageID
image_pro_fret_16 = { index: 139, name: "pro-fret-16" }
image_pro_fret_17 :: ImageID
image_pro_fret_17 = { index: 140, name: "pro-fret-17" }
image_pro_fret_18 :: ImageID
image_pro_fret_18 = { index: 141, name: "pro-fret-18" }
image_pro_fret_19 :: ImageID
image_pro_fret_19 = { index: 142, name: "pro-fret-19" }
image_pro_fret_20 :: ImageID
image_pro_fret_20 = { index: 143, name: "pro-fret-20" }
image_pro_fret_21 :: ImageID
image_pro_fret_21 = { index: 144, name: "pro-fret-21" }
image_pro_fret_22 :: ImageID
image_pro_fret_22 = { index: 145, name: "pro-fret-22" }
image_pro_fret_23 :: ImageID
image_pro_fret_23 = { index: 146, name: "pro-fret-23" }
image_pro_fret_24 :: ImageID
image_pro_fret_24 = { index: 147, name: "pro-fret-24" }

allImageIDs :: Array ImageID
allImageIDs = [image_button_gear, image_button_pause, image_button_play, image_gem_black, image_gem_black_hopo, image_gem_black_tap, image_gem_blackkey, image_gem_blackkey_energy, image_gem_blackwhite, image_gem_blackwhite_hopo, image_gem_blackwhite_tap, image_gem_blue, image_gem_blue_cymbal, image_gem_blue_hhclosed, image_gem_blue_hhopen, image_gem_blue_hhpedal, image_gem_blue_hopo, image_gem_blue_pro, image_gem_blue_pro_hopo, image_gem_blue_pro_tap, image_gem_blue_tap, image_gem_catch, image_gem_energy, image_gem_energy_cymbal, image_gem_energy_hhclosed, image_gem_energy_hhopen, image_gem_energy_hhpedal, image_gem_energy_hopo, image_gem_energy_mute, image_gem_energy_mute_hopo, image_gem_energy_mute_tap, image_gem_energy_pro, image_gem_energy_pro_hopo, image_gem_energy_pro_tap, image_gem_energy_rimshot, image_gem_energy_tap, image_gem_ghl_energy, image_gem_green, image_gem_green_cymbal, image_gem_green_hopo, image_gem_green_pro, image_gem_green_pro_hopo, image_gem_green_pro_tap, image_gem_green_rimshot, image_gem_green_tap, image_gem_kick, image_gem_kick_energy, image_gem_mute, image_gem_mute_hopo, image_gem_mute_tap, image_gem_open, image_gem_open_energy, image_gem_open_energy_hopo, image_gem_open_energy_tap, image_gem_open_hopo, image_gem_open_tap, image_gem_openghl, image_gem_openghl_energy, image_gem_openghl_hopo, image_gem_openghl_tap, image_gem_orange, image_gem_orange_cymbal, image_gem_orange_hopo, image_gem_orange_pro, image_gem_orange_pro_hopo, image_gem_orange_pro_tap, image_gem_orange_tap, image_gem_purple_pro, image_gem_purple_pro_hopo, image_gem_purple_pro_tap, image_gem_red, image_gem_red_cymbal, image_gem_red_hopo, image_gem_red_pro, image_gem_red_pro_hopo, image_gem_red_pro_tap, image_gem_red_rimshot, image_gem_red_tap, image_gem_white, image_gem_white_hopo, image_gem_white_tap, image_gem_whiteblack, image_gem_whiteblack_hopo, image_gem_whiteblack_tap, image_gem_whitekey, image_gem_whitekey_energy, image_gem_yellow, image_gem_yellow_cymbal, image_gem_yellow_hhclosed, image_gem_yellow_hhopen, image_gem_yellow_hhpedal, image_gem_yellow_hopo, image_gem_yellow_pro, image_gem_yellow_pro_hopo, image_gem_yellow_pro_tap, image_gem_yellow_tap, image_highway_catch_target, image_highway_ghl_target, image_highway_prokeys_target, image_highway_protar_target_blue, image_highway_protar_target_green, image_highway_protar_target_orange, image_highway_protar_target_purple, image_highway_protar_target_red, image_highway_protar_target_yellow, image_highway_target_blue, image_highway_target_green, image_highway_target_orange, image_highway_target_red, image_highway_target_yellow, image_icon_amplitude, image_icon_bass, image_icon_drums, image_icon_ghl, image_icon_guitar, image_icon_keys, image_icon_pro_bass, image_icon_pro_drums, image_icon_pro_guitar, image_icon_pro_keys, image_icon_vocal_1, image_icon_vocal_2, image_icon_vocal_3, image_pro_fret_00, image_pro_fret_01, image_pro_fret_02, image_pro_fret_03, image_pro_fret_04, image_pro_fret_05, image_pro_fret_06, image_pro_fret_07, image_pro_fret_08, image_pro_fret_09, image_pro_fret_10, image_pro_fret_11, image_pro_fret_12, image_pro_fret_13, image_pro_fret_14, image_pro_fret_15, image_pro_fret_16, image_pro_fret_17, image_pro_fret_18, image_pro_fret_19, image_pro_fret_20, image_pro_fret_21, image_pro_fret_22, image_pro_fret_23, image_pro_fret_24]

-- end generated section

imageURL :: ImageID -> String
imageURL iid = "images/" <> iid.name <> ".png"

withImages
  :: ((ImageID -> CanvasImageSource) -> Effect Unit)
  -> Effect Unit
withImages = let
  loadTuple iid = ContT $ withImage $ imageURL iid
  withImage path f = tryLoadImage path $ \mimg -> case mimg of
    Just img -> f img
    Nothing  -> throwException $ error $ "could not load image from path: " <> path
  pairsToFn :: Array CanvasImageSource -> ImageID -> CanvasImageSource
  pairsToFn pairs iid = case Array.index pairs iid.index of
    Just img -> img
    Nothing  -> unsafeThrow $ "panic! couldn't access image " <> iid.name
  in runContT $ map pairsToFn $ parTraverse loadTuple allImageIDs

protarFrets :: Array ImageID
protarFrets =
  [ image_pro_fret_00
  , image_pro_fret_01
  , image_pro_fret_02
  , image_pro_fret_03
  , image_pro_fret_04
  , image_pro_fret_05
  , image_pro_fret_06
  , image_pro_fret_07
  , image_pro_fret_08
  , image_pro_fret_09
  , image_pro_fret_10
  , image_pro_fret_11
  , image_pro_fret_12
  , image_pro_fret_13
  , image_pro_fret_14
  , image_pro_fret_15
  , image_pro_fret_16
  , image_pro_fret_17
  , image_pro_fret_18
  , image_pro_fret_19
  , image_pro_fret_20
  , image_pro_fret_21
  , image_pro_fret_22
  , image_pro_fret_23
  , image_pro_fret_24
  ]
