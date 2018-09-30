module Images (ImageID(..), withImages, protarFrets) where

import           Prelude

import           Control.Monad.Cont      (ContT (..), runContT)
import           Control.Parallel        (parTraverse)
import           Data.Map                as Map
import           Data.Maybe              (Maybe (..))
import           Data.Tuple              (Tuple (..))
import           Effect                  (Effect)
import           Effect.Exception        (error, throwException)
import           Effect.Exception.Unsafe (unsafeThrow)
import           Graphics.Canvas         (CanvasImageSource, tryLoadImage)

-- starting from here, generated with images.rb

data ImageID
  = Image_button_gear
  | Image_button_pause
  | Image_button_play
  | Image_gem_black
  | Image_gem_black_hopo
  | Image_gem_black_tap
  | Image_gem_blackkey
  | Image_gem_blackkey_energy
  | Image_gem_blackwhite
  | Image_gem_blackwhite_hopo
  | Image_gem_blackwhite_tap
  | Image_gem_blue
  | Image_gem_blue_cymbal
  | Image_gem_blue_hopo
  | Image_gem_blue_pro
  | Image_gem_blue_pro_hopo
  | Image_gem_blue_pro_tap
  | Image_gem_blue_tap
  | Image_gem_catch
  | Image_gem_energy
  | Image_gem_energy_cymbal
  | Image_gem_energy_hopo
  | Image_gem_energy_mute
  | Image_gem_energy_mute_hopo
  | Image_gem_energy_mute_tap
  | Image_gem_energy_pro
  | Image_gem_energy_pro_hopo
  | Image_gem_energy_pro_tap
  | Image_gem_energy_tap
  | Image_gem_ghl_energy
  | Image_gem_green
  | Image_gem_green_cymbal
  | Image_gem_green_hopo
  | Image_gem_green_pro
  | Image_gem_green_pro_hopo
  | Image_gem_green_pro_tap
  | Image_gem_green_tap
  | Image_gem_kick
  | Image_gem_kick_energy
  | Image_gem_mute
  | Image_gem_mute_hopo
  | Image_gem_mute_tap
  | Image_gem_open
  | Image_gem_open_energy
  | Image_gem_open_energy_hopo
  | Image_gem_open_energy_tap
  | Image_gem_open_hopo
  | Image_gem_open_tap
  | Image_gem_openghl
  | Image_gem_openghl_energy
  | Image_gem_openghl_hopo
  | Image_gem_openghl_tap
  | Image_gem_orange
  | Image_gem_orange_cymbal
  | Image_gem_orange_hopo
  | Image_gem_orange_pro
  | Image_gem_orange_pro_hopo
  | Image_gem_orange_pro_tap
  | Image_gem_orange_tap
  | Image_gem_purple_pro
  | Image_gem_purple_pro_hopo
  | Image_gem_purple_pro_tap
  | Image_gem_red
  | Image_gem_red_cymbal
  | Image_gem_red_hopo
  | Image_gem_red_pro
  | Image_gem_red_pro_hopo
  | Image_gem_red_pro_tap
  | Image_gem_red_tap
  | Image_gem_white
  | Image_gem_white_hopo
  | Image_gem_white_tap
  | Image_gem_whiteblack
  | Image_gem_whiteblack_hopo
  | Image_gem_whiteblack_tap
  | Image_gem_whitekey
  | Image_gem_whitekey_energy
  | Image_gem_yellow
  | Image_gem_yellow_cymbal
  | Image_gem_yellow_hopo
  | Image_gem_yellow_pro
  | Image_gem_yellow_pro_hopo
  | Image_gem_yellow_pro_tap
  | Image_gem_yellow_tap
  | Image_highway_catch_target
  | Image_highway_ghl_target
  | Image_highway_prokeys_target
  | Image_highway_protar_target_blue
  | Image_highway_protar_target_green
  | Image_highway_protar_target_orange
  | Image_highway_protar_target_purple
  | Image_highway_protar_target_red
  | Image_highway_protar_target_yellow
  | Image_highway_target_blue
  | Image_highway_target_green
  | Image_highway_target_orange
  | Image_highway_target_red
  | Image_highway_target_yellow
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
  | Image_pro_fret_23
  | Image_pro_fret_24

allImageIDs :: Array ImageID
allImageIDs = [Image_button_gear, Image_button_pause, Image_button_play, Image_gem_black, Image_gem_black_hopo, Image_gem_black_tap, Image_gem_blackkey, Image_gem_blackkey_energy, Image_gem_blackwhite, Image_gem_blackwhite_hopo, Image_gem_blackwhite_tap, Image_gem_blue, Image_gem_blue_cymbal, Image_gem_blue_hopo, Image_gem_blue_pro, Image_gem_blue_pro_hopo, Image_gem_blue_pro_tap, Image_gem_blue_tap, Image_gem_catch, Image_gem_energy, Image_gem_energy_cymbal, Image_gem_energy_hopo, Image_gem_energy_mute, Image_gem_energy_mute_hopo, Image_gem_energy_mute_tap, Image_gem_energy_pro, Image_gem_energy_pro_hopo, Image_gem_energy_pro_tap, Image_gem_energy_tap, Image_gem_ghl_energy, Image_gem_green, Image_gem_green_cymbal, Image_gem_green_hopo, Image_gem_green_pro, Image_gem_green_pro_hopo, Image_gem_green_pro_tap, Image_gem_green_tap, Image_gem_kick, Image_gem_kick_energy, Image_gem_mute, Image_gem_mute_hopo, Image_gem_mute_tap, Image_gem_open, Image_gem_open_energy, Image_gem_open_energy_hopo, Image_gem_open_energy_tap, Image_gem_open_hopo, Image_gem_open_tap, Image_gem_openghl, Image_gem_openghl_energy, Image_gem_openghl_hopo, Image_gem_openghl_tap, Image_gem_orange, Image_gem_orange_cymbal, Image_gem_orange_hopo, Image_gem_orange_pro, Image_gem_orange_pro_hopo, Image_gem_orange_pro_tap, Image_gem_orange_tap, Image_gem_purple_pro, Image_gem_purple_pro_hopo, Image_gem_purple_pro_tap, Image_gem_red, Image_gem_red_cymbal, Image_gem_red_hopo, Image_gem_red_pro, Image_gem_red_pro_hopo, Image_gem_red_pro_tap, Image_gem_red_tap, Image_gem_white, Image_gem_white_hopo, Image_gem_white_tap, Image_gem_whiteblack, Image_gem_whiteblack_hopo, Image_gem_whiteblack_tap, Image_gem_whitekey, Image_gem_whitekey_energy, Image_gem_yellow, Image_gem_yellow_cymbal, Image_gem_yellow_hopo, Image_gem_yellow_pro, Image_gem_yellow_pro_hopo, Image_gem_yellow_pro_tap, Image_gem_yellow_tap, Image_highway_catch_target, Image_highway_ghl_target, Image_highway_prokeys_target, Image_highway_protar_target_blue, Image_highway_protar_target_green, Image_highway_protar_target_orange, Image_highway_protar_target_purple, Image_highway_protar_target_red, Image_highway_protar_target_yellow, Image_highway_target_blue, Image_highway_target_green, Image_highway_target_orange, Image_highway_target_red, Image_highway_target_yellow, Image_pro_fret_00, Image_pro_fret_01, Image_pro_fret_02, Image_pro_fret_03, Image_pro_fret_04, Image_pro_fret_05, Image_pro_fret_06, Image_pro_fret_07, Image_pro_fret_08, Image_pro_fret_09, Image_pro_fret_10, Image_pro_fret_11, Image_pro_fret_12, Image_pro_fret_13, Image_pro_fret_14, Image_pro_fret_15, Image_pro_fret_16, Image_pro_fret_17, Image_pro_fret_18, Image_pro_fret_19, Image_pro_fret_20, Image_pro_fret_21, Image_pro_fret_22, Image_pro_fret_23, Image_pro_fret_24]

imagePath :: ImageID -> String
imagePath Image_button_gear = "button-gear"
imagePath Image_button_pause = "button-pause"
imagePath Image_button_play = "button-play"
imagePath Image_gem_black = "gem-black"
imagePath Image_gem_black_hopo = "gem-black-hopo"
imagePath Image_gem_black_tap = "gem-black-tap"
imagePath Image_gem_blackkey = "gem-blackkey"
imagePath Image_gem_blackkey_energy = "gem-blackkey-energy"
imagePath Image_gem_blackwhite = "gem-blackwhite"
imagePath Image_gem_blackwhite_hopo = "gem-blackwhite-hopo"
imagePath Image_gem_blackwhite_tap = "gem-blackwhite-tap"
imagePath Image_gem_blue = "gem-blue"
imagePath Image_gem_blue_cymbal = "gem-blue-cymbal"
imagePath Image_gem_blue_hopo = "gem-blue-hopo"
imagePath Image_gem_blue_pro = "gem-blue-pro"
imagePath Image_gem_blue_pro_hopo = "gem-blue-pro-hopo"
imagePath Image_gem_blue_pro_tap = "gem-blue-pro-tap"
imagePath Image_gem_blue_tap = "gem-blue-tap"
imagePath Image_gem_catch = "gem-catch"
imagePath Image_gem_energy = "gem-energy"
imagePath Image_gem_energy_cymbal = "gem-energy-cymbal"
imagePath Image_gem_energy_hopo = "gem-energy-hopo"
imagePath Image_gem_energy_mute = "gem-energy-mute"
imagePath Image_gem_energy_mute_hopo = "gem-energy-mute-hopo"
imagePath Image_gem_energy_mute_tap = "gem-energy-mute-tap"
imagePath Image_gem_energy_pro = "gem-energy-pro"
imagePath Image_gem_energy_pro_hopo = "gem-energy-pro-hopo"
imagePath Image_gem_energy_pro_tap = "gem-energy-pro-tap"
imagePath Image_gem_energy_tap = "gem-energy-tap"
imagePath Image_gem_ghl_energy = "gem-ghl-energy"
imagePath Image_gem_green = "gem-green"
imagePath Image_gem_green_cymbal = "gem-green-cymbal"
imagePath Image_gem_green_hopo = "gem-green-hopo"
imagePath Image_gem_green_pro = "gem-green-pro"
imagePath Image_gem_green_pro_hopo = "gem-green-pro-hopo"
imagePath Image_gem_green_pro_tap = "gem-green-pro-tap"
imagePath Image_gem_green_tap = "gem-green-tap"
imagePath Image_gem_kick = "gem-kick"
imagePath Image_gem_kick_energy = "gem-kick-energy"
imagePath Image_gem_mute = "gem-mute"
imagePath Image_gem_mute_hopo = "gem-mute-hopo"
imagePath Image_gem_mute_tap = "gem-mute-tap"
imagePath Image_gem_open = "gem-open"
imagePath Image_gem_open_energy = "gem-open-energy"
imagePath Image_gem_open_energy_hopo = "gem-open-energy-hopo"
imagePath Image_gem_open_energy_tap = "gem-open-energy-tap"
imagePath Image_gem_open_hopo = "gem-open-hopo"
imagePath Image_gem_open_tap = "gem-open-tap"
imagePath Image_gem_openghl = "gem-openghl"
imagePath Image_gem_openghl_energy = "gem-openghl-energy"
imagePath Image_gem_openghl_hopo = "gem-openghl-hopo"
imagePath Image_gem_openghl_tap = "gem-openghl-tap"
imagePath Image_gem_orange = "gem-orange"
imagePath Image_gem_orange_cymbal = "gem-orange-cymbal"
imagePath Image_gem_orange_hopo = "gem-orange-hopo"
imagePath Image_gem_orange_pro = "gem-orange-pro"
imagePath Image_gem_orange_pro_hopo = "gem-orange-pro-hopo"
imagePath Image_gem_orange_pro_tap = "gem-orange-pro-tap"
imagePath Image_gem_orange_tap = "gem-orange-tap"
imagePath Image_gem_purple_pro = "gem-purple-pro"
imagePath Image_gem_purple_pro_hopo = "gem-purple-pro-hopo"
imagePath Image_gem_purple_pro_tap = "gem-purple-pro-tap"
imagePath Image_gem_red = "gem-red"
imagePath Image_gem_red_cymbal = "gem-red-cymbal"
imagePath Image_gem_red_hopo = "gem-red-hopo"
imagePath Image_gem_red_pro = "gem-red-pro"
imagePath Image_gem_red_pro_hopo = "gem-red-pro-hopo"
imagePath Image_gem_red_pro_tap = "gem-red-pro-tap"
imagePath Image_gem_red_tap = "gem-red-tap"
imagePath Image_gem_white = "gem-white"
imagePath Image_gem_white_hopo = "gem-white-hopo"
imagePath Image_gem_white_tap = "gem-white-tap"
imagePath Image_gem_whiteblack = "gem-whiteblack"
imagePath Image_gem_whiteblack_hopo = "gem-whiteblack-hopo"
imagePath Image_gem_whiteblack_tap = "gem-whiteblack-tap"
imagePath Image_gem_whitekey = "gem-whitekey"
imagePath Image_gem_whitekey_energy = "gem-whitekey-energy"
imagePath Image_gem_yellow = "gem-yellow"
imagePath Image_gem_yellow_cymbal = "gem-yellow-cymbal"
imagePath Image_gem_yellow_hopo = "gem-yellow-hopo"
imagePath Image_gem_yellow_pro = "gem-yellow-pro"
imagePath Image_gem_yellow_pro_hopo = "gem-yellow-pro-hopo"
imagePath Image_gem_yellow_pro_tap = "gem-yellow-pro-tap"
imagePath Image_gem_yellow_tap = "gem-yellow-tap"
imagePath Image_highway_catch_target = "highway-catch-target"
imagePath Image_highway_ghl_target = "highway-ghl-target"
imagePath Image_highway_prokeys_target = "highway-prokeys-target"
imagePath Image_highway_protar_target_blue = "highway-protar-target-blue"
imagePath Image_highway_protar_target_green = "highway-protar-target-green"
imagePath Image_highway_protar_target_orange = "highway-protar-target-orange"
imagePath Image_highway_protar_target_purple = "highway-protar-target-purple"
imagePath Image_highway_protar_target_red = "highway-protar-target-red"
imagePath Image_highway_protar_target_yellow = "highway-protar-target-yellow"
imagePath Image_highway_target_blue = "highway-target-blue"
imagePath Image_highway_target_green = "highway-target-green"
imagePath Image_highway_target_orange = "highway-target-orange"
imagePath Image_highway_target_red = "highway-target-red"
imagePath Image_highway_target_yellow = "highway-target-yellow"
imagePath Image_pro_fret_00 = "pro-fret-00"
imagePath Image_pro_fret_01 = "pro-fret-01"
imagePath Image_pro_fret_02 = "pro-fret-02"
imagePath Image_pro_fret_03 = "pro-fret-03"
imagePath Image_pro_fret_04 = "pro-fret-04"
imagePath Image_pro_fret_05 = "pro-fret-05"
imagePath Image_pro_fret_06 = "pro-fret-06"
imagePath Image_pro_fret_07 = "pro-fret-07"
imagePath Image_pro_fret_08 = "pro-fret-08"
imagePath Image_pro_fret_09 = "pro-fret-09"
imagePath Image_pro_fret_10 = "pro-fret-10"
imagePath Image_pro_fret_11 = "pro-fret-11"
imagePath Image_pro_fret_12 = "pro-fret-12"
imagePath Image_pro_fret_13 = "pro-fret-13"
imagePath Image_pro_fret_14 = "pro-fret-14"
imagePath Image_pro_fret_15 = "pro-fret-15"
imagePath Image_pro_fret_16 = "pro-fret-16"
imagePath Image_pro_fret_17 = "pro-fret-17"
imagePath Image_pro_fret_18 = "pro-fret-18"
imagePath Image_pro_fret_19 = "pro-fret-19"
imagePath Image_pro_fret_20 = "pro-fret-20"
imagePath Image_pro_fret_21 = "pro-fret-21"
imagePath Image_pro_fret_22 = "pro-fret-22"
imagePath Image_pro_fret_23 = "pro-fret-23"
imagePath Image_pro_fret_24 = "pro-fret-24"

imageNumber :: ImageID -> Int
imageNumber Image_button_gear = 0
imageNumber Image_button_pause = 1
imageNumber Image_button_play = 2
imageNumber Image_gem_black = 3
imageNumber Image_gem_black_hopo = 4
imageNumber Image_gem_black_tap = 5
imageNumber Image_gem_blackkey = 6
imageNumber Image_gem_blackkey_energy = 7
imageNumber Image_gem_blackwhite = 8
imageNumber Image_gem_blackwhite_hopo = 9
imageNumber Image_gem_blackwhite_tap = 10
imageNumber Image_gem_blue = 11
imageNumber Image_gem_blue_cymbal = 12
imageNumber Image_gem_blue_hopo = 13
imageNumber Image_gem_blue_pro = 14
imageNumber Image_gem_blue_pro_hopo = 15
imageNumber Image_gem_blue_pro_tap = 16
imageNumber Image_gem_blue_tap = 17
imageNumber Image_gem_catch = 18
imageNumber Image_gem_energy = 19
imageNumber Image_gem_energy_cymbal = 20
imageNumber Image_gem_energy_hopo = 21
imageNumber Image_gem_energy_mute = 22
imageNumber Image_gem_energy_mute_hopo = 23
imageNumber Image_gem_energy_mute_tap = 24
imageNumber Image_gem_energy_pro = 25
imageNumber Image_gem_energy_pro_hopo = 26
imageNumber Image_gem_energy_pro_tap = 27
imageNumber Image_gem_energy_tap = 28
imageNumber Image_gem_ghl_energy = 29
imageNumber Image_gem_green = 30
imageNumber Image_gem_green_cymbal = 31
imageNumber Image_gem_green_hopo = 32
imageNumber Image_gem_green_pro = 33
imageNumber Image_gem_green_pro_hopo = 34
imageNumber Image_gem_green_pro_tap = 35
imageNumber Image_gem_green_tap = 36
imageNumber Image_gem_kick = 37
imageNumber Image_gem_kick_energy = 38
imageNumber Image_gem_mute = 39
imageNumber Image_gem_mute_hopo = 40
imageNumber Image_gem_mute_tap = 41
imageNumber Image_gem_open = 42
imageNumber Image_gem_open_energy = 43
imageNumber Image_gem_open_energy_hopo = 44
imageNumber Image_gem_open_energy_tap = 45
imageNumber Image_gem_open_hopo = 46
imageNumber Image_gem_open_tap = 47
imageNumber Image_gem_openghl = 48
imageNumber Image_gem_openghl_energy = 49
imageNumber Image_gem_openghl_hopo = 50
imageNumber Image_gem_openghl_tap = 51
imageNumber Image_gem_orange = 52
imageNumber Image_gem_orange_cymbal = 53
imageNumber Image_gem_orange_hopo = 54
imageNumber Image_gem_orange_pro = 55
imageNumber Image_gem_orange_pro_hopo = 56
imageNumber Image_gem_orange_pro_tap = 57
imageNumber Image_gem_orange_tap = 58
imageNumber Image_gem_purple_pro = 59
imageNumber Image_gem_purple_pro_hopo = 60
imageNumber Image_gem_purple_pro_tap = 61
imageNumber Image_gem_red = 62
imageNumber Image_gem_red_cymbal = 63
imageNumber Image_gem_red_hopo = 64
imageNumber Image_gem_red_pro = 65
imageNumber Image_gem_red_pro_hopo = 66
imageNumber Image_gem_red_pro_tap = 67
imageNumber Image_gem_red_tap = 68
imageNumber Image_gem_white = 69
imageNumber Image_gem_white_hopo = 70
imageNumber Image_gem_white_tap = 71
imageNumber Image_gem_whiteblack = 72
imageNumber Image_gem_whiteblack_hopo = 73
imageNumber Image_gem_whiteblack_tap = 74
imageNumber Image_gem_whitekey = 75
imageNumber Image_gem_whitekey_energy = 76
imageNumber Image_gem_yellow = 77
imageNumber Image_gem_yellow_cymbal = 78
imageNumber Image_gem_yellow_hopo = 79
imageNumber Image_gem_yellow_pro = 80
imageNumber Image_gem_yellow_pro_hopo = 81
imageNumber Image_gem_yellow_pro_tap = 82
imageNumber Image_gem_yellow_tap = 83
imageNumber Image_highway_catch_target = 84
imageNumber Image_highway_ghl_target = 85
imageNumber Image_highway_prokeys_target = 86
imageNumber Image_highway_protar_target_blue = 87
imageNumber Image_highway_protar_target_green = 88
imageNumber Image_highway_protar_target_orange = 89
imageNumber Image_highway_protar_target_purple = 90
imageNumber Image_highway_protar_target_red = 91
imageNumber Image_highway_protar_target_yellow = 92
imageNumber Image_highway_target_blue = 93
imageNumber Image_highway_target_green = 94
imageNumber Image_highway_target_orange = 95
imageNumber Image_highway_target_red = 96
imageNumber Image_highway_target_yellow = 97
imageNumber Image_pro_fret_00 = 98
imageNumber Image_pro_fret_01 = 99
imageNumber Image_pro_fret_02 = 100
imageNumber Image_pro_fret_03 = 101
imageNumber Image_pro_fret_04 = 102
imageNumber Image_pro_fret_05 = 103
imageNumber Image_pro_fret_06 = 104
imageNumber Image_pro_fret_07 = 105
imageNumber Image_pro_fret_08 = 106
imageNumber Image_pro_fret_09 = 107
imageNumber Image_pro_fret_10 = 108
imageNumber Image_pro_fret_11 = 109
imageNumber Image_pro_fret_12 = 110
imageNumber Image_pro_fret_13 = 111
imageNumber Image_pro_fret_14 = 112
imageNumber Image_pro_fret_15 = 113
imageNumber Image_pro_fret_16 = 114
imageNumber Image_pro_fret_17 = 115
imageNumber Image_pro_fret_18 = 116
imageNumber Image_pro_fret_19 = 117
imageNumber Image_pro_fret_20 = 118
imageNumber Image_pro_fret_21 = 119
imageNumber Image_pro_fret_22 = 120
imageNumber Image_pro_fret_23 = 121
imageNumber Image_pro_fret_24 = 122

-- end generated section

-- these used to use generics, which worked great.
-- purescript's derive Eq/Ord, and generics-rep,
-- have awful code size and compilation time for big enums...
instance eqImageID :: Eq ImageID where
  eq x y = eq (imageNumber x) (imageNumber y)
instance ordImageID :: Ord ImageID where
  compare x y = compare (imageNumber x) (imageNumber y)

withImages
  :: ((ImageID -> CanvasImageSource) -> Effect Unit)
  -> Effect Unit
withImages = let
  loadTuple iid = let
    path = "images/" <> imagePath iid <> ".png"
    in map (Tuple iid) $ ContT $ withImage path
  withImage path f = tryLoadImage path $ \mimg -> case mimg of
    Just img -> f img
    Nothing  -> throwException $ error $ "could not load image from path: " <> path
  pairsToFn :: Array (Tuple ImageID CanvasImageSource) -> ImageID -> CanvasImageSource
  pairsToFn pairs = let
    imageMap :: Map.Map ImageID CanvasImageSource
    imageMap = Map.fromFoldable pairs
    in \iid -> case Map.lookup iid imageMap of
      Just img -> img
      Nothing  -> unsafeThrow $ "panic! unknown image ID " <> imagePath iid
  in runContT $ map pairsToFn $ parTraverse loadTuple allImageIDs

protarFrets :: Array ImageID
protarFrets =
  [ Image_pro_fret_00
  , Image_pro_fret_01
  , Image_pro_fret_02
  , Image_pro_fret_03
  , Image_pro_fret_04
  , Image_pro_fret_05
  , Image_pro_fret_06
  , Image_pro_fret_07
  , Image_pro_fret_08
  , Image_pro_fret_09
  , Image_pro_fret_10
  , Image_pro_fret_11
  , Image_pro_fret_12
  , Image_pro_fret_13
  , Image_pro_fret_14
  , Image_pro_fret_15
  , Image_pro_fret_16
  , Image_pro_fret_17
  , Image_pro_fret_18
  , Image_pro_fret_19
  , Image_pro_fret_20
  , Image_pro_fret_21
  , Image_pro_fret_22
  , Image_pro_fret_23
  , Image_pro_fret_24
  ]
