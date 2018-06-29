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
  = Image_button_bass
  | Image_button_bass_off
  | Image_button_bass6
  | Image_button_bass6_off
  | Image_button_drums
  | Image_button_drums_off
  | Image_button_guitar
  | Image_button_guitar_off
  | Image_button_guitar6
  | Image_button_guitar6_off
  | Image_button_keys
  | Image_button_keys_off
  | Image_button_pause
  | Image_button_play
  | Image_button_probass
  | Image_button_probass_off
  | Image_button_proguitar
  | Image_button_proguitar_off
  | Image_button_prokeys
  | Image_button_prokeys_off
  | Image_button_vocal
  | Image_button_vocal_off
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
  | Image_highway_drums_bar
  | Image_highway_drums_beat
  | Image_highway_drums_halfbeat
  | Image_highway_drums_solo_edge
  | Image_highway_drums_target
  | Image_highway_drums5_target
  | Image_highway_ghl_bar
  | Image_highway_ghl_beat
  | Image_highway_ghl_solo_edge
  | Image_highway_ghl_target
  | Image_highway_grybo_bar
  | Image_highway_grybo_beat
  | Image_highway_grybo_halfbeat
  | Image_highway_grybo_solo_edge
  | Image_highway_grybo_target
  | Image_highway_grybo_target_lefty
  | Image_highway_prokeys_bar
  | Image_highway_prokeys_beat
  | Image_highway_prokeys_halfbeat
  | Image_highway_prokeys_solo_edge
  | Image_highway_prokeys_target
  | Image_highway_protar_beat
  | Image_highway_protar_halfbeat
  | Image_highway_protar_target
  | Image_highway_protar_target_lefty
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

allImageIDs :: Array ImageID
allImageIDs = [Image_button_bass, Image_button_bass_off, Image_button_bass6, Image_button_bass6_off, Image_button_drums, Image_button_drums_off, Image_button_guitar, Image_button_guitar_off, Image_button_guitar6, Image_button_guitar6_off, Image_button_keys, Image_button_keys_off, Image_button_pause, Image_button_play, Image_button_probass, Image_button_probass_off, Image_button_proguitar, Image_button_proguitar_off, Image_button_prokeys, Image_button_prokeys_off, Image_button_vocal, Image_button_vocal_off, Image_gem_black, Image_gem_black_hopo, Image_gem_black_tap, Image_gem_blackkey, Image_gem_blackkey_energy, Image_gem_blackwhite, Image_gem_blackwhite_hopo, Image_gem_blackwhite_tap, Image_gem_blue, Image_gem_blue_cymbal, Image_gem_blue_hopo, Image_gem_blue_pro, Image_gem_blue_pro_hopo, Image_gem_blue_pro_tap, Image_gem_blue_tap, Image_gem_energy, Image_gem_energy_cymbal, Image_gem_energy_hopo, Image_gem_energy_mute, Image_gem_energy_mute_hopo, Image_gem_energy_mute_tap, Image_gem_energy_pro, Image_gem_energy_pro_hopo, Image_gem_energy_pro_tap, Image_gem_energy_tap, Image_gem_ghl_energy, Image_gem_green, Image_gem_green_cymbal, Image_gem_green_hopo, Image_gem_green_pro, Image_gem_green_pro_hopo, Image_gem_green_pro_tap, Image_gem_green_tap, Image_gem_kick, Image_gem_kick_energy, Image_gem_mute, Image_gem_mute_hopo, Image_gem_mute_tap, Image_gem_open, Image_gem_open_energy, Image_gem_open_energy_hopo, Image_gem_open_energy_tap, Image_gem_open_hopo, Image_gem_open_tap, Image_gem_openghl, Image_gem_openghl_energy, Image_gem_openghl_hopo, Image_gem_openghl_tap, Image_gem_orange, Image_gem_orange_cymbal, Image_gem_orange_hopo, Image_gem_orange_pro, Image_gem_orange_pro_hopo, Image_gem_orange_pro_tap, Image_gem_orange_tap, Image_gem_purple_pro, Image_gem_purple_pro_hopo, Image_gem_purple_pro_tap, Image_gem_red, Image_gem_red_cymbal, Image_gem_red_hopo, Image_gem_red_pro, Image_gem_red_pro_hopo, Image_gem_red_pro_tap, Image_gem_red_tap, Image_gem_white, Image_gem_white_hopo, Image_gem_white_tap, Image_gem_whiteblack, Image_gem_whiteblack_hopo, Image_gem_whiteblack_tap, Image_gem_whitekey, Image_gem_whitekey_energy, Image_gem_yellow, Image_gem_yellow_cymbal, Image_gem_yellow_hopo, Image_gem_yellow_pro, Image_gem_yellow_pro_hopo, Image_gem_yellow_pro_tap, Image_gem_yellow_tap, Image_highway_drums_bar, Image_highway_drums_beat, Image_highway_drums_halfbeat, Image_highway_drums_solo_edge, Image_highway_drums_target, Image_highway_drums5_target, Image_highway_ghl_bar, Image_highway_ghl_beat, Image_highway_ghl_solo_edge, Image_highway_ghl_target, Image_highway_grybo_bar, Image_highway_grybo_beat, Image_highway_grybo_halfbeat, Image_highway_grybo_solo_edge, Image_highway_grybo_target, Image_highway_grybo_target_lefty, Image_highway_prokeys_bar, Image_highway_prokeys_beat, Image_highway_prokeys_halfbeat, Image_highway_prokeys_solo_edge, Image_highway_prokeys_target, Image_highway_protar_beat, Image_highway_protar_halfbeat, Image_highway_protar_target, Image_highway_protar_target_lefty, Image_pro_fret_00, Image_pro_fret_01, Image_pro_fret_02, Image_pro_fret_03, Image_pro_fret_04, Image_pro_fret_05, Image_pro_fret_06, Image_pro_fret_07, Image_pro_fret_08, Image_pro_fret_09, Image_pro_fret_10, Image_pro_fret_11, Image_pro_fret_12, Image_pro_fret_13, Image_pro_fret_14, Image_pro_fret_15, Image_pro_fret_16, Image_pro_fret_17, Image_pro_fret_18, Image_pro_fret_19, Image_pro_fret_20, Image_pro_fret_21, Image_pro_fret_22, Image_sustain_end, Image_sustain_key_end]

imagePath :: ImageID -> String
imagePath Image_button_bass = "button-bass"
imagePath Image_button_bass_off = "button-bass-off"
imagePath Image_button_bass6 = "button-bass6"
imagePath Image_button_bass6_off = "button-bass6-off"
imagePath Image_button_drums = "button-drums"
imagePath Image_button_drums_off = "button-drums-off"
imagePath Image_button_guitar = "button-guitar"
imagePath Image_button_guitar_off = "button-guitar-off"
imagePath Image_button_guitar6 = "button-guitar6"
imagePath Image_button_guitar6_off = "button-guitar6-off"
imagePath Image_button_keys = "button-keys"
imagePath Image_button_keys_off = "button-keys-off"
imagePath Image_button_pause = "button-pause"
imagePath Image_button_play = "button-play"
imagePath Image_button_probass = "button-probass"
imagePath Image_button_probass_off = "button-probass-off"
imagePath Image_button_proguitar = "button-proguitar"
imagePath Image_button_proguitar_off = "button-proguitar-off"
imagePath Image_button_prokeys = "button-prokeys"
imagePath Image_button_prokeys_off = "button-prokeys-off"
imagePath Image_button_vocal = "button-vocal"
imagePath Image_button_vocal_off = "button-vocal-off"
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
imagePath Image_highway_drums_bar = "highway-drums-bar"
imagePath Image_highway_drums_beat = "highway-drums-beat"
imagePath Image_highway_drums_halfbeat = "highway-drums-halfbeat"
imagePath Image_highway_drums_solo_edge = "highway-drums-solo-edge"
imagePath Image_highway_drums_target = "highway-drums-target"
imagePath Image_highway_drums5_target = "highway-drums5-target"
imagePath Image_highway_ghl_bar = "highway-ghl-bar"
imagePath Image_highway_ghl_beat = "highway-ghl-beat"
imagePath Image_highway_ghl_solo_edge = "highway-ghl-solo-edge"
imagePath Image_highway_ghl_target = "highway-ghl-target"
imagePath Image_highway_grybo_bar = "highway-grybo-bar"
imagePath Image_highway_grybo_beat = "highway-grybo-beat"
imagePath Image_highway_grybo_halfbeat = "highway-grybo-halfbeat"
imagePath Image_highway_grybo_solo_edge = "highway-grybo-solo-edge"
imagePath Image_highway_grybo_target = "highway-grybo-target"
imagePath Image_highway_grybo_target_lefty = "highway-grybo-target-lefty"
imagePath Image_highway_prokeys_bar = "highway-prokeys-bar"
imagePath Image_highway_prokeys_beat = "highway-prokeys-beat"
imagePath Image_highway_prokeys_halfbeat = "highway-prokeys-halfbeat"
imagePath Image_highway_prokeys_solo_edge = "highway-prokeys-solo-edge"
imagePath Image_highway_prokeys_target = "highway-prokeys-target"
imagePath Image_highway_protar_beat = "highway-protar-beat"
imagePath Image_highway_protar_halfbeat = "highway-protar-halfbeat"
imagePath Image_highway_protar_target = "highway-protar-target"
imagePath Image_highway_protar_target_lefty = "highway-protar-target-lefty"
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
imagePath Image_sustain_end = "sustain-end"
imagePath Image_sustain_key_end = "sustain-key-end"

imageNumber :: ImageID -> Int
imageNumber Image_button_bass = 0
imageNumber Image_button_bass_off = 1
imageNumber Image_button_bass6 = 2
imageNumber Image_button_bass6_off = 3
imageNumber Image_button_drums = 4
imageNumber Image_button_drums_off = 5
imageNumber Image_button_guitar = 6
imageNumber Image_button_guitar_off = 7
imageNumber Image_button_guitar6 = 8
imageNumber Image_button_guitar6_off = 9
imageNumber Image_button_keys = 10
imageNumber Image_button_keys_off = 11
imageNumber Image_button_pause = 12
imageNumber Image_button_play = 13
imageNumber Image_button_probass = 14
imageNumber Image_button_probass_off = 15
imageNumber Image_button_proguitar = 16
imageNumber Image_button_proguitar_off = 17
imageNumber Image_button_prokeys = 18
imageNumber Image_button_prokeys_off = 19
imageNumber Image_button_vocal = 20
imageNumber Image_button_vocal_off = 21
imageNumber Image_gem_black = 22
imageNumber Image_gem_black_hopo = 23
imageNumber Image_gem_black_tap = 24
imageNumber Image_gem_blackkey = 25
imageNumber Image_gem_blackkey_energy = 26
imageNumber Image_gem_blackwhite = 27
imageNumber Image_gem_blackwhite_hopo = 28
imageNumber Image_gem_blackwhite_tap = 29
imageNumber Image_gem_blue = 30
imageNumber Image_gem_blue_cymbal = 31
imageNumber Image_gem_blue_hopo = 32
imageNumber Image_gem_blue_pro = 33
imageNumber Image_gem_blue_pro_hopo = 34
imageNumber Image_gem_blue_pro_tap = 35
imageNumber Image_gem_blue_tap = 36
imageNumber Image_gem_energy = 37
imageNumber Image_gem_energy_cymbal = 38
imageNumber Image_gem_energy_hopo = 39
imageNumber Image_gem_energy_mute = 40
imageNumber Image_gem_energy_mute_hopo = 41
imageNumber Image_gem_energy_mute_tap = 42
imageNumber Image_gem_energy_pro = 43
imageNumber Image_gem_energy_pro_hopo = 44
imageNumber Image_gem_energy_pro_tap = 45
imageNumber Image_gem_energy_tap = 46
imageNumber Image_gem_ghl_energy = 47
imageNumber Image_gem_green = 48
imageNumber Image_gem_green_cymbal = 49
imageNumber Image_gem_green_hopo = 50
imageNumber Image_gem_green_pro = 51
imageNumber Image_gem_green_pro_hopo = 52
imageNumber Image_gem_green_pro_tap = 53
imageNumber Image_gem_green_tap = 54
imageNumber Image_gem_kick = 55
imageNumber Image_gem_kick_energy = 56
imageNumber Image_gem_mute = 57
imageNumber Image_gem_mute_hopo = 58
imageNumber Image_gem_mute_tap = 59
imageNumber Image_gem_open = 60
imageNumber Image_gem_open_energy = 61
imageNumber Image_gem_open_energy_hopo = 62
imageNumber Image_gem_open_energy_tap = 63
imageNumber Image_gem_open_hopo = 64
imageNumber Image_gem_open_tap = 65
imageNumber Image_gem_openghl = 66
imageNumber Image_gem_openghl_energy = 67
imageNumber Image_gem_openghl_hopo = 68
imageNumber Image_gem_openghl_tap = 69
imageNumber Image_gem_orange = 70
imageNumber Image_gem_orange_cymbal = 71
imageNumber Image_gem_orange_hopo = 72
imageNumber Image_gem_orange_pro = 73
imageNumber Image_gem_orange_pro_hopo = 74
imageNumber Image_gem_orange_pro_tap = 75
imageNumber Image_gem_orange_tap = 76
imageNumber Image_gem_purple_pro = 77
imageNumber Image_gem_purple_pro_hopo = 78
imageNumber Image_gem_purple_pro_tap = 79
imageNumber Image_gem_red = 80
imageNumber Image_gem_red_cymbal = 81
imageNumber Image_gem_red_hopo = 82
imageNumber Image_gem_red_pro = 83
imageNumber Image_gem_red_pro_hopo = 84
imageNumber Image_gem_red_pro_tap = 85
imageNumber Image_gem_red_tap = 86
imageNumber Image_gem_white = 87
imageNumber Image_gem_white_hopo = 88
imageNumber Image_gem_white_tap = 89
imageNumber Image_gem_whiteblack = 90
imageNumber Image_gem_whiteblack_hopo = 91
imageNumber Image_gem_whiteblack_tap = 92
imageNumber Image_gem_whitekey = 93
imageNumber Image_gem_whitekey_energy = 94
imageNumber Image_gem_yellow = 95
imageNumber Image_gem_yellow_cymbal = 96
imageNumber Image_gem_yellow_hopo = 97
imageNumber Image_gem_yellow_pro = 98
imageNumber Image_gem_yellow_pro_hopo = 99
imageNumber Image_gem_yellow_pro_tap = 100
imageNumber Image_gem_yellow_tap = 101
imageNumber Image_highway_drums_bar = 102
imageNumber Image_highway_drums_beat = 103
imageNumber Image_highway_drums_halfbeat = 104
imageNumber Image_highway_drums_solo_edge = 105
imageNumber Image_highway_drums_target = 106
imageNumber Image_highway_drums5_target = 107
imageNumber Image_highway_ghl_bar = 108
imageNumber Image_highway_ghl_beat = 109
imageNumber Image_highway_ghl_solo_edge = 110
imageNumber Image_highway_ghl_target = 111
imageNumber Image_highway_grybo_bar = 112
imageNumber Image_highway_grybo_beat = 113
imageNumber Image_highway_grybo_halfbeat = 114
imageNumber Image_highway_grybo_solo_edge = 115
imageNumber Image_highway_grybo_target = 116
imageNumber Image_highway_grybo_target_lefty = 117
imageNumber Image_highway_prokeys_bar = 118
imageNumber Image_highway_prokeys_beat = 119
imageNumber Image_highway_prokeys_halfbeat = 120
imageNumber Image_highway_prokeys_solo_edge = 121
imageNumber Image_highway_prokeys_target = 122
imageNumber Image_highway_protar_beat = 123
imageNumber Image_highway_protar_halfbeat = 124
imageNumber Image_highway_protar_target = 125
imageNumber Image_highway_protar_target_lefty = 126
imageNumber Image_pro_fret_00 = 127
imageNumber Image_pro_fret_01 = 128
imageNumber Image_pro_fret_02 = 129
imageNumber Image_pro_fret_03 = 130
imageNumber Image_pro_fret_04 = 131
imageNumber Image_pro_fret_05 = 132
imageNumber Image_pro_fret_06 = 133
imageNumber Image_pro_fret_07 = 134
imageNumber Image_pro_fret_08 = 135
imageNumber Image_pro_fret_09 = 136
imageNumber Image_pro_fret_10 = 137
imageNumber Image_pro_fret_11 = 138
imageNumber Image_pro_fret_12 = 139
imageNumber Image_pro_fret_13 = 140
imageNumber Image_pro_fret_14 = 141
imageNumber Image_pro_fret_15 = 142
imageNumber Image_pro_fret_16 = 143
imageNumber Image_pro_fret_17 = 144
imageNumber Image_pro_fret_18 = 145
imageNumber Image_pro_fret_19 = 146
imageNumber Image_pro_fret_20 = 147
imageNumber Image_pro_fret_21 = 148
imageNumber Image_pro_fret_22 = 149
imageNumber Image_sustain_end = 150
imageNumber Image_sustain_key_end = 151

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
  ]
