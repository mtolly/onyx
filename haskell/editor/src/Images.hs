{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Images where

import           Control.Exception (bracket)
import           Control.Monad     (forM_)
import qualified Data.ByteString   as B
import           Data.FileEmbed    (embedDir)
import           Data.List         (stripPrefix)
import qualified Data.Map.Strict   as Map
import           Foreign
import           SDL               (($=))
import qualified SDL

import           SDLBindings

data ImageID
  = Image_sustain_blackkey_energy
  | Image_gem_blackkey_energy
  | Image_gem_blackkey
  | Image_gem_blue_cymbal
  | Image_gem_blue_hopo
  | Image_gem_blue
  | Image_gem_energy_cymbal
  | Image_gem_energy_hopo
  | Image_gem_energy
  | Image_gem_green_cymbal
  | Image_gem_green_hopo
  | Image_gem_green
  | Image_gem_kick_energy
  | Image_gem_kick
  | Image_gem_orange_hopo
  | Image_gem_orange
  | Image_gem_red_cymbal
  | Image_gem_red_hopo
  | Image_gem_red
  | Image_sustain_whitekey_energy
  | Image_gem_whitekey_energy
  | Image_gem_whitekey
  | Image_gem_yellow_cymbal
  | Image_gem_yellow_hopo
  | Image_gem_yellow
  | Image_highway_drums_bar
  | Image_highway_drums_beat
  | Image_highway_drums_halfbeat
  | Image_highway_drums_solo_edge
  | Image_highway_drums_solo
  | Image_highway_drums_target
  | Image_highway_drums
  | Image_highway_grybo_bar
  | Image_highway_grybo_beat
  | Image_highway_grybo_halfbeat
  | Image_highway_grybo_solo_edge
  | Image_highway_grybo_solo
  | Image_highway_grybo_target
  | Image_highway_grybo
  | Image_highway_prokeys_bar
  | Image_highway_prokeys_beat
  | Image_highway_prokeys_halfbeat
  | Image_highway_prokeys_arange
  | Image_highway_prokeys_crange
  | Image_highway_prokeys_drange
  | Image_highway_prokeys_erange
  | Image_highway_prokeys_frange
  | Image_highway_prokeys_grange
  | Image_highway_prokeys_solo_arange
  | Image_highway_prokeys_solo_edge_arange
  | Image_highway_prokeys_solo_edge
  | Image_highway_prokeys_solo
  | Image_highway_prokeys_target
  | Image_highway_prokeys
  | Image_sustain_key_end
  | Image_sustain_blackkey
  | Image_sustain_blue
  | Image_sustain_end
  | Image_sustain_energy
  | Image_sustain_green
  | Image_sustain_orange
  | Image_sustain_red
  | Image_sustain_whitekey
  | Image_sustain_yellow
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

imageFolder :: [(FilePath, B.ByteString)]
imageFolder = $(embedDir "images/")

getImageByteString :: ImageID -> B.ByteString
getImageByteString iid = let
  filename = case stripPrefix "Image_" $ show iid of
    Nothing -> error $ "getImageByteString: couldn't get filename for " ++ show iid
    Just s -> map (\case '_' -> '-'; c -> c) s ++ ".png"
  in case lookup filename imageFolder of
    Just bs -> bs
    Nothing -> error $ "getImageByteString: couldn't find image for " ++ show iid

withImages :: SDL.Renderer -> ((ImageID -> SDL.Texture) -> IO a) -> IO a
withImages rend f = let
  iids = [minBound .. maxBound]
  withTexture surf = bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture
  in withMany (withImageFromByteString . getImageByteString) iids $ \surfs ->
    withMany withTexture surfs $ \texs -> do
      let table = Map.fromList $ zip iids texs
          getImage iid = case Map.lookup iid table of
            Nothing -> error $ "withImages: couldn't find image for " ++ show iid
            Just surf -> surf
      forM_
        [ Image_highway_prokeys_crange
        , Image_highway_prokeys_drange
        , Image_highway_prokeys_erange
        , Image_highway_prokeys_frange
        , Image_highway_prokeys_grange
        , Image_highway_prokeys_arange
        ] $ \rng -> SDL.textureAlphaMod (getImage rng) $= round (0.3 * 255 :: Double)
      f getImage
