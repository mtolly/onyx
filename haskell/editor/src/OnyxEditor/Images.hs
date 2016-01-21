{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module OnyxEditor.Images where

import           Control.Exception   (bracket)
import qualified Data.ByteString     as B
import           Data.FileEmbed      (embedDir)
import           Data.List           (stripPrefix)
import qualified Data.Map.Strict     as Map
import           Foreign
import qualified SDL

import           OnyxiteDisplay.Draw (ImageID (..))
import           OnyxEditor.SDLBindings

imageFolder :: [(FilePath, B.ByteString)]
imageFolder = $(embedDir "../web-display/www/images/")

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
      f getImage
