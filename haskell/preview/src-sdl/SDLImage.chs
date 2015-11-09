module SDLImage where

import Foreign
import Foreign.C
import Control.Exception
import qualified SDL
import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified SDL.Raw as Raw

#include "SDL_image.h"

{#fun IMG_Load_RW as imgLoadRW
  { castPtr `Ptr Raw.RWops'
  , `Bool'
  } -> `Ptr Raw.Surface' castPtr
  #}

imageFromByteString :: B.ByteString -> IO SDL.Surface
imageFromByteString bs = unsafeUseAsCStringLen bs $ \(p, len) -> do
  rw <- Raw.rwFromMem (castPtr p) (fromIntegral len)
  psurf <- imgLoadRW rw True
  return $ SDL.Surface psurf Nothing

withImageFromByteString :: B.ByteString -> (SDL.Surface -> IO a) -> IO a
withImageFromByteString bs = bracket (imageFromByteString bs) SDL.freeSurface

withImageBinary :: SDL.Renderer -> B.ByteString -> (SDL.Texture -> IO a) -> IO a
withImageBinary render bs f = withImageFromByteString bs $ \surf ->
  bracket (SDL.createTextureFromSurface render surf) SDL.destroyTexture f

{#enum define ImgInitFlag
  { IMG_INIT_JPG as IMG_INIT_JPG
  , IMG_INIT_PNG as IMG_INIT_PNG
  , IMG_INIT_TIF as IMG_INIT_TIF
  } deriving (Eq, Ord, Show, Read, Bounded) #}

{#fun IMG_Init as imgInit { `Int' } -> `Int' #}
{#fun IMG_Quit as imgQuit {} -> `()' #}

withImgInit :: [ImgInitFlag] -> ([ImgInitFlag] -> IO a) -> IO a
withImgInit flags f = let
  flagsIn = foldr (.|.) 0 $ map fromEnum flags
  allFlags = [minBound .. maxBound] :: [ImgInitFlag]
  flagsOut int = filter (\flag -> int .&. fromEnum flag /= 0) allFlags
  in bracket (imgInit flagsIn) (\_ -> imgQuit) $ f . flagsOut
