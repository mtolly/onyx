module OnyxEditor.SDLBindings where

import           Control.Exception         (bracket, bracket_)
import qualified Data.ByteString as B
import           Data.ByteString.Unsafe    (unsafeUseAsCStringLen)
import           Foreign
import           Foreign.C
import qualified SDL
import qualified SDL.Raw                   as Raw

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

#include <SDL_mixer.h>

mixDefaultFormat :: Word16
mixDefaultFormat = {#const MIX_DEFAULT_FORMAT#}

{#enum MIX_InitFlags as MixInitFlag {}
  deriving (Eq, Ord, Show, Read, Bounded) #}

{#fun Mix_Init as ^ { `CInt' } -> `CInt' #}
{#fun Mix_Quit as ^ {} -> `()' #}
{#fun Mix_OpenAudio as ^ { `CInt', `Word16', `CInt', `CInt' } -> `CInt' #}
{#fun Mix_CloseAudio as ^ {} -> `()' #}

{#pointer *Mix_Music as MixMusic #}
{#fun Mix_LoadMUS as ^ { `CString' } -> `MixMusic' #}
{#fun Mix_FreeMusic as ^ { `MixMusic' } -> `()' #}
{#fun Mix_PlayMusic as ^ { `MixMusic', `CInt' } -> `CInt' #}
{#fun Mix_PauseMusic as ^ {} -> `()' #}
{#fun Mix_ResumeMusic as ^ {} -> `()' #}
{#fun Mix_SetMusicPosition as ^ { `Double' } -> `CInt' #}

sdlCode :: (a -> Bool) -> IO a -> IO ()
sdlCode f act = do
  x <- act
  if f x
    then return ()
    else Raw.getError >>= peekCString >>= error

zero :: (Eq a, Num a) => IO a -> IO ()
zero = sdlCode (== 0)

withMixer :: [MixInitFlag] -> IO a -> IO a
withMixer flags = let
  val = fromIntegral $ foldr (.|.) 0 $ map fromEnum flags
  in bracket_ (sdlCode (== val) $ mixInit val) mixQuit

withMixerAudio :: CInt -> Word16 -> CInt -> CInt -> IO a -> IO a
withMixerAudio a b c d = bracket_ (zero $ mixOpenAudio a b c d) mixCloseAudio
