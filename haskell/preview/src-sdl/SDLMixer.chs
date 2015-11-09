module SDLMixer where

import Foreign
import Foreign.C
import Control.Exception (bracket_)
import qualified SDL.Raw as Raw

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

withMixer :: [MixInitFlag] -> IO a -> IO a
withMixer flags = let
  val = fromIntegral $ foldr (.|.) 0 $ map fromEnum flags
  in bracket_ (sdlCode (== val) $ mixInit val) mixQuit

withMixerAudio :: CInt -> Word16 -> CInt -> CInt -> IO a -> IO a
withMixerAudio a b c d = bracket_ (zero $ mixOpenAudio a b c d) mixCloseAudio

sdlCode :: (a -> Bool) -> IO a -> IO ()
sdlCode f act = do
  x <- act
  if f x
    then return ()
    else Raw.getError >>= peekCString >>= error

zero :: (Eq a, Num a) => IO a -> IO ()
zero = sdlCode (== 0)
