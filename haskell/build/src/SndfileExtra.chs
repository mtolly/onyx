module SndfileExtra where

import Foreign
import Foreign.C
import Sound.File.Sndfile

#include <sndfile.h>

{#fun sf_command as ^
  { `Ptr ()'
  , `CInt'
  , `Ptr ()'
  , `CInt'
  } -> `CInt' #}

{#enum define Consts
  { SFC_SET_VBR_ENCODING_QUALITY as SFC_SET_VBR_ENCODING_QUALITY
  } deriving (Eq, Ord, Show) #}

setVBREncodingQuality :: Handle -> Double -> IO Bool
setVBREncodingQuality hsnd d = with d $ \p -> toBool <$> sfCommand
  (hPtr hsnd)
  (fromIntegral $ fromEnum SFC_SET_VBR_ENCODING_QUALITY)
  (castPtr p)
  (fromIntegral $ sizeOf d)
