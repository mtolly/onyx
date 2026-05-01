{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Onyx.Audio.SndfileExtra (setVBREncodingQuality) where

import           Foreign
import           Foreign.C
import qualified Language.C.Inline  as C
import           Sound.File.Sndfile

C.context C.baseCtx

C.include "sndfile.h"

setVBREncodingQuality :: Handle -> Double -> IO Bool
setVBREncodingQuality hsnd d = with d $ \p -> let
  psnd = hPtr hsnd
  p' = castPtr p
  in toBool <$> [C.exp| int {
    sf_command
      ( $(void* psnd)
      , SFC_SET_VBR_ENCODING_QUALITY
      , $(void* p')
      , sizeof(double)
      )
    } |]
