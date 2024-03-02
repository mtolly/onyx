module Onyx.VorbisFile where

import Foreign.C
import Foreign (Ptr, free, mallocBytes, nullPtr, intPtrToPtr)
import Data.Int (Int64)
import Control.Monad.Trans.Resource
import Control.Monad (when, void)
import Onyx.Harmonix.MOGG.Crypt (OVCallbacks(..), handleOVCallbacks)
import System.IO (Handle)

#include "vorbis/codec.h"
#include "vorbis/vorbisfile.h"

{#pointer *OggVorbis_File as OggVorbis_File newtype #}
{#pointer *vorbis_info as VorbisInfo newtype #}

{#fun ov_fopen
  { `CString'
  , `OggVorbis_File'
  } -> `CInt'
#}

{#fun ov_raw_seek
  { `OggVorbis_File'
  , `CLong'
  } -> `CInt'
#}

{#fun ov_pcm_seek
  { `OggVorbis_File'
  , `Int64'
  } -> `CInt'
#}

{#fun ov_pcm_total
  { `OggVorbis_File'
  , `CInt'
  } -> `Int64'
#}

{#fun ov_pcm_tell
  { `OggVorbis_File'
  } -> `Int64'
#}

{#fun ov_read_float
  { `OggVorbis_File'
  , id `Ptr (Ptr (Ptr CFloat))'
  , `CInt'
  , id `Ptr CInt'
  } -> `CLong'
#}

{#fun ov_clear
  { `OggVorbis_File'
  } -> `CInt'
#}

{#fun ov_info
  { `OggVorbis_File'
  , `CInt'
  } -> `VorbisInfo'
#}

foreign import ccall "onyx_ov_open_callbacks"
  onyx_ov_open_callbacks :: Ptr () -> OggVorbis_File -> CString -> CLong -> OVCallbacks -> IO CInt

getChannelsRate :: OggVorbis_File -> IO (CInt, CLong)
getChannelsRate ov = do
  p <- ov_info ov (-1)
  chans <- {#get vorbis_info->channels #} p
  rate <- {#get vorbis_info->rate #} p
  return (chans, rate)

loadVorbisHandle :: (MonadResource m) => Handle -> m (Maybe (m (), OggVorbis_File))
loadVorbisHandle h = do
  (pkey, p) <- allocate (mallocBytes {#sizeof OggVorbis_File#}) free
  (cbkey, (cb, _)) <- allocate (handleOVCallbacks h) snd
  let ov = OggVorbis_File p
  -- can't just pass nullPtr as first arg; in libvorbisfile,
  -- ov_open_callbacks calls _ov_open1 which errors weirdly on null data source
  (reskey, res) <- allocate (onyx_ov_open_callbacks (intPtrToPtr 1) ov nullPtr 0 cb)
    (\n -> when (n == 0) $ void $ ov_clear ov)
  if res == 0
    then return $ Just (mapM_ release [reskey, cbkey, pkey], ov)
    else do
      mapM_ release [cbkey, pkey]
      return Nothing
