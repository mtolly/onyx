{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Onyx.VorbisFile where

import           Control.Monad                (void, when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import qualified Data.Map                     as Map
import           Foreign                      hiding (void)
import           Foreign.C
import qualified Language.C.Inline            as C
import qualified Language.C.Inline.Context    as C
import qualified Language.C.Inline.Unsafe     as CU
import qualified Language.C.Types             as C
import           Onyx.Harmonix.MOGG.Crypt     (COVCallbacks, OVCallbacks (..),
                                               handleOVCallbacks)
import           System.IO                    (Handle)

data COggVorbis_File

newtype OggVorbis_File = OggVorbis_File (Ptr COggVorbis_File)

C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "OggVorbis_File", [t| COggVorbis_File |])
    , (C.TypeName "ov_callbacks"  , [t| COVCallbacks    |])
    ]
  }

C.include "vorbis/codec.h"
C.include "vorbis/vorbisfile.h"

ov_fopen :: CString -> OggVorbis_File -> IO CInt
ov_fopen path (OggVorbis_File vf) =
  [C.exp| int { ov_fopen($(char* path), $(OggVorbis_File* vf)) } |]

ov_raw_seek :: OggVorbis_File -> CLong -> IO CInt
ov_raw_seek (OggVorbis_File vf) pos =
  [C.exp| int { ov_raw_seek($(OggVorbis_File* vf), $(long pos)) } |]

ov_pcm_seek :: OggVorbis_File -> Int64 -> IO CInt
ov_pcm_seek (OggVorbis_File vf) pos =
  [C.exp| int { ov_pcm_seek($(OggVorbis_File* vf), $(int64_t pos)) } |]

ov_pcm_total :: OggVorbis_File -> CInt -> IO Int64
ov_pcm_total (OggVorbis_File vf) i =
  [C.exp| int64_t { ov_pcm_total($(OggVorbis_File* vf), $(int i)) } |]

ov_pcm_tell :: OggVorbis_File -> IO Int64
ov_pcm_tell (OggVorbis_File vf) =
  [C.exp| int64_t { ov_pcm_tell($(OggVorbis_File* vf)) } |]

ov_read_float :: OggVorbis_File -> Ptr (Ptr (Ptr CFloat)) -> CInt -> Ptr CInt -> IO CLong
ov_read_float (OggVorbis_File vf) pcm_channels samples bitstream =
  [C.exp| long {
    ov_read_float
      ( $(OggVorbis_File* vf)
      , $(float*** pcm_channels)
      , $(int samples)
      , $(int* bitstream)
      )
  } |]

ov_clear :: OggVorbis_File -> IO CInt
ov_clear (OggVorbis_File vf) =
  [C.exp| int { ov_clear($(OggVorbis_File* vf)) } |]

getChannelsRate :: OggVorbis_File -> IO (CInt, CLong)
getChannelsRate (OggVorbis_File vf) =
  alloca $ \prate -> do
    chans <- [C.block| int {
      vorbis_info *info = ov_info($(OggVorbis_File* vf), -1);
      *$(long* prate) = info->rate;
      return info->channels;
    } |]
    rate <- peek prate
    return (chans, rate)

ov_open_callbacks :: Ptr a -> OggVorbis_File -> CString -> CLong -> OVCallbacks -> IO CInt
ov_open_callbacks (castPtr -> datasource) (OggVorbis_File vf) initial ibytes (OVCallbacks callbacks) =
  [C.exp| int {
    ov_open_callbacks
      ( $(void* datasource)
      , $(OggVorbis_File* vf)
      , $(char* initial)
      , $(long ibytes)
      , *$(ov_callbacks* callbacks)
      )
  } |]

loadVorbisHandle :: (MonadResource m) => Handle -> m (Maybe (m (), OggVorbis_File))
loadVorbisHandle h = do
  vfSize <- fromIntegral <$> liftIO [CU.exp| int { sizeof(OggVorbis_File) } |]
  (pkey, p) <- allocate (mallocBytes vfSize) free
  (cbkey, (cb, _)) <- allocate (handleOVCallbacks h) snd
  let ov = OggVorbis_File p
  -- can't just pass nullPtr as first arg; in libvorbisfile,
  -- ov_open_callbacks calls _ov_open1 which errors weirdly on null data source
  (reskey, res) <- allocate (ov_open_callbacks (intPtrToPtr 1) ov nullPtr 0 cb)
    (\n -> when (n == 0) $ void $ ov_clear ov)
  if res == 0
    then return $ Just (mapM_ release [reskey, cbkey, pkey], ov)
    else do
      mapM_ release [cbkey, pkey]
      return Nothing
