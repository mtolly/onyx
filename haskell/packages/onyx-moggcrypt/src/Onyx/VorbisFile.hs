{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
module Onyx.VorbisFile where

import           Control.Monad                (guard, void, when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Char                    (isDigit)
import           Data.List                    (stripPrefix, tails)
import qualified Data.Map                     as Map
import           Data.Maybe                   (listToMaybe)
import           Foreign                      hiding (void)
import           Foreign.C
import qualified Language.C.Inline            as C
import qualified Language.C.Inline.Context    as C
import qualified Language.C.Inline.Unsafe     as CU
import qualified Language.C.Types             as C
import           Onyx.Harmonix.MOGG.Crypt     (OVCallbacks, handleOVCallbacks)
import           System.IO                    (Handle)

data OggVorbis_File

C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "OggVorbis_File", [t| OggVorbis_File |])
    , (C.TypeName "ov_callbacks"  , [t| OVCallbacks    |])
    ]
  }

C.include "<stdint.h>"
C.include "vorbis/codec.h"
C.include "vorbis/vorbisfile.h"

ov_fopen :: CString -> Ptr OggVorbis_File -> IO CInt
ov_fopen path vf =
  [C.exp| int { ov_fopen($(char* path), $(OggVorbis_File* vf)) } |]

ov_raw_seek :: Ptr OggVorbis_File -> CLong -> IO CInt
ov_raw_seek vf pos =
  [C.exp| int { ov_raw_seek($(OggVorbis_File* vf), $(long pos)) } |]

ov_pcm_seek :: Ptr OggVorbis_File -> Int64 -> IO CInt
ov_pcm_seek vf pos =
  [C.exp| int { ov_pcm_seek($(OggVorbis_File* vf), $(int64_t pos)) } |]

ov_pcm_total :: Ptr OggVorbis_File -> CInt -> IO Int64
ov_pcm_total vf i =
  [C.exp| int64_t { ov_pcm_total($(OggVorbis_File* vf), $(int i)) } |]

ov_pcm_tell :: Ptr OggVorbis_File -> IO Int64
ov_pcm_tell vf =
  [C.exp| int64_t { ov_pcm_tell($(OggVorbis_File* vf)) } |]

ov_read_float :: Ptr OggVorbis_File -> Ptr (Ptr (Ptr CFloat)) -> CInt -> Ptr CInt -> IO CLong
ov_read_float vf pcm_channels samples bitstream =
  [C.exp| long {
    ov_read_float
      ( $(OggVorbis_File* vf)
      , $(float*** pcm_channels)
      , $(int samples)
      , $(int* bitstream)
      )
  } |]

ov_clear :: Ptr OggVorbis_File -> IO CInt
ov_clear vf =
  [C.exp| int { ov_clear($(OggVorbis_File* vf)) } |]

getChannelsRate :: Ptr OggVorbis_File -> IO (CInt, CLong)
getChannelsRate vf =
  alloca \prate -> do
    chans <- [C.block| int {
      vorbis_info *info = ov_info($(OggVorbis_File* vf), -1);
      *$(long* prate) = info->rate;
      return info->channels;
    } |]
    rate <- peek prate
    return (chans, rate)

ov_open_callbacks :: Ptr a -> Ptr OggVorbis_File -> CString -> CLong -> Ptr OVCallbacks -> IO CInt
ov_open_callbacks (castPtr -> datasource) vf initial ibytes callbacks =
  [C.exp| int {
    ov_open_callbacks
      ( $(void* datasource)
      , $(OggVorbis_File* vf)
      , $(char* initial)
      , $(long ibytes)
      , *$(ov_callbacks* callbacks)
      )
  } |]

loadVorbisHandle :: (MonadResource m) => Handle -> m (Maybe (m (), Ptr OggVorbis_File))
loadVorbisHandle h = do
  vfSize <- fromIntegral <$> liftIO [CU.exp| int { sizeof(OggVorbis_File) } |]
  (pkey, ov) <- allocate (mallocBytes vfSize) free
  (cbkey, (cb, _)) <- allocate (handleOVCallbacks h) snd
  -- can't just pass nullPtr as first arg; in libvorbisfile,
  -- ov_open_callbacks calls _ov_open1 which errors weirdly on null data source
  (reskey, res) <- allocate (ov_open_callbacks (intPtrToPtr 1) ov nullPtr 0 cb)
    (\n -> when (n == 0) $ void $ ov_clear ov)
  if res == 0
    then return $ Just (mapM_ release [reskey, cbkey, pkey], ov)
    else do
      mapM_ release [cbkey, pkey]
      return Nothing

--------------------------------------------------------------------------------

-- Guessing at -q encoding quality

data VorbisBitrates = VorbisBitrates
  { channels :: Int
  , rate     :: Int
  , upper    :: Int -- ^ -1 or 0 if unset
  , nominal  :: Int -- ^ -1 or 0 if unset
  , lower    :: Int -- ^ -1 or 0 if unset
  , average  :: Int -- ^ actual average bitrate of the whole file, from ov_bitrate
  , vendor   :: String -- ^ encoder vendor string from the comment header
  } deriving (Show)

getVorbisBitrates :: Ptr OggVorbis_File -> IO VorbisBitrates
getVorbisBitrates vf =
  alloca \prate -> alloca \pup -> alloca \pnom -> alloca \plow -> alloca \pavg -> alloca \pvendor -> do
    chans <- [C.block| int {
      vorbis_info *info = ov_info($(OggVorbis_File* vf), -1);
      vorbis_comment *vc = ov_comment($(OggVorbis_File* vf), -1);
      *$(long* prate) = info->rate;
      *$(long* pup) = info->bitrate_upper;
      *$(long* pnom) = info->bitrate_nominal;
      *$(long* plow) = info->bitrate_lower;
      *$(long* pavg) = ov_bitrate($(OggVorbis_File* vf), -1);
      *$(char** pvendor) = vc ? vc->vendor : NULL;
      return info->channels;
    } |]
    let get = fmap fromIntegral . peek
    vendorPtr <- peek pvendor
    VorbisBitrates (fromIntegral chans)
      <$> get prate <*> get pup <*> get pnom <*> get plow <*> get pavg
      <*> (if vendorPtr == nullPtr then return "" else peekCString vendorPtr)

-- | The release date in a libvorbis vendor string,
-- e.g. 20050304 from "Xiph.Org libVorbis I 20050304 (1.1.1)".
libvorbisVendorDate :: String -> Maybe Int
libvorbisVendorDate v = listToMaybe do
  rest <- tails v
  Just after <- [stripPrefix "libVorbis I " rest]
  let digits = takeWhile isDigit after
  guard $ length digits == 8
  return $ read digits

-- | Converts a bitrate back to the oggenc -q value that would produce it as
-- the header's nominal bitrate. libvorbis (in quality mode) computes the
-- nominal bitrate by interpolating a per-channel table indexed by quality,
-- so for libvorbis files this is exact. Tables come from libvorbis's
-- lib/modes/setup_*.h; templates are picked by sample rate and channel count
-- the same way as get_setup_template in vorbisenc.c. Values for quality 0
-- and above have been the same since 2004 (libvorbis 1.1.0).
-- The Bool says whether the encoder had the 5.1 template (libvorbis 1.3.0+),
-- which is only used for exactly 6 channels at 40-70 kHz.
-- Returns Nothing outside the supported rates (8-50 kHz, or up to 70 kHz for 5.1)
-- or outside the table range.
bitrateToQuality :: Bool -> Int -> Int -> Int -> Maybe Double
bitrateToQuality has51 chans rate bitrate = do
  guard $ chans > 0 && bitrate > 0
  let stereo = chans == 2
      -- quality_mapping tables, multiplied by 10 to get -q values
      q44 = [-1 .. 10]
      q16 = [-1, 0.5, 5, 10]
      q8  = [-1, 0, 10]
      inRate lo hi = lo <= rate && rate <= hi
  (qualities, table) <- if
    | has51 && chans == 6 && inRate 40000 70000 -> Just (q44, -- setup_44p51.h
      [14000, 20000, 28000, 38000, 46000, 54000, 75000, 96000, 120000, 140000, 180000, 240001])
    | inRate 40000 50000 -> Just $ (q44,) $ if stereo
      then [22500, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 250001] -- setup_44.h
      else [32000, 48000, 60000, 70000, 80000, 86000, 96000, 110000, 120000, 140000, 160000, 240001] -- setup_44u.h
    | inRate 26000 40000 -> Just $ (q44,) $ if stereo -- setup_32.h
      then [18000, 28000, 35000, 45000, 56000, 60000, 75000, 90000, 100000, 115000, 150000, 190000]
      else [30000, 42000, 52000, 64000, 72000, 78000, 86000, 92000, 110000, 120000, 140000, 190000]
    | inRate 19000 26000 -> Just $ (q16,) $ if stereo -- setup_22.h
      then [15000, 20000, 44000, 86000]
      else [16000, 28000, 50000, 90000]
    | inRate 15000 19000 -> Just $ (q16,) $ if stereo -- setup_16.h
      then [12000, 20000, 44000, 86000]
      else [16000, 28000, 64000, 100000]
    | inRate 9000 15000 -> Just $ (q8,) $ if stereo -- setup_11.h
      then [8000, 13000, 44000]
      else [12000, 20000, 50000]
    | inRate 8000 9000 -> Just $ (q8,) $ if stereo -- setup_8.h
      then [6000, 9000, 32000]
      else [8000, 14000, 42000]
    | otherwise -> Nothing -- setup_X.h: no bitrate table, nominal is not meaningful
  let perChan = fromIntegral bitrate / fromIntegral chans :: Double
  listToMaybe do
    ((qlo, qhi), (lo, hi)) <- zip (pairs qualities) (pairs table)
    guard $ lo <= perChan && perChan <= hi
    return $ qlo + (qhi - qlo) * (perChan - lo) / (hi - lo)
  where pairs xs = zip xs $ drop 1 xs

-- | Estimated encoder quality from the header's nominal bitrate.
-- The Bool is False if the file has bitrate limits set (encoded in bitrate
-- mode rather than quality mode), so the value is only an equivalent.
vorbisNominalQuality :: VorbisBitrates -> Maybe (Double, Bool)
vorbisNominalQuality vb = do
  -- 5.1 template added in libvorbis 1.3.0 (2010-02-25).
  -- Assume non-Xiph vendor strings (e.g. aoTuV) are recent enough
  let has51 = maybe True (>= 20100225) $ libvorbisVendorDate vb.vendor
  q <- bitrateToQuality has51 vb.channels vb.rate vb.nominal
  return (q, vb.upper <= 0 && vb.lower <= 0)
