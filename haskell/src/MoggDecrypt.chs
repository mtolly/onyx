{-# LANGUAGE LambdaCase #-}
module MoggDecrypt
( moggToOgg, oggToMogg
, OggVorbis_File, runVorbisFile, ov_pcm_seek, ov_read_float, ov_pcm_total, getChannelsRate
, ov_fopen, ov_clear, newOV
) where

import           Audio                          (audioLength)
import           Control.Applicative            (liftA2)
import           Control.Exception              (bracket)
import           Control.Monad                  (forM_, void, when)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.StackTrace
import           Data.Binary.Get                (getWord32le, runGet)
import           Data.Binary.Put                (putLazyByteString, putWord32le,
                                                 runPut)
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toUpper)
import           Foreign                        hiding (void)
import           Foreign.C
import           Numeric                        (showHex)

#include "vorbis/codec.h"
#include "vorbis/vorbisfile.h"

-- | Just strips the header off an unencrypted MOGG for now.
moggToOgg :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
moggToOgg mogg ogg = do
  bs <- liftIO $ BL.readFile mogg
  let (moggType, oggStart) = runGet (liftA2 (,) getWord32le getWord32le) bs
      hex = "0x" <> map toUpper (showHex moggType "")
  if moggType == 0xA
    then liftIO $ BL.writeFile ogg $ BL.drop (fromIntegral oggStart) bs
    else fatal $ "moggToOgg: encrypted MOGG (type " <> hex <> ") not supported"

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

getChannelsRate :: OggVorbis_File -> IO (CInt, CLong)
getChannelsRate ov = do
  p <- ov_info ov (-1)
  chans <- {#get vorbis_info->channels #} p
  rate <- {#get vorbis_info->rate #} p
  return (chans, rate)

newOV :: IO OggVorbis_File
newOV = OggVorbis_File <$> mallocBytes {#sizeof OggVorbis_File#}

runVorbisFile :: FilePath -> (Maybe OggVorbis_File -> IO a) -> IO a
runVorbisFile fogg fn = allocaBytes {#sizeof OggVorbis_File#} $ \p -> let
  ov = OggVorbis_File p
  -- TODO does this need a short name hack on Windows for non-ascii chars?
  in bracket
    (withCString fogg $ \cstr -> ov_fopen cstr ov)
    (\n -> when (n == 0) $ void $ ov_clear ov) $ \case
      0 -> fn $ Just ov
      _ -> fn Nothing

makeMoggTable :: Word32 -> Word32 -> [(Word32, Word32)] -> [(Word32, Word32)]
makeMoggTable bufSize audioLen = go 0 (0, 0) where
  go curSample prevPair pairs = if curSample >= audioLen
    then []
    else case pairs of
      []                       -> prevPair : go (curSample + bufSize) prevPair pairs
      p@(_bytes, samples) : ps -> if samples <= curSample
        then go curSample p ps
        else prevPair : go (curSample + bufSize) prevPair pairs

oggToMogg :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
oggToMogg ogg mogg = do
  audioLen <- stackIO (audioLength ogg) >>= maybe
    (fatal "couldn't get length of OGG file to generate MOGG")
    (return . fromIntegral)
  mtable <- stackIO $ runVorbisFile ogg $ mapM $ \ov -> let
    go bytes = ov_raw_seek ov bytes >>= \case
      0 -> do
        samples <- ov_pcm_tell ov
        ((fromIntegral bytes, fromIntegral samples) :) <$> go (bytes + 0x8000)
      _ -> return []
    in go 0
  table <- maybe (fatal "couldn't open OGG file to generate MOGG") return mtable
  let bufSize = 20000
      table' = makeMoggTable bufSize audioLen table
  stackIO $ do
    oggBS <- BL.readFile ogg
    BL.writeFile mogg $ runPut $ do
      let len = fromIntegral $ length table' :: Word32
      putWord32le 0xA -- unencrypted
      putWord32le $ 20 + 8 * len -- size of header before the ogg
      putWord32le 0x10 -- "ogg map version"
      putWord32le bufSize -- buffer size
      putWord32le len -- number of pairs
      forM_ table' $ \(bytes, samples) -> do
        putWord32le bytes
        putWord32le samples
      putLazyByteString oggBS
