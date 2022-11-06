{-# LANGUAGE LambdaCase #-}
module Onyx.Harmonix.MOGG
( moggToOgg, moggToOggHandle, oggToMogg, sourceVorbisFile
, encryptRB1
) where

import           Control.Applicative            (liftA2)
import           Control.Monad                  (forM_, void, when, forM)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Resource
import           Onyx.StackTrace
import           Data.Binary.Get                (getWord32le, runGet)
import           Data.Binary.Put                (putLazyByteString, putWord32le,
                                                 runPut)
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toUpper)
import           Foreign                        hiding (void)
import           Foreign.C
import           Numeric                        (showHex)
import qualified Data.Vector.Storable.Mutable   as MV
import qualified Data.Vector.Storable as V
import qualified Data.Conduit as C
import qualified Data.Conduit.Audio as CA
import Onyx.Util.Handle (Readable, useHandle, subHandle, saveReadable, fileReadable)
import Sound.MOGG.EncryptRB1 (encryptRB1)

#include "vorbis/codec.h"
#include "vorbis/vorbisfile.h"

-- | Just strips the header off an unencrypted MOGG for now.
moggToOgg :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
moggToOgg mogg ogg = do
  oggHandle <- moggToOggHandle $ fileReadable mogg
  stackIO $ saveReadable oggHandle ogg

moggToOggHandle :: (MonadIO m) => Readable -> StackTraceT m Readable
moggToOggHandle ioh = do
  bs <- stackIO $ useHandle ioh $ \h -> BL.hGet h 8
  let (moggType, oggStart) = runGet (liftA2 (,) getWord32le getWord32le) bs
      hex = "0x" <> map toUpper (showHex moggType "")
  if moggType == 0xA
    then return $ subHandle (<> " | mogg -> ogg") (fromIntegral oggStart) Nothing ioh
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

loadVorbisFile :: (MonadResource m) => FilePath -> m (Maybe (m (), OggVorbis_File))
loadVorbisFile f = do
  (pkey, p) <- allocate (mallocBytes {#sizeof OggVorbis_File#}) free
  let ov = OggVorbis_File p
  -- TODO does this need a short name hack on Windows for non-ascii chars?
  (reskey, res) <- allocate (withCString f $ \cstr -> ov_fopen cstr ov)
    (\n -> when (n == 0) $ void $ ov_clear ov)
  if res == 0
    then return $ Just (release reskey >> release pkey, ov)
    else do
      release pkey
      return Nothing

runVorbisFile :: FilePath -> (Maybe OggVorbis_File -> IO a) -> IO a
runVorbisFile fogg fn = runResourceT $ loadVorbisFile fogg >>= liftIO . fn . fmap snd

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
  pair <- stackIO $ runVorbisFile ogg $ mapM $ \ov -> let
    go bytes = ov_raw_seek ov bytes >>= \case
      0 -> do
        samples <- ov_pcm_tell ov
        ((fromIntegral bytes, fromIntegral samples) :) <$> go (bytes + 0x8000)
      _ -> return []
    in do
      audioLen <- ov_pcm_total ov (-1)
      table <- go 0
      return (audioLen, table)
  (audioLen, table) <- maybe (fatal "couldn't open OGG file to generate MOGG") return pair
  let bufSize = 20000
      table' = makeMoggTable bufSize (fromIntegral audioLen) table
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

sourceVorbisFile :: (MonadResource m, MonadIO f) => CA.Duration -> FilePath -> f (CA.AudioSource m Float)
sourceVorbisFile pos ogg = liftIO $ runVorbisFile ogg $ \case
  Nothing -> error $ "Couldn't load OGG file: " <> ogg
  Just ovInit -> do
    (chans, rate) <- getChannelsRate ovInit
    total <- ov_pcm_total ovInit (-1)
    let seekTo = fromIntegral $ case pos of
          CA.Seconds secs -> CA.secondsToFrames secs (fromIntegral rate)
          CA.Frames fms -> fms
    return $ CA.reorganize CA.chunkSize $ CA.AudioSource
      { CA.source = if total <= seekTo
        then return () -- seeked past end of file
        else loadVorbisFile ogg >>= \case
          Nothing -> return () -- failed to load, maybe log somewhere?
          Just (releaseOV, ov) -> let
            loop = do
              (numRead, ppfloat) <- liftIO $ alloca $ \pppfloat -> do
                with (-1) $ \psection -> do
                  numRead <- ov_read_float ov pppfloat (fromIntegral CA.chunkSize) psection
                  ppfloat <- peek pppfloat
                  return (numRead, ppfloat)
              if numRead <= 0
                then return ()
                else do
                  pfloats <- liftIO $ peekArray (fromIntegral chans) ppfloat
                  vchans <- liftIO $ forM pfloats $ \pfloat -> do
                    fptr <- newForeignPtr_ ((castPtr :: Ptr CFloat -> Ptr Float) pfloat)
                    let mv = MV.unsafeFromForeignPtr0 fptr $ fromIntegral numRead
                    V.freeze mv
                  C.yield $ CA.interleave vchans
                  loop
            in do
              void $ liftIO $ ov_pcm_seek ov seekTo
              loop
              releaseOV
      , CA.rate = fromIntegral rate
      , CA.channels = fromIntegral chans
      , CA.frames = max 0 $ fromIntegral total - fromIntegral seekTo
      }
