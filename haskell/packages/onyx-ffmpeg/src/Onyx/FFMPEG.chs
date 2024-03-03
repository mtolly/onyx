{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ViewPatterns               #-}
module Onyx.FFMPEG where

import           Control.Concurrent.Async     (forConcurrently)
import           Control.Concurrent.STM       (atomically, newTBQueueIO,
                                               readTBQueue, writeTBQueue)
import           Control.Exception            (Exception (..), throwIO)
import           Control.Monad                (forM_, unless, when)
import           Control.Monad.Trans.Resource (MonadResource, liftResourceT,
                                               resourceForkIO)
import           Data.Coerce                  (coerce)
import           Data.Conduit
import qualified Data.Conduit.Audio           as CA
import           Data.Maybe                   (catMaybes)
import           Onyx.Util.Handle            (Readable, rOpen)
import           Data.Typeable                (Typeable)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Foreign
import           Foreign.C
import           System.IO                    (Handle, SeekMode (..), hClose,
                                               hFileSize, hGetBuf, hIsWritable,
                                               hPutBuf, hSeek, hTell)
import           System.Posix.Internals       (sEEK_CUR, sEEK_END, sEEK_SET)
import           Text.Read                    (readMaybe)
import           UnliftIO                     (MonadIO, MonadUnliftIO, bracket,
                                               liftIO)

#include "libavcodec/avcodec.h"
#include "libavformat/avformat.h"
#include "libavutil/imgutils.h"
#include "libavutil/opt.h"
#include "libswscale/swscale.h"
#include "libavfilter/avfilter.h"
#include "libavfilter/buffersink.h"
#include "libavfilter/buffersrc.h"
#include "libswresample/swresample.h"

#include "ffmacros.h"

----------------------------

{#pointer *AVFormatContext as AVFormatContext newtype #}
deriving instance Storable AVFormatContext

{#pointer *AVFrame as AVFrame newtype #}
deriving instance Storable AVFrame
deriving instance Show AVFrame

{#pointer *AVIOContext as AVIOContext newtype #}
deriving instance Storable AVIOContext
deriving instance Show AVIOContext

{#pointer *AVPacket as AVPacket newtype #}
deriving instance Storable AVPacket
deriving instance Show AVPacket

{#pointer *AVCodec as AVCodec newtype #}
deriving instance Storable AVCodec
deriving instance Show AVCodec

{#pointer *AVCodecContext as AVCodecContext newtype #}
deriving instance Storable AVCodecContext
deriving instance Show AVCodecContext

{#pointer *AVStream as AVStream newtype #}
deriving instance Storable AVStream
deriving instance Show AVStream

{#pointer *AVFilter as AVFilter newtype #}
deriving instance Storable AVFilter
deriving instance Show AVFilter

{#pointer *AVFilterInOut as AVFilterInOut newtype #}
deriving instance Storable AVFilterInOut
deriving instance Show AVFilterInOut

{#pointer *AVFilterLink as AVFilterLink newtype #}
deriving instance Storable AVFilterLink
deriving instance Show AVFilterLink

{#pointer *AVFilterGraph as AVFilterGraph newtype #}
deriving instance Storable AVFilterGraph
deriving instance Show AVFilterGraph

{#pointer *AVFilterContext as AVFilterContext newtype #}
deriving instance Storable AVFilterContext
deriving instance Show AVFilterContext

{#pointer *AVDictionary as AVDictionary newtype #}
deriving instance Storable AVDictionary
deriving instance Show AVDictionary

{#pointer *AVDictionaryEntry as AVDictionaryEntry newtype #}
deriving instance Storable AVDictionaryEntry
deriving instance Show AVDictionaryEntry

{#enum AVCodecID {} deriving (Eq, Show) #}

{#enum AVMediaType {} deriving (Eq, Show) #}

{#enum AVPixelFormat {} deriving (Eq, Show) #}

{#enum AVSampleFormat {} deriving (Eq, Show) #}

{#enum AVPacketSideDataType {} deriving (Eq, Show) #}

{#enum AV_BUFFERSRC_FLAG_NO_CHECK_FORMAT as AV_BUFFERSRC_FLAG {}
  deriving (Eq, Show) #}

{#pointer *AVCodecParameters as AVCodecParameters newtype #}
deriving instance Storable AVCodecParameters

{#pointer *SwsContext as SwsContext newtype #}
deriving instance Show SwsContext

{#pointer *SwrContext as SwrContext newtype #}
deriving instance Show SwrContext
deriving instance Storable SwrContext

-----------------------------

{#fun av_packet_get_side_data
  { `AVPacket'
  , `AVPacketSideDataType'
  , id `Ptr CInt'
  } -> `Ptr Word8' castPtr
#}

{#fun av_packet_new_side_data
  { `AVPacket'
  , `AVPacketSideDataType'
  , `CInt'
  } -> `Ptr Word8' castPtr
#}

{#fun avformat_alloc_context
  {} -> `AVFormatContext'
#}

{#fun avformat_free_context
  { `AVFormatContext'
  } -> `()'
#}

{#fun avformat_open_input
  { id `Ptr AVFormatContext'
  , `CString'
  , id `Ptr ()'
  , id `Ptr AVDictionary'
  } -> `CInt'
#}

{#fun avformat_close_input
  { id `Ptr AVFormatContext'
  } -> `()'
#}

{#fun av_dump_format
  { `AVFormatContext'
  , `CInt'
  , `CString'
  , `CInt'
  } -> `()'
#}

{#fun avformat_find_stream_info
  { `AVFormatContext'
  , id `Ptr AVDictionary'
  } -> `CInt'
#}

{#fun av_frame_alloc
  {} -> `AVFrame'
#}

{#fun av_frame_free
  { id `Ptr AVFrame'
  } -> `()'
#}

{#fun av_frame_unref
  { `AVFrame'
  } -> `()'
#}

{#fun av_packet_alloc
  {} -> `AVPacket'
#}

{#fun av_packet_free
  { id `Ptr AVPacket'
  } -> `()'
#}

{#fun av_packet_unref
  { `AVPacket'
  } -> `()'
#}

{#fun av_read_frame
  { `AVFormatContext'
  , `AVPacket'
  } -> `CInt'
#}

{#fun av_seek_frame
  { `AVFormatContext'
  , `CInt'
  , `Int64'
  , `CInt'
  } -> `CInt'
#}

getStreams :: AVFormatContext -> IO [AVStream]
getStreams ctx = do
  n <- {#get AVFormatContext->nb_streams#} ctx
  p <- {#get AVFormatContext->streams#} ctx
  peekArray (fromIntegral n) p

stream_index :: AVStream -> IO CInt
stream_index = {#get AVStream->index #}

stream_codecpar :: AVStream -> IO AVCodecParameters
stream_codecpar = {#get AVStream->codecpar #}

avfc_duration :: AVFormatContext -> IO Double
avfc_duration fc = do
  n <- {#get AVFormatContext->duration #} fc
  return $ realToFrac n / {#const AV_TIME_BASE #}

avfc_start_time :: AVFormatContext -> IO (Maybe Double)
avfc_start_time fc = do
  n <- fromIntegral <$> {#get AVFormatContext->start_time #} fc
  -- this define has a cast that breaks c2hs #const
  let av_NOPTS_VALUE = fromIntegral (0x8000000000000000 :: Word64) :: Int64
  return $ if n == av_NOPTS_VALUE
    then Nothing
    else Just $ realToFrac n / {#const AV_TIME_BASE #}
{-
  // from libavformat/dump.c
  if (ic->start_time != AV_NOPTS_VALUE) {
      int secs, us;
      av_log(NULL, AV_LOG_INFO, ", start: ");
      secs = llabs(ic->start_time / AV_TIME_BASE);
      us   = llabs(ic->start_time % AV_TIME_BASE);
      av_log(NULL, AV_LOG_INFO, "%s%d.%06d",
             ic->start_time >= 0 ? "" : "-",
             secs,
             (int) av_rescale(us, 1000000, AV_TIME_BASE));
  }
-}

codec_type :: AVCodecParameters -> IO AVMediaType
codec_type = fmap (toEnum . fromIntegral) . {#get AVCodecParameters->codec_type #}

codec_id :: AVCodecParameters -> IO AVCodecID
codec_id = fmap (toEnum . fromIntegral) . {#get AVCodecParameters->codec_id #}

cp_width, cp_height :: AVCodecParameters -> IO CInt
cp_width  = {#get AVCodecParameters->width  #}
cp_height = {#get AVCodecParameters->height #}

-- only an AVPixelFormat for video streams; for audio, it's a AVSampleFormat
-- cp_format :: AVCodecParameters -> IO AVPixelFormat
-- cp_format = fmap (toEnum . fromIntegral) . {#get AVCodecParameters->format #}

{#fun avcodec_find_decoder
  { `AVCodecID'
  } -> `AVCodec'
#}

{#fun avcodec_find_encoder
  { `AVCodecID'
  } -> `AVCodec'
#}

stream_time_base :: AVStream -> IO (CInt, CInt)
stream_time_base s = do
  num <- {#get AVStream->time_base.num #} s
  den <- {#get AVStream->time_base.den #} s
  return (num, den)

{#fun avcodec_alloc_context3
  { `AVCodec'
  } -> `AVCodecContext'
#}

{#fun avcodec_free_context
  { id `Ptr AVCodecContext'
  } -> `()'
#}

{#fun avcodec_parameters_to_context
  { `AVCodecContext'
  , `AVCodecParameters'
  } -> `CInt'
#}

{#fun avcodec_open2
  { `AVCodecContext'
  , `AVCodec'
  , id `Ptr AVDictionary'
  } -> `CInt'
#}

packet_stream_index :: AVPacket -> IO CInt
packet_stream_index = {#get AVPacket->stream_index #}

{#fun avcodec_send_packet
  { `AVCodecContext'
  , `AVPacket'
  } -> `CInt'
#}

{#fun avcodec_receive_frame
  { `AVCodecContext'
  , `AVFrame'
  } -> `CInt'
#}

{#fun avcodec_send_frame
  { `AVCodecContext'
  , `AVFrame'
  } -> `CInt'
#}

{#fun avcodec_receive_packet
  { `AVCodecContext'
  , `AVPacket'
  } -> `CInt'
#}

{#fun sws_getContext
  { `CInt'
  , `CInt'
  , `AVPixelFormat'
  , `CInt'
  , `CInt'
  , `AVPixelFormat'
  , `CInt'
  , `Ptr ()'
  , `Ptr ()'
  , id `Ptr CDouble'
  } -> `SwsContext'
#}

{#fun sws_scale
  { `SwsContext'
  , id `Ptr (Ptr CUChar)'
  , id `Ptr CInt'
  , `CInt'
  , `CInt'
  , id `Ptr (Ptr CUChar)'
  , id `Ptr CInt'
  } -> `CInt'
#}

sws_BILINEAR :: CInt
sws_BILINEAR = {#const SWS_BILINEAR #}

pix_fmt :: AVCodecContext -> IO AVPixelFormat
pix_fmt = fmap (toEnum . fromIntegral) . {#get AVCodecContext->pix_fmt #}

{#fun av_image_fill_arrays
  { id `Ptr (Ptr CUChar)'
  , id `Ptr CInt'
  , id `Ptr CUChar'
  , `AVPixelFormat'
  , `CInt'
  , `CInt'
  , `CInt'
  } -> `CInt'
#}

frame_data :: AVFrame -> IO (Ptr (Ptr CUChar))
frame_data = {#get AVFrame->data #}

frame_linesize :: AVFrame -> IO (Ptr CInt)
frame_linesize = {#get AVFrame->linesize #}

frame_pts :: AVFrame -> IO Int64
frame_pts = fmap fromIntegral . {#get AVFrame->pts #}

packet_pts :: AVPacket -> IO Int64
packet_pts = fmap fromIntegral . {#get AVPacket->pts #}

ctx_set_pix_fmt :: AVCodecContext -> AVPixelFormat -> IO ()
ctx_set_pix_fmt c = {#set AVCodecContext->pix_fmt #} c . fromIntegral . fromEnum

ctx_set_height :: AVCodecContext -> CInt -> IO ()
ctx_set_height = {#set AVCodecContext->height #}

ctx_set_width :: AVCodecContext -> CInt -> IO ()
ctx_set_width = {#set AVCodecContext->width #}

ctx_set_codec_type :: AVCodecContext -> AVMediaType -> IO ()
ctx_set_codec_type c = {#set AVCodecContext->codec_type #} c . fromIntegral . fromEnum

ctx_time_base :: AVCodecContext -> IO (CInt, CInt)
ctx_time_base c = do
  num <- {#get AVCodecContext->time_base.num #} c
  den <- {#get AVCodecContext->time_base.den #} c
  return (num, den)

ctx_set_time_base_num :: AVCodecContext -> CInt -> IO ()
ctx_set_time_base_num = {#set AVCodecContext->time_base.num #}

ctx_set_time_base_den :: AVCodecContext -> CInt -> IO ()
ctx_set_time_base_den = {#set AVCodecContext->time_base.den #}

{#fun av_init_packet
  { `AVPacket'
  } -> `()'
#}

packet_set_size :: AVPacket -> CInt -> IO ()
packet_set_size = {#set AVPacket->size #}

packet_set_data :: AVPacket -> Ptr CUChar -> IO ()
packet_set_data = {#set AVPacket->data #}

{#fun av_image_alloc
  { id `Ptr (Ptr CUChar)'
  , id `Ptr CInt'
  , `CInt'
  , `CInt'
  , `AVPixelFormat'
  , `CInt'
  } -> `CInt'
#}

{#fun av_freep
  { castPtr `Ptr a'
  } -> `()'
#}

{#fun av_free
  { castPtr `Ptr a'
  } -> `()'
#}

frame_set_width :: AVFrame -> CInt -> IO ()
frame_set_width = {#set AVFrame->width #}

frame_set_height :: AVFrame -> CInt -> IO ()
frame_set_height = {#set AVFrame->height #}

frame_set_format :: AVFrame -> AVPixelFormat -> IO ()
frame_set_format f = {#set AVFrame->format #} f . fromIntegral . fromEnum

packet_size :: AVPacket -> IO CInt
packet_size = {#get AVPacket->size #}

packet_data :: AVPacket -> IO (Ptr CUChar)
packet_data = {#get AVPacket->data #}

{#fun av_log_set_level
  { `CInt'
  } -> `()'
#}

avseek_FLAG_BACKWARD :: CInt
avseek_FLAG_BACKWARD = {#const AVSEEK_FLAG_BACKWARD #}

avseek_FLAG_FRAME :: CInt
avseek_FLAG_FRAME = {#const AVSEEK_FLAG_FRAME #}

avseek_FLAG_ANY :: CInt
avseek_FLAG_ANY = {#const AVSEEK_FLAG_ANY #}

{#fun av_find_best_stream
  { `AVFormatContext'
  , `AVMediaType'
  , `CInt'
  , `CInt'
  , id `Ptr AVCodec'
  , `CInt'
  } -> `CInt'
#}

{#fun avfilter_get_by_name
  { `CString'
  } -> `AVFilter'
#}

{#fun avfilter_inout_alloc
  {} -> `AVFilterInOut'
#}

{#fun avfilter_inout_free
  { id `Ptr AVFilterInOut'
  } -> `()'
#}

{#fun avfilter_graph_alloc
  {} -> `AVFilterGraph'
#}

{#fun avfilter_graph_free
  { id `Ptr AVFilterGraph'
  } -> `()'
#}

{#fun avfilter_graph_create_filter
  { id `Ptr AVFilterContext'
  , `AVFilter'
  , `CString'
  , `CString'
  , `Ptr ()'
  , `AVFilterGraph'
  } -> `CInt'
#}

{#fun av_buffersrc_add_frame_flags
  { `AVFilterContext'
  , `AVFrame'
  , `CInt'
  } -> `CInt'
#}

{#fun av_buffersink_get_frame
  { `AVFilterContext'
  , `AVFrame'
  } -> `CInt'
#}

{#fun av_get_default_channel_layout
  { `CInt'
  } -> `Int64'
#}

{#fun av_get_sample_fmt_name
  { `AVSampleFormat'
  } -> `CString'
#}

{#fun av_opt_set_bin
  { castPtr `Ptr obj'
  , `CString'
  , castPtr `Ptr Word8'
  , `CInt'
  , `CInt'
  } -> `CInt'
#}

{#fun av_strdup
  { `CString'
  } -> `CString'
#}

av_OPT_SEARCH_CHILDREN :: CInt
av_OPT_SEARCH_CHILDREN = 1 -- AV_OPT_SEARCH_CHILDREN is "1 << 0"

av_opt_set_int_list :: (Storable a) => Ptr obj -> String -> [a] -> CInt -> IO CInt
av_opt_set_int_list obj name vals flags = do
  let arraySize = fromIntegral $ sizeOf (head vals) * length vals -- TODO this is not necessarily right, see av_int_list_length
  withArray vals $ \pvals -> do
    withCString name $ \pname -> do
      av_opt_set_bin obj pname (castPtr pvals) arraySize flags

{#fun avfilter_graph_parse_ptr
  { `AVFilterGraph'
  , `CString'
  , id `Ptr AVFilterInOut'
  , id `Ptr AVFilterInOut'
  , `Ptr ()'
  } -> `CInt'
#}

{#fun avfilter_graph_config
  { `AVFilterGraph'
  , `Ptr ()'
  } -> `CInt'
#}

{#fun av_dict_get
  { `AVDictionary'
  , `CString'
  , `AVDictionaryEntry'
  , `CInt'
  } -> `AVDictionaryEntry'
#}

{#fun av_dict_count
  { `AVDictionary'
  } -> `CInt'
#}

{#fun av_dict_get_string
  { `AVDictionary'
  , id `Ptr CString'
  , id `CChar'
  , id `CChar'
  } -> `CInt'
#}

{#fun av_get_channel_layout_nb_channels
  { `Word64'
  } -> `CInt'
#}

{#fun avio_alloc_context
  { id `Ptr CUChar'
  , `CInt'
  , `CInt'
  , `Ptr ()'
  , castFunPtr `FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt)'
  , castFunPtr `FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt)'
  , castFunPtr `FunPtr (Ptr () -> Int64 -> CInt -> IO Int64)'
  } -> `AVIOContext'
#}

{#fun avio_context_free
  { id `Ptr AVIOContext'
  } -> `()'
#}

{#fun av_malloc
  { fromIntegral `CSize'
  } -> `Ptr ()'
#}

foreign import ccall "wrapper"
  makeReadWriteFn
    ::            (Ptr () -> Ptr Word8 -> CInt -> IO CInt)
    -> IO (FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt))

foreign import ccall "wrapper"
  makeSeekFn
    ::            (Ptr () -> Int64 -> CInt -> IO Int64)
    -> IO (FunPtr (Ptr () -> Int64 -> CInt -> IO Int64))

{#fun hs_AVERROR_EOF
  {} -> `CInt'
#}

{#fun av_get_bytes_per_sample
  { `AVSampleFormat'
  } -> `CInt'
#}

{#fun avfilter_link
  { `AVFilterContext'
  , `CUInt'
  , `AVFilterContext'
  , `CUInt'
  } -> `CInt'
#}

{#fun swr_alloc_set_opts
  { `SwrContext'
  , `Int64'
  , `AVSampleFormat'
  , `CInt'
  , `Int64'
  , `AVSampleFormat'
  , `CInt'
  , `CInt'
  , `Ptr ()'
  } -> `SwrContext'
#}

{#fun swr_init
  { `SwrContext'
  } -> `CInt'
#}

{#fun swr_convert
  { `SwrContext'
  , castPtr `Ptr (Ptr Word8)'
  , `CInt'
  , castPtr `Ptr (Ptr Word8)'
  , `CInt'
  } -> `CInt'
#}

{#fun swr_free
  { id `Ptr SwrContext'
  } -> `()'
#}

newtype Bracket m = Bracket { runBracket :: forall a b. IO a -> (a -> IO ()) -> (a -> m b) -> m b }

conduitBracket :: (MonadResource m) => Bracket (ConduitM i o m)
conduitBracket = Bracket bracketP

unliftBracket :: (MonadUnliftIO m) => Bracket m
unliftBracket = Bracket $ \acq rel -> bracket (liftIO acq) (liftIO . rel)

withHandleAVIO :: (MonadIO m) => Bracket m -> Handle -> (AVIOContext -> m a) -> m a
withHandleAVIO (runBracket -> brkt) h f = do
  liftIO $ hSeek h AbsoluteSeek 0
  canWrite <- liftIO $ hIsWritable h
  let initSize = 4096
  initBuf <- liftIO $ av_malloc initSize -- TODO do we need to free the buffer?
  let readFunction _ buf size = do
        -- putStrLn $ "read: " <> show (buf, size)
        hGetBuf h buf (fromIntegral size) >>= \case
          0 -> hs_AVERROR_EOF -- if we return 0 you get "Invalid return value 0 for stream protocol"
          n -> return $ fromIntegral n
  brkt (makeReadWriteFn readFunction) freeHaskellFunPtr $ \reader -> do
    let writeFunction _ buf size = do
          -- putStrLn $ "write: " <> show (buf, size)
          hPutBuf h buf (fromIntegral size) >> return size -- is this right? hPutBuf returns ()
    brkt (makeReadWriteFn writeFunction) freeHaskellFunPtr $ \writer -> do
      let modeMap =
            [ (sEEK_END, SeekFromEnd)
            , (sEEK_CUR, RelativeSeek)
            , (sEEK_SET, AbsoluteSeek)
            ]
          seekFunction _ posn whence = if whence .&. {#const AVSEEK_SIZE #} == {#const AVSEEK_SIZE #}
            then fmap fromIntegral $ hFileSize h
            else do
              -- TODO is there a more reliable way to get rid of all extra ffmpeg stuff?
              -- AVSEEK_SIZE is 0x10000 and AVSEEK_FORCE is 0x20000
              mode <- case lookup (whence .&. 0xFFFF) modeMap of
                Nothing -> do
                  putStrLn $ "Warning: ffmpeg passed us an unrecognized seek mode " <> show whence
                  return AbsoluteSeek
                Just mode -> return mode
              -- putStrLn $ "seek: " <> show (posn, whence)
              hSeek h mode (fromIntegral posn) >> fmap fromIntegral (hTell h)
      brkt (makeSeekFn seekFunction) freeHaskellFunPtr $ \seeker -> do
        brkt
          (avio_alloc_context (castPtr initBuf) (fromIntegral initSize) (if canWrite then 1 else 0) nullPtr reader writer seeker)
          (\p -> with p avio_context_free)
          f

data FFMPEGError = FFMPEGError
  { ffContext :: String -- usually a function name
  , ffCode    :: Int
  } deriving (Show, Typeable)
instance Exception FFMPEGError

ffCheck :: (Integral a) => String -> (a -> Bool) -> IO a -> IO ()
ffCheck ctx test act = act >>= \ret -> unless (test ret) $ throwIO FFMPEGError
  { ffContext = ctx
  , ffCode    = fromIntegral ret
  }

withStream
  :: (MonadIO m)
  => Bracket m
  -> AVMediaType
  -> Either Readable FilePath
  -> (AVFormatContext -> AVCodecContext -> AVStream -> m a)
  -> m a
withStream brkt mediaType input fn = do
  runBracket brkt avformat_alloc_context (\p -> with p avformat_close_input) $ \fmt_ctx -> let
    openInput = case input of
      Right f -> do
        liftIO $ with fmt_ctx $ \pctx -> do
          withCString f $ \s -> do
            ffCheck "avformat_open_input" (== 0) $ avformat_open_input pctx s nullPtr nullPtr
        afterOpenInput
      Left r -> runBracket brkt (rOpen r) hClose $ \h -> withHandleAVIO brkt h $ \avio -> do
        liftIO $ {#set AVFormatContext->pb #} fmt_ctx avio
        liftIO $ with fmt_ctx $ \pctx -> do
          ffCheck "avformat_open_input" (== 0) $ avformat_open_input pctx nullPtr nullPtr nullPtr
        afterOpenInput
    afterOpenInput = do
      liftIO $ ffCheck "avformat_find_stream_info" (>= 0) $ avformat_find_stream_info fmt_ctx nullPtr
      (streamIndex, dec) <- liftIO $ alloca $ \pdec -> do
        streamIndex <- av_find_best_stream fmt_ctx mediaType -1 -1 pdec 0
        dec <- peek pdec
        return (streamIndex, dec)
      runBracket brkt (avcodec_alloc_context3 dec) (\p -> with p avcodec_free_context) $ \dec_ctx -> do
        stream <- liftIO $ (!! fromIntegral streamIndex) <$> getStreams fmt_ctx
        params <- liftIO $ stream_codecpar stream
        liftIO $ ffCheck "avcodec_parameters_to_context" (>= 0) $ avcodec_parameters_to_context dec_ctx params
        liftIO $ ffCheck "avcodec_open2" (== 0) $ avcodec_open2 dec_ctx dec nullPtr
        fn fmt_ctx dec_ctx stream
    in openInput

class (Storable a) => FFSourceSample a where
  ffSourceSampleFormat :: a -> AVSampleFormat

instance FFSourceSample Int16 where
  ffSourceSampleFormat _ = AV_SAMPLE_FMT_S16

instance FFSourceSample Float where
  ffSourceSampleFormat _ = AV_SAMPLE_FMT_FLT

ffSource :: (MonadResource m, FFSourceSample a) => Either Readable FilePath -> IO (CA.AudioSource m a)
ffSource = ffSourceFrom $ CA.Frames 0

ffSourceFrom :: forall m a. (MonadResource m, FFSourceSample a) =>
  CA.Duration -> Either Readable FilePath -> IO (CA.AudioSource m a)
ffSourceFrom dur input = do
  (rate, channels, frames) <- withStream unliftBracket AVMEDIA_TYPE_AUDIO input $ \_fmt_ctx dec_ctx stream -> do
    -- TODO all these metadata pieces appear to be set elsewhere in ffmpeg 5.0
    rate <- {#get AVCodecContext->sample_rate #} dec_ctx
    channels <- {#get AVCodecContext->channels #} dec_ctx
    frames <- {#get AVStream->nb_frames #} stream >>= \case
      0 -> do
        -- nb_frames appears to be 0 in ogg and bik files? so we do this as backup.
        streamRes <- (\(num, den) -> realToFrac num / realToFrac den) <$> stream_time_base stream
        streamDur <- {#get AVStream->duration #} stream -- TODO still wrong in .bik files
        -- streamRes might be 1 / rate? but better to be sure
        return $ round ((fromIntegral streamDur * streamRes) * fromIntegral rate :: Double)
      frames -> return frames
    return (rate, channels, frames)
  let startFrame = case dur of
        CA.Frames  f -> f
        CA.Seconds s -> floor $ s * fromIntegral rate
      startSecs = fromIntegral startFrame / fromIntegral rate
  return CA.AudioSource
    { CA.source   = withStream conduitBracket AVMEDIA_TYPE_AUDIO input $ \fmt_ctx dec_ctx stream -> do
      seekInfo <- if startSecs == 0
        then return Nothing
        else liftIO $ do
          -- not sure if these can be different
          streamRes <- (\(num, den) -> realToFrac num / realToFrac den) <$> stream_time_base stream
          codecRes <- (\(num, den) -> realToFrac num / realToFrac den) <$> stream_time_base stream
          -- putStrLn $ "Resolution = " <> show (streamRes :: Double, codecRes :: Double)
          streamIndex <- stream_index stream
          let wantPTS = floor $ startSecs / streamRes
          _code <- av_seek_frame
            fmt_ctx
            streamIndex
            wantPTS
            avseek_FLAG_BACKWARD
          -- TODO warn if code < 0
          return $ Just (wantPTS, streamRes :: Double, codecRes :: Double)
      audio_stream_index <- liftIO $ stream_index stream
      -- channel_layout is set in opus, mp3, others; but unset in e.g. wav
      channelLayout <- liftIO $ {#get AVCodecContext->channel_layout #} dec_ctx >>= \case
        0 -> av_get_default_channel_layout channels >>= \case
          -- 0 means probably a mogg with a weird large channel count
          -- Might also be some other way to set channel count directly, but this works
          0 -> return $ foldr (.|.) 0 $ map bit [0 .. fromIntegral channels - 1]
          n -> return $ fromIntegral n
        n -> return $ fromIntegral n
      inSampleFormat <- liftIO $ toEnum . fromIntegral <$> {#get AVCodecContext->sample_fmt #} dec_ctx
      let swrChangeFormat = swr_alloc_set_opts
            (SwrContext nullPtr)
            channelLayout
            (ffSourceSampleFormat (undefined :: a))
            rate
            channelLayout
            inSampleFormat
            rate
            0
            nullPtr
      bracketP swrChangeFormat (\f -> with f swr_free) $ \swr -> do
      liftIO $ ffCheck "swr_init" (>= 0) $ swr_init swr
      bracketP av_frame_alloc (\f -> with f av_frame_free) $ \frame -> do
        bracketP av_packet_alloc (\p -> with p av_packet_free) $ \packet -> do
          queue <- liftIO $ newTBQueueIO 10
          let loop = do
                codePacket <- liftIO $ av_read_frame fmt_ctx packet
                if codePacket < 0
                  then do
                    -- probably reached end of file
                    liftIO $ atomically $ writeTBQueue queue Nothing
                  else do
                    packetIndex <- liftIO $ packet_stream_index packet
                    if packetIndex /= audio_stream_index
                      then do
                        liftIO $ av_packet_unref packet
                        loop
                      else do
                        -- skip frames if needed
                        -- thanks to https://stackoverflow.com/a/60317000/509936
                        skipManual <- case seekInfo of
                          Nothing -> return 0
                          Just (wantPTS, streamRes, codecRes) -> do
                            pts <- liftIO $ packet_pts packet
                            if pts < wantPTS
                              then liftIO $ do
                                let skipFrames = round $ fromIntegral (wantPTS - pts) / streamRes * codecRes
                                -- putStrLn $ "Should skip " <> show skipFrames <> " frames"
                                return skipFrames
                              else return 0
                        codeSendPacket <- liftIO $ avcodec_send_packet dec_ctx packet
                        if codeSendPacket >= 0
                          then do
                            liftIO $ av_packet_unref packet
                            loopReceiveFrames skipManual
                          else do
                            -- some weird files appear to have corrupt data at the end, just treat as end of file
                            liftIO $ atomically $ writeTBQueue queue Nothing
              loopReceiveFrames skipManual = do
                codeFrame <- liftIO $ avcodec_receive_frame dec_ctx frame
                if codeFrame < 0
                  then loop -- no more frames to get, time to feed more packets
                  else do
                    countSamples <- liftIO $ {#get AVFrame->nb_samples #} frame
                    when (countSamples > skipManual) $ do
                      -- when (skipManual > 0) $ liftIO $ putStrLn $ "Frame has " <> show countSamples <> " samples"
                      mvec <- liftIO $ MV.new $ fromIntegral $ countSamples * channels
                      liftIO $ MV.unsafeWith mvec $ \p -> do
                        inputPlanes <- frame_data frame
                        withArray [p] $ \outputPlanes -> do
                          ffCheck "swr_convert" (>= 0) $ swr_convert
                            swr
                            (castPtr outputPlanes)
                            countSamples
                            (castPtr inputPlanes)
                            countSamples
                      vec <- liftIO $ V.drop (fromIntegral $ skipManual * channels) <$> V.unsafeFreeze mvec
                      liftIO $ atomically $ writeTBQueue queue $ Just vec
                    loopReceiveFrames $ max 0 $ skipManual - countSamples
          _threadId <- liftResourceT $ resourceForkIO loop
          let readLoop = liftIO (atomically $ readTBQueue queue) >>= \case
                Nothing  -> return () -- file is done
                Just vec -> yield vec >> readLoop
          readLoop
    , CA.rate     = fromIntegral rate
    , CA.channels = fromIntegral channels
    , CA.frames   = max 0 $ fromIntegral frames - fromIntegral startFrame
    }

graphCreateFilter :: String -> Maybe String -> Maybe String -> AVFilterGraph -> IO AVFilterContext
graphCreateFilter filterType name args graph = do
  avfilter <- withCString filterType avfilter_get_by_name
  when (coerce avfilter == nullPtr) $ error $ "graphCreateFilter: no filter type named " <> show filterType
  alloca $ \p -> do
    maybe ($ nullPtr) withCString name $ \pname -> do
      maybe ($ nullPtr) withCString args $ \pargs -> do
        ffCheck "avfilter_graph_create_filter" (>= 0) $ do
          avfilter_graph_create_filter p avfilter pname pargs nullPtr graph
    peek p

addFilterSource :: AVCodecContext -> AVStream -> AVFilterGraph -> IO AVFilterContext
addFilterSource dec_ctx stream graph = do
  (num, den) <- stream_time_base stream
  {#get AVCodecContext->channel_layout #} dec_ctx >>= \case
    0 -> {#get AVCodecContext->channels #} dec_ctx
      >>= av_get_default_channel_layout
      >>= {#set AVCodecContext->channel_layout #} dec_ctx . fromIntegral
    _ -> return ()
  args <- do
    rate <- {#get AVCodecContext->sample_rate #} dec_ctx
    fmt <- {#get AVCodecContext->sample_fmt #} dec_ctx >>= av_get_sample_fmt_name . toEnum . fromIntegral >>= peekCString
    layout <- {#get AVCodecContext->channel_layout #} dec_ctx
    return $ concat ["time_base=", show num, "/", show den, ":sample_rate=", show rate, ":sample_fmt=", fmt, ":channel_layout=", show layout]
  graphCreateFilter "abuffer" Nothing (Just args) graph

filterOutputs :: AVFilterContext -> IO [AVFilterLink]
filterOutputs avfc = do
  n <- {#get AVFilterContext->nb_outputs #} avfc
  {#get AVFilterContext->outputs #} avfc >>= peekArray (fromIntegral n)

data GraphInput = GraphInput
  { giFormat :: AVFormatContext
  , giCodec :: AVCodecContext
  , giStream :: AVStream
  , giPacket :: AVPacket
  , giBuffer :: AVFilterContext -- of type "abuffer"
  }

supplyAudio :: [GraphInput] -> IO ()
supplyAudio inputs = do
  newFrames <- forConcurrently inputs $ \input -> do
    audio_stream_index <- stream_index $ giStream input
    output <- filterOutputs (giBuffer input) >>= \case
      [output] -> return output
      _        -> error "supplyAudio: not exactly 1 output on this filter"
    {#get AVFilterLink->frame_wanted_out #} output >>= \case
      -- This used to work, but now doesn't...?
      -- 0 -> return Nothing -- no data needed for this input
      _ -> let
        readFrame = do
          ret <- av_read_frame (giFormat input) (giPacket input)
          if ret < 0
            then do -- out of frames
              -- passing NULL to av_buffersrc_add_frame_flags means EOF
              return $ Just (AVFrame nullPtr, giBuffer input)
            else do
              si <- packet_stream_index $ giPacket input
              if si == audio_stream_index
                then do
                  -- this is an audio packet
                  avcodec_send_packet (giCodec input) (giPacket input) >>= \case
                    0 -> return ()
                    n -> putStrLn $ "avcodec_send_packet: " <> show n
                  av_packet_unref $ giPacket input
                  receiveFrame
                else do
                  av_packet_unref $ giPacket input
                  readFrame
        receiveFrame = do
          frame <- av_frame_alloc
          ret <- avcodec_receive_frame (giCodec input) frame
          if ret < 0
            then readFrame -- need more packets
            else return $ Just (frame, giBuffer input)
        in readFrame
  forM_ (catMaybes newFrames) $ \(frame, buffer) -> do
    av_buffersrc_add_frame_flags buffer frame 0 >>= \case
      0 -> return ()
      n -> putStrLn $ "av_buffersrc_add_frame_flags: " <> show n
    when (coerce frame /= nullPtr) $ do
      av_frame_unref frame
      with frame av_frame_free

data GraphOutput a
  = GraphEOF
  | GraphNeedInput
  | GraphOutput a

tryGetGraphOutput :: AVFilterContext -> (AVFrame -> IO a) -> IO (GraphOutput a)
tryGetGraphOutput buffersink_ctx withFrame = bracket av_frame_alloc (\p -> with p av_frame_free) $ \filt_frame -> do
  ret <- av_buffersink_get_frame buffersink_ctx filt_frame
  eof <- hs_AVERROR_EOF
  if ret < 0
    then return $ if ret == eof then GraphEOF else GraphNeedInput
    else do
      x <- withFrame filt_frame
      av_frame_unref filt_frame
      return $ GraphOutput x

audioIntegratedVolume :: FilePath -> IO (Maybe Float)
audioIntegratedVolume f = do

  withStream unliftBracket AVMEDIA_TYPE_AUDIO (Right f) $ \fmt_ctx dec_ctx stream -> do

  bracket avfilter_graph_alloc (\p -> with p avfilter_graph_free) $ \graph -> do

  -- buffer audio source: the decoded frames from the decoder will be inserted here.
  buffersrc_ctx <- addFilterSource dec_ctx stream graph

  -- make ebur128 filter
  ebur128_ctx <- graphCreateFilter "ebur128" Nothing (Just "metadata=1") graph

  -- buffer audio sink: to terminate the filter chain.
  buffersink_ctx <- graphCreateFilter "abuffersink" (Just "out") Nothing graph

  ffCheck "avfilter_link (abuffer to ebur128)" (== 0) $ avfilter_link buffersrc_ctx 0 ebur128_ctx 0
  ffCheck "avfilter_link (ebur128 to abuffersink)" (== 0) $ avfilter_link ebur128_ctx 0 buffersink_ctx 0

  ffCheck "avfilter_graph_config" (>= 0) $ avfilter_graph_config graph nullPtr

  -- read all packets
  alloca $ \(AVPacket -> packet) -> do
  let loop vol = do
        result <- tryGetGraphOutput buffersink_ctx $ \filt_frame -> do
          meta <- {#get AVFrame->metadata #} filt_frame
          entry <- withCString "lavfi.r128.I" $ \k -> av_dict_get meta k (AVDictionaryEntry nullPtr) 0
          case entry of
            AVDictionaryEntry p | p == nullPtr -> return Nothing
            _ -> fmap readMaybe $ {#get AVDictionaryEntry->value #} entry >>= peekCString
        case result of
          GraphNeedInput -> do
            let input = GraphInput
                  { giFormat = fmt_ctx
                  , giCodec = dec_ctx
                  , giStream = stream
                  , giPacket = packet
                  , giBuffer = buffersrc_ctx
                  }
            supplyAudio [input]
            loop vol
          GraphEOF -> return vol
          GraphOutput Nothing  -> loop vol
          GraphOutput (Just v) -> loop $ Just v
  loop Nothing
