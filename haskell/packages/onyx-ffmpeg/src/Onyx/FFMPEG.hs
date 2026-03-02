{-
Claude Code did most of the work of translating this binding from c2hs to inline-c
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}
module Onyx.FFMPEG where

import           Control.Concurrent.Async     (forConcurrently)
import           Control.Concurrent.STM       (atomically, newTBQueueIO,
                                               readTBQueue, writeTBQueue)
import           Control.Exception            (Exception (..), throwIO)
import           Control.Monad                (forM, forM_, unless, when)
import           Control.Monad.Trans.Resource (MonadResource, liftResourceT,
                                               resourceForkIO)
import           Data.Coerce                  (coerce)
import           Data.Conduit                 (ConduitM, bracketP, yield)
import qualified Data.Conduit.Audio           as CA
import           Data.IORef                   (newIORef, readIORef, writeIORef)
import           Data.List.Split              (splitOn)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes)
import           Data.StateVar                (SettableStateVar, StateVar, get,
                                               makeSettableStateVar,
                                               makeStateVar, ($=))
import           Data.Typeable                (Typeable)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Foreign
import           Foreign.C
import           GHC.Records                  (HasField (..))
import qualified Language.C.Inline            as C
import qualified Language.C.Inline.Context    as C
import qualified Language.C.Types             as C
import           Onyx.MP3                     (MP3GaplessInfo (..), stripMP3)
import           Onyx.Util.Handle             (Readable, fileReadable, rOpen)
import           System.IO                    (Handle, SeekMode (..), hClose,
                                               hFileSize, hGetBuf, hIsWritable,
                                               hPutBuf, hSeek, hTell)
import           System.Posix.Internals       (sEEK_CUR, sEEK_END, sEEK_SET)
import           Text.Read                    (readMaybe)
import           UnliftIO                     (MonadIO, MonadUnliftIO, bracket,
                                               liftIO)

-- Opaque pointer types
newtype AVFormatContext = AVFormatContext (Ptr AVFormatContext)
deriving instance Storable AVFormatContext

newtype AVInputFormat = AVInputFormat (Ptr AVInputFormat)
deriving instance Storable AVInputFormat

newtype AVFrame = AVFrame (Ptr AVFrame)
deriving instance Storable AVFrame
deriving instance Show AVFrame

newtype AVIOContext = AVIOContext (Ptr AVIOContext)
deriving instance Storable AVIOContext
deriving instance Show AVIOContext

newtype AVPacket = AVPacket (Ptr AVPacket)
deriving instance Storable AVPacket
deriving instance Show AVPacket

newtype AVCodec = AVCodec (Ptr AVCodec)
deriving instance Storable AVCodec
deriving instance Show AVCodec

newtype AVCodecContext = AVCodecContext (Ptr AVCodecContext)
deriving instance Storable AVCodecContext
deriving instance Show AVCodecContext

newtype AVStream = AVStream (Ptr AVStream)
deriving instance Storable AVStream
deriving instance Show AVStream

newtype AVFilter = AVFilter (Ptr AVFilter)
deriving instance Storable AVFilter
deriving instance Show AVFilter

newtype AVFilterInOut = AVFilterInOut (Ptr AVFilterInOut)
deriving instance Storable AVFilterInOut
deriving instance Show AVFilterInOut

newtype AVFilterLink = AVFilterLink (Ptr AVFilterLink)
deriving instance Storable AVFilterLink
deriving instance Show AVFilterLink

newtype AVFilterGraph = AVFilterGraph (Ptr AVFilterGraph)
deriving instance Storable AVFilterGraph
deriving instance Show AVFilterGraph

newtype AVFilterContext = AVFilterContext (Ptr AVFilterContext)
deriving instance Storable AVFilterContext
deriving instance Show AVFilterContext

newtype AVDictionary = AVDictionary (Ptr AVDictionary)
deriving instance Storable AVDictionary
deriving instance Show AVDictionary

newtype AVDictionaryEntry = AVDictionaryEntry (Ptr AVDictionaryEntry)
deriving instance Storable AVDictionaryEntry
deriving instance Show AVDictionaryEntry

newtype AVCodecParameters = AVCodecParameters (Ptr AVCodecParameters)
deriving instance Storable AVCodecParameters

newtype AVChannelLayout = AVChannelLayout (Ptr AVChannelLayout)
deriving instance Storable AVChannelLayout

newtype SwsContext = SwsContext (Ptr SwsContext)
deriving instance Show SwsContext

newtype SwrContext = SwrContext (Ptr SwrContext)
deriving instance Show SwrContext
deriving instance Storable SwrContext

-- Set up inline-c context
C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "AVFormatContext"   , [t| AVFormatContext   |])
    , (C.Struct   "AVInputFormat"     , [t| AVInputFormat     |])
    , (C.TypeName "AVFrame"           , [t| AVFrame           |])
    , (C.TypeName "AVIOContext"       , [t| AVIOContext       |])
    , (C.TypeName "AVPacket"          , [t| AVPacket          |])
    , (C.TypeName "AVCodec"           , [t| AVCodec           |])
    , (C.TypeName "AVCodecContext"    , [t| AVCodecContext    |])
    , (C.TypeName "AVStream"          , [t| AVStream          |])
    , (C.TypeName "AVFilter"          , [t| AVFilter          |])
    , (C.TypeName "AVFilterInOut"     , [t| AVFilterInOut     |])
    , (C.TypeName "AVFilterLink"      , [t| AVFilterLink      |])
    , (C.TypeName "AVFilterGraph"     , [t| AVFilterGraph     |])
    , (C.TypeName "AVFilterContext"   , [t| AVFilterContext   |])
    , (C.TypeName "AVDictionary"      , [t| AVDictionary      |])
    , (C.TypeName "AVDictionaryEntry" , [t| AVDictionaryEntry |])
    , (C.TypeName "AVCodecParameters" , [t| AVCodecParameters |])
    , (C.TypeName "AVChannelLayout"   , [t| AVChannelLayout   |])
    , (C.Struct   "SwsContext"        , [t| SwsContext        |])
    , (C.Struct   "SwrContext"        , [t| SwrContext        |])
    ]
  }

-- Include headers
C.include "<stdint.h>"
C.include "libavcodec/avcodec.h"
C.include "libavformat/avformat.h"
C.include "libavutil/imgutils.h"
C.include "libavutil/opt.h"
C.include "libswscale/swscale.h"
C.include "libavfilter/avfilter.h"
C.include "libavfilter/buffersink.h"
C.include "libavfilter/buffersrc.h"
C.include "libswresample/swresample.h"

-- Enum types
newtype AVCodecID = AVCodecID CInt
  deriving (Eq, Show)

pattern AV_CODEC_ID_MP3 :: AVCodecID
pattern AV_CODEC_ID_MP3 <- ((== AVCodecID [C.pure| int { AV_CODEC_ID_MP3 } |]) -> True) where
  AV_CODEC_ID_MP3 = AVCodecID [C.pure| int { AV_CODEC_ID_MP3 } |]

newtype AVMediaType = AVMediaType CInt
  deriving (Eq, Show)

pattern AVMEDIA_TYPE_VIDEO :: AVMediaType
pattern AVMEDIA_TYPE_VIDEO <- ((== AVMediaType [C.pure| int { AVMEDIA_TYPE_VIDEO } |]) -> True) where
  AVMEDIA_TYPE_VIDEO = AVMediaType [C.pure| int { AVMEDIA_TYPE_VIDEO } |]
pattern AVMEDIA_TYPE_AUDIO :: AVMediaType
pattern AVMEDIA_TYPE_AUDIO <- ((== AVMediaType [C.pure| int { AVMEDIA_TYPE_AUDIO } |]) -> True) where
  AVMEDIA_TYPE_AUDIO = AVMediaType [C.pure| int { AVMEDIA_TYPE_AUDIO } |]

newtype AVPixelFormat = AVPixelFormat CInt
  deriving (Eq, Show)

pattern AV_PIX_FMT_RGBA :: AVPixelFormat
pattern AV_PIX_FMT_RGBA <- ((== AVPixelFormat [C.pure| int { AV_PIX_FMT_RGBA } |]) -> True) where
  AV_PIX_FMT_RGBA = AVPixelFormat [C.pure| int { AV_PIX_FMT_RGBA } |]

newtype AVSampleFormat = AVSampleFormat CInt
  deriving (Eq, Show)

pattern AV_SAMPLE_FMT_S16 :: AVSampleFormat
pattern AV_SAMPLE_FMT_S16 <- ((== AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_S16 } |]) -> True) where
  AV_SAMPLE_FMT_S16 = AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_S16 } |]
pattern AV_SAMPLE_FMT_FLT :: AVSampleFormat
pattern AV_SAMPLE_FMT_FLT <- ((== AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_FLT } |]) -> True) where
  AV_SAMPLE_FMT_FLT = AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_FLT } |]

-----------------------------

avformat_alloc_context :: IO AVFormatContext
avformat_alloc_context = AVFormatContext <$>
  [C.exp| AVFormatContext* { avformat_alloc_context() } |]

avformat_free_context :: AVFormatContext -> IO ()
avformat_free_context (AVFormatContext ctx) =
  [C.block| void { avformat_free_context($(AVFormatContext* ctx)); } |]

avformat_open_input :: Ptr AVFormatContext -> CString -> Ptr () -> Ptr AVDictionary -> IO CInt
avformat_open_input (doublePtr -> pctx) filename fmt_opt (doublePtr -> options) =
  [C.exp| int {
    avformat_open_input(
      $(AVFormatContext** pctx),
      $(char* filename),
      $(void* fmt_opt),
      $(AVDictionary** options)
    )
  } |]

avformat_close_input :: Ptr AVFormatContext -> IO ()
avformat_close_input (doublePtr -> pctx) =
  [C.block| void { avformat_close_input($(AVFormatContext** pctx)); } |]

av_dump_format :: AVFormatContext -> CInt -> CString -> CInt -> IO ()
av_dump_format (AVFormatContext ctx) index url is_output =
  [C.block| void {
    av_dump_format(
      $(AVFormatContext* ctx),
      $(int index),
      $(char* url),
      $(int is_output)
    );
  } |]

avformat_find_stream_info :: AVFormatContext -> Ptr AVDictionary -> IO CInt
avformat_find_stream_info (AVFormatContext ctx) (doublePtr -> options) =
  [C.exp| int {
    avformat_find_stream_info(
      $(AVFormatContext* ctx),
      $(AVDictionary** options)
    )
  } |]

av_frame_alloc :: IO AVFrame
av_frame_alloc = AVFrame <$>
  [C.exp| AVFrame* { av_frame_alloc() } |]

av_frame_free :: Ptr AVFrame -> IO ()
av_frame_free (doublePtr -> pframe) =
  [C.block| void { av_frame_free($(AVFrame** pframe)); } |]

av_frame_unref :: AVFrame -> IO ()
av_frame_unref (AVFrame frame) =
  [C.block| void { av_frame_unref($(AVFrame* frame)); } |]

av_packet_alloc :: IO AVPacket
av_packet_alloc = AVPacket <$>
  [C.exp| AVPacket* { av_packet_alloc() } |]

av_packet_free :: Ptr AVPacket -> IO ()
av_packet_free (doublePtr -> ppacket) =
  [C.block| void { av_packet_free($(AVPacket** ppacket)); } |]

av_packet_unref :: AVPacket -> IO ()
av_packet_unref (AVPacket packet) =
  [C.block| void { av_packet_unref($(AVPacket* packet)); } |]

av_read_frame :: AVFormatContext -> AVPacket -> IO CInt
av_read_frame (AVFormatContext ctx) (AVPacket packet) =
  [C.exp| int { av_read_frame($(AVFormatContext* ctx), $(AVPacket* packet)) } |]

av_seek_frame :: AVFormatContext -> CInt -> Int64 -> CInt -> IO CInt
av_seek_frame (AVFormatContext ctx) streamIndex timestamp flags =
  [C.exp| int {
    av_seek_frame(
      $(AVFormatContext* ctx),
      $(int streamIndex),
      $(int64_t timestamp),
      $(int flags)
    )
  } |]

getStreams :: AVFormatContext -> IO [AVStream]
getStreams (AVFormatContext ctx) = do
  n <- [C.exp| unsigned int { $(AVFormatContext* ctx)->nb_streams } |]
  p <- [C.exp| AVStream** { $(AVFormatContext* ctx)->streams } |]
  map AVStream <$> peekArray (fromIntegral n) p

instance HasField "index" AVStream (IO CInt) where
  getField (AVStream stream) = [C.exp| int { $(AVStream* stream)->index } |]

instance HasField "codecpar" AVStream (IO AVCodecParameters) where
  getField (AVStream stream) = AVCodecParameters <$> [C.exp| AVCodecParameters* { $(AVStream* stream)->codecpar } |]

durationSeconds :: AVFormatContext -> IO Double
durationSeconds (AVFormatContext ctx) = do
  duration <- [C.exp| int64_t { $(AVFormatContext* ctx)->duration } |]
  return $ realToFrac duration / realToFrac [C.pure| int64_t { AV_TIME_BASE } |]

startTimeSeconds :: AVFormatContext -> IO (Maybe Double)
startTimeSeconds (AVFormatContext ctx) = do
  start_time <- [C.exp| int64_t { $(AVFormatContext* ctx)->start_time } |]
  return $ if start_time == [C.pure| int64_t { AV_NOPTS_VALUE } |]
    then Nothing
    else Just $ realToFrac start_time / realToFrac [C.pure| int64_t { AV_TIME_BASE } |]
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

instance HasField "codec_type" AVCodecParameters (IO AVMediaType) where
  getField (AVCodecParameters params) = AVMediaType <$> [C.exp| int { $(AVCodecParameters* params)->codec_type } |]

instance HasField "codec_id" AVCodecParameters (IO AVCodecID) where
  getField (AVCodecParameters params) = AVCodecID <$> [C.exp| int { $(AVCodecParameters* params)->codec_id } |]

instance HasField "width" AVCodecParameters (IO CInt) where
  getField (AVCodecParameters params) = [C.exp| int { $(AVCodecParameters* params)->width } |]

instance HasField "height" AVCodecParameters (IO CInt) where
  getField (AVCodecParameters params) = [C.exp| int { $(AVCodecParameters* params)->height } |]

avcodec_find_decoder :: AVCodecID -> IO AVCodec
avcodec_find_decoder (coerce -> codecId) = AVCodec <$>
  [C.exp| const AVCodec* { avcodec_find_decoder($(int codecId)) } |]

avcodec_find_encoder :: AVCodecID -> IO AVCodec
avcodec_find_encoder (coerce -> codecId) = AVCodec <$>
  [C.exp| const AVCodec* { avcodec_find_encoder($(int codecId)) } |]

instance HasField "time_base" AVStream (IO (CInt, CInt)) where
  getField (AVStream stream) = do
    num <- [C.exp| int { $(AVStream* stream)->time_base.num } |]
    den <- [C.exp| int { $(AVStream* stream)->time_base.den } |]
    return (num, den)

avcodec_alloc_context3 :: AVCodec -> IO AVCodecContext
avcodec_alloc_context3 (AVCodec codec) = AVCodecContext <$>
  [C.exp| AVCodecContext* { avcodec_alloc_context3($(AVCodec* codec)) } |]

avcodec_free_context :: Ptr AVCodecContext -> IO ()
avcodec_free_context (doublePtr -> pctx) =
  [C.block| void { avcodec_free_context($(AVCodecContext** pctx)); } |]

avcodec_parameters_to_context :: AVCodecContext -> AVCodecParameters -> IO CInt
avcodec_parameters_to_context (AVCodecContext ctx) (AVCodecParameters params) =
  [C.exp| int {
    avcodec_parameters_to_context(
      $(AVCodecContext* ctx),
      $(AVCodecParameters* params)
    )
  } |]

avcodec_open2 :: AVCodecContext -> AVCodec -> Ptr AVDictionary -> IO CInt
avcodec_open2 (AVCodecContext ctx) (AVCodec codec) (doublePtr -> options) =
  [C.exp| int {
    avcodec_open2(
      $(AVCodecContext* ctx),
      $(AVCodec* codec),
      $(AVDictionary** options)
    )
  } |]

instance HasField "stream_index" AVPacket (IO CInt) where
  getField (AVPacket packet) = [C.exp| int { $(AVPacket* packet)->stream_index } |]

avcodec_send_packet :: AVCodecContext -> AVPacket -> IO CInt
avcodec_send_packet (AVCodecContext ctx) (AVPacket packet) =
  [C.exp| int {
    avcodec_send_packet($(AVCodecContext* ctx), $(AVPacket* packet))
  } |]

avcodec_receive_frame :: AVCodecContext -> AVFrame -> IO CInt
avcodec_receive_frame (AVCodecContext ctx) (AVFrame frame) =
  [C.exp| int {
    avcodec_receive_frame($(AVCodecContext* ctx), $(AVFrame* frame))
  } |]

avcodec_send_frame :: AVCodecContext -> AVFrame -> IO CInt
avcodec_send_frame (AVCodecContext ctx) (AVFrame frame) =
  [C.exp| int {
    avcodec_send_frame($(AVCodecContext* ctx), $(AVFrame* frame))
  } |]

avcodec_receive_packet :: AVCodecContext -> AVPacket -> IO CInt
avcodec_receive_packet (AVCodecContext ctx) (AVPacket packet) =
  [C.exp| int {
    avcodec_receive_packet($(AVCodecContext* ctx), $(AVPacket* packet))
  } |]

sws_getContext :: CInt -> CInt -> AVPixelFormat -> CInt -> CInt -> AVPixelFormat -> CInt -> Ptr () -> Ptr () -> Ptr CDouble -> IO SwsContext
sws_getContext srcW srcH (coerce -> srcFormat) dstW dstH (coerce -> dstFormat) flags srcFilter dstFilter param = SwsContext <$>
  [C.exp| struct SwsContext* {
    sws_getContext(
      $(int srcW),
      $(int srcH),
      $(int srcFormat),
      $(int dstW),
      $(int dstH),
      $(int dstFormat),
      $(int flags),
      $(void* srcFilter),
      $(void* dstFilter),
      $(double* param)
    )
  } |]

sws_scale :: SwsContext -> Ptr (Ptr Word8) -> Ptr CInt -> CInt -> CInt -> Ptr (Ptr Word8) -> Ptr CInt -> IO CInt
sws_scale (SwsContext ctx) srcSlice srcStride srcSliceY srcSliceH dst dstStride =
  [C.exp| int {
    sws_scale(
      $(struct SwsContext* ctx),
      $(const uint8_t * const* srcSlice),
      $(int* srcStride),
      $(int srcSliceY),
      $(int srcSliceH),
      $(uint8_t** dst),
      $(int* dstStride)
    )
  } |]

sws_BILINEAR :: CInt
sws_BILINEAR = [C.pure| int { SWS_BILINEAR } |]

av_image_fill_arrays :: Ptr (Ptr Word8) -> Ptr CInt -> Ptr Word8 -> AVPixelFormat -> CInt -> CInt -> CInt -> IO CInt
av_image_fill_arrays dst_data dst_linesize src (coerce -> pix_fmt) width height align =
  [C.exp| int {
    av_image_fill_arrays(
      $(uint8_t** dst_data),
      $(int* dst_linesize),
      $(uint8_t* src),
      $(int pix_fmt),
      $(int width),
      $(int height),
      $(int align)
    )
  } |]

instance HasField "extended_data" AVFrame (IO (Ptr (Ptr Word8))) where
  getField (AVFrame frame) = [C.exp| uint8_t** { $(AVFrame* frame)->extended_data } |]
-- extended_data is required for audio with more than AV_NUM_DATA_POINTERS (8) channels
-- but we should always be able to use it in place of AVFrame->data

instance HasField "linesize" AVFrame (IO (Ptr CInt)) where
  getField (AVFrame frame) = [C.exp| int* { $(AVFrame* frame)->linesize } |]

instance HasField "pts" AVFrame (IO Int64) where
  getField (AVFrame frame) = [C.exp| int64_t { $(AVFrame* frame)->pts } |]

instance HasField "pts" AVPacket (IO Int64) where
  getField (AVPacket packet) = [C.exp| int64_t { $(AVPacket* packet)->pts } |]

instance HasField "pix_fmt" AVCodecContext (IO AVPixelFormat) where
  getField (AVCodecContext ctx) = AVPixelFormat <$> [C.exp| int { $(AVCodecContext* ctx)->pix_fmt } |]

instance HasField "time_base" AVCodecContext (IO (CInt, CInt)) where
  getField (AVCodecContext ctx) = do
    num <- [C.exp| int { $(AVCodecContext* ctx)->time_base.num } |]
    den <- [C.exp| int { $(AVCodecContext* ctx)->time_base.den } |]
    return (num, den)

av_image_alloc :: Ptr (Ptr Word8) -> Ptr CInt -> CInt -> CInt -> AVPixelFormat -> CInt -> IO CInt
av_image_alloc pointers linesizes w h (coerce -> pix_fmt) align =
  [C.exp| int {
    av_image_alloc(
      $(uint8_t** pointers),
      $(int* linesizes),
      $(int w),
      $(int h),
      $(int pix_fmt),
      $(int align)
    )
  } |]

av_freep :: Ptr (Ptr a) -> IO ()
av_freep (castPtr -> ptr) =
  [C.block| void { av_freep($(void** ptr)); } |]

instance HasField "width" AVFrame (SettableStateVar CInt) where
  getField (AVFrame frame) = makeSettableStateVar $ \width ->
    [C.block| void { $(AVFrame* frame)->width = $(int width); } |]

instance HasField "height" AVFrame (SettableStateVar CInt) where
  getField (AVFrame frame) = makeSettableStateVar $ \height ->
    [C.block| void { $(AVFrame* frame)->height = $(int height); } |]

instance HasField "format" AVFrame (SettableStateVar AVPixelFormat) where
  getField (AVFrame frame) = makeSettableStateVar $ \(coerce -> fmt) ->
    [C.block| void {
      $(AVFrame* frame)->format = $(int fmt);
    } |]

av_log_set_level :: CInt -> IO ()
av_log_set_level level =
  [C.block| void { av_log_set_level($(int level)); } |]

avseek_FLAG_BACKWARD :: CInt
avseek_FLAG_BACKWARD = [C.pure| int { AVSEEK_FLAG_BACKWARD } |]

av_find_best_stream :: AVFormatContext -> AVMediaType -> CInt -> CInt -> Ptr AVCodec -> CInt -> IO CInt
av_find_best_stream (AVFormatContext ctx) (coerce -> type_) wanted_stream_nb related_stream (doublePtr -> decoder_ret) flags =
  [C.exp| int {
    av_find_best_stream(
      $(AVFormatContext* ctx),
      $(int type_),
      $(int wanted_stream_nb),
      $(int related_stream),
      $(const AVCodec** decoder_ret),
      $(int flags)
    )
  } |]

avfilter_get_by_name :: CString -> IO AVFilter
avfilter_get_by_name name = AVFilter <$>
  [C.exp| const AVFilter* { avfilter_get_by_name($(char* name)) } |]

avfilter_inout_alloc :: IO AVFilterInOut
avfilter_inout_alloc = AVFilterInOut <$>
  [C.exp| AVFilterInOut* { avfilter_inout_alloc() } |]

avfilter_inout_free :: Ptr AVFilterInOut -> IO ()
avfilter_inout_free (doublePtr -> pinout) =
  [C.block| void { avfilter_inout_free($(AVFilterInOut** pinout)); } |]

avfilter_graph_alloc :: IO AVFilterGraph
avfilter_graph_alloc = AVFilterGraph <$>
  [C.exp| AVFilterGraph* { avfilter_graph_alloc() } |]

avfilter_graph_free :: Ptr AVFilterGraph -> IO ()
avfilter_graph_free (doublePtr -> pgraph) =
  [C.block| void { avfilter_graph_free($(AVFilterGraph** pgraph)); } |]

avfilter_graph_create_filter :: Ptr AVFilterContext -> AVFilter -> CString -> CString -> Ptr () -> AVFilterGraph -> IO CInt
avfilter_graph_create_filter (doublePtr -> filt_ctx) (AVFilter filt) name args opaque (AVFilterGraph graph_ctx) =
  [C.exp| int {
    avfilter_graph_create_filter(
      $(AVFilterContext** filt_ctx),
      $(AVFilter* filt),
      $(char* name),
      $(char* args),
      $(void* opaque),
      $(AVFilterGraph* graph_ctx)
    )
  } |]

av_buffersrc_add_frame_flags :: AVFilterContext -> AVFrame -> CInt -> IO CInt
av_buffersrc_add_frame_flags (AVFilterContext ctx) (AVFrame frame) flags =
  [C.exp| int {
    av_buffersrc_add_frame_flags(
      $(AVFilterContext* ctx),
      $(AVFrame* frame),
      $(int flags)
    )
  } |]

av_buffersink_get_frame :: AVFilterContext -> AVFrame -> IO CInt
av_buffersink_get_frame (AVFilterContext ctx) (AVFrame frame) =
  [C.exp| int {
    av_buffersink_get_frame($(AVFilterContext* ctx), $(AVFrame* frame))
  } |]

-- TODO switch from this to av_channel_layout_default
av_get_default_channel_layout :: CInt -> IO Int64
av_get_default_channel_layout nb_channels =
  [C.exp| int64_t { av_get_default_channel_layout($(int nb_channels)) } |]

av_get_sample_fmt_name :: AVSampleFormat -> IO CString
av_get_sample_fmt_name (coerce -> fmt) =
  [C.exp| const char* { av_get_sample_fmt_name($(int fmt)) } |]

av_opt_set_bin :: Ptr () -> CString -> Ptr Word8 -> CInt -> CInt -> IO CInt
av_opt_set_bin obj name val val_size search_flags =
  [C.exp| int {
    av_opt_set_bin(
      $(void* obj),
      $(char* name),
      $(uint8_t* val),
      $(int val_size),
      $(int search_flags)
    )
  } |]

av_OPT_SEARCH_CHILDREN :: CInt
av_OPT_SEARCH_CHILDREN = [C.pure| int { AV_OPT_SEARCH_CHILDREN } |]

av_opt_set_int_list :: (Storable a) => Ptr () -> String -> [a] -> CInt -> IO CInt
av_opt_set_int_list obj name vals flags = do
  let arraySize = fromIntegral $ sizeOf (head vals) * length vals
  withArray vals $ \pvals -> do
    withCString name $ \pname -> do
      av_opt_set_bin obj pname (castPtr pvals) arraySize flags

avfilter_graph_parse_ptr :: AVFilterGraph -> CString -> Ptr AVFilterInOut -> Ptr AVFilterInOut -> Ptr () -> IO CInt
avfilter_graph_parse_ptr (AVFilterGraph graph) filters (doublePtr -> inputs) (doublePtr -> outputs) log_ctx =
  [C.exp| int {
    avfilter_graph_parse_ptr(
      $(AVFilterGraph* graph),
      $(char* filters),
      $(AVFilterInOut** inputs),
      $(AVFilterInOut** outputs),
      $(void* log_ctx)
    )
  } |]

avfilter_graph_config :: AVFilterGraph -> Ptr () -> IO CInt
avfilter_graph_config (AVFilterGraph graph) log_ctx =
  [C.exp| int {
    avfilter_graph_config($(AVFilterGraph* graph), $(void* log_ctx))
  } |]

av_dict_get :: AVDictionary -> CString -> AVDictionaryEntry -> CInt -> IO AVDictionaryEntry
av_dict_get (AVDictionary dict) key (AVDictionaryEntry prev) flags = AVDictionaryEntry <$>
  [C.exp| AVDictionaryEntry* {
    av_dict_get(
      $(AVDictionary* dict),
      $(char* key),
      $(AVDictionaryEntry* prev),
      $(int flags)
    )
  } |]

av_dict_count :: AVDictionary -> IO CInt
av_dict_count (AVDictionary dict) =
  [C.exp| int { av_dict_count($(AVDictionary* dict)) } |]

av_dict_get_string :: AVDictionary -> Ptr CString -> CChar -> CChar -> IO CInt
av_dict_get_string (AVDictionary dict) buffer key_val_sep pairs_sep =
  [C.exp| int {
    av_dict_get_string(
      $(AVDictionary* dict),
      $(char** buffer),
      $(char key_val_sep),
      $(char pairs_sep)
    )
  } |]

avio_alloc_context :: Ptr Word8 -> CInt -> CInt -> Ptr () -> FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt) -> FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt) -> FunPtr (Ptr () -> Int64 -> CInt -> IO Int64) -> IO AVIOContext
avio_alloc_context buffer buffer_size write_flag opaque read_packet write_packet seek = AVIOContext <$>
  [C.exp| AVIOContext* {
    avio_alloc_context(
      $(uint8_t* buffer),
      $(int buffer_size),
      $(int write_flag),
      $(void* opaque),
      $(int(*read_packet)(void *, uint8_t *, int)),
      $(int(*write_packet)(void *, uint8_t *, int)),
      $(int64_t(*seek)(void *, int64_t, int))
    )
  } |]

avio_context_free :: Ptr AVIOContext -> IO ()
avio_context_free (doublePtr -> pctx) =
  [C.block| void { avio_context_free($(AVIOContext** pctx)); } |]

av_malloc :: CSize -> IO (Ptr ())
av_malloc size =
  [C.exp| void* { av_malloc($(size_t size)) } |]

foreign import ccall "wrapper"
  makeReadWriteFn
    ::            (Ptr () -> Ptr Word8 -> CInt -> IO CInt)
    -> IO (FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt))

foreign import ccall "wrapper"
  makeSeekFn
    ::            (Ptr () -> Int64 -> CInt -> IO Int64)
    -> IO (FunPtr (Ptr () -> Int64 -> CInt -> IO Int64))

averror_EOF :: IO CInt
averror_EOF = [C.exp| int { AVERROR_EOF } |]

av_get_bytes_per_sample :: AVSampleFormat -> IO CInt
av_get_bytes_per_sample (coerce -> fmt) =
  [C.exp| int { av_get_bytes_per_sample($(int fmt)) } |]

avfilter_link :: AVFilterContext -> CUInt -> AVFilterContext -> CUInt -> IO CInt
avfilter_link (AVFilterContext src) srcpad (AVFilterContext dst) dstpad =
  [C.exp| int {
    avfilter_link(
      $(AVFilterContext* src),
      $(unsigned int srcpad),
      $(AVFilterContext* dst),
      $(unsigned int dstpad)
    )
  } |]

-- TODO switch from this to swr_alloc_set_opts_2
swr_alloc_set_opts :: SwrContext -> Int64 -> AVSampleFormat -> CInt -> Int64 -> AVSampleFormat -> CInt -> CInt -> Ptr () -> IO SwrContext
swr_alloc_set_opts (SwrContext swr) out_ch_layout (coerce -> out_sample_fmt) out_sample_rate in_ch_layout (coerce -> in_sample_fmt) in_sample_rate log_offset log_ctx
  = SwrContext <$>
    [C.exp| struct SwrContext* {
      swr_alloc_set_opts(
        $(struct SwrContext* swr),
        $(int64_t out_ch_layout),
        $(int out_sample_fmt),
        $(int out_sample_rate),
        $(int64_t in_ch_layout),
        $(int in_sample_fmt),
        $(int in_sample_rate),
        $(int log_offset),
        $(void* log_ctx)
      )
    } |]

swr_init :: SwrContext -> IO CInt
swr_init (SwrContext swr) =
  [C.exp| int { swr_init($(struct SwrContext* swr)) } |]

swr_convert :: SwrContext -> Ptr (Ptr Word8) -> CInt -> Ptr (Ptr Word8) -> CInt -> IO CInt
swr_convert (SwrContext swr) out out_count in_ in_count =
  [C.exp| int {
    swr_convert(
      $(struct SwrContext* swr),
      $(uint8_t** out),
      $(int out_count),
      $(const uint8_t ** in_),
      $(int in_count)
    )
  } |]

swr_free :: Ptr SwrContext -> IO ()
swr_free (doublePtr -> pswr) =
  [C.block| void { swr_free($(struct SwrContext** pswr)); } |]

newtype Bracket m = Bracket { runBracket :: forall a b. IO a -> (a -> IO ()) -> (a -> m b) -> m b }

conduitBracket :: (MonadResource m) => Bracket (ConduitM i o m)
conduitBracket = Bracket bracketP

unliftBracket :: (MonadUnliftIO m) => Bracket m
unliftBracket = Bracket $ \acq rel -> bracket (liftIO acq) (liftIO . rel)

-- hack needed due to how inline-c pointers work
doublePtr :: Ptr a -> Ptr (Ptr a)
doublePtr = castPtr

withHandleAVIO :: (MonadIO m) => Bracket m -> Handle -> (AVIOContext -> m a) -> m a
withHandleAVIO (runBracket -> brkt) h f = do
  liftIO $ hSeek h AbsoluteSeek 0
  canWrite <- liftIO $ hIsWritable h
  let initSize = 4096
  initBuf <- liftIO $ av_malloc initSize -- TODO do we need to free the buffer?
  let readFunction _ buf size = do
        -- putStrLn $ "read: " <> show (buf, size)
        hGetBuf h buf (fromIntegral size) >>= \case
          0 -> averror_EOF -- if we return 0 you get "Invalid return value 0 for stream protocol"
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
          avseek_SIZE = [C.pure| int { AVSEEK_SIZE } |]
          seekFunction _ posn whence = if whence .&. avseek_SIZE == avseek_SIZE
            then fromIntegral <$> hFileSize h
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

setAVFormatContextPB :: AVFormatContext -> AVIOContext -> IO ()
setAVFormatContextPB (AVFormatContext ctx) (AVIOContext pb) =
  [C.block| void { $(AVFormatContext* ctx)->pb = $(AVIOContext* pb); } |]

getAVCodecContextSampleRate :: AVCodecContext -> IO CInt
getAVCodecContextSampleRate (AVCodecContext ctx) =
  [C.exp| int { $(AVCodecContext* ctx)->sample_rate } |]

instance HasField "sample_rate" AVCodecContext (IO CInt) where
  getField (AVCodecContext ctx) = [C.exp| int { $(AVCodecContext* ctx)->sample_rate } |]

-- TODO switch from this to ch_layout.nb_channels
instance HasField "channels" AVCodecContext (IO CInt) where
  getField (AVCodecContext ctx) = [C.exp| int { $(AVCodecContext* ctx)->channels } |]

instance HasField "iformat" AVFormatContext (IO AVInputFormat) where
  getField (AVFormatContext ctx) = AVInputFormat <$>
    [C.exp| const struct AVInputFormat* { $(AVFormatContext* ctx)->iformat } |]

instance HasField "name" AVInputFormat (IO CString) where
  getField (AVInputFormat iformat) =
    [C.exp| const char* { ($(struct AVInputFormat* iformat))->name } |]

instance HasField "nb_streams" AVFormatContext (IO CUInt) where
  getField (AVFormatContext ctx) = [C.exp| unsigned int { $(AVFormatContext* ctx)->nb_streams } |]

instance HasField "nb_frames" AVStream (IO Int64) where
  getField (AVStream stream) = [C.exp| int64_t { $(AVStream* stream)->nb_frames } |]

instance HasField "duration" AVStream (IO Int64) where
  getField (AVStream stream) = [C.exp| int64_t { $(AVStream* stream)->duration } |]

instance HasField "codec_id" AVCodecContext (IO CInt) where
  getField (AVCodecContext ctx) = [C.exp| int { $(AVCodecContext* ctx)->codec_id } |]

instance HasField "frame_size" AVCodecContext (IO CInt) where
  getField (AVCodecContext ctx) = [C.exp| int { $(AVCodecContext* ctx)->frame_size } |]

-- TODO switch from this to ch_layout
instance HasField "channel_layout" AVCodecContext (StateVar Word64) where
  getField (AVCodecContext ctx) = makeStateVar
    [C.exp| uint64_t { $(AVCodecContext* ctx)->channel_layout } |]
    $ \layout -> [C.block| void { $(AVCodecContext* ctx)->channel_layout = $(uint64_t layout); } |]

instance HasField "ch_layout" AVCodecContext (IO AVChannelLayout) where
  getField (AVCodecContext ctx) = AVChannelLayout <$>
    [C.exp| AVChannelLayout* { &($(AVCodecContext* ctx)->ch_layout) } |]

instance HasField "nb_channels" AVChannelLayout (IO CInt) where
  getField (AVChannelLayout layout) = [C.exp| int { $(AVChannelLayout* layout)->nb_channels } |]

instance HasField "sample_fmt" AVCodecContext (IO AVSampleFormat) where
  getField (AVCodecContext ctx) = AVSampleFormat <$> [C.exp| int { $(AVCodecContext* ctx)->sample_fmt } |]

instance HasField "nb_samples" AVFrame (IO CInt) where
  getField (AVFrame frame) = [C.exp| int { $(AVFrame* frame)->nb_samples } |]

getAVFrameMetadata :: AVFrame -> IO AVDictionary
getAVFrameMetadata (AVFrame frame) = AVDictionary <$>
  [C.exp| AVDictionary* { $(AVFrame* frame)->metadata } |]

getAVDictionaryEntryValue :: AVDictionaryEntry -> IO CString
getAVDictionaryEntryValue (AVDictionaryEntry entry) =
  [C.exp| char* { $(AVDictionaryEntry* entry)->value } |]

getAVFilterContextNbOutputs :: AVFilterContext -> IO CUInt
getAVFilterContextNbOutputs (AVFilterContext ctx) =
  [C.exp| unsigned int { $(AVFilterContext* ctx)->nb_outputs } |]

getAVFilterContextOutputs :: AVFilterContext -> IO (Ptr (Ptr ()))
getAVFilterContextOutputs (AVFilterContext ctx) =
  [C.exp| void** { (void**)$(AVFilterContext* ctx)->outputs } |]

getAVFilterLinkFrameWantedOut :: AVFilterLink -> IO CInt
getAVFilterLinkFrameWantedOut (AVFilterLink link) =
  [C.exp| int { $(AVFilterLink* link)->frame_wanted_out } |]

withStream
  :: (MonadIO m)
  => Bracket m
  -> AVMediaType
  -> Readable
  -> (AVFormatContext -> AVCodecContext -> AVStream -> m a)
  -> m a
withStream brkt mediaType r fn = do
  -- avformat_open_input for some reason says
  -- "Note that a user-supplied AVFormatContext will be freed on failure."
  -- so we need to not call avformat_close_input in that case
  openInputError <- liftIO $ newIORef False
  let freeIfNeeded p = readIORef openInputError >>= \case
        True  -> return ()
        False -> with p avformat_close_input
  runBracket brkt avformat_alloc_context freeIfNeeded $ \fmt_ctx -> let
    handleOpenInputResult res = do
      when (res /= 0) $ writeIORef openInputError True
      ffCheck "avformat_open_input" (== 0) $ return res
    openInput = runBracket brkt (rOpen r) hClose $ \h -> withHandleAVIO brkt h $ \avio -> do
      liftIO $ setAVFormatContextPB fmt_ctx avio
      liftIO $ with fmt_ctx $ \pctx -> do
        avformat_open_input pctx nullPtr nullPtr nullPtr >>= handleOpenInputResult
      afterOpenInput
    afterOpenInput = do
      liftIO $ ffCheck "avformat_find_stream_info" (>= 0) $ avformat_find_stream_info fmt_ctx nullPtr
      (streamIndex, dec) <- liftIO $ alloca $ \pdec -> do
        streamIndex <- av_find_best_stream fmt_ctx mediaType -1 -1 pdec 0
        dec <- peek pdec
        return (streamIndex, dec)
      runBracket brkt (avcodec_alloc_context3 dec) (\p -> with p avcodec_free_context) $ \dec_ctx -> do
        stream <- liftIO $ (!! fromIntegral streamIndex) <$> getStreams fmt_ctx
        params <- liftIO stream.codecpar
        liftIO $ ffCheck "avcodec_parameters_to_context" (>= 0) $ avcodec_parameters_to_context dec_ctx params
        liftIO $ ffCheck "avcodec_open2" (== 0) $ avcodec_open2 dec_ctx dec nullPtr
        fn fmt_ctx dec_ctx stream
    in openInput

withAllStreams
  :: (MonadIO m)
  => Bracket m
  -> AVMediaType
  -> Readable
  -> (AVFormatContext -> [(AVCodecContext, AVStream)] -> m a)
  -> m a
withAllStreams brkt mediaType r fn = do
  runBracket brkt avformat_alloc_context (\p -> with p avformat_close_input) $ \fmt_ctx -> let
    openInput = runBracket brkt (rOpen r) hClose $ \h -> withHandleAVIO brkt h $ \avio -> do
      liftIO $ setAVFormatContextPB fmt_ctx avio
      liftIO $ with fmt_ctx $ \pctx -> do
        ffCheck "avformat_open_input" (== 0) $ avformat_open_input pctx nullPtr nullPtr nullPtr
      afterOpenInput
    afterOpenInput = do
      liftIO $ ffCheck "avformat_find_stream_info" (>= 0) $ avformat_find_stream_info fmt_ctx nullPtr
      numStreams <- liftIO fmt_ctx.nb_streams
      matchingStreams <- liftIO $ alloca $ \pdec -> do
        fmap catMaybes $ forM [0 .. fromIntegral numStreams - 1] $ \i -> do
          streamIndex <- av_find_best_stream fmt_ctx mediaType i -1 pdec 0
          if streamIndex /= i -- could also compare to AVERROR_STREAM_NOT_FOUND
            then return Nothing
            else do
              dec <- peek pdec
              return $ Just (i, dec)
      allStreams <- liftIO $ getStreams fmt_ctx
      let loadStream (i, dec) rest = do
            runBracket brkt (avcodec_alloc_context3 dec) (\p -> with p avcodec_free_context) $ \dec_ctx -> do
              let stream = allStreams !! fromIntegral i
              params <- liftIO stream.codecpar
              liftIO $ ffCheck "avcodec_parameters_to_context" (>= 0) $ avcodec_parameters_to_context dec_ctx params
              liftIO $ ffCheck "avcodec_open2" (== 0) $ avcodec_open2 dec_ctx dec nullPtr
              rest (dec_ctx, stream)
      withMany loadStream matchingStreams $ fn fmt_ctx
    in openInput

class (Storable a) => FFSourceSample a where
  ffSourceSampleFormat :: a -> AVSampleFormat

instance FFSourceSample Int16 where
  ffSourceSampleFormat _ = AV_SAMPLE_FMT_S16

instance FFSourceSample Float where
  ffSourceSampleFormat _ = AV_SAMPLE_FMT_FLT

ffSource :: (MonadResource m, FFSourceSample a) => Readable -> IO (CA.AudioSource m a)
ffSource = ffSourceFrom $ CA.Frames 0

ffSourceFrom :: forall m a. (MonadResource m, FFSourceSample a) =>
  CA.Duration -> Readable -> IO (CA.AudioSource m a)
ffSourceFrom dur input = do

  let getStreamInfo :: AVFormatContext -> AVCodecContext -> AVStream -> IO (CInt, CInt, Int64)
      getStreamInfo fmt_ctx dec_ctx stream = do
        -- some of this metadata appeared to be set elsewhere in ffmpeg 5.0,
        -- but with 6.1 everything seems fine?
        rate <- dec_ctx.sample_rate
        channels <- dec_ctx.channels
        -- trying to handle festival's m4a-opus files right, they have weird low nb_frames
        isM4A <- fmt_ctx.iformat >>= \fmt -> if coerce fmt == nullPtr
          then return False
          else fmt.name >>= \s -> if s == nullPtr
            then return False
            else do
              str <- peekCString s
              return $ elem "m4a" $ splitOn "," str
        framesOrig <- stream.nb_frames >>= \frames -> if frames == 0 || isM4A
          then do
            -- nb_frames appears to be 0 in ogg and bik files? so we do this as backup.
            streamRes <- (\(num, den) -> realToFrac num / realToFrac den) <$> stream.time_base
            streamDur <- stream.duration
            if streamDur < 0
              then do -- .bik files
                secs <- durationSeconds fmt_ctx
                return $ round $ secs * fromIntegral rate
              else do
                -- streamRes might be 1 / rate? but better to be sure
                return $ round ((fromIntegral streamDur * streamRes) * fromIntegral rate :: Double)
          else do
            return frames
        return (rate, channels, framesOrig)

  (rate, channels, framesOrig, isMP3) <- withStream unliftBracket AVMEDIA_TYPE_AUDIO input $ \fmt_ctx dec_ctx stream -> do
    (rate, channels, frames) <- getStreamInfo fmt_ctx dec_ctx stream
    isMP3 <- (\codecID -> AVCodecID codecID == AV_CODEC_ID_MP3) <$> dec_ctx.codec_id
    return (rate, channels, frames, isMP3)

  (inputStripped, skipStart, frames) <- if isMP3
    then do
      (stripped, gapless) <- stripMP3 input
      (_, _, framesRaw) <- withStream unliftBracket AVMEDIA_TYPE_AUDIO stripped getStreamInfo
      let skipStart = case gapless of
            -- the "+ 528 + 1" comes from FFMPEG mp3dec.c mp3_parse_info_tag
            Just info -> fromIntegral info.startDelay + 528 + 1
            Nothing   -> 576 + 528 + 1 -- default assumption
          -- not sure of the reasoning here
          skipStartEnd = skipStart + (1152 - 1105)
          frames = case gapless of
            Just info -> case info.originalSamples of
              Just orig -> fromIntegral orig
              Nothing   -> fromIntegral framesRaw - skipStartEnd
            Nothing -> fromIntegral framesRaw - skipStartEnd
      return (stripped, skipStart, frames)
    else return (input, 0, fromIntegral framesOrig)

  let startFrame = skipStart + case dur of
        CA.Frames  f -> f
        CA.Seconds s -> floor $ s * fromIntegral rate
      startSecs :: Double
      startSecs = fromIntegral startFrame / fromIntegral rate

  return CA.AudioSource
    { CA.source   = withStream conduitBracket AVMEDIA_TYPE_AUDIO inputStripped $ \fmt_ctx dec_ctx stream -> do

      bracketP av_packet_alloc (\p -> with p av_packet_free) $ \packet -> do
      bracketP av_frame_alloc (\f -> with f av_frame_free) $ \frame -> do

      audio_stream_index <- liftIO stream.index

      -- may not need to seek if we want pts 0 anymore, but good to be safe
      streamRes <- liftIO $ (\(num, den) -> realToFrac num / realToFrac den) <$> stream.time_base
      codecRes <- liftIO $ (\(num, den) -> realToFrac num / realToFrac den) <$> dec_ctx.time_base
      let wantPTS = floor $ startSecs / streamRes
      _code <- liftIO $ av_seek_frame
        fmt_ctx
        audio_stream_index
        wantPTS
        avseek_FLAG_BACKWARD
      -- TODO warn if code < 0

      -- channel_layout is set in opus, mp3, others; but unset in e.g. wav
      channelLayout <- liftIO $ get dec_ctx.channel_layout >>= \case
        0 -> av_get_default_channel_layout channels >>= \case
          -- 0 means probably a mogg with a weird large channel count
          -- Might also be some other way to set channel count directly, but this works
          0 -> return $ foldr (.|.) 0 $ map bit [0 .. fromIntegral channels - 1]
          n -> return $ fromIntegral n
        n -> return $ fromIntegral n

      inSampleFormat <- liftIO dec_ctx.sample_fmt

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

      queue <- liftIO $ newTBQueueIO 10

      let loop = do
            codePacket <- liftIO $ av_read_frame fmt_ctx packet
            if codePacket < 0
              then do
                -- probably reached end of file
                liftIO $ atomically $ writeTBQueue queue Nothing
              else do
                packetIndex <- liftIO packet.stream_index
                if packetIndex /= audio_stream_index
                  then do
                    liftIO $ av_packet_unref packet
                    loop
                  else do
                    -- skip frames if needed
                    -- thanks to https://stackoverflow.com/a/60317000/509936
                    skipManual <- do
                      pts <- liftIO packet.pts
                      return $ if pts < wantPTS
                        then round $ (fromIntegral (wantPTS - pts) * streamRes) / codecRes
                        else 0
                    codeSendPacket <- liftIO $ avcodec_send_packet dec_ctx packet
                    liftIO $ av_packet_unref packet
                    if codeSendPacket >= 0
                      then loopReceiveFrames skipManual
                      else do
                        -- some weird files appear to have corrupt data at the end, just treat as end of file
                        liftIO $ atomically $ writeTBQueue queue Nothing

          loopReceiveFrames skipManual = do
            codeFrame <- liftIO $ avcodec_receive_frame dec_ctx frame
            if codeFrame < 0
              then loop -- no more frames to get, time to feed more packets
              else do
                countSamples <- liftIO frame.nb_samples
                when (countSamples > skipManual) $ do
                  mvec <- liftIO $ MV.new $ fromIntegral $ countSamples * channels
                  liftIO $ MV.unsafeWith mvec $ \p -> do
                    inputPlanes <- frame.extended_data
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

-- this is all hacks!! probably should use filtergraph to do the merging
ffSourceBinkFrom :: forall m a. (MonadResource m, FFSourceSample a) =>
  CA.Duration -> Readable -> IO (CA.AudioSource m a)
ffSourceBinkFrom dur input = do

  (rate, channels, frames) <- withAllStreams unliftBracket AVMEDIA_TYPE_AUDIO input $ \fmt_ctx decStreams -> do
    rates <- mapM ((.sample_rate) . fst) decStreams
    rate <- case rates of
      [] -> fail "No streams in .bik file"
      r : rs -> if all (== r) rs
        then return r
        else fail $ "More than one sample rate in .bik file: " <> show rates
    channels <- sum <$> mapM ((.channels) . fst) decStreams
    frames <- do
      secs <- durationSeconds fmt_ctx
      return $ round $ secs * fromIntegral rate
    return (rate, channels, frames)

  let startFrame = case dur of
        CA.Frames  f -> f
        CA.Seconds s -> floor $ s * fromIntegral rate

  return CA.AudioSource
    { CA.source   = withAllStreams conduitBracket AVMEDIA_TYPE_AUDIO input $ \fmt_ctx decStreams -> do

      bracketP av_packet_alloc (\p -> with p av_packet_free) $ \packet -> do
      bracketP av_frame_alloc (\f -> with f av_frame_free) $ \frame -> do

      let startSecs :: Double
          startSecs = fromIntegral startFrame / fromIntegral rate

      streamList <- liftIO $ forM decStreams $ \(dec_ctx, stream) -> do

        audio_stream_index <- stream.index

        streamRes <- (\(num, den) -> realToFrac num / realToFrac den) <$> stream.time_base
        codecRes <- (\(num, den) -> realToFrac num / realToFrac den) <$> dec_ctx.time_base
        let wantPTS = floor $ startSecs / streamRes
        _code <- av_seek_frame
          fmt_ctx
          audio_stream_index
          wantPTS
          avseek_FLAG_BACKWARD
        -- TODO warn if code < 0

        queue <- liftIO $ newTBQueueIO 100 -- need a more elegant solution here

        return (audio_stream_index, (dec_ctx, streamRes, codecRes, wantPTS, queue))

      let streamLookup = Map.fromList streamList
          allQueues = map (\(_, (_, _, _, _, queue)) -> queue) streamList
          endQueues = forM_ allQueues $ \queue -> do
            liftIO $ atomically $ writeTBQueue queue Nothing

      -- assume always mono for now
      channelLayout <- liftIO $ av_get_default_channel_layout 1
      inSampleFormat <- liftIO (fst $ head decStreams).sample_fmt
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

      let loop = do
            codePacket <- liftIO $ av_read_frame fmt_ctx packet
            if codePacket < 0
              then do
                -- probably reached end of file
                endQueues
              else do
                packetIndex <- liftIO packet.stream_index
                case Map.lookup packetIndex streamLookup of
                  Nothing -> do
                    liftIO $ av_packet_unref packet
                    loop
                  Just (dec_ctx, streamRes, codecRes, wantPTS, queue) -> do
                    -- skip frames if needed
                    -- thanks to https://stackoverflow.com/a/60317000/509936
                    skipManual <- do
                      pts <- liftIO packet.pts
                      return $ if pts < wantPTS
                        then round $ (fromIntegral (wantPTS - pts) * streamRes) / codecRes
                        else 0
                    codeSendPacket <- liftIO $ avcodec_send_packet dec_ctx packet
                    liftIO $ av_packet_unref packet
                    if codeSendPacket >= 0
                      then loopReceiveFrames skipManual dec_ctx queue
                      else do
                        -- some weird files appear to have corrupt data at the end, just treat as end of file
                        endQueues

          loopReceiveFrames skipManual dec_ctx queue = do
            codeFrame <- liftIO $ avcodec_receive_frame dec_ctx frame
            if codeFrame < 0
              then loop -- no more frames to get, time to feed more packets
              else do
                countSamples <- liftIO frame.nb_samples
                let channelsThis = 1
                when (countSamples > skipManual) $ do
                  mvec <- liftIO $ MV.new $ fromIntegral $ countSamples * channelsThis
                  liftIO $ MV.unsafeWith mvec $ \p -> do
                    inputPlanes <- frame.extended_data
                    withArray [p] $ \outputPlanes -> do
                      ffCheck "swr_convert" (>= 0) $ swr_convert
                        swr
                        (castPtr outputPlanes)
                        countSamples
                        (castPtr inputPlanes)
                        countSamples
                  vec <- liftIO $ V.drop (fromIntegral $ skipManual * channelsThis) <$> V.unsafeFreeze mvec
                  liftIO $ atomically $ writeTBQueue queue $ Just vec
                loopReceiveFrames (max 0 $ skipManual - countSamples) dec_ctx queue

      _threadId <- liftResourceT $ resourceForkIO loop
      let readLoop = do
            next <- liftIO $ mapM (atomically . readTBQueue) allQueues
            case sequence next of
              Nothing   -> return () -- file is done
              Just vecs -> do
                yield $ CA.interleave vecs
                readLoop
      readLoop

    , CA.rate     = fromIntegral rate
    , CA.channels = fromIntegral channels
    , CA.frames   = max 0 $ frames - fromIntegral startFrame
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
  (num, den) <- stream.time_base
  get dec_ctx.channel_layout >>= \case
    0 -> dec_ctx.channels
      >>= av_get_default_channel_layout
      >>= (dec_ctx.channel_layout $=) . fromIntegral
    _ -> return ()
  args <- do
    rate <- dec_ctx.sample_rate
    fmt <- dec_ctx.sample_fmt >>= av_get_sample_fmt_name >>= peekCString
    layout <- get dec_ctx.channel_layout
    return $ concat ["time_base=", show num, "/", show den, ":sample_rate=", show rate, ":sample_fmt=", fmt, ":channel_layout=", show layout]
  graphCreateFilter "abuffer" Nothing (Just args) graph

filterOutputs :: AVFilterContext -> IO [AVFilterLink]
filterOutputs avfc = do
  n <- getAVFilterContextNbOutputs avfc
  outputs_ptr <- getAVFilterContextOutputs avfc
  filter_links <- peekArray (fromIntegral n) (castPtr outputs_ptr)
  return $ map AVFilterLink filter_links

data GraphInput = GraphInput
  { giFormat :: AVFormatContext
  , giCodec  :: AVCodecContext
  , giStream :: AVStream
  , giPacket :: AVPacket
  , giBuffer :: AVFilterContext -- of type "abuffer"
  }

supplyAudio :: [GraphInput] -> IO ()
supplyAudio inputs = do
  newFrames <- forConcurrently inputs $ \input -> do
    audio_stream_index <- (giStream input).index
    output <- filterOutputs (giBuffer input) >>= \case
      [output] -> return output
      _        -> error "supplyAudio: not exactly 1 output on this filter"
    getAVFilterLinkFrameWantedOut output >>= \case
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
              si <- (giPacket input).stream_index
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
  eof <- averror_EOF
  if ret < 0
    then return $ if ret == eof then GraphEOF else GraphNeedInput
    else do
      x <- withFrame filt_frame
      av_frame_unref filt_frame
      return $ GraphOutput x

audioIntegratedVolume :: FilePath -> IO (Maybe Float)
audioIntegratedVolume f = do

  withStream unliftBracket AVMEDIA_TYPE_AUDIO (fileReadable f) $ \fmt_ctx dec_ctx stream -> do

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
          meta <- getAVFrameMetadata filt_frame
          entry <- withCString "lavfi.r128.I" $ \k -> av_dict_get meta k (AVDictionaryEntry nullPtr) 0
          case entry of
            AVDictionaryEntry p | p == nullPtr -> return Nothing
            _ -> fmap readMaybe $ getAVDictionaryEntryValue entry >>= peekCString
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
