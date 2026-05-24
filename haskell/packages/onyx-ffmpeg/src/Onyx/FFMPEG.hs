{-
Claude Code did most of the work of translating this binding from c2hs to inline-c
-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE EmptyDataDecls             #-}
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
import           Control.Monad.Trans.Resource (MonadResource, ResourceT,
                                               liftResourceT, resourceForkIO,
                                               runResourceT)
import           Data.Char                    (toLower)
import           Data.Coerce                  (coerce)
import           Data.Conduit                 (ConduitM, bracketP, runConduit,
                                               yield, (.|))
import qualified Data.Conduit.Audio           as CA
import qualified Data.Conduit.List            as CL
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
import           System.FilePath              (takeExtension)
import           System.IO                    (Handle, IOMode (ReadWriteMode),
                                               SeekMode (..), hClose, hFileSize,
                                               hGetBuf, hIsWritable, hPutBuf,
                                               hSeek, hTell, withBinaryFile)
import           System.Posix.Internals       (sEEK_CUR, sEEK_END, sEEK_SET)
import           Text.Read                    (readMaybe)
import           UnliftIO                     (MonadIO, MonadUnliftIO, bracket,
                                               liftIO)

data AVFormatContext
data AVInputFormat
data AVOutputFormat
data AVFrame
data AVIOContext
data AVPacket
data AVCodec
data AVCodecContext
data AVStream
data AVFilter
data AVFilterInOut
data AVFilterLink
data AVFilterGraph
data AVFilterContext
data AVDictionary
data AVDictionaryEntry
data AVCodecParameters
data AVChannelLayout
data SwsContext
data SwrContext
data AVAudioFifo

-- enum types
newtype AVCodecID = AVCodecID CInt
  deriving (Eq, Show)
newtype AVMediaType = AVMediaType CInt
  deriving (Eq, Show)
newtype AVPixelFormat = AVPixelFormat CInt
  deriving (Eq, Show)
newtype AVSampleFormat = AVSampleFormat CInt
  deriving (Eq, Show)

-- Set up inline-c context
C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "AVFormatContext"   , [t| AVFormatContext   |])
    , (C.TypeName "AVInputFormat"     , [t| AVInputFormat     |])
    , (C.TypeName "AVOutputFormat"    , [t| AVOutputFormat    |])
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
    , (C.TypeName "AVAudioFifo"       , [t| AVAudioFifo       |])
    , (C.Enum     "AVCodecID"         , [t| AVCodecID         |])
    , (C.Enum     "AVMediaType"       , [t| AVMediaType       |])
    , (C.Enum     "AVPixelFormat"     , [t| AVPixelFormat     |])
    , (C.Enum     "AVSampleFormat"    , [t| AVSampleFormat    |])
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
C.include "libavutil/audio_fifo.h"

-- enum values we care about

pattern AV_CODEC_ID_MP3 :: AVCodecID
pattern AV_CODEC_ID_MP3 <- ((== AVCodecID [C.pure| int { AV_CODEC_ID_MP3 } |]) -> True) where
  AV_CODEC_ID_MP3 = AVCodecID [C.pure| int { AV_CODEC_ID_MP3 } |]
pattern AV_CODEC_ID_VORBIS :: AVCodecID
pattern AV_CODEC_ID_VORBIS <- ((== AVCodecID [C.pure| int { AV_CODEC_ID_VORBIS } |]) -> True) where
  AV_CODEC_ID_VORBIS = AVCodecID [C.pure| int { AV_CODEC_ID_VORBIS } |]
pattern AV_CODEC_ID_OPUS :: AVCodecID
pattern AV_CODEC_ID_OPUS <- ((== AVCodecID [C.pure| int { AV_CODEC_ID_OPUS } |]) -> True) where
  AV_CODEC_ID_OPUS = AVCodecID [C.pure| int { AV_CODEC_ID_OPUS } |]
pattern AV_CODEC_ID_PCM_S16LE :: AVCodecID
pattern AV_CODEC_ID_PCM_S16LE <- ((== AVCodecID [C.pure| int { AV_CODEC_ID_PCM_S16LE } |]) -> True) where
  AV_CODEC_ID_PCM_S16LE = AVCodecID [C.pure| int { AV_CODEC_ID_PCM_S16LE } |]

pattern AVMEDIA_TYPE_VIDEO :: AVMediaType
pattern AVMEDIA_TYPE_VIDEO <- ((== AVMediaType [C.pure| int { AVMEDIA_TYPE_VIDEO } |]) -> True) where
  AVMEDIA_TYPE_VIDEO = AVMediaType [C.pure| int { AVMEDIA_TYPE_VIDEO } |]
pattern AVMEDIA_TYPE_AUDIO :: AVMediaType
pattern AVMEDIA_TYPE_AUDIO <- ((== AVMediaType [C.pure| int { AVMEDIA_TYPE_AUDIO } |]) -> True) where
  AVMEDIA_TYPE_AUDIO = AVMediaType [C.pure| int { AVMEDIA_TYPE_AUDIO } |]

pattern AV_PIX_FMT_RGBA :: AVPixelFormat
pattern AV_PIX_FMT_RGBA <- ((== AVPixelFormat [C.pure| int { AV_PIX_FMT_RGBA } |]) -> True) where
  AV_PIX_FMT_RGBA = AVPixelFormat [C.pure| int { AV_PIX_FMT_RGBA } |]

pattern AV_SAMPLE_FMT_S16 :: AVSampleFormat
pattern AV_SAMPLE_FMT_S16 <- ((== AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_S16 } |]) -> True) where
  AV_SAMPLE_FMT_S16 = AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_S16 } |]
pattern AV_SAMPLE_FMT_FLT :: AVSampleFormat
pattern AV_SAMPLE_FMT_FLT <- ((== AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_FLT } |]) -> True) where
  AV_SAMPLE_FMT_FLT = AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_FLT } |]
pattern AV_SAMPLE_FMT_S16P :: AVSampleFormat
pattern AV_SAMPLE_FMT_S16P <- ((== AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_S16P } |]) -> True) where
  AV_SAMPLE_FMT_S16P = AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_S16P } |]
pattern AV_SAMPLE_FMT_FLTP :: AVSampleFormat
pattern AV_SAMPLE_FMT_FLTP <- ((== AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_FLTP } |]) -> True) where
  AV_SAMPLE_FMT_FLTP = AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_FLTP } |]
pattern AV_SAMPLE_FMT_NONE :: AVSampleFormat
pattern AV_SAMPLE_FMT_NONE <- ((== AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_NONE } |]) -> True) where
  AV_SAMPLE_FMT_NONE = AVSampleFormat [C.pure| int { AV_SAMPLE_FMT_NONE } |]

-----------------------------

avformat_alloc_context :: IO (Ptr AVFormatContext)
avformat_alloc_context =
  [C.exp| AVFormatContext* { avformat_alloc_context() } |]

avformat_alloc_output_context2
  :: Ptr (Ptr AVFormatContext) -> Ptr AVOutputFormat -> CString -> CString -> IO CInt
avformat_alloc_output_context2 ctx oformat formatName fileName =
  [C.exp| int { avformat_alloc_output_context2($(AVFormatContext** ctx), $(AVOutputFormat* oformat), $(char* formatName), $(char* fileName)) } |]

avformat_free_context :: Ptr AVFormatContext -> IO ()
avformat_free_context ctx =
  [C.block| void { avformat_free_context($(AVFormatContext* ctx)); } |]

avformat_open_input :: Ptr (Ptr AVFormatContext) -> CString -> Ptr () -> Ptr (Ptr AVDictionary) -> IO CInt
avformat_open_input pctx filename fmt_opt options =
  [C.exp| int {
    avformat_open_input(
      $(AVFormatContext** pctx),
      $(char* filename),
      $(void* fmt_opt),
      $(AVDictionary** options)
    )
  } |]

avformat_close_input :: Ptr (Ptr AVFormatContext) -> IO ()
avformat_close_input pctx =
  [C.block| void { avformat_close_input($(AVFormatContext** pctx)); } |]

av_dump_format :: Ptr AVFormatContext -> CInt -> CString -> CInt -> IO ()
av_dump_format ctx index url is_output =
  [C.block| void {
    av_dump_format(
      $(AVFormatContext* ctx),
      $(int index),
      $(char* url),
      $(int is_output)
    );
  } |]

avformat_find_stream_info :: Ptr AVFormatContext -> Ptr (Ptr AVDictionary) -> IO CInt
avformat_find_stream_info ctx options =
  [C.exp| int {
    avformat_find_stream_info(
      $(AVFormatContext* ctx),
      $(AVDictionary** options)
    )
  } |]

av_frame_alloc :: IO (Ptr AVFrame)
av_frame_alloc =
  [C.exp| AVFrame* { av_frame_alloc() } |]

av_frame_free :: Ptr (Ptr AVFrame) -> IO ()
av_frame_free pframe =
  [C.block| void { av_frame_free($(AVFrame** pframe)); } |]

av_frame_unref :: Ptr AVFrame -> IO ()
av_frame_unref frame =
  [C.block| void { av_frame_unref($(AVFrame* frame)); } |]

av_frame_get_buffer :: Ptr AVFrame -> CInt -> IO CInt
av_frame_get_buffer frame align =
  [C.exp| int { av_frame_get_buffer($(AVFrame* frame), $(int align)) } |]

av_frame_make_writable :: Ptr AVFrame -> IO CInt
av_frame_make_writable frame =
  [C.exp| int { av_frame_make_writable($(AVFrame* frame)) } |]

av_packet_alloc :: IO (Ptr AVPacket)
av_packet_alloc =
  [C.exp| AVPacket* { av_packet_alloc() } |]

av_packet_free :: Ptr (Ptr AVPacket) -> IO ()
av_packet_free ppacket =
  [C.block| void { av_packet_free($(AVPacket** ppacket)); } |]

av_packet_unref :: Ptr AVPacket -> IO ()
av_packet_unref packet =
  [C.block| void { av_packet_unref($(AVPacket* packet)); } |]

av_read_frame :: Ptr AVFormatContext -> Ptr AVPacket -> IO CInt
av_read_frame ctx packet =
  [C.exp| int { av_read_frame($(AVFormatContext* ctx), $(AVPacket* packet)) } |]

av_seek_frame :: Ptr AVFormatContext -> CInt -> Int64 -> CInt -> IO CInt
av_seek_frame ctx streamIndex timestamp flags =
  [C.exp| int {
    av_seek_frame(
      $(AVFormatContext* ctx),
      $(int streamIndex),
      $(int64_t timestamp),
      $(int flags)
    )
  } |]

avformat_new_stream :: Ptr AVFormatContext -> Ptr AVCodec -> IO (Ptr AVStream)
avformat_new_stream s c =
  [C.exp| AVStream* { avformat_new_stream($(AVFormatContext* s), $(AVCodec* c)) } |]

avformat_write_header :: Ptr AVFormatContext -> Ptr (Ptr AVDictionary) -> IO CInt
avformat_write_header s options =
  [C.exp| int { avformat_write_header($(AVFormatContext* s), $(AVDictionary** options)) } |]

av_write_trailer :: Ptr AVFormatContext -> IO CInt
av_write_trailer s =
  [C.exp| int { av_write_trailer($(AVFormatContext* s)) } |]

getStreams :: Ptr AVFormatContext -> IO [Ptr AVStream]
getStreams ctx = do
  n <- [C.exp| unsigned int { $(AVFormatContext* ctx)->nb_streams } |]
  p <- [C.exp| AVStream** { $(AVFormatContext* ctx)->streams } |]
  peekArray (fromIntegral n) p

instance HasField "index" (Ptr AVStream) (IO CInt) where
  getField stream = [C.exp| int { $(AVStream* stream)->index } |]

instance HasField "codecpar" (Ptr AVStream) (IO (Ptr AVCodecParameters)) where
  getField stream = [C.exp| AVCodecParameters* { $(AVStream* stream)->codecpar } |]

durationSeconds :: Ptr AVFormatContext -> IO Double
durationSeconds ctx = do
  duration <- [C.exp| int64_t { $(AVFormatContext* ctx)->duration } |]
  return $ realToFrac duration / realToFrac [C.pure| int64_t { AV_TIME_BASE } |]

startTimeSeconds :: Ptr AVFormatContext -> IO (Maybe Double)
startTimeSeconds ctx = do
  start_time <- [C.exp| int64_t { $(AVFormatContext* ctx)->start_time } |]
  return if start_time == [C.pure| int64_t { AV_NOPTS_VALUE } |]
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

instance HasField "codec_type" (Ptr AVCodecParameters) (IO AVMediaType) where
  getField params = [C.exp| enum AVMediaType { $(AVCodecParameters* params)->codec_type } |]

instance HasField "codec_id" (Ptr AVCodecParameters) (IO AVCodecID) where
  getField params = [C.exp| enum AVCodecID { $(AVCodecParameters* params)->codec_id } |]

instance HasField "width" (Ptr AVCodecParameters) (IO CInt) where
  getField params = [C.exp| int { $(AVCodecParameters* params)->width } |]

instance HasField "height" (Ptr AVCodecParameters) (IO CInt) where
  getField params = [C.exp| int { $(AVCodecParameters* params)->height } |]

avcodec_find_decoder :: AVCodecID -> IO (Ptr AVCodec)
avcodec_find_decoder codecId =
  [C.exp| const AVCodec* { avcodec_find_decoder($(enum AVCodecID codecId)) } |]

avcodec_find_encoder :: AVCodecID -> IO (Ptr AVCodec)
avcodec_find_encoder codecId =
  [C.exp| const AVCodec* { avcodec_find_encoder($(enum AVCodecID codecId)) } |]

instance HasField "time_base" (Ptr AVStream) (IO (CInt, CInt)) where
  getField stream = do
    num <- [C.exp| int { $(AVStream* stream)->time_base.num } |]
    den <- [C.exp| int { $(AVStream* stream)->time_base.den } |]
    return (num, den)

avcodec_alloc_context3 :: Ptr AVCodec -> IO (Ptr AVCodecContext)
avcodec_alloc_context3 codec =
  [C.exp| AVCodecContext* { avcodec_alloc_context3($(AVCodec* codec)) } |]

avcodec_free_context :: Ptr (Ptr AVCodecContext) -> IO ()
avcodec_free_context pctx =
  [C.block| void { avcodec_free_context($(AVCodecContext** pctx)); } |]

avcodec_parameters_to_context :: Ptr AVCodecContext -> Ptr AVCodecParameters -> IO CInt
avcodec_parameters_to_context ctx params =
  [C.exp| int {
    avcodec_parameters_to_context(
      $(AVCodecContext* ctx),
      $(AVCodecParameters* params)
    )
  } |]

avcodec_parameters_from_context :: Ptr AVCodecParameters -> Ptr AVCodecContext -> IO CInt
avcodec_parameters_from_context par codec =
  [C.exp| int {
    avcodec_parameters_from_context(
      $(AVCodecParameters* par),
      $(AVCodecContext* codec)
    )
  } |]

avcodec_open2 :: Ptr AVCodecContext -> Ptr AVCodec -> Ptr (Ptr AVDictionary) -> IO CInt
avcodec_open2 ctx codec options =
  [C.exp| int {
    avcodec_open2(
      $(AVCodecContext* ctx),
      $(AVCodec* codec),
      $(AVDictionary** options)
    )
  } |]

instance HasField "stream_index" (Ptr AVPacket) (IO CInt) where
  getField packet = [C.exp| int { $(AVPacket* packet)->stream_index } |]

avcodec_send_packet :: Ptr AVCodecContext -> Ptr AVPacket -> IO CInt
avcodec_send_packet ctx packet =
  [C.exp| int {
    avcodec_send_packet($(AVCodecContext* ctx), $(AVPacket* packet))
  } |]

avcodec_receive_frame :: Ptr AVCodecContext -> Ptr AVFrame -> IO CInt
avcodec_receive_frame ctx frame =
  [C.exp| int {
    avcodec_receive_frame($(AVCodecContext* ctx), $(AVFrame* frame))
  } |]

avcodec_send_frame :: Ptr AVCodecContext -> Ptr AVFrame -> IO CInt
avcodec_send_frame ctx frame =
  [C.exp| int {
    avcodec_send_frame($(AVCodecContext* ctx), $(AVFrame* frame))
  } |]

avcodec_receive_packet :: Ptr AVCodecContext -> Ptr AVPacket -> IO CInt
avcodec_receive_packet ctx packet =
  [C.exp| int {
    avcodec_receive_packet($(AVCodecContext* ctx), $(AVPacket* packet))
  } |]

sws_getContext :: CInt -> CInt -> AVPixelFormat -> CInt -> CInt -> AVPixelFormat -> CInt -> Ptr () -> Ptr () -> Ptr CDouble -> IO (Ptr SwsContext)
sws_getContext srcW srcH srcFormat dstW dstH dstFormat flags srcFilter dstFilter param =
  [C.exp| struct SwsContext* {
    sws_getContext(
      $(int srcW),
      $(int srcH),
      $(enum AVPixelFormat srcFormat),
      $(int dstW),
      $(int dstH),
      $(enum AVPixelFormat dstFormat),
      $(int flags),
      $(void* srcFilter),
      $(void* dstFilter),
      $(double* param)
    )
  } |]

sws_scale :: Ptr SwsContext -> Ptr (Ptr Word8) -> Ptr CInt -> CInt -> CInt -> Ptr (Ptr Word8) -> Ptr CInt -> IO CInt
sws_scale ctx srcSlice srcStride srcSliceY srcSliceH dst dstStride =
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
av_image_fill_arrays dst_data dst_linesize src pix_fmt width height align =
  [C.exp| int {
    av_image_fill_arrays(
      $(uint8_t** dst_data),
      $(int* dst_linesize),
      $(uint8_t* src),
      $(enum AVPixelFormat pix_fmt),
      $(int width),
      $(int height),
      $(int align)
    )
  } |]

instance HasField "extended_data" (Ptr AVFrame) (IO (Ptr (Ptr Word8))) where
  getField frame = [C.exp| uint8_t** { $(AVFrame* frame)->extended_data } |]
-- extended_data is required for audio with more than AV_NUM_DATA_POINTERS (8) channels
-- but we should always be able to use it in place of AVFrame->data

instance HasField "linesize" (Ptr AVFrame) (IO (Ptr CInt)) where
  getField frame = [C.exp| int* { $(AVFrame* frame)->linesize } |]

instance HasField "pts" (Ptr AVFrame) (IO Int64) where
  getField frame = [C.exp| int64_t { $(AVFrame* frame)->pts } |]

instance HasField "pts" (Ptr AVPacket) (IO Int64) where
  getField packet = [C.exp| int64_t { $(AVPacket* packet)->pts } |]

instance HasField "pix_fmt" (Ptr AVCodecContext) (IO AVPixelFormat) where
  getField ctx = [C.exp| enum AVPixelFormat { $(AVCodecContext* ctx)->pix_fmt } |]

instance HasField "time_base" (Ptr AVCodecContext) (IO (CInt, CInt)) where
  getField ctx = do
    num <- [C.exp| int { $(AVCodecContext* ctx)->time_base.num } |]
    den <- [C.exp| int { $(AVCodecContext* ctx)->time_base.den } |]
    return (num, den)

av_image_alloc :: Ptr (Ptr Word8) -> Ptr CInt -> CInt -> CInt -> AVPixelFormat -> CInt -> IO CInt
av_image_alloc pointers linesizes w h pix_fmt align =
  [C.exp| int {
    av_image_alloc(
      $(uint8_t** pointers),
      $(int* linesizes),
      $(int w),
      $(int h),
      $(enum AVPixelFormat pix_fmt),
      $(int align)
    )
  } |]

av_freep :: Ptr (Ptr a) -> IO ()
av_freep (castPtr -> ptr) =
  [C.block| void { av_freep($(void** ptr)); } |]

instance HasField "width" (Ptr AVFrame) (SettableStateVar CInt) where
  getField frame = makeSettableStateVar \width ->
    [C.block| void { $(AVFrame* frame)->width = $(int width); } |]

instance HasField "height" (Ptr AVFrame) (SettableStateVar CInt) where
  getField frame = makeSettableStateVar \height ->
    [C.block| void { $(AVFrame* frame)->height = $(int height); } |]

instance HasField "format" (Ptr AVFrame) (SettableStateVar AVPixelFormat) where
  getField frame = makeSettableStateVar \fmt ->
    [C.block| void {
      $(AVFrame* frame)->format = $(enum AVPixelFormat fmt);
    } |]

av_log_set_level :: CInt -> IO ()
av_log_set_level level =
  [C.block| void { av_log_set_level($(int level)); } |]

avseek_FLAG_BACKWARD :: CInt
avseek_FLAG_BACKWARD = [C.pure| int { AVSEEK_FLAG_BACKWARD } |]

av_find_best_stream :: Ptr AVFormatContext -> AVMediaType -> CInt -> CInt -> Ptr (Ptr AVCodec) -> CInt -> IO CInt
av_find_best_stream ctx type_ wanted_stream_nb related_stream decoder_ret flags =
  [C.exp| int {
    av_find_best_stream(
      $(AVFormatContext* ctx),
      $(enum AVMediaType type_),
      $(int wanted_stream_nb),
      $(int related_stream),
      $(const AVCodec** decoder_ret),
      $(int flags)
    )
  } |]

avfilter_get_by_name :: CString -> IO (Ptr AVFilter)
avfilter_get_by_name name =
  [C.exp| const AVFilter* { avfilter_get_by_name($(char* name)) } |]

avfilter_inout_alloc :: IO (Ptr AVFilterInOut)
avfilter_inout_alloc =
  [C.exp| AVFilterInOut* { avfilter_inout_alloc() } |]

avfilter_inout_free :: Ptr (Ptr AVFilterInOut) -> IO ()
avfilter_inout_free pinout =
  [C.block| void { avfilter_inout_free($(AVFilterInOut** pinout)); } |]

avfilter_graph_alloc :: IO (Ptr AVFilterGraph)
avfilter_graph_alloc =
  [C.exp| AVFilterGraph* { avfilter_graph_alloc() } |]

avfilter_graph_free :: Ptr (Ptr AVFilterGraph) -> IO ()
avfilter_graph_free pgraph =
  [C.block| void { avfilter_graph_free($(AVFilterGraph** pgraph)); } |]

avfilter_graph_create_filter :: Ptr (Ptr AVFilterContext) -> Ptr AVFilter -> CString -> CString -> Ptr () -> Ptr AVFilterGraph -> IO CInt
avfilter_graph_create_filter filt_ctx filt name args opaque graph_ctx =
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

av_buffersrc_add_frame_flags :: Ptr AVFilterContext -> Ptr AVFrame -> CInt -> IO CInt
av_buffersrc_add_frame_flags ctx frame flags =
  [C.exp| int {
    av_buffersrc_add_frame_flags(
      $(AVFilterContext* ctx),
      $(AVFrame* frame),
      $(int flags)
    )
  } |]

av_buffersink_get_frame :: Ptr AVFilterContext -> Ptr AVFrame -> IO CInt
av_buffersink_get_frame ctx frame =
  [C.exp| int {
    av_buffersink_get_frame($(AVFilterContext* ctx), $(AVFrame* frame))
  } |]

-- TODO switch from this to av_channel_layout_default
av_get_default_channel_layout :: CInt -> IO Int64
av_get_default_channel_layout nb_channels =
  [C.exp| int64_t { av_get_default_channel_layout($(int nb_channels)) } |]

av_channel_layout_default :: Ptr AVChannelLayout -> CInt -> IO ()
av_channel_layout_default p nb_channels =
  [C.block| void { av_channel_layout_default($(AVChannelLayout* p), $(int nb_channels)); } |]

av_get_sample_fmt_name :: AVSampleFormat -> IO CString
av_get_sample_fmt_name fmt =
  [C.exp| const char* { av_get_sample_fmt_name($(enum AVSampleFormat fmt)) } |]

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
  withArray vals \pvals -> do
    withCString name \pname -> do
      av_opt_set_bin obj pname (castPtr pvals) arraySize flags

avfilter_graph_parse_ptr :: Ptr AVFilterGraph -> CString -> Ptr (Ptr AVFilterInOut) -> Ptr (Ptr AVFilterInOut) -> Ptr () -> IO CInt
avfilter_graph_parse_ptr graph filters inputs outputs log_ctx =
  [C.exp| int {
    avfilter_graph_parse_ptr(
      $(AVFilterGraph* graph),
      $(char* filters),
      $(AVFilterInOut** inputs),
      $(AVFilterInOut** outputs),
      $(void* log_ctx)
    )
  } |]

avfilter_graph_config :: Ptr AVFilterGraph -> Ptr () -> IO CInt
avfilter_graph_config graph log_ctx =
  [C.exp| int {
    avfilter_graph_config($(AVFilterGraph* graph), $(void* log_ctx))
  } |]

av_dict_get :: Ptr AVDictionary -> CString -> Ptr AVDictionaryEntry -> CInt -> IO (Ptr AVDictionaryEntry)
av_dict_get dict key prev flags =
  [C.exp| AVDictionaryEntry* {
    av_dict_get(
      $(AVDictionary* dict),
      $(char* key),
      $(AVDictionaryEntry* prev),
      $(int flags)
    )
  } |]

av_dict_count :: Ptr AVDictionary -> IO CInt
av_dict_count dict =
  [C.exp| int { av_dict_count($(AVDictionary* dict)) } |]

av_dict_get_string :: Ptr AVDictionary -> Ptr CString -> CChar -> CChar -> IO CInt
av_dict_get_string dict buffer key_val_sep pairs_sep =
  [C.exp| int {
    av_dict_get_string(
      $(AVDictionary* dict),
      $(char** buffer),
      $(char key_val_sep),
      $(char pairs_sep)
    )
  } |]

avio_alloc_context :: Ptr Word8 -> CInt -> CInt -> Ptr () -> FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt) -> FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt) -> FunPtr (Ptr () -> Int64 -> CInt -> IO Int64) -> IO (Ptr AVIOContext)
avio_alloc_context buffer buffer_size write_flag opaque read_packet write_packet seek =
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

avio_context_free :: Ptr (Ptr AVIOContext) -> IO ()
avio_context_free pctx =
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
av_get_bytes_per_sample fmt =
  [C.exp| int { av_get_bytes_per_sample($(enum AVSampleFormat fmt)) } |]

avfilter_link :: Ptr AVFilterContext -> CUInt -> Ptr AVFilterContext -> CUInt -> IO CInt
avfilter_link src srcpad dst dstpad =
  [C.exp| int {
    avfilter_link(
      $(AVFilterContext* src),
      $(unsigned int srcpad),
      $(AVFilterContext* dst),
      $(unsigned int dstpad)
    )
  } |]

-- TODO switch from this to swr_alloc_set_opts2
swr_alloc_set_opts :: Ptr SwrContext -> Int64 -> AVSampleFormat -> CInt -> Int64 -> AVSampleFormat -> CInt -> CInt -> Ptr () -> IO (Ptr SwrContext)
swr_alloc_set_opts swr out_ch_layout out_sample_fmt out_sample_rate in_ch_layout in_sample_fmt in_sample_rate log_offset log_ctx
  = [C.exp| struct SwrContext* {
      swr_alloc_set_opts(
        $(struct SwrContext* swr),
        $(int64_t out_ch_layout),
        $(enum AVSampleFormat out_sample_fmt),
        $(int out_sample_rate),
        $(int64_t in_ch_layout),
        $(enum AVSampleFormat in_sample_fmt),
        $(int in_sample_rate),
        $(int log_offset),
        $(void* log_ctx)
      )
    } |]

swr_alloc_set_opts2 :: Ptr (Ptr SwrContext) -> Ptr AVChannelLayout -> AVSampleFormat -> CInt -> Ptr AVChannelLayout -> AVSampleFormat -> CInt -> CInt -> Ptr () -> IO CInt
swr_alloc_set_opts2 ps out_ch_layout out_sample_fmt out_sample_rate in_ch_layout in_sample_fmt in_sample_rate log_offset log_ctx
  = [C.exp| int {
      swr_alloc_set_opts2(
        $(struct SwrContext** ps),
        $(const AVChannelLayout* out_ch_layout),
        $(enum AVSampleFormat out_sample_fmt),
        $(int out_sample_rate),
        $(const AVChannelLayout* in_ch_layout),
        $(enum AVSampleFormat in_sample_fmt),
        $(int in_sample_rate),
        $(int log_offset),
        $(void* log_ctx)
      )
    } |]

swr_init :: Ptr SwrContext -> IO CInt
swr_init swr =
  [C.exp| int { swr_init($(struct SwrContext* swr)) } |]

swr_convert :: Ptr SwrContext -> Ptr (Ptr Word8) -> CInt -> Ptr (Ptr Word8) -> CInt -> IO CInt
swr_convert swr out out_count in_ in_count =
  [C.exp| int {
    swr_convert(
      $(struct SwrContext* swr),
      $(uint8_t** out),
      $(int out_count),
      $(const uint8_t ** in_),
      $(int in_count)
    )
  } |]

swr_free :: Ptr (Ptr SwrContext) -> IO ()
swr_free pswr =
  [C.block| void { swr_free($(struct SwrContext** pswr)); } |]

swr_get_delay :: Ptr SwrContext -> Int64 -> IO Int64
swr_get_delay swr base =
  [C.exp| int64_t { swr_get_delay($(struct SwrContext* swr), $(int64_t base)) } |]

av_audio_fifo_alloc :: AVSampleFormat -> CInt -> CInt -> IO (Ptr AVAudioFifo)
av_audio_fifo_alloc fmt channels nb_samples =
  [C.exp| AVAudioFifo* {
    av_audio_fifo_alloc(
      $(enum AVSampleFormat fmt),
      $(int channels),
      $(int nb_samples)
    )
  } |]

av_audio_fifo_free :: Ptr AVAudioFifo -> IO ()
av_audio_fifo_free fifo =
  [C.block| void { av_audio_fifo_free($(AVAudioFifo* fifo)); } |]

av_audio_fifo_write :: Ptr AVAudioFifo -> Ptr (Ptr Word8) -> CInt -> IO CInt
av_audio_fifo_write fifo dataPtr nb_samples =
  [C.exp| int {
    av_audio_fifo_write(
      $(AVAudioFifo* fifo),
      (void**) $(uint8_t** dataPtr),
      $(int nb_samples)
    )
  } |]

av_audio_fifo_read :: Ptr AVAudioFifo -> Ptr (Ptr Word8) -> CInt -> IO CInt
av_audio_fifo_read fifo dataPtr nb_samples =
  [C.exp| int {
    av_audio_fifo_read(
      $(AVAudioFifo* fifo),
      (void**) $(uint8_t** dataPtr),
      $(int nb_samples)
    )
  } |]

av_audio_fifo_size :: Ptr AVAudioFifo -> IO CInt
av_audio_fifo_size fifo =
  [C.exp| int { av_audio_fifo_size($(AVAudioFifo* fifo)) } |]

newtype Bracket m = Bracket { runBracket :: forall a b. IO a -> (a -> IO ()) -> (a -> m b) -> m b }

conduitBracket :: (MonadResource m) => Bracket (ConduitM i o m)
conduitBracket = Bracket bracketP

unliftBracket :: (MonadUnliftIO m) => Bracket m
unliftBracket = Bracket \acq rel -> bracket (liftIO acq) (liftIO . rel)

withHandleAVIO :: (MonadIO m) => Bracket m -> Handle -> (Ptr AVIOContext -> m a) -> m a
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
  brkt (makeReadWriteFn readFunction) freeHaskellFunPtr \reader -> do
    let writeFunction _ buf size = do
          -- putStrLn $ "write: " <> show (buf, size)
          hPutBuf h buf (fromIntegral size) >> return size -- is this right? hPutBuf returns ()
    brkt (makeReadWriteFn writeFunction) freeHaskellFunPtr \writer -> do
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
      brkt (makeSeekFn seekFunction) freeHaskellFunPtr \seeker -> do
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

setAVFormatContextPB :: Ptr AVFormatContext -> Ptr AVIOContext -> IO ()
setAVFormatContextPB ctx pb =
  [C.block| void { $(AVFormatContext* ctx)->pb = $(AVIOContext* pb); } |]

-- Tells avformat_close_input not to free pb itself, since withHandleAVIO owns it.
setAVFormatFlagCustomIO :: Ptr AVFormatContext -> IO ()
setAVFormatFlagCustomIO ctx =
  [C.block| void { $(AVFormatContext* ctx)->flags |= AVFMT_FLAG_CUSTOM_IO; } |]

getAVCodecContextSampleRate :: Ptr AVCodecContext -> IO CInt
getAVCodecContextSampleRate ctx =
  [C.exp| int { $(AVCodecContext* ctx)->sample_rate } |]

instance HasField "sample_rate" (Ptr AVCodecContext) (IO CInt) where
  getField ctx = [C.exp| int { $(AVCodecContext* ctx)->sample_rate } |]

-- TODO switch from this to ch_layout.nb_channels
instance HasField "channels" (Ptr AVCodecContext) (IO CInt) where
  getField ctx = [C.exp| int { $(AVCodecContext* ctx)->channels } |]

instance HasField "iformat" (Ptr AVFormatContext) (IO (Ptr AVInputFormat)) where
  getField ctx =
    [C.exp| const AVInputFormat* { $(AVFormatContext* ctx)->iformat } |]

instance HasField "name" (Ptr AVInputFormat) (IO CString) where
  getField iformat =
    [C.exp| const char* { ($(AVInputFormat* iformat))->name } |]

instance HasField "nb_streams" (Ptr AVFormatContext) (IO CUInt) where
  getField ctx = [C.exp| unsigned int { $(AVFormatContext* ctx)->nb_streams } |]

instance HasField "nb_frames" (Ptr AVStream) (IO Int64) where
  getField stream = [C.exp| int64_t { $(AVStream* stream)->nb_frames } |]

instance HasField "duration" (Ptr AVStream) (IO Int64) where
  getField stream = [C.exp| int64_t { $(AVStream* stream)->duration } |]

instance HasField "codec_id" (Ptr AVCodecContext) (IO AVCodecID) where
  getField ctx = [C.exp| enum AVCodecID { $(AVCodecContext* ctx)->codec_id } |]

instance HasField "frame_size" (Ptr AVCodecContext) (IO CInt) where
  getField ctx = [C.exp| int { $(AVCodecContext* ctx)->frame_size } |]

-- TODO switch from this to ch_layout
instance HasField "channel_layout" (Ptr AVCodecContext) (StateVar Word64) where
  getField ctx = makeStateVar
    [C.exp| uint64_t { $(AVCodecContext* ctx)->channel_layout } |]
    \layout -> [C.block| void { $(AVCodecContext* ctx)->channel_layout = $(uint64_t layout); } |]

instance HasField "ch_layout" (Ptr AVCodecContext) (IO (Ptr AVChannelLayout)) where
  getField ctx =
    [C.exp| AVChannelLayout* { &($(AVCodecContext* ctx)->ch_layout) } |]

instance HasField "nb_channels" (Ptr AVChannelLayout) (IO CInt) where
  getField layout = [C.exp| int { $(AVChannelLayout* layout)->nb_channels } |]

instance HasField "sample_fmt" (Ptr AVCodecContext) (IO AVSampleFormat) where
  getField ctx = [C.exp| enum AVSampleFormat { $(AVCodecContext* ctx)->sample_fmt } |]

instance HasField "nb_samples" (Ptr AVFrame) (IO CInt) where
  getField frame = [C.exp| int { $(AVFrame* frame)->nb_samples } |]

getAVFrameMetadata :: Ptr AVFrame -> IO (Ptr AVDictionary)
getAVFrameMetadata frame =
  [C.exp| AVDictionary* { $(AVFrame* frame)->metadata } |]

getAVDictionaryEntryValue :: Ptr AVDictionaryEntry -> IO CString
getAVDictionaryEntryValue entry =
  [C.exp| char* { $(AVDictionaryEntry* entry)->value } |]

getAVFilterContextNbOutputs :: Ptr AVFilterContext -> IO CUInt
getAVFilterContextNbOutputs ctx =
  [C.exp| unsigned int { $(AVFilterContext* ctx)->nb_outputs } |]

getAVFilterContextOutputs :: Ptr AVFilterContext -> IO (Ptr (Ptr ()))
getAVFilterContextOutputs ctx =
  [C.exp| void** { (void**)$(AVFilterContext* ctx)->outputs } |]

getAVFilterLinkFrameWantedOut :: Ptr AVFilterLink -> IO CInt
getAVFilterLinkFrameWantedOut link =
  [C.exp| int { $(AVFilterLink* link)->frame_wanted_out } |]

withStream
  :: (MonadIO m)
  => Bracket m
  -> AVMediaType
  -> Readable
  -> (Ptr AVFormatContext -> Ptr AVCodecContext -> Ptr AVStream -> m a)
  -> m a
withStream brkt mediaType r fn = do
  -- avformat_open_input for some reason says
  -- "Note that a user-supplied AVFormatContext will be freed on failure."
  -- so we need to not call avformat_close_input in that case
  openInputError <- liftIO $ newIORef False
  let freeIfNeeded p = readIORef openInputError >>= \case
        True  -> return ()
        False -> with p avformat_close_input
      handleOpenInputResult res = do
        when (res /= 0) $ writeIORef openInputError True
        ffCheck "avformat_open_input" (== 0) $ return res
  -- The handle/AVIO brackets must wrap the format-context bracket so that
  -- avformat_close_input runs while pb (and the underlying Handle) are still
  -- valid. AVFMT_FLAG_CUSTOM_IO tells avformat_close_input not to free pb.
  runBracket brkt (rOpen r) hClose \h -> withHandleAVIO brkt h \avio ->
    runBracket brkt avformat_alloc_context freeIfNeeded \fmt_ctx -> do
      liftIO $ setAVFormatContextPB fmt_ctx avio
      liftIO $ setAVFormatFlagCustomIO fmt_ctx
      liftIO $ with fmt_ctx \pctx -> do
        avformat_open_input pctx nullPtr nullPtr nullPtr >>= handleOpenInputResult
      liftIO $ ffCheck "avformat_find_stream_info" (>= 0) $ avformat_find_stream_info fmt_ctx nullPtr
      (streamIndex, dec) <- liftIO $ alloca \pdec -> do
        streamIndex <- av_find_best_stream fmt_ctx mediaType -1 -1 pdec 0
        dec <- peek pdec
        return (streamIndex, dec)
      runBracket brkt (avcodec_alloc_context3 dec) (\p -> with p avcodec_free_context) \dec_ctx -> do
        stream <- liftIO $ (!! fromIntegral streamIndex) <$> getStreams fmt_ctx
        params <- liftIO stream.codecpar
        liftIO $ ffCheck "avcodec_parameters_to_context" (>= 0) $ avcodec_parameters_to_context dec_ctx params
        liftIO $ ffCheck "avcodec_open2" (== 0) $ avcodec_open2 dec_ctx dec nullPtr
        fn fmt_ctx dec_ctx stream

withAllStreams
  :: (MonadIO m)
  => Bracket m
  -> AVMediaType
  -> Readable
  -> (Ptr AVFormatContext -> [(Ptr AVCodecContext, Ptr AVStream)] -> m a)
  -> m a
withAllStreams brkt mediaType r fn = do
  -- See withStream for why the handle/AVIO brackets wrap the format-context
  -- bracket and why AVFMT_FLAG_CUSTOM_IO is set.
  runBracket brkt (rOpen r) hClose \h -> withHandleAVIO brkt h \avio ->
    runBracket brkt avformat_alloc_context (\p -> with p avformat_close_input) \fmt_ctx -> do
      liftIO $ setAVFormatContextPB fmt_ctx avio
      liftIO $ setAVFormatFlagCustomIO fmt_ctx
      liftIO $ with fmt_ctx \pctx -> do
        ffCheck "avformat_open_input" (== 0) $ avformat_open_input pctx nullPtr nullPtr nullPtr
      liftIO $ ffCheck "avformat_find_stream_info" (>= 0) $ avformat_find_stream_info fmt_ctx nullPtr
      numStreams <- liftIO fmt_ctx.nb_streams
      matchingStreams <- liftIO $ alloca \pdec -> do
        fmap catMaybes $ forM [0 .. fromIntegral numStreams - 1] \i -> do
          streamIndex <- av_find_best_stream fmt_ctx mediaType i -1 pdec 0
          if streamIndex /= i -- could also compare to AVERROR_STREAM_NOT_FOUND
            then return Nothing
            else do
              dec <- peek pdec
              return $ Just (i, dec)
      allStreams <- liftIO $ getStreams fmt_ctx
      let loadStream (i, dec) rest = do
            runBracket brkt (avcodec_alloc_context3 dec) (\p -> with p avcodec_free_context) \dec_ctx -> do
              let stream = allStreams !! fromIntegral i
              params <- liftIO stream.codecpar
              liftIO $ ffCheck "avcodec_parameters_to_context" (>= 0) $ avcodec_parameters_to_context dec_ctx params
              liftIO $ ffCheck "avcodec_open2" (== 0) $ avcodec_open2 dec_ctx dec nullPtr
              rest (dec_ctx, stream)
      withMany loadStream matchingStreams $ fn fmt_ctx

class (Storable a) => FFSample a where
  ffSampleFormat :: a -> AVSampleFormat

instance FFSample Int16 where
  ffSampleFormat _ = AV_SAMPLE_FMT_S16

instance FFSample Float where
  ffSampleFormat _ = AV_SAMPLE_FMT_FLT

ffSource :: (MonadResource m, FFSample a) => Readable -> IO (CA.AudioSource m a)
ffSource = ffSourceFrom $ CA.Frames 0

ffSourceFrom :: forall m a. (MonadResource m, FFSample a) =>
  CA.Duration -> Readable -> IO (CA.AudioSource m a)
ffSourceFrom dur input = do

  let getStreamInfo :: Ptr AVFormatContext -> Ptr AVCodecContext -> Ptr AVStream -> IO (CInt, CInt, Int64)
      getStreamInfo fmt_ctx dec_ctx stream = do
        -- some of this metadata appeared to be set elsewhere in ffmpeg 5.0,
        -- but with 6.1 everything seems fine?
        rate <- dec_ctx.sample_rate
        channels <- dec_ctx.channels
        -- trying to handle festival's m4a-opus files right, they have weird low nb_frames
        isM4A <- fmt_ctx.iformat >>= \fmt -> if fmt == nullPtr
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

  (rate, channels, framesOrig, isMP3) <- withStream unliftBracket AVMEDIA_TYPE_AUDIO input \fmt_ctx dec_ctx stream -> do
    (rate, channels, frames) <- getStreamInfo fmt_ctx dec_ctx stream
    isMP3 <- (AV_CODEC_ID_MP3 ==) <$> dec_ctx.codec_id
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
    { CA.source   = withStream conduitBracket AVMEDIA_TYPE_AUDIO inputStripped \fmt_ctx dec_ctx stream -> do

      bracketP av_packet_alloc (\p -> with p av_packet_free) \packet -> do
      bracketP av_frame_alloc (\f -> with f av_frame_free) \frame -> do

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
            nullPtr
            channelLayout
            (ffSampleFormat (undefined :: a))
            rate
            channelLayout
            inSampleFormat
            rate
            0
            nullPtr
      bracketP swrChangeFormat (\f -> with f swr_free) \swr -> do
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
                      return if pts < wantPTS
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
                when (countSamples > skipManual) do
                  mvec <- liftIO $ MV.new $ fromIntegral $ countSamples * channels
                  liftIO $ MV.unsafeWith mvec \p -> do
                    inputPlanes <- frame.extended_data
                    withArray [p] \outputPlanes -> do
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
ffSourceBinkFrom :: forall m a. (MonadResource m, FFSample a) =>
  CA.Duration -> Readable -> IO (CA.AudioSource m a)
ffSourceBinkFrom dur input = do

  (rate, channels, frames) <- withAllStreams unliftBracket AVMEDIA_TYPE_AUDIO input \fmt_ctx decStreams -> do
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
    { CA.source   = withAllStreams conduitBracket AVMEDIA_TYPE_AUDIO input \fmt_ctx decStreams -> do

      bracketP av_packet_alloc (\p -> with p av_packet_free) \packet -> do
      bracketP av_frame_alloc (\f -> with f av_frame_free) \frame -> do

      let startSecs :: Double
          startSecs = fromIntegral startFrame / fromIntegral rate

      streamList <- liftIO $ forM decStreams \(dec_ctx, stream) -> do

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
          endQueues = forM_ allQueues \queue -> do
            liftIO $ atomically $ writeTBQueue queue Nothing

      -- assume always mono for now
      channelLayout <- liftIO $ av_get_default_channel_layout 1
      inSampleFormat <- liftIO (fst $ head decStreams).sample_fmt
      let swrChangeFormat = swr_alloc_set_opts
            nullPtr
            channelLayout
            (ffSampleFormat (undefined :: a))
            rate
            channelLayout
            inSampleFormat
            rate
            0
            nullPtr
      bracketP swrChangeFormat (\f -> with f swr_free) \swr -> do
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
                      return if pts < wantPTS
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
                when (countSamples > skipManual) do
                  mvec <- liftIO $ MV.new $ fromIntegral $ countSamples * channelsThis
                  liftIO $ MV.unsafeWith mvec \p -> do
                    inputPlanes <- frame.extended_data
                    withArray [p] \outputPlanes -> do
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

-- Pick a sample format the encoder accepts;
-- if FLT possible use that, othewise whatever the encoder lists first
chooseOutputSampleFormat :: Ptr AVCodec -> IO AVSampleFormat
chooseOutputSampleFormat codec =
  [C.block| enum AVSampleFormat {
    const enum AVSampleFormat *p = $(AVCodec* codec)->sample_fmts;
    if (!p)
      return AV_SAMPLE_FMT_NONE;

    enum AVSampleFormat first = *p;
    for (; *p != AV_SAMPLE_FMT_NONE; p++) {
      if (*p == AV_SAMPLE_FMT_FLT) return AV_SAMPLE_FMT_FLT;
    }
    return first;
  } |]

encodeAndWrite :: Ptr AVFormatContext -> Ptr AVCodecContext -> Ptr AVStream -> Ptr AVFrame -> IO CInt
encodeAndWrite fmt_ctx enc_ctx st frame =
  [C.block| int {
    AVFormatContext* fmt_ctx = $(AVFormatContext* fmt_ctx);
    AVCodecContext* enc_ctx = $(AVCodecContext* enc_ctx);
    AVStream* st = $(AVStream* st);
    AVFrame* frame = $(AVFrame* frame);

    int ret = avcodec_send_frame(enc_ctx, frame);
    if (ret < 0) {
      return ret;
    }

    AVPacket *pkt = av_packet_alloc();
    if (!pkt)
      return AVERROR(ENOMEM);

    while (1) {
      ret = avcodec_receive_packet(enc_ctx, pkt);
      if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF) {
        ret = 0;
        break;
      }
      if (ret < 0) {
        break;
      }

      av_packet_rescale_ts(pkt, enc_ctx->time_base, st->time_base);
      pkt->stream_index = st->index;

      ret = av_interleaved_write_frame(fmt_ctx, pkt);
      av_packet_unref(pkt);
      if (ret < 0) {
        break;
      }
    }

    av_packet_free(&pkt);
    return ret;
  } |]

-- For Ogg Vorbis output, takes quality in oggenc style (typically 0-10)
ffSink :: Maybe Double -> FilePath -> CA.AudioSource (ResourceT IO) Float -> IO ()
ffSink oggQuality fout src = do

  (muxer, codecID, outRate) <- case map toLower $ takeExtension fout of
    ".ogg"  -> return ("ogg", AV_CODEC_ID_VORBIS   , round src.rate)
    ".opus" -> return ("ogg", AV_CODEC_ID_OPUS     , 48000         )
    ".wav"  -> return ("wav", AV_CODEC_ID_PCM_S16LE, round src.rate)
    _       -> fail $ "Unsupported output audio file extension: " <> fout

  -- VBR-by-quality only applies to Vorbis. FFmpeg expects the oggenc-style
  -- quality scaled by FF_QP2LAMBDA (= 118) in global_quality, with the
  -- AV_CODEC_FLAG_QSCALE flag set; bit_rate must be left at 0.
  let qp2lambda = coerce [C.pure| double { FF_QP2LAMBDA } |] :: Double
      (useQscale, globalQuality) = case (codecID, oggQuality) of
        (AV_CODEC_ID_VORBIS, Just q) -> (1 :: CInt, round (q * qp2lambda) :: CInt)
        _                            -> (0       , 0                             )

  let contextGet = alloca \p -> do
        n <- withCString muxer \m ->
          withCString fout $ avformat_alloc_output_context2 p nullPtr m
        fmt_ctx <- peek p
        return (n, fmt_ctx)
      contextClear (_, fmt_ctx) = when (fmt_ctx /= nullPtr) $ avformat_free_context fmt_ctx
  bracket contextGet contextClear \(n, fmt_ctx) -> do
  when (n < 0 || fmt_ctx == nullPtr) $ fail "Couldn't create output context"

  codec <- avcodec_find_encoder codecID
  when (codec == nullPtr) $ fail "Codec not available"

  st <- avformat_new_stream fmt_ctx nullPtr
  when (st == nullPtr) $ fail "ENOMEM"

  let encGet = avcodec_alloc_context3 codec
      encClear enc_ctx = when (enc_ctx /= nullPtr) $ with enc_ctx avcodec_free_context
  bracket encGet encClear \enc_ctx -> do
  when (enc_ctx == nullPtr) $ fail "ENOMEM"

  outSampleFormat <- chooseOutputSampleFormat codec
  when (outSampleFormat == AV_SAMPLE_FMT_NONE) $ fail "Encoder advertises no sample formats"

  pChannelLayout <- enc_ctx.ch_layout
  av_channel_layout_default pChannelLayout $ fromIntegral src.channels
  -- NOTE: pswr has to start out pointing to null so ffmpeg allocates correctly;
  -- "alloca \pswr" causes a (not consistent) segfault!
  let swrGet = with nullPtr \pswr -> do
        ffCheck "Loading swr" (>= 0) $ swr_alloc_set_opts2
          pswr
          pChannelLayout
          outSampleFormat
          outRate
          pChannelLayout
          AV_SAMPLE_FMT_FLT
          (round src.rate)
          0
          nullPtr
        peek pswr
      swrClear swr = when (swr /= nullPtr) $ with swr swr_free
  bracket swrGet swrClear \swr -> do
  when (swr == nullPtr) $ fail "Null swr"
  ffCheck "swr_init" (>= 0) $ swr_init swr

  [C.block| void {
    AVCodecContext* enc_ctx = $(AVCodecContext* enc_ctx);
    enc_ctx->sample_fmt = $(enum AVSampleFormat outSampleFormat);
    enc_ctx->sample_rate = $(int outRate);
    enc_ctx->time_base = (AVRational){ 1, $(int outRate) };
    enc_ctx->strict_std_compliance = FF_COMPLIANCE_EXPERIMENTAL; // needed for vorbis and opus encoders

    if ($(int useQscale)) {
      enc_ctx->flags |= AV_CODEC_FLAG_QSCALE;
      enc_ctx->global_quality = $(int globalQuality);
    } else {
      enc_ctx->bit_rate = 128000;
    }

    if ($(AVFormatContext* fmt_ctx)->oformat->flags & AVFMT_GLOBALHEADER) {
      enc_ctx->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;
    }
  } |]

  ffCheck "Opening encoder" (>= 0) $ avcodec_open2 enc_ctx codec nullPtr

  codecpar <- st.codecpar
  ffCheck "Setting codec parameters" (>= 0) $ avcodec_parameters_from_context codecpar enc_ctx
  [C.block| void { $(AVStream* st)->time_base = $(AVCodecContext* enc_ctx)->time_base; } |]

  withBinaryFile fout ReadWriteMode \h -> do
  withHandleAVIO unliftBracket h \avio -> do
  when (avio == nullPtr) $ fail "Couldn't create AVIO"
  setAVFormatContextPB fmt_ctx avio

  ffCheck "Writing header" (>= 0) $ avformat_write_header fmt_ctx nullPtr

  bracket av_frame_alloc (\f -> with f av_frame_free) \frame -> do
  when (frame == nullPtr) $ fail "ENOMEM"

  ffCheck "Copying channel layout" (>= 0) [C.block| int {
    AVFrame* frame = $(AVFrame* frame);
    AVCodecContext* enc_ctx = $(AVCodecContext* enc_ctx);

    frame->format      = enc_ctx->sample_fmt;
    frame->sample_rate = enc_ctx->sample_rate;
    // PCM encoders report frame_size == 0, meaning any size works
    frame->nb_samples  = enc_ctx->frame_size > 0 ? enc_ctx->frame_size : 1024;
    return av_channel_layout_copy(&frame->ch_layout, &enc_ctx->ch_layout);
  } |]

  frameSize <- frame.nb_samples

  ffCheck "Allocate frame buffer" (>= 0) $ av_frame_get_buffer frame 0

  -- Resampler output is staged in this scratch frame before being sliced into
  -- encoder-sized frames via the FIFO. Capacity grows on demand.
  bracket av_frame_alloc (\f -> with f av_frame_free) \scratch -> do
  when (scratch == nullPtr) $ fail "ENOMEM"

  let configureScratch nbSamples = do
        av_frame_unref scratch
        ffCheck "Configuring scratch frame" (>= 0) [C.block| int {
          AVFrame* scratch = $(AVFrame* scratch);
          AVCodecContext* enc_ctx = $(AVCodecContext* enc_ctx);
          scratch->format      = enc_ctx->sample_fmt;
          scratch->sample_rate = enc_ctx->sample_rate;
          scratch->nb_samples  = $(int nbSamples);
          return av_channel_layout_copy(&scratch->ch_layout, &enc_ctx->ch_layout);
        } |]
        ffCheck "Allocate scratch buffer" (>= 0) $ av_frame_get_buffer scratch 0

  scratchCapRef <- newIORef (max frameSize 4096)
  readIORef scratchCapRef >>= configureScratch

  let fifoGet = av_audio_fifo_alloc outSampleFormat
        (fromIntegral src.channels) frameSize
      fifoFree f = when (f /= nullPtr) $ av_audio_fifo_free f
  bracket fifoGet fifoFree \fifo -> do
  when (fifo == nullPtr) $ fail "Couldn't allocate AVAudioFifo"

  let srcRate :: CInt
      srcRate = round src.rate

      ensureScratch needed = do
        cap <- readIORef scratchCapRef
        when (needed > cap) do
          let newCap = max needed (cap * 2)
          configureScratch newCap
          writeIORef scratchCapRef newCap

      pushScratchToFifo produced = when (produced > 0) do
        planes <- scratch.extended_data
        written <- av_audio_fifo_write fifo (castPtr planes) produced
        when (written /= produced) $ fail
          "av_audio_fifo_write returned fewer samples than requested"

  vPTS <- newIORef (0 :: CInt)

  let drainFrame nbSamples = do
        [C.block| void { $(AVFrame* frame)->nb_samples = $(int frameSize); } |]
        ffCheck "av_frame_make_writable" (>= 0) $ av_frame_make_writable frame
        planes <- frame.extended_data
        readN <- av_audio_fifo_read fifo (castPtr planes) nbSamples
        when (readN /= nbSamples) $ fail
          "av_audio_fifo_read returned fewer samples than requested"
        pts <- readIORef vPTS
        [C.block| void {
          $(AVFrame* frame)->nb_samples = $(int nbSamples);
          $(AVFrame* frame)->pts = $(int pts);
        } |]
        writeIORef vPTS $ pts + nbSamples
        ffCheck "Writing frame" (>= 0) $ encodeAndWrite fmt_ctx enc_ctx st frame

      drainFull = do
        sz <- av_audio_fifo_size fifo
        when (sz >= frameSize) do
          drainFrame frameSize
          drainFull

      flushResampler = do
        cap <- readIORef scratchCapRef
        planes <- scratch.extended_data
        produced <- swr_convert swr (castPtr planes) cap nullPtr 0
        when (produced < 0) $ throwIO FFMPEGError
          { ffContext = "swr_convert (flush)", ffCode = fromIntegral produced }
        pushScratchToFifo produced
        when (produced > 0) flushResampler

  runResourceT $ runConduit $ src.source .| CL.mapM_ \buf -> liftIO do

    let countSamples = fromIntegral $ CA.vectorFrames buf src.channels
    delay <- swr_get_delay swr (fromIntegral srcRate)
    let needed64 :: Int64
        needed64 = ((delay + fromIntegral countSamples) * fromIntegral outRate
                     + fromIntegral srcRate - 1) `div` fromIntegral srcRate
    ensureScratch (fromIntegral needed64)
    cap <- readIORef scratchCapRef
    scratchPlanes <- scratch.extended_data
    produced <- V.unsafeWith buf \p -> do
      withArray [p] \inputPlanes -> do
        r <- swr_convert swr (castPtr scratchPlanes) cap
          (castPtr inputPlanes) countSamples
        when (r < 0) $ throwIO FFMPEGError
          { ffContext = "swr_convert", ffCode = fromIntegral r }
        return r
    pushScratchToFifo produced
    drainFull

  flushResampler
  drainFull
  remaining <- av_audio_fifo_size fifo
  when (remaining > 0) $ drainFrame remaining

  ffCheck "Flushing" (>= 0) $ encodeAndWrite fmt_ctx enc_ctx st nullPtr

  ffCheck "Writing trailer" (>= 0) $ av_write_trailer fmt_ctx

graphCreateFilter :: String -> Maybe String -> Maybe String -> Ptr AVFilterGraph -> IO (Ptr AVFilterContext)
graphCreateFilter filterType name args graph = do
  avfilter <- withCString filterType avfilter_get_by_name
  when (avfilter == nullPtr) $ error $ "graphCreateFilter: no filter type named " <> show filterType
  alloca \p -> do
    maybe ($ nullPtr) withCString name \pname -> do
      maybe ($ nullPtr) withCString args \pargs -> do
        ffCheck "avfilter_graph_create_filter" (>= 0) do
          avfilter_graph_create_filter p avfilter pname pargs nullPtr graph
    peek p

addFilterSource :: Ptr AVCodecContext -> Ptr AVStream -> Ptr AVFilterGraph -> IO (Ptr AVFilterContext)
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

filterOutputs :: Ptr AVFilterContext -> IO [Ptr AVFilterLink]
filterOutputs avfc = do
  n <- getAVFilterContextNbOutputs avfc
  outputs_ptr <- getAVFilterContextOutputs avfc
  peekArray (fromIntegral n) (castPtr outputs_ptr)

data GraphInput = GraphInput
  { giFormat :: Ptr AVFormatContext
  , giCodec  :: Ptr AVCodecContext
  , giStream :: Ptr AVStream
  , giPacket :: Ptr AVPacket
  , giBuffer :: Ptr AVFilterContext -- of type "abuffer"
  }

supplyAudio :: [GraphInput] -> IO ()
supplyAudio inputs = do
  newFrames <- forConcurrently inputs \input -> do
    audio_stream_index <- input.giStream.index
    output <- filterOutputs input.giBuffer >>= \case
      [output] -> return output
      _        -> error "supplyAudio: not exactly 1 output on this filter"
    getAVFilterLinkFrameWantedOut output >>= \case
      -- This used to work, but now doesn't...?
      -- 0 -> return Nothing -- no data needed for this input
      _ -> let
        readFrame = do
          ret <- av_read_frame input.giFormat input.giPacket
          if ret < 0
            then do -- out of frames
              -- passing NULL to av_buffersrc_add_frame_flags means EOF
              return $ Just (nullPtr, input.giBuffer)
            else do
              si <- input.giPacket.stream_index
              if si == audio_stream_index
                then do
                  -- this is an audio packet
                  avcodec_send_packet input.giCodec input.giPacket >>= \case
                    0 -> return ()
                    n -> putStrLn $ "avcodec_send_packet: " <> show n
                  av_packet_unref input.giPacket
                  receiveFrame
                else do
                  av_packet_unref input.giPacket
                  readFrame
        receiveFrame = do
          frame <- av_frame_alloc
          ret <- avcodec_receive_frame input.giCodec frame
          if ret < 0
            then readFrame -- need more packets
            else return $ Just (frame, input.giBuffer)
        in readFrame
  forM_ (catMaybes newFrames) \(frame, buffer) -> do
    av_buffersrc_add_frame_flags buffer frame 0 >>= \case
      0 -> return ()
      n -> putStrLn $ "av_buffersrc_add_frame_flags: " <> show n
    when (frame /= nullPtr) do
      av_frame_unref frame
      with frame av_frame_free

data GraphOutput a
  = GraphEOF
  | GraphNeedInput
  | GraphOutput a

tryGetGraphOutput :: Ptr AVFilterContext -> (Ptr AVFrame -> IO a) -> IO (GraphOutput a)
tryGetGraphOutput buffersink_ctx withFrame = bracket av_frame_alloc (\p -> with p av_frame_free) \filt_frame -> do
  ret <- av_buffersink_get_frame buffersink_ctx filt_frame
  eof <- averror_EOF
  if ret < 0
    then return if ret == eof then GraphEOF else GraphNeedInput
    else do
      x <- withFrame filt_frame
      av_frame_unref filt_frame
      return $ GraphOutput x

audioIntegratedVolume :: FilePath -> IO (Maybe Float)
audioIntegratedVolume f = do

  withStream unliftBracket AVMEDIA_TYPE_AUDIO (fileReadable f) \fmt_ctx dec_ctx stream -> do

  bracket avfilter_graph_alloc (\p -> with p avfilter_graph_free) \graph -> do

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
  bracket av_packet_alloc (\p -> with p av_packet_free) \packet -> do
  let loop vol = do
        result <- tryGetGraphOutput buffersink_ctx \filt_frame -> do
          meta <- getAVFrameMetadata filt_frame
          entry <- withCString "lavfi.r128.I" \k -> av_dict_get meta k nullPtr 0
          if entry == nullPtr
            then return Nothing
            else fmap readMaybe $ getAVDictionaryEntryValue entry >>= peekCString
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
