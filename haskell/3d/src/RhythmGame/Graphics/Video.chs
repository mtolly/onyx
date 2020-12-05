{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RhythmGame.Graphics.Video where

import Foreign
import Foreign.C

#include "libavcodec/avcodec.h"
#include "libavformat/avformat.h"
#include "libavutil/imgutils.h"
#include "libswscale/swscale.h"

----------------------------

{#pointer *AVFormatContext as AVFormatContext newtype #}
deriving instance Storable AVFormatContext

{#pointer *AVFrame as AVFrame newtype #}
deriving instance Show AVFrame

{#pointer *AVPacket as AVPacket newtype #}
deriving instance Storable AVPacket
deriving instance Show AVPacket

{#pointer *AVCodec as AVCodec newtype #}
deriving instance Show AVCodec

{#pointer *AVCodecContext as AVCodecContext newtype #}
deriving instance Show AVCodecContext

{#pointer *AVStream as AVStream newtype #}
deriving instance Storable AVStream
deriving instance Show AVStream

{#enum AVCodecID {} deriving (Eq, Show) #}

{#enum AVMediaType {} deriving (Eq, Show) #}

{#enum AVPixelFormat {} deriving (Eq, Show) #}

{#pointer *AVCodecParameters as AVCodecParameters newtype #}
deriving instance Storable AVCodecParameters

{#pointer *SwsContext as SwsContext newtype #}
deriving instance Show SwsContext

-----------------------------

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
  , id `Ptr (Ptr ())'
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
  , id `Ptr (Ptr ())'
  } -> `CInt'
#}

{#fun av_frame_alloc
  {} -> `AVFrame'
#}

{#fun av_frame_free
  { id `Ptr AVFrame'
  } -> `()'
#}

{#fun av_packet_alloc
  {} -> `AVPacket'
#}

{#fun av_packet_free
  { id `Ptr AVPacket'
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

codecpar :: AVStream -> IO AVCodecParameters
codecpar = {#get AVStream->codecpar #}

codec_type :: AVCodecParameters -> IO AVMediaType
codec_type = fmap (toEnum . fromIntegral) . {#get AVCodecParameters->codec_type #}

codec_id :: AVCodecParameters -> IO AVCodecID
codec_id = fmap (toEnum . fromIntegral) . {#get AVCodecParameters->codec_id #}

cp_width, cp_height :: AVCodecParameters -> IO CInt
cp_width  = {#get AVCodecParameters->width  #}
cp_height = {#get AVCodecParameters->height #}

-- only an AVPixelFormat for video streams; for audio, it's a AVSampleFormat
cp_format :: AVCodecParameters -> IO AVPixelFormat
cp_format = fmap (toEnum . fromIntegral) . {#get AVCodecParameters->format #}

{#fun avcodec_find_decoder
  { `AVCodecID'
  } -> `AVCodec'
#}

{#fun avcodec_find_encoder
  { `AVCodecID'
  } -> `AVCodec'
#}

time_base :: AVStream -> IO (CInt, CInt)
time_base s = do
  num <- {#get AVStream->time_base.num #} s
  den <- {#get AVStream->time_base.den #} s
  return (num, den)

{#fun avcodec_alloc_context3
  { `AVCodec'
  } -> `AVCodecContext'
#}

{#fun avcodec_parameters_to_context
  { `AVCodecContext'
  , `AVCodecParameters'
  } -> `CInt'
#}

{#fun avcodec_open2
  { `AVCodecContext'
  , `AVCodec'
  , id `Ptr (Ptr ())'
  } -> `CInt'
#}

stream_index :: AVPacket -> IO CInt
stream_index = {#get AVPacket->stream_index #}

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
  , `CInt'
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

ctx_set_pix_fmt :: AVCodecContext -> AVPixelFormat -> IO ()
ctx_set_pix_fmt c = {#set AVCodecContext->pix_fmt #} c . fromIntegral . fromEnum

ctx_set_height :: AVCodecContext -> CInt -> IO ()
ctx_set_height = {#set AVCodecContext->height #}

ctx_set_width :: AVCodecContext -> CInt -> IO ()
ctx_set_width = {#set AVCodecContext->width #}

ctx_set_codec_type :: AVCodecContext -> AVMediaType -> IO ()
ctx_set_codec_type c = {#set AVCodecContext->codec_type #} c . fromIntegral . fromEnum

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

-- deprecated
{#fun avcodec_decode_video2
  { `AVCodecContext'
  , `AVFrame'
  , id `Ptr CInt'
  , `AVPacket'
  } -> `CInt'
#}

{#fun av_log_set_level
  { `CInt'
  } -> `()'
#}
