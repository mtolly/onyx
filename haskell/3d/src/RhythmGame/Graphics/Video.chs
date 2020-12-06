{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RhythmGame.Graphics.Video where

import Foreign hiding (void)
import Foreign.C
import Control.Monad.Trans.StackTrace
import Control.Monad.Trans.Resource
import qualified Codec.Picture as P
import Control.Concurrent.Chan
import Control.Concurrent (forkIO, threadDelay)
import Data.IORef
import Control.Monad (void, unless, when, filterM)

#include "libavcodec/avcodec.h"
#include "libavformat/avformat.h"
#include "libavutil/imgutils.h"
#include "libswscale/swscale.h"

----------------------------

{#pointer *AVFormatContext as AVFormatContext newtype #}
deriving instance Storable AVFormatContext

{#pointer *AVFrame as AVFrame newtype #}
deriving instance Storable AVFrame
deriving instance Show AVFrame

{#pointer *AVPacket as AVPacket newtype #}
deriving instance Storable AVPacket
deriving instance Show AVPacket

{#pointer *AVCodec as AVCodec newtype #}
deriving instance Show AVCodec

{#pointer *AVCodecContext as AVCodecContext newtype #}
deriving instance Storable AVCodecContext
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

----------------------------------------

data FrameLoader = FrameLoader
  { frameMessage :: FrameMessage -> IO ()
  , getFrame     :: IO (Maybe (Double, P.Image P.PixelRGBA8))
  }

data FrameMessage
  = RequestFrame Double
  | CloseLoader

forkFrameLoader :: ((MessageLevel, Message) -> IO ()) -> FilePath -> IO FrameLoader
forkFrameLoader logger videoPath = do
  queue <- newChan
  ref <- newIORef Nothing
  void $ forkIO $ void $ runResourceT $ logIO logger $ inside ("Playing video: " <> videoPath) $ do
    let res a f = snd <$> allocate a f
        check s p a = inside s $ do
          code <- stackIO a
          unless (p code) $ fatal $ "Return code: " <> show code
        check_ s p a = void $ check s p a
        checkRes s p a f = inside s $ do
          let f' code = when (p code) f
          code <- snd <$> allocate a f'
          unless (p code) $ fatal $ "Return code: " <> show code
    stackIO $ av_log_set_level 56 -- debug info
    -- setup
    ctx <- res avformat_alloc_context avformat_free_context
    checkRes "avformat_open_input" (== 0)
      (with ctx $ \pctx ->
        withCString videoPath $ \pvid ->
          avformat_open_input pctx pvid nullPtr nullPtr)
      (with ctx avformat_close_input)
    check_ "avformat_find_stream_info" (>= 0) $ avformat_find_stream_info ctx nullPtr
    stackIO $ withCString videoPath $ \pvid -> av_dump_format ctx 0 pvid 0
    videoStreams <- stackIO $ getStreams ctx >>= filterM (\stream -> do
      params <- codecpar stream
      (AVMEDIA_TYPE_VIDEO ==) <$> codec_type params)
    stream <- case videoStreams of
      [s] -> return s
      s : _ : _ -> do
        warn "Multiple video streams; picking the first one"
        return s
      [] -> fatal "No video streams found"
    params <- stackIO $ codecpar stream
    codec <- stackIO $ codec_id params >>= avcodec_find_decoder
    w <- stackIO $ cp_width params
    h <- stackIO $ cp_height params
    fmt <- stackIO $ cp_format params
    (num, den) <- stackIO $ time_base stream
    cctx <- res (avcodec_alloc_context3 codec) $ \c -> with c avcodec_free_context
    check_ "avcodec_parameters_to_context" (>= 0) $ avcodec_parameters_to_context cctx params
    check_ "avcodec_open2 (input)" (>= 0) $ avcodec_open2 cctx codec nullPtr
    frame <- res av_frame_alloc $ \f -> with f av_frame_free
    frameRGBA <- res av_frame_alloc $ \f -> with f av_frame_free
    packet <- res av_packet_alloc $ \p -> with p av_packet_free
    stackIO $ print (cctx, frame, packet)

    let demoImage = P.generateImage (\_ _ -> P.PixelRGBA8 255 0 255 255) 256 256
    stackIO $ writeIORef ref $ Just (0, demoImage)

    -- TODO fix crash caused by avformat_free_context
    let loop = do
          stackIO $ threadDelay 1000000
          loop
    loop

    {-
      -- read a frame
      inData <- frame_data frame
      inLinesize <- frame_linesize frame
      void $ checkCode "av_image_alloc (input)" (>= 0) $ av_image_alloc inData inLinesize w h fmt 16
      void $ checkCode "av_read_frame" (>= 0) $ av_read_frame ctx packet
      void $ checkCode "stream_index" (== i) $ stream_index packet
      void $ checkCode "avcodec_send_packet" (>= 0) $ avcodec_send_packet cctx packet
      void $ checkCode "avcodec_receive_frame" (>= 0) $ avcodec_receive_frame cctx frame
      pfmt <- pix_fmt cctx
      print pfmt
      outData <- frame_data frameRGBA
      outLinesize <- frame_linesize frameRGBA
      void $ checkCode "av_image_alloc (output)" (>= 0) $ av_image_alloc outData outLinesize w h AV_PIX_FMT_RGBA 1
      frame_set_width frameRGBA w
      frame_set_height frameRGBA h
      frame_set_format frameRGBA AV_PIX_FMT_RGBA
      sws_ctx <- sws_getContext
        w
        h
        pfmt
        w
        h
        AV_PIX_FMT_RGBA
        sws_BILINEAR
        nullPtr
        nullPtr
        nullPtr
      peekArray 4 inData >>= print
      peekArray 4 inLinesize >>= print
      void $ checkCode "sws_scale" (>= 0) $ join $ sws_scale
        <$> pure sws_ctx
        <*> frame_data frame
        <*> frame_linesize frame
        <*> pure 0
        <*> pure h
        <*> frame_data frameRGBA
        <*> frame_linesize frameRGBA
      peekArray 4 inData >>= print
      peekArray 4 inLinesize >>= print
      -- save the png
      codecOut <- avcodec_find_encoder AV_CODEC_ID_PNG
      contextOut <- avcodec_alloc_context3 codecOut
      ctx_set_pix_fmt contextOut AV_PIX_FMT_RGBA
      ctx_set_width contextOut w
      ctx_set_height contextOut h
      ctx_set_codec_type contextOut AVMEDIA_TYPE_VIDEO
      ctx_set_time_base_num contextOut num
      ctx_set_time_base_den contextOut den
      void $ checkCode "avcodec_open2 (output)" (>= 0) $ avcodec_open2 contextOut codecOut nullPtr
      alloca $ \p -> do
        let outPacket = AVPacket p
        av_init_packet outPacket
        packet_set_size outPacket 0
        packet_set_data outPacket nullPtr
        void $ checkCode "avcodec_send_frame" (>= 0) $ avcodec_send_frame contextOut frameRGBA
        void $ checkCode "avcodec_receive_packet" (>= 0) $ avcodec_receive_packet contextOut outPacket
        d <- packet_data outPacket
        s <- packet_size outPacket
        bs <- B.packCStringLen (castPtr d, fromIntegral s)
        B.writeFile png bs
    -}
  return FrameLoader
    { frameMessage = writeChan queue
    , getFrame     = readIORef ref
    }
