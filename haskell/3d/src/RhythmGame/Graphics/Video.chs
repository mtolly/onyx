{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module RhythmGame.Graphics.Video where

import Foreign hiding (void)
import Foreign.C
import Control.Monad.Trans.StackTrace
import Control.Monad.Trans.Resource
import qualified Codec.Picture as P
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)
import Data.IORef
import Control.Monad (join, void, unless, when, filterM)
import qualified Data.Vector.Storable.Mutable   as MV
import qualified Data.Vector.Storable as V
import Config (VideoInfo(..))
import Data.Fixed (mod')

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

stream_index :: AVStream -> IO CInt
stream_index = {#get AVStream->index #}

stream_codecpar :: AVStream -> IO AVCodecParameters
stream_codecpar = {#get AVStream->codecpar #}

avfc_duration :: AVFormatContext -> IO Double
avfc_duration fc = do
  n <- {#get AVFormatContext->duration #} fc
  return $ realToFrac n / {#const AV_TIME_BASE #}

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

forkFrameLoader :: ((MessageLevel, Message) -> IO ()) -> VideoInfo FilePath -> IO FrameLoader
forkFrameLoader logger vi = do
  let videoPath = _fileVideo vi
  queue <- newTChanIO
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
    ctx <- res avformat_alloc_context (const $ return ()) -- avformat_free_context TODO figure out why this crashes
    checkRes "avformat_open_input" (== 0)
      (with ctx $ \pctx ->
        withCString videoPath $ \pvid ->
          avformat_open_input pctx pvid nullPtr nullPtr)
      (with ctx avformat_close_input)
    check_ "avformat_find_stream_info" (>= 0) $ avformat_find_stream_info ctx nullPtr
    stackIO $ withCString videoPath $ \pvid -> av_dump_format ctx 0 pvid 0
    videoStreams <- stackIO $ getStreams ctx >>= filterM (\stream -> do
      params <- stream_codecpar stream
      (AVMEDIA_TYPE_VIDEO ==) <$> codec_type params)
    stream <- case videoStreams of
      [s] -> return s
      s : _ : _ -> do
        warn "Multiple video streams; picking the first one"
        return s
      [] -> fatal "No video streams found"
    streamIndex <- stackIO $ stream_index stream
    params <- stackIO $ stream_codecpar stream
    codec <- stackIO $ codec_id params >>= avcodec_find_decoder
    w <- stackIO $ cp_width params
    h <- stackIO $ cp_height params
    fmt <- stackIO $ cp_format params
    (num, den) <- stackIO $ time_base stream
    let resolution = realToFrac num / realToFrac den :: Double
    duration <- stackIO $ avfc_duration ctx
    let videoStart = maybe 0 realToFrac $ _videoStartTime vi
        videoEnd = maybe duration realToFrac $ _videoEndTime vi
        playLength = videoEnd - videoStart
    cctx <- res (avcodec_alloc_context3 codec) $ \c -> with c avcodec_free_context
    check_ "avcodec_parameters_to_context" (>= 0) $ avcodec_parameters_to_context cctx params
    check_ "avcodec_open2 (input)" (>= 0) $ avcodec_open2 cctx codec nullPtr
    frame <- res av_frame_alloc $ \f -> with f av_frame_free
    frameRGBA <- res av_frame_alloc $ \f -> with f av_frame_free
    packet <- res av_packet_alloc $ \p -> with p av_packet_free
    inData <- stackIO $ frame_data frame
    inLinesize <- stackIO $ frame_linesize frame
    check_ "av_image_alloc (input)" (>= 0) $ av_image_alloc inData inLinesize w h fmt 16
    pfmt <- stackIO $ pix_fmt cctx
    outData <- stackIO $ frame_data frameRGBA
    outLinesize <- stackIO $ frame_linesize frameRGBA
    check_ "av_image_alloc (output)" (>= 0) $ av_image_alloc outData outLinesize w h AV_PIX_FMT_RGBA 1
    stackIO $ frame_set_width frameRGBA w
    stackIO $ frame_set_height frameRGBA h
    stackIO $ frame_set_format frameRGBA AV_PIX_FMT_RGBA
    sws_ctx <- stackIO $ sws_getContext
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

    let readImage lastTimePTS thisTimeMIDI = do
          let thisTimeNoLoop = thisTimeMIDI + videoStart
              thisTimeMaybe = if thisTimeNoLoop < 0
                then Nothing -- before start of video
                else if thisTimeNoLoop >= videoEnd
                  then if _videoLoop vi
                    then Just $ videoStart + mod' thisTimeMIDI playLength
                    else Nothing -- after end of video
                  else Just thisTimeNoLoop
          case thisTimeMaybe of
            Nothing -> do
              stackIO $ writeIORef ref Nothing
              checkMessages Nothing
            Just thisTime -> do
              let skipSeek = case lastTimePTS of
                    Nothing     -> False
                    Just (t, _) -> t < thisTime && thisTime - t < seeklessDuration
                  wantPTS = floor $ thisTime / resolution
              unless skipSeek $ check_ "av_seek_frame" (>= 0) $ av_seek_frame
                ctx
                streamIndex
                wantPTS
                {#const AVSEEK_FLAG_ANY #}
              case lastTimePTS of
                Just (_, pts) | skipSeek && pts >= wantPTS
                  -> checkMessages lastTimePTS -- previous frame is still good
                _ -> do
                  (pts, img) <- readFrame wantPTS
                  stackIO $ writeIORef ref $ Just (thisTime, img)
                  checkMessages $ Just (thisTime, pts)

        seeklessDuration = 1 :: Double

        readFrame wantPTS = do
          check_ "av_read_frame" (>= 0) $ av_read_frame ctx packet
          packetIndex <- stackIO $ packet_stream_index packet
          if streamIndex /= packetIndex
            then readFrame wantPTS -- not a packet for the video stream, probably audio
            else do
              check_ "avcodec_send_packet" (>= 0) $ avcodec_send_packet cctx packet
              code <- stackIO $ avcodec_receive_frame cctx frame
              if code < 0
                then readFrame wantPTS -- no image received yet, need more packets
                else do
                  pts <- stackIO $ frame_pts frame
                  if pts >= wantPTS
                    then do
                      img <- convertImage -- image ready to go!
                      return (pts, img)
                    else readFrame wantPTS -- we need to skip some images

        convertImage = do
          check_ "sws_scale" (>= 0) $ join $ sws_scale
            <$> pure sws_ctx
            <*> frame_data frame
            <*> frame_linesize frame
            <*> pure 0
            <*> pure h
            <*> frame_data frameRGBA
            <*> frame_linesize frameRGBA
          p <- stackIO $ frame_data frameRGBA >>= peek
          fptr <- stackIO $ newForeignPtr_ $ (castPtr :: Ptr CUChar -> Ptr Word8) p
          v <- stackIO $ V.freeze $ MV.unsafeFromForeignPtr0 fptr $ fromIntegral $ w * h * 4
          return P.Image
            { P.imageWidth = fromIntegral w
            , P.imageHeight = fromIntegral h
            , P.imageData = v
            }

        checkMessages lastTime = do
          mt <- stackIO $ atomically $ readTChan queue >>= \case
            CloseLoader    -> return Nothing
            RequestFrame t -> checkRestMessages t
          mapM_ (readImage lastTime) mt

        checkRestMessages t = tryReadTChan queue >>= \case
          Nothing                -> return $ Just t
          Just CloseLoader       -> return Nothing
          Just (RequestFrame t') -> checkRestMessages t'

    readImage Nothing 0

  return FrameLoader
    { frameMessage = atomically . writeTChan queue
    , getFrame     = readIORef ref
    }
