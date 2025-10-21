{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Onyx.Game.Graphics.Video
( forkFrameLoader
, FrameLoader(..)
, FrameMessage(..)
) where

import qualified Codec.Picture                as P
import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Monad                (filterM, join, unless, void,
                                               when)
import           Control.Monad.Trans.Resource
import           Data.Fixed                   (mod')
import           Data.IORef
import           Data.Maybe                   (fromMaybe)
import           Data.StateVar                (($=))
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Foreign                      hiding (void)
import           Foreign.C
import           Onyx.FFMPEG
import           Onyx.Project                 (VideoInfo (..))
import           Onyx.StackTrace

data FrameLoader = FrameLoader
  { frameMessage :: FrameMessage -> IO ()
  , getFrame     :: IO (Maybe (Double, P.Image P.PixelRGBA8))
  }

data FrameMessage
  = RequestFrame Double
  | CloseLoader

forkFrameLoader :: ((MessageLevel, Message) -> IO ()) -> VideoInfo FilePath -> IO FrameLoader
forkFrameLoader logger vi = do
  let videoPath = vi.fileVideo
  queue <- newTChanIO
  ref <- newIORef Nothing
  let logResult f = f >>= \case
        Right ()              -> return ()
        Left  (Messages msgs) -> mapM_ (\m -> logger (MessageWarning, m)) msgs
  void $ forkIO $ logResult $ runResourceT $ logIO logger $ inside ("Playing video: " <> videoPath) do
    let res a f = snd <$> allocate a f
        check_ s p a = inside s do
          code <- stackIO a
          unless (p code) $ fatal $ "Return code: " <> show code
        checkRes s p a f = inside s do
          let f' code = when (p code) f
          code <- snd <$> allocate a f'
          unless (p code) $ fatal $ "Return code: " <> show code
    -- stackIO $ av_log_set_level 56 -- debug info
    -- setup
    ctx <- res avformat_alloc_context (const $ return ())
    -- don't need avformat_free_context, because avformat_close_input does that.
    -- but maybe we should fix so if avformat_open_input fails,
    -- the context is still freed?
    checkRes "avformat_open_input" (== 0)
      (with ctx \pctx ->
        withCString videoPath \pvid ->
          avformat_open_input pctx pvid nullPtr nullPtr)
      (with ctx avformat_close_input)
    check_ "avformat_find_stream_info" (>= 0) $ avformat_find_stream_info ctx nullPtr
    stackIO $ withCString videoPath \pvid -> av_dump_format ctx 0 pvid 0
    videoStreams <- stackIO $ getStreams ctx >>= filterM (\stream -> do
      params <- stream.codecpar
      (AVMEDIA_TYPE_VIDEO ==) <$> params.codec_type)
    stream <- case videoStreams of
      [s] -> return s
      s : _ : _ -> do
        warn "Multiple video streams; picking the first one"
        return s
      [] -> fatal "No video streams found"
    streamIndex <- stackIO stream.index
    params <- stackIO stream.codecpar
    codec <- stackIO $ params.codec_id >>= avcodec_find_decoder
    w <- stackIO params.width
    h <- stackIO params.height
    (num, den) <- stackIO stream.time_base
    let resolution = realToFrac num / realToFrac den :: Double
    duration <- stackIO $ durationSeconds ctx
    -- this is usually 0, but have seen a small offset in .mpg files
    startOffset <- stackIO $ fromMaybe 0 <$> startTimeSeconds ctx
    let videoStart = maybe 0 realToFrac vi.videoStartTime
        videoEnd = maybe duration realToFrac vi.videoEndTime
        playLength = videoEnd - videoStart
    cctx <- res (avcodec_alloc_context3 codec) \c -> with c avcodec_free_context
    check_ "avcodec_parameters_to_context" (>= 0) $ avcodec_parameters_to_context cctx params
    check_ "avcodec_open2 (input)" (>= 0) $ avcodec_open2 cctx codec nullPtr
    frame <- res av_frame_alloc \f -> with f av_frame_free
    frameRGBA <- res av_frame_alloc \f -> with f av_frame_free
    packet <- res av_packet_alloc \p -> with p av_packet_free
    pfmt <- stackIO cctx.pix_fmt
    outData <- stackIO frameRGBA.extended_data
    outLinesize <- stackIO frameRGBA.linesize
    -- buffer size alignment of 4 means no padding between the rows of 4-byte pixels,
    -- so we can just copy it exactly into a JuicyPixels image later
    checkRes "av_image_alloc (output)" (>= 0)
      (av_image_alloc outData outLinesize w h AV_PIX_FMT_RGBA 4)
      (av_freep outData)
    stackIO $ frameRGBA.width $= w
    stackIO $ frameRGBA.height $= h
    stackIO $ frameRGBA.format $= AV_PIX_FMT_RGBA
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
                  then if vi.videoLoop
                    then Just $ videoStart + mod' thisTimeMIDI playLength + startOffset
                    else Nothing -- after end of video
                  else Just $ thisTimeNoLoop + startOffset
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
                avseek_FLAG_BACKWARD -- I think this means "seek to the keyframe at or before the requested time"
              case lastTimePTS of
                Just (_, pts) | skipSeek && pts >= wantPTS
                  -> checkMessages lastTimePTS -- previous frame is still good
                _ -> readFrame wantPTS >>= \case
                  Nothing -> do
                    stackIO $ writeIORef ref Nothing
                    checkMessages Nothing
                  Just (pts, img) -> do
                    stackIO $ writeIORef ref $ Just (thisTime, img)
                    checkMessages $ Just (thisTime, pts)

        seeklessDuration = 1 :: Double

        readFrame wantPTS = do
          codePacket <- stackIO $ av_read_frame ctx packet
          if codePacket < 0
            then return Nothing -- probably reached end of file
            else do
              packetIndex <- stackIO packet.stream_index
              if streamIndex /= packetIndex
                then do
                  -- not a packet for the video stream, probably audio
                  stackIO $ av_packet_unref packet
                  readFrame wantPTS
                else do
                  check_ "avcodec_send_packet" (>= 0) $ avcodec_send_packet cctx packet
                  stackIO $ av_packet_unref packet
                  codeFrame <- stackIO $ avcodec_receive_frame cctx frame
                  if codeFrame < 0
                    then readFrame wantPTS -- no image received yet, need more packets
                    else do
                      pts <- stackIO frame.pts
                      if pts >= wantPTS
                        then do
                          img <- convertImage -- image ready to go!
                          return $ Just (pts, img)
                        else readFrame wantPTS -- we need to skip some images

        convertImage = do
          check_ "sws_scale" (>= 0) $ join $ sws_scale
            <$> pure sws_ctx
            <*> frame.extended_data
            <*> frame.linesize
            <*> pure 0
            <*> pure h
            <*> frameRGBA.extended_data
            <*> frameRGBA.linesize
          p <- stackIO $ frameRGBA.extended_data >>= peek
          fptr <- stackIO $ newForeignPtr_ p
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
