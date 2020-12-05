{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           CommandLine                    (commandLine)
import           Control.Exception              (displayException)
import           Control.Monad                  (unless, forM_, void, join)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Resource   (runResourceT)
import           Control.Monad.Trans.StackTrace
import           GUI.FLTK                       (launchGUI)
import           System.Environment             (getArgs)
import           System.Exit
import           System.Info                    (os)
import           System.IO                      (hPutStr, hPutStrLn, stderr)
import           System.Process
import RhythmGame.Graphics.Video
import Foreign hiding (void)
import Foreign.C
import qualified Data.ByteString as B

checkShell :: (SendMessage m, MonadIO m) => String -> StackTraceT m ()
checkShell s = liftIO (readCreateProcessWithExitCode (shell s) "") >>= \case
  (ExitSuccess  , _, _) -> return ()
  (ExitFailure _, _, _) -> warn "An external program was not found on your PATH."

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [] -> launchGUI
    ["dump", video, png] -> do
      let checkCode s p f = f >>= \code -> if p code then return code else error $ show (s, code)
      av_log_set_level 56
      ctx <- avformat_alloc_context
      void $ checkCode "avformat_open_input" (== 0) $ with ctx $ \pctx ->
        withCString video $ \pvid ->
          avformat_open_input pctx pvid nullPtr nullPtr
      void $ checkCode "avformat_find_stream_info" (>= 0) $ avformat_find_stream_info ctx nullPtr
      withCString video $ \pvid -> av_dump_format ctx 0 pvid 0
      getStreams ctx >>= \streams -> forM_ (zip [0..] streams) $ \(i, stream) -> do
        print stream
        params <- codecpar stream
        codec_type params >>= \case
          AVMEDIA_TYPE_VIDEO -> do
            -- setup
            codec <- codec_id params >>= avcodec_find_decoder
            w <- cp_width params
            h <- cp_height params
            fmt <- cp_format params
            (num, den) <- time_base stream
            cctx <- avcodec_alloc_context3 codec
            void $ checkCode "avcodec_parameters_to_context" (>= 0) $ avcodec_parameters_to_context cctx params
            void $ checkCode "avcodec_open2 (input)" (>= 0) $ avcodec_open2 cctx codec nullPtr
            frame <- av_frame_alloc
            frameRGBA <- av_frame_alloc
            packet <- av_packet_alloc
            print (cctx, frame, packet)
            -- read a frame
            inData <- frame_data frame
            inLinesize <- frame_linesize frame
            void $ checkCode "av_image_alloc (input)" (>= 0) $ av_image_alloc inData inLinesize w h fmt 16
            void $ checkCode "av_read_frame" (>= 0) $ av_read_frame ctx packet
            void $ checkCode "stream_index" (== i) $ stream_index packet
            void $ checkCode "avcodec_send_packet" (>= 0) $ avcodec_send_packet cctx packet
            void $ checkCode "avcodec_receive_frame" (>= 0) $ avcodec_receive_frame cctx frame
            -- void $ checkCode (>= 0) $ avcodec_decode_video2 cctx frame nullPtr packet
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
              <*> (frame_linesize frame >>= peek)
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
          _ -> return ()
    _  -> do
      res <- logStdout $ do
        case os of
          "mingw32" -> return ()
          _ -> do
            inside "checking if Wine is installed" $ checkShell "wine --version"
        files <- mapStackTraceT (mapQueueLog runResourceT) $ commandLine argv
        unless (null files) $ lg $ unlines $ "Done! Created files:" : files
      case res of
        Right () -> return ()
        Left msgs -> do
          hPutStrLn stderr "ERROR!"
          hPutStr stderr $ displayException msgs
          exitFailure
