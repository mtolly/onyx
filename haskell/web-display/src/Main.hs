{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Main (main) where

import           Control.Applicative              (liftA2)
import           Control.Concurrent.STM
import           Control.Monad                    (void, when, guard)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Aeson                       as A
import qualified Data.ByteString.Lazy             as BL
import           Data.Foldable                    (toList)
import           Data.JSString                    (pack)
import           Data.Maybe                       (isJust)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal                    (fromJSVal)
import           GHCJS.Types
import           JavaScript.Web.AnimationFrame    (waitForAnimationFrame)
import qualified JavaScript.Web.Canvas            as C
import           JavaScript.Web.XMLHttpRequest
import           Linear                           (V2 (..), V4 (..))
import           Linear.Affine                    (Point (..))
import           OnyxiteDisplay.Draw
import           OnyxiteDisplay.Process

import qualified Audio
import           Images

foreign import javascript unsafe
  " location.search.substr(1).split('&').map(function(pair){ \
  \   var tmp = pair.split('=');                             \
  \   return [tmp[0], decodeURIComponent(tmp[1])];           \
  \ })                                                       "
  js_retrieveGET :: IO JSVal

retrieveGET :: IO [(String, String)]
retrieveGET = js_retrieveGET >>= fromJSVal >>= \case
  Just pairs -> return pairs
  Nothing    -> error "retrieveGET: could not read JS value as list of pairs"

foreign import javascript unsafe "$4.addEventListener($1, $2, $3);"
  addEventListener :: JSString -> Callback a -> Bool -> JSVal -> IO ()

newtype DrawCanvas a = DrawCanvas
  { runDrawCanvas :: ReaderT (C.Canvas, C.Context, ImageID -> C.Image) IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadDraw DrawCanvas where
  getDims = do
    (canv, _, _) <- DrawCanvas ask
    liftIO $ liftA2 V2 (canvasWidth canv) (canvasHeight canv)
  setColor (V4 r g b a) = do
    (_, ctx, _) <- DrawCanvas ask
    liftIO $ C.fillStyle (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a / 255) ctx
  fillRect (P (V2 x y)) (V2 w h) = do
    (_, ctx, _) <- DrawCanvas ask
    liftIO $ C.fillRect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) ctx
  drawImage iid (P (V2 x y)) = do
    (_, ctx, getImage) <- DrawCanvas ask
    let img = getImage iid
    w <- liftIO $ imageWidth img
    h <- liftIO $ imageHeight img
    liftIO $ C.drawImage (getImage iid) x y w h ctx

foreign import javascript unsafe "$1.width"
  canvasWidth :: C.Canvas -> IO Int
foreign import javascript unsafe "$1.height"
  canvasHeight :: C.Canvas -> IO Int

foreign import javascript unsafe "$1.width"
  imageWidth :: C.Image -> IO Int
foreign import javascript unsafe "$1.height"
  imageHeight :: C.Image -> IO Int

foreign import javascript unsafe "document.getElementById('the-canvas')"
  theCanvas :: C.Canvas

foreign import javascript unsafe
  " $1.width = window.innerWidth; \
  \ $1.height = window.innerHeight; "
  resizeCanvas :: C.Canvas -> IO ()

foreign import javascript unsafe "$1.clientX" mouseEventX :: JSVal -> IO Int
foreign import javascript unsafe "$1.clientY" mouseEventY :: JSVal -> IO Int

data MouseEvent
  = MouseDown Int Int
  -- | MouseUp   Int Int
  -- | MouseMove Int Int
  deriving (Eq, Ord, Show, Read)

readAll :: TChan a -> STM [a]
readAll c = tryReadTChan c >>= \case
  Just x  -> fmap (x :) $ readAll c
  Nothing -> return []

data Settings = Settings
  { seeGuitar  :: Bool
  , seeBass    :: Bool
  , seeKeys    :: Bool
  , seeProKeys :: Bool
  , seeDrums   :: Bool
  , halfFrames :: Bool
  } deriving (Eq, Ord, Show, Read)

data App
  = Paused
    { pausedSongTime :: Double
    , settings       :: Settings
    }
  | Playing
    { startedPageTime :: Double
    , startedSongTime :: Double
    , settings        :: Settings
    }
  deriving (Eq, Ord, Show)

main :: IO ()
main = do

  clicks <- atomically newTChan

  let addMouseListener s f = do
        cb <- asyncCallback1 $ \v -> do
          x <- mouseEventX v
          y <- mouseEventY v
          atomically $ writeTChan clicks $ f x y
        addEventListener s cb False $ jsval theCanvas
  addMouseListener "mousedown" MouseDown
  -- addMouseListener "mouseup"   MouseUp
  -- addMouseListener "mousemove" MouseMove

  get <- retrieveGET
  let folder = case (lookup "artist" get, lookup "title" get) of
        (Just artist, Just title) -> "songs/" ++ artist ++ "/" ++ title ++ "/gen/plan/album"
        _ -> "."
  resp <- xhrByteString $ Request
    { reqMethod = GET
    , reqURI = pack $ folder ++ "/display.json"
    , reqLogin = Nothing
    , reqHeaders = []
    , reqWithCredentials = False
    , reqData = NoData
    }
  jsonbs <- case contents resp of
    Just bs -> return bs
    Nothing -> error "couldn't get processed json as bytestring"
  Processed mgtr mbass mkeys mdrums mprokeys beat end <- case A.decode $ BL.fromStrict jsonbs of
    Just proc -> return proc
    Nothing -> error "couldn't decode json into processed data"

  ctx <- C.getContext theCanvas
  getImage <- imageGetter
  howl <- Audio.load $ do
    ext <- ["ogg", "mp3"]
    return $ folder ++ "/preview-audio." ++ ext
  audioLen <- Audio.getDuration howl
  let endEvent = if end > 0 then end else audioLen

  let pxToSecs targetY now px = let
        secs = fromIntegral (targetY - px) * 0.003 + realToFrac now :: Double
        in if secs < 0 then 0 else secs
      secsToPx targetY now px = round (negate $ (realToFrac px - realToFrac now) / 0.003 - targetY :: Double)

  let _M, _B :: (Num a) => a
      _M = 20 -- margin
      _B = 41 -- height/width of buttons
      drawFrame :: Double -> App -> IO ()
      drawFrame t state = do
        resizeCanvas theCanvas
        windowW <- canvasWidth theCanvas
        windowH <- canvasHeight theCanvas
        C.fillStyle 54 59 123 1.0 ctx
        C.fillRect 0 0 (fromIntegral windowW) (fromIntegral windowH) ctx
        let targetY :: (Num a) => a
            targetY = fromIntegral windowH - 50
        (\act -> runReaderT (runDrawCanvas act) (theCanvas, ctx, getImage)) $ do
          let Settings{..} = settings state
              tracks :: [(Int, Int -> DrawCanvas ())]
              tracks = concat
                [ if not seeGuitar  then [] else flip map (toList mgtr    ) $ \gtr     -> (182, \x -> drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 x targetY) gtr     beat True)
                , if not seeBass    then [] else flip map (toList mbass   ) $ \bass    -> (182, \x -> drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 x targetY) bass    beat True)
                , if not seeDrums   then [] else flip map (toList mdrums  ) $ \drums   -> (146, \x -> drawDrums   (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 x targetY) drums   beat True)
                , if not seeKeys    then [] else flip map (toList mkeys   ) $ \keys    -> (182, \x -> drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 x targetY) keys    beat True)
                , if not seeProKeys then [] else flip map (toList mprokeys) $ \prokeys -> (282, \x -> drawProKeys (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 x targetY) prokeys beat True)
                ]
              drawTracks :: (Monad m) => Int -> [(Int, Int -> m ())] -> m ()
              drawTracks _ []                   = return ()
              drawTracks x ((w, drawfn) : rest) = do
                drawfn x
                drawTracks (x + w + 20) rest
          drawTracks (_M + _B + _M + _B + _M) tracks
          let buttons = concat
                [ guard (isJust mprokeys) >> [ if seeProKeys then Image_button_prokeys else Image_button_prokeys_off ]
                , guard (isJust mkeys   ) >> [ if seeKeys    then Image_button_keys    else Image_button_keys_off    ]
                , guard (isJust mdrums  ) >> [ if seeDrums   then Image_button_drums   else Image_button_drums_off   ]
                , guard (isJust mbass   ) >> [ if seeBass    then Image_button_bass    else Image_button_bass_off    ]
                , guard (isJust mgtr    ) >> [ if seeGuitar  then Image_button_guitar  else Image_button_guitar_off  ]
                ]
              drawButtons _ []           = return ()
              drawButtons y (iid : iids) = do
                drawImage iid $ P $ V2 (_M + _B + _M) y
                drawButtons (y - _M - _B) iids
          drawButtons (windowH - _M - _B) $ buttons
          let playPause = case state of
                Paused{}  -> Image_button_play
                Playing{} -> Image_button_pause
          drawImage playPause $ P $ V2 _M (windowH - _M - _B - _M - _B)
          drawImage (if halfFrames then Image_button_half_fps else Image_button_half_fps_off) $
            P $ V2 _M (windowH - _M - _B)
          let timelineH = windowH - 4 * _M - 2 * _B - 2
              filled = t / endEvent
          setColor $ V4 0 0 0 255
          fillRect (P $ V2 _M _M) $ V2 _B $ timelineH + 2
          setColor $ V4 255 255 255 255
          fillRect (P $ V2 (_M + 1) (_M + 1)) $ V2 (_B - 2) timelineH
          setColor $ V4 100 130 255 255
          fillRect (P $ V2 (_M + 1) $ _M + 1 + round (fromIntegral timelineH * (1 - filled))) $ V2 (_B - 2) $ round $ fromIntegral timelineH * filled
  let initSettings = Settings True True True True True False
  drawFrame 0 $ Paused 0 initSettings
  _msStart <- waitForAnimationFrame
  aud <- Audio.play howl
  Audio.pause aud howl
  let loop state = do
        when (halfFrames $ settings state) $ void $ waitForAnimationFrame
        ms <- waitForAnimationFrame
        let nowSeconds = case state of
              Paused{..} -> pausedSongTime
              Playing{..} -> startedSongTime + realToFrac (ms / 1000) - startedPageTime
        drawFrame (min nowSeconds endEvent) state
        windowH <- canvasHeight theCanvas
        let handle []       s = loop s
            handle (e : es) s = case e of
              MouseDown x y -> if _M <= x && x <= _M + _B
                then if windowH - 2*_M - 2*_B <= y && y <= windowH - 2*_M - _B
                  then case state of -- play/pause button
                    Paused{..} -> do
                      ms' <- waitForAnimationFrame
                      Audio.setPosSafe pausedSongTime aud howl
                      handle es Playing
                        { startedPageTime = realToFrac $ ms' / 1000
                        , startedSongTime = pausedSongTime
                        , ..
                        }
                    Playing{..} -> do
                      Audio.pause aud howl
                      handle es Paused
                        { pausedSongTime = nowSeconds
                        , ..
                        }
                  else if _M <= y && y <= windowH - 3*_M - 2*_B
                    then let -- progress bar
                      frac = 1 - (fromIntegral y - _M) / (fromIntegral windowH - 4*_M - 2*_B)
                      t = frac * endEvent
                      in case state of
                        Paused{..} -> handle es Paused
                          { pausedSongTime = t
                          , ..
                          }
                        Playing{..} -> do
                          ms' <- waitForAnimationFrame
                          Audio.setPosSafe t aud howl
                          handle es Playing
                            { startedPageTime = realToFrac $ ms' / 1000
                            , startedSongTime = t
                            , ..
                            }
                    else if windowH - _M - _B <= y && y <= windowH - _M
                      then handle es s{ settings = (settings s){ halfFrames = not $ halfFrames $ settings s } }
                      else handle es s
                else if 2*_M + _B <= x && x <= 2*_M + 2*_B
                  then let
                    buttons = concat
                      [ guard (isJust mprokeys) >> [ (\sets -> sets { seeProKeys = not $ seeProKeys sets }) ]
                      , guard (isJust mkeys   ) >> [ (\sets -> sets { seeKeys    = not $ seeKeys    sets }) ]
                      , guard (isJust mdrums  ) >> [ (\sets -> sets { seeDrums   = not $ seeDrums   sets }) ]
                      , guard (isJust mbass   ) >> [ (\sets -> sets { seeBass    = not $ seeBass    sets }) ]
                      , guard (isJust mgtr    ) >> [ (\sets -> sets { seeGuitar  = not $ seeGuitar  sets }) ]
                      ]
                    go s' []                   = handle es s'
                    go s' ((i, action) : rest) = do
                      let ystart = windowH - i * (_M + _B)
                          yend   = ystart + _B
                      if ystart <= y && y <= yend
                        then handle es s{ settings = action $ settings s }
                        else go s' rest
                    in go s $ zip [1..] buttons
                  else handle es s
              -- _ -> handle es s
        atomically (readAll clicks) >>= flip handle state
  loop $ Paused 0 initSettings
