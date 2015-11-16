{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import OnyxiteDisplay.Draw
import OnyxiteDisplay.Process
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import qualified JavaScript.Web.Canvas as C
import JavaScript.Web.AnimationFrame (waitForAnimationFrame)
import Control.Applicative (liftA2)
import Linear (V2(..), V4(..))
import Linear.Affine (Point (..))
import JavaScript.Web.XMLHttpRequest
import Control.Monad.Trans.StackTrace (runStackTrace)
import qualified RockBand.File                    as RB
import RockBand.Common (Difficulty(..))
import qualified Data.EventList.Absolute.TimeBody as ATB
import           Control.Monad                    (unless)
import qualified Data.Map.Strict                  as Map
import qualified RockBand.Events                  as Events
import qualified RockBand.FiveButton              as Five
import qualified Sound.MIDI.Util                  as U
import           Data.Maybe                       (listToMaybe, fromMaybe)
import qualified Data.EventList.Relative.TimeBody as RTB
import GHCJS.Marshal (fromJSVal, fromJSValUnchecked)
import Data.JSString (pack)
import GHCJS.Types
import GHCJS.Foreign.Callback
import Control.Concurrent.STM

import qualified Audio
import Images
import MIDI

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

foreign import javascript unsafe "$1.x" mouseEventX :: JSVal -> IO Int
foreign import javascript unsafe "$1.y" mouseEventY :: JSVal -> IO Int

foreign import javascript unsafe "prompt($1)"
  js_prompt :: JSString -> IO JSVal

prompt :: String -> IO (Maybe String)
prompt s = js_prompt (pack s) >>= fromJSValUnchecked

data MouseEvent
  = MouseDown Int Int
  | MouseUp   Int Int
  | MouseMove Int Int
  deriving (Eq, Ord, Show, Read)

readAll :: TChan a -> STM [a]
readAll c = tryReadTChan c >>= \case
  Just x  -> fmap (x :) $ readAll c
  Nothing -> return []

data App
  = Paused
    { pausedSongTime :: U.Seconds
    }
  | Playing
    { startedPageTime :: U.Seconds
    , startedSongTime :: U.Seconds
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
  addMouseListener "mouseup"   MouseUp
  addMouseListener "mousemove" MouseMove

  get <- retrieveGET
  let param s = case lookup s get of
        Just x  -> return x
        Nothing -> prompt ("Enter " ++ s) >>= \case
          Just x  -> return x
          Nothing -> error "User canceled required prompt."
  artist <- param "artist"
  title <- param "title"
  resp <- xhrByteString $ Request
    { reqMethod = GET
    , reqURI = pack $ "songs/" ++ artist ++ "/" ++ title ++ "/gen/plan/album/2p/notes.mid"
    , reqLogin = Nothing
    , reqHeaders = []
    , reqWithCredentials = False
    , reqData = NoData
    }
  midbs <- case contents resp of
    Just bs -> return bs
    Nothing -> error "couldn't get MIDI as bytestring"
  mid <- readMIDI midbs
  song <- case runStackTrace $ RB.readMIDIFile mid of
    (Right song, _) -> return song
    (Left _, _) -> error "Error when reading MIDI file"
  let gtr = processFive (Just $ 170 / 480) (RB.s_tempos song)
        $ foldr RTB.merge RTB.empty [ t | RB.PartGuitar t <- RB.s_tracks song ]
      bass = processFive (Just $ 170 / 480) (RB.s_tempos song)
        $ foldr RTB.merge RTB.empty [ t | RB.PartBass t <- RB.s_tracks song ]
      keys = processFive Nothing (RB.s_tempos song)
        $ foldr RTB.merge RTB.empty [ t | RB.PartKeys t <- RB.s_tracks song ]
      drums = processDrums (RB.s_tempos song)
        $ foldr RTB.merge RTB.empty [ t | RB.PartDrums t <- RB.s_tracks song ]
      prokeys = processProKeys (RB.s_tempos song)
        $ foldr RTB.merge RTB.empty [ t | RB.PartRealKeys Expert t <- RB.s_tracks song ]
      beat = processBeat (RB.s_tempos song)
        $ foldr RTB.merge RTB.empty [ t | RB.Beat t <- RB.s_tracks song ]

  ctx <- C.getContext theCanvas
  getImage <- imageGetter
  howl <- Audio.load $ do
    ext <- ["ogg", "mp3"]
    return $ "songs/" ++ artist ++ "/" ++ title ++ "/gen/plan/album/preview-audio." ++ ext
  audioLen <- Audio.getDuration howl

  let pxToSecs targetY now px = let
        secs = fromIntegral (targetY - px) * 0.003 + realToFrac now :: Rational
        in if secs < 0 then 0 else realToFrac secs
      secsToPx targetY now px = round (negate $ (realToFrac px - realToFrac now) / 0.003 - targetY :: Rational)

  let fiveNull      five = all Map.null $ Map.elems $                         fiveNotes five
      fiveOnlyGreen five = all Map.null $ Map.elems $ Map.delete Five.Green $ fiveNotes five
      gtrNull = fiveNull gtr
      bassNull = fiveNull bass
      keysNull = fiveNull keys || fiveOnlyGreen keys
      drumsNull = Map.null $ drumNotes drums
      proKeysNull = all Map.null $ Map.elems $ proKeysNotes prokeys
      endEvent = fromMaybe (realToFrac audioLen) $ listToMaybe $ do
        RB.Events t <- RB.s_tracks song
        (bts, Events.End) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 t
        return $ U.applyTempoMap (RB.s_tempos song) bts
      drawFrame :: U.Seconds -> App -> IO ()
      drawFrame t state = do
        resizeCanvas theCanvas
        windowW <- canvasWidth theCanvas
        windowH <- canvasHeight theCanvas
        C.fillStyle 54 59 123 1.0 ctx
        C.fillRect 0 0 (fromIntegral windowW) (fromIntegral windowH) ctx
        let targetY :: (Num a) => a
            targetY = fromIntegral windowH - 50
        (\act -> runReaderT (runDrawCanvas act) (theCanvas, ctx, getImage)) $ do
          unless gtrNull     $ drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 100 targetY) gtr     beat True
          unless bassNull    $ drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 325 targetY) bass    beat True
          unless drumsNull   $ drawDrums   (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 550 targetY) drums   beat True
          unless keysNull    $ drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 739 targetY) keys    beat True
          unless proKeysNull $ drawProKeys (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 964 targetY) prokeys beat True
          setColor $ V4 0 0 0 255
          fillRect (P $ V2 20 $ windowH - 70) $ V2 50 50
          fillRect (P $ V2 20 20) $ V2 50 $ windowH - 110
          setColor $ V4 255 255 255 255
          fillRect (P $ V2 23 $ windowH - 67) $ V2 44 44
          let timelineH = windowH - 116
          fillRect (P $ V2 23 23) $ V2 44 timelineH
          setColor $ V4 0 0 0 255
          case state of
            Paused{}  -> liftIO $ do
              C.beginPath ctx
              C.moveTo 30 (fromIntegral windowH - 60) ctx
              C.lineTo 30 (fromIntegral windowH - 30) ctx
              C.lineTo 60 (fromIntegral windowH - 45) ctx
              C.closePath ctx
              C.fill ctx
            Playing{} -> do
              fillRect (P $ V2 30 $ windowH - 60) $ V2 11 30
              fillRect (P $ V2 49 $ windowH - 60) $ V2 11 30
          let filled = realToFrac $ t / endEvent :: Rational
          setColor $ V4 100 130 255 255
          fillRect (P $ V2 23 $ 23 + round (fromIntegral timelineH * (1 - filled))) $ V2 44 $ round $ fromIntegral timelineH * filled
  drawFrame 0 (Paused 0)
  msStart <- waitForAnimationFrame
  aud <- Audio.play howl
  let loop state = do
        ms <- waitForAnimationFrame
        let nowSeconds = case state of
              Paused{..} -> pausedSongTime
              Playing{..} -> startedSongTime + realToFrac (ms / 1000) - startedPageTime
        drawFrame (min nowSeconds endEvent) state
        windowH <- canvasHeight theCanvas
        let handle s []       = loop s
            handle s (e : es) = case e of
              MouseDown x y -> if 20 <= x && x <= 70
                then if windowH - 70 <= y && y <= windowH - 20
                  then case state of
                    Paused{..} -> do
                      ms' <- waitForAnimationFrame
                      Audio.setPosSafe pausedSongTime aud howl
                      handle (Playing (realToFrac $ ms' / 1000) pausedSongTime) es
                    Playing{..} -> do
                      Audio.pause aud howl
                      handle (Paused nowSeconds) es
                  else if 20 <= y && y <= windowH - 90
                    then let
                      frac = 1 - (fromIntegral y - 20) / (fromIntegral windowH - 110) :: Rational
                      t = realToFrac frac * endEvent
                      in case state of
                        Paused{..} -> handle (Paused t) es
                        Playing{..} -> do
                          ms' <- waitForAnimationFrame
                          Audio.setPosSafe t aud howl
                          handle (Playing (realToFrac $ ms' / 1000) t) es
                    else handle s es
                else handle s es
              _ -> handle s es
        atomically (readAll clicks) >>= handle state
  loop $ Playing (realToFrac $ msStart / 1000) 0
