{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
module OnyxEditor.Main (runEditor) where

import           Control.Concurrent               (threadDelay)
import           Control.Exception                (bracket, bracket_)
import           Control.Monad                    (unless, when)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace   (printMessage, runStackTrace)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (listToMaybe)
import qualified Data.Vector.Storable             as V
import           Foreign.C                        (withCString)
import           Linear                           (V2 (..), V4 (..))
import           Linear.Affine                    (Point (..))
import           Numeric.NonNegative.Class        ((-|))
import           RockBand.Common                  (Difficulty (..))
import qualified RockBand.Events                  as Events
import qualified RockBand.File                    as RB
import qualified RockBand.FiveButton              as Five
import           SDL                              (($=))
import qualified SDL
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.Util                  as U

import           OnyxEditor.Images
import           OnyxEditor.SDLBindings

import           OnyxiteDisplay.Draw
import           OnyxiteDisplay.Process

newtype DrawSDL a = DrawSDL
  { runDrawSDL :: ReaderT (SDL.Window, SDL.Renderer, ImageID -> SDL.Texture) IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadDraw DrawSDL where
  getDims = do
    (wind, _, _) <- DrawSDL ask
    fmap fromIntegral <$> SDL.get (SDL.windowSize wind)
  setColor color = do
    (_, rend, _) <- DrawSDL ask
    SDL.rendererDrawColor rend $= color
  fillRect pos dims = do
    (_, rend, _) <- DrawSDL ask
    SDL.fillRect rend $ Just $ fromIntegral <$> SDL.Rectangle pos dims
  fillRects rects = do
    (_, rend, _) <- DrawSDL ask
    SDL.fillRects rend $ V.fromList $ map (fmap fromIntegral . uncurry SDL.Rectangle) rects
  drawImage img pos = do
    (_, rend, getImage) <- DrawSDL ask
    let tex = getImage img
    SDL.TextureInfo{ SDL.textureWidth = w, SDL.textureHeight = h } <- SDL.queryTexture tex
    SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (fromIntegral <$> pos) (V2 w h)

data App
  = Paused
    { pausedSongTime :: U.Seconds
    }
  | Playing
    { startedSDLTime  :: U.Seconds
    , startedSongTime :: U.Seconds
    }
  deriving (Eq, Ord, Show)

runEditor :: FilePath -> FilePath -> IO ()
runEditor midPath oggPath = do

  mid <- Load.fromFile midPath
  song <- case runStackTrace $ RB.readMIDIFile mid of
    (Right song, warns) -> mapM_ printMessage warns >> return song
    (Left errs, _) -> mapM_ printMessage errs >> error "Error when reading MIDI file"
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

  bracket_ (SDL.initialize [SDL.InitTimer, SDL.InitVideo, SDL.InitAudio]) SDL.quit $ do
  bracket (SDL.createWindow "Onyx Editor" SDL.defaultWindow{ SDL.windowInitialSize = V2 1000 600, SDL.windowResizable = True }) SDL.destroyWindow $ \wind -> do
  bracket (SDL.createRenderer wind (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \rend -> do
  withImgInit [IMG_INIT_PNG] $ \_ -> do
  withImages rend $ \getImage -> do
  withMixer [MIX_INIT_OGG] $ do
  withMixerAudio 44100 mixDefaultFormat 2 1024 $ do
  mus <- withCString oggPath mixLoadMUS

  SDL.rendererDrawBlendMode rend $= SDL.BlendAlphaBlend

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
      endEvent = listToMaybe $ do
        RB.Events t <- RB.s_tracks song
        (bts, Events.End) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 t
        return $ U.applyTempoMap (RB.s_tempos song) bts
      drawFrame :: U.Seconds -> IO ()
      drawFrame t = do
        SDL.rendererDrawColor rend $= V4 54 59 123 255
        SDL.clear rend
        V2 _ windowH <- SDL.get $ SDL.windowSize wind
        let targetY :: (Num a) => a
            targetY = fromIntegral windowH - 50
        (\sdl -> runReaderT (runDrawSDL sdl) (wind, rend, getImage)) $ do
          unless gtrNull     $ drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 50  targetY) gtr     beat True
          unless bassNull    $ drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 275 targetY) bass    beat True
          unless drumsNull   $ drawDrums   (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 500 targetY) drums   beat True
          unless keysNull    $ drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 689 targetY) keys    beat True
          unless proKeysNull $ drawProKeys (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 914 targetY) prokeys beat True
        SDL.present rend
  drawFrame 0
  firstSDLTime <- SDL.time
  zero $ mixPlayMusic mus 1
  let loop state = do
        startTicks <- SDL.ticks
        currentSDLTime <- SDL.time
        let currentSongTime = maybe id min endEvent $ case state of
              Paused {..} -> pausedSongTime
              Playing{..} -> startedSongTime + (currentSDLTime - startedSDLTime)
        drawFrame currentSongTime
        let playFrom :: U.Seconds -> IO ()
            playFrom t = do
              err <- mixSetMusicPosition $ realToFrac t
              when (err /= 0) $ do
                zero $ mixPlayMusic mus 1
                zero $ mixSetMusicPosition $ realToFrac t
                -- TODO: if this goes past the end of the audio,
                -- it will play from the beginning.
              mixResumeMusic
            applyEvents s []                   = do
              endTicks <- SDL.ticks
              let took = fromIntegral endTicks - fromIntegral startTicks :: Int
                  -- fps = round (1000 / fromIntegral (max 16 took) :: Double) :: Int
              threadDelay $ (16 - took) * 1000
              loop s
            applyEvents s (SDL.Event _ e : es) = case e of
              SDL.QuitEvent -> return ()
              KeyPress SDL.ScancodeSpace -> case s of
                Paused{..} -> do
                  playFrom currentSongTime
                  applyEvents (Playing{ startedSDLTime = currentSDLTime, startedSongTime = currentSongTime }) es
                Playing{..} -> do
                  mixPauseMusic
                  applyEvents (Paused{ pausedSongTime = currentSongTime }) es
              KeyPress SDL.ScancodeLeft -> case s of
                Paused{..} -> do
                  applyEvents (Paused{ pausedSongTime = pausedSongTime -| 5 }) es
                Playing{..} -> do
                  let currentSongTime' = startedSongTime + (currentSDLTime - startedSDLTime)
                      newSongTime = currentSongTime' -| 5
                  mixPauseMusic
                  playFrom newSongTime
                  applyEvents (Playing{ startedSDLTime = currentSDLTime, startedSongTime = newSongTime }) es
              KeyPress SDL.ScancodeRight -> case s of
                Paused{..} -> do
                  applyEvents (Paused{ pausedSongTime = pausedSongTime + 5 }) es
                Playing{..} -> do
                  let currentSongTime' = startedSongTime + (currentSDLTime - startedSDLTime)
                      newSongTime = currentSongTime' + 5
                  mixPauseMusic
                  playFrom newSongTime
                  applyEvents (Playing{ startedSDLTime = currentSDLTime, startedSongTime = newSongTime }) es
              _ -> applyEvents s es
        SDL.pollEvents >>= applyEvents state
  loop Playing{ startedSDLTime = firstSDLTime, startedSongTime = 0 }

pattern KeyPress scan <- SDL.KeyboardEvent SDL.KeyboardEventData
  { SDL.keyboardEventKeyMotion = SDL.Pressed
  , SDL.keyboardEventRepeat = False
  , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymScancode = scan }
  }
