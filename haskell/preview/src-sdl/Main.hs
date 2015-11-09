{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE TemplateHaskell          #-}
module Main where

import           Control.Concurrent             (threadDelay)
import           Control.Exception              (bracket, bracket_, evaluate)
import           Control.Monad                  (forM_)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                as B
import           Data.FileEmbed                 (embedDir)
import           Data.Maybe                     (mapMaybe)
import           Data.Time                      (diffUTCTime, getCurrentTime)
import           Foreign
import           Foreign.C
import           Linear                         (V2 (..))
import           Linear.Affine                  (Point (..))
import qualified RockBand.File                  as File
import           SDL                            (($=))
import qualified SDL                            as SDL
import qualified Sound.MIDI.File.Load           as Load
import qualified Sound.MIDI.Util                as U
import           System.Environment
import           System.FilePath                ((<.>), (</>))

import           Draw
import           Midi
import           SDLImage
import           SDLMixer

data Event
  = PlayPause
  | Forward U.Seconds
  | Backward U.Seconds
  | Quit
  deriving (Eq, Ord, Show)

imageFiles :: [(ImageID, B.ByteString)]
imageFiles = let
  dir = $(embedDir "www/rbprev")
  filename iid = drop 6 (show iid) <.> "png"
  in do
    iid <- [minBound .. maxBound]
    case lookup (filename iid) dir of
      Nothing -> error $ "Main.imageFiles: panic! couldn't find " ++ show iid
      Just bytes -> return (iid, bytes)

main :: IO ()
main = getArgs >>= \case
  ["--list-options"] -> return () -- be quiet fish
  [dir] -> do

    preview <- Load.fromFile (dir </> "gen/plan/album/2p/notes.mid")
      >>= printStackTraceIO . File.readMIDIFile
      >>= evaluate . buildPreview

    bracket_ (SDL.initialize [SDL.InitTimer, SDL.InitVideo, SDL.InitAudio]) SDL.quit $ do
    withImgInit [IMG_INIT_PNG] $ \_ -> do
    bracket (SDL.createWindow "onyxpreview" SDL.defaultWindow{ SDL.windowInitialSize = V2 640 480 }) SDL.destroyWindow $ \window -> do
    bracket (SDL.createRenderer window (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \render -> do
    withMixer [MIX_INIT_OGG] $ do
    withMixerAudio 44100 mixDefaultFormat 2 1024 $ do

    withMany (withImageBinary render . snd) imageFiles $ \texs -> do
    let imageTable = zip (map fst imageFiles) texs
        image iid = case lookup iid imageTable of
          Nothing  -> error $ "panic! no texture found for " ++ show iid
          Just tex -> tex
        performDraw t = do
          forM_ (draw t preview) $ \case
            DrawImage iid (x, y, w, h) opacity -> do
              let tex = image iid
              SDL.textureAlphaMod tex $= round (opacity * 255)
              SDL.copy render tex Nothing $ Just $
                SDL.Rectangle (P $ V2 (round x) (round y)) (V2 (round w) (round h))
              SDL.textureAlphaMod tex $= 255
            Text _ _ _ -> return ()
          SDL.present render

    mus <- withCString (dir </> "gen/plan/album/song-countin.ogg") mixLoadMUS

    start <- getCurrentTime
    zero $ mixPlayMusic mus 1
    let playing startUTC startSecs = do
          nowUTC <- getCurrentTime
          let nowSecs = realToFrac (diffUTCTime nowUTC startUTC) + startSecs
          performDraw nowSecs
          threadDelay 10000
          evts <- getEvents
          let seekForward  = sum [ s | Forward  s <- evts ]
              seekBackward = sum [ s | Backward s <- evts ]
          if  | any (== Quit) evts -> return ()
              | any (== PlayPause) evts -> do
                mixPauseMusic
                paused nowSecs
              | seekForward /= 0 || seekBackward /= 0 -> do
                let newSecs = nowSecs + seekForward - seekBackward
                zero $ mixSetMusicPosition $ realToFrac newSecs
                playing nowUTC newSecs
              | otherwise -> playing startUTC startSecs
        paused nowSecs = do
          performDraw nowSecs
          threadDelay 10000
          evts <- getEvents
          let seekForward  = sum [ s | Forward  s <- evts ]
              seekBackward = sum [ s | Backward s <- evts ]
          if  | any (== Quit) evts -> return ()
              | any (== PlayPause) evts -> do
                zero $ mixSetMusicPosition $ realToFrac nowSecs
                mixResumeMusic
                startUTC <- getCurrentTime
                playing startUTC nowSecs
              | otherwise -> paused $ nowSecs + seekForward - seekBackward
        readEvent (SDL.Event _ pay) = case pay of
          SDL.QuitEvent -> Just Quit
          KeyPress SDL.ScancodeLeft -> Just $ Backward 10
          KeyPress SDL.ScancodeRight -> Just $ Forward 10
          KeyPress SDL.ScancodeSpace -> Just PlayPause
          _ -> Nothing
        getEvents = fmap (mapMaybe readEvent) SDL.pollEvents
    playing start 0
  _ -> do
    prog <- getProgName
    error $ "usage: " ++ prog ++ " .../songs/the-artist/the-song/"

pattern KeyPress scan <- SDL.KeyboardEvent SDL.KeyboardEventData
  { SDL.keyboardEventKeyMotion = SDL.Pressed
  , SDL.keyboardEventRepeat = False
  , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymScancode = scan }
  }
