{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE TemplateHaskell          #-}
module Main where

import           Control.Concurrent    (threadDelay)
import           Control.Exception     (evaluate)
import           Control.Monad         (forM_)
import qualified Data.ByteString       as B
import           Data.FileEmbed        (embedDir)
import           Data.Maybe            (mapMaybe)
import           Data.Time             (diffUTCTime, getCurrentTime)
import           Foreign
import           Foreign.C
import qualified Graphics.UI.SDL       as SDL
import qualified Graphics.UI.SDL.Image as Image
import qualified RockBand.File         as File
import qualified Sound.MIDI.File.Load  as Load
import qualified Sound.MIDI.Util       as U
import           Control.Monad.Trans.StackTrace
import           System.Environment
import           System.FilePath       ((<.>), (</>))

import           Draw
import           Midi
import           SDLMixer
import           SDLUtil

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

    withSDL [SDL.SDL_INIT_TIMER, SDL.SDL_INIT_VIDEO, SDL.SDL_INIT_AUDIO] $ do
    withSDLImage [Image.InitPNG] $ do
    withWindowAndRenderer "onyxpreview" 640 480 0 $ \_window render -> do
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
              zero $ SDL.setTextureAlphaMod tex $ round $ opacity * 255
              with (SDL.Rect (round x) (round y) (round w) (round h)) $ \rect ->
                zero $ SDL.renderCopy render tex nullPtr rect
              zero $ SDL.setTextureAlphaMod tex 255
            Text _ _ _ -> return ()
          SDL.renderPresent render

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
        readEvent = \case
          SDL.QuitEvent{} -> Just Quit
          KeyPress SDL.SDL_SCANCODE_LEFT -> Just $ Backward 10
          KeyPress SDL.SDL_SCANCODE_RIGHT -> Just $ Forward 10
          KeyPress SDL.SDL_SCANCODE_SPACE -> Just PlayPause
          _ -> Nothing
        getEvents = fmap (mapMaybe readEvent) $ untilNothing pollEvent
    playing start 0
  _ -> do
    prog <- getProgName
    error $ "usage: " ++ prog ++ " .../songs/the-artist/the-song/"
