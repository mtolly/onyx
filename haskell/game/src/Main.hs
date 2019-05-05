{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Build                            (loadYaml)
import           Config
import           Control.Arrow                    (first)
import           Control.Concurrent               (threadDelay)
import           Control.Exception                (bracket, bracket_, throwIO)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Resource     (runResourceT)
import           Control.Monad.Trans.StackTrace
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import           Graphics.GL.Core33
import           Import                           (importSTFS)
import qualified RhythmGame.Audio                 as RGAudio
import qualified RhythmGame.Drums                 as RGDrums
import           RockBand.Codec.Drums
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Common                  (Difficulty (..))
import           Scripts                          (loadMIDI)
import           SDL                              (($=))
import qualified SDL
import qualified Sound.MIDI.Util                  as U
import           System.Environment               (getArgs)
import           System.FilePath                  ((</>))

main :: IO ()
main = getArgs >>= \case
  [con] -> do
    res <- runResourceT $ logStdout $ tempDir "onyx_game" $ \dir -> do
      _ <- importSTFS con Nothing dir
      song <- loadMIDI $ dir </> "notes.mid"
      let tempos = RBFile.s_tempos song
          drums = RBFile.fixedPartDrums $ RBFile.s_tracks song
          drums'
            = Map.fromList
            $ map (first $ realToFrac . U.applyTempoMap tempos)
            $ ATB.toPairList
            $ RTB.toAbsoluteEventList 0
            $ RTB.collectCoincident
            $ fmap RGDrums.Upcoming
            $ drumGems
            $ fromMaybe mempty
            $ Map.lookup Expert
            $ drumDifficulties drums
      yml <- loadYaml $ dir </> "song.yml"
      (pans, vols) <- case HM.toList $ _plans yml of
        [(_, MoggPlan{..})] -> return (map realToFrac _pans, map realToFrac _vols)
        _                   -> fatal "Couldn't find pans and vols after importing STFS"
      let trk = RGDrums.Track drums' Map.empty 0 0.2
      liftIO $ bracket_ SDL.initializeAll SDL.quit $ do
        let windowConf = SDL.defaultWindow
              { SDL.windowResizable = True
              , SDL.windowHighDPI = False
              , SDL.windowInitialSize = SDL.V2 800 600
              , SDL.windowOpenGL = Just SDL.defaultOpenGL
                { SDL.glProfile = SDL.Core SDL.Normal 3 3
                }
              }
        bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
          SDL.windowMinimumSize window $= SDL.V2 800 600
          bracket (SDL.glCreateContext window) (\ctx -> glFinish >> SDL.glDeleteContext ctx) $ \_ctx -> do
            threadDelay 1000000 -- this prevents a weird crash, see https://github.com/haskell-game/sdl2/issues/176
            RGAudio.playMOGG pans vols (dir </> "audio.mogg") $ do
              RGDrums.playDrums window trk
    case res of
      Left err -> throwIO err
      Right () -> return ()
  _ -> error "Usage: onyx-game song_rb3con"
