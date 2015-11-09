{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import SDLBindings
import qualified Data.ByteString as B
import Data.FileEmbed (embedDir)
import Data.List (stripPrefix)
import qualified SDL
import SDL (($=))
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))
import Control.Exception (bracket, bracket_)
import Foreign
import Control.Monad (forM_, when, guard)
import qualified RockBand.File as RB
import qualified RockBand.FiveButton as Five
import RockBand.Common (Difficulty(..))
import qualified Sound.MIDI.Util as U
import Control.Monad.Trans.StackTrace (runStackTrace, printMessage)
import qualified Sound.MIDI.File.Load as Load
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.Map as Map
import Control.Concurrent (threadDelay)
import Control.Monad.Fix (fix)
import Foreign.C (withCString)

data ImageID
  = Image_gem_blackkey_energy_sustain
  | Image_gem_blackkey_energy
  | Image_gem_blackkey
  | Image_gem_blue_cymbal
  | Image_gem_blue_hopo
  | Image_gem_blue
  | Image_gem_energy_cymbal
  | Image_gem_energy_hopo
  | Image_gem_energy
  | Image_gem_green_cymbal
  | Image_gem_green_hopo
  | Image_gem_green
  | Image_gem_kick_energy
  | Image_gem_kick
  | Image_gem_orange_hopo
  | Image_gem_orange
  | Image_gem_red_cymbal
  | Image_gem_red_hopo
  | Image_gem_red
  | Image_gem_whitekey_energy_sustain
  | Image_gem_whitekey_energy
  | Image_gem_whitekey
  | Image_gem_yellow_cymbal
  | Image_gem_yellow_hopo
  | Image_gem_yellow
  | Image_highway_drums_bar
  | Image_highway_drums_beat
  | Image_highway_drums_halfbeat
  | Image_highway_drums_solo_edge
  | Image_highway_drums_solo
  | Image_highway_drums_target
  | Image_highway_drums
  | Image_highway_grybo_bar
  | Image_highway_grybo_beat
  | Image_highway_grybo_halfbeat
  | Image_highway_grybo_solo_edge
  | Image_highway_grybo_solo
  | Image_highway_grybo_target
  | Image_highway_grybo
  | Image_highway_prokeys_arange
  | Image_highway_prokeys_crange
  | Image_highway_prokeys_drange
  | Image_highway_prokeys_erange
  | Image_highway_prokeys_frange
  | Image_highway_prokeys_grange
  | Image_highway_prokeys_solo_arange
  | Image_highway_prokeys_solo_edge_arange
  | Image_highway_prokeys_solo_edge
  | Image_highway_prokeys_solo
  | Image_highway_prokeys_target
  | Image_highway_prokeys
  | Image_sustain_blackkey_end
  | Image_sustain_blackkey
  | Image_sustain_blue
  | Image_sustain_end
  | Image_sustain_energy
  | Image_sustain_green
  | Image_sustain_orange
  | Image_sustain_red
  | Image_sustain_whitekey_end
  | Image_sustain_whitekey
  | Image_sustain_yellow
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

imageFolder :: [(FilePath, B.ByteString)]
imageFolder = $(embedDir "images/")

getImageByteString :: ImageID -> B.ByteString
getImageByteString iid = let
  filename = case stripPrefix "Image_" $ show iid of
    Nothing -> error $ "getImageByteString: couldn't get filename for " ++ show iid
    Just s -> map (\case '_' -> '-'; c -> c) s ++ ".png"
  in case lookup filename imageFolder of
    Just bs -> bs
    Nothing -> error $ "getImageByteString: couldn't find image for " ++ show iid

withImages :: SDL.Renderer -> ((ImageID -> SDL.Texture) -> IO a) -> IO a
withImages rend f = let
  iids = [minBound .. maxBound]
  withTexture surf = bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture
  in withMany (withImageFromByteString . getImageByteString) iids $ \surfs ->
    withMany withTexture surfs $ \texs -> let
      table = zip iids texs
      in f $ \iid -> case lookup iid table of
        Nothing -> error $ "withImages: couldn't find image for " ++ show iid
        Just surf -> surf

type Draw a = SDL.Window -> SDL.Renderer -> (ImageID -> SDL.Texture) -> IO a

draw1x :: SDL.Renderer -> SDL.Texture -> Point V2 Int -> IO ()
draw1x rend tex xy = do
  SDL.TextureInfo{ SDL.textureWidth = w, SDL.textureHeight = h } <- SDL.queryTexture tex
  SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (fromIntegral <$> xy) (V2 w h)

gryboHighway :: Int -> Draw ()
gryboHighway x wind rend getImage = do
  let tex = getImage Image_highway_grybo
  V2 _w h <- SDL.get $ SDL.windowSize wind
  forM_ [0 .. fromIntegral h - 1] $ \y -> draw1x rend tex $ P $ V2 x y

image :: ImageID -> Point V2 Int -> Draw ()
image iid p _wind rend getImage = draw1x rend (getImage iid) p

data Five = Five
  { fiveNotes :: Map.Map U.Seconds [Five.AssignedNote]
  , isSustain :: U.Seconds -> Five.Color -> Bool
  }

processFive :: U.TempoMap -> U.Beats -> RTB.T U.Beats Five.Event -> Five
processFive tmap threshold trk = let
  expert = flip RTB.mapMaybe trk $ \case Five.DiffEvent Expert e -> Just e; _ -> Nothing
  assigned = Five.assignHOPO threshold expert
  assigned' = RTB.normalize $ U.applyTempoTrack tmap assigned
  -- TODO: get rid of non-sustain sustains
  notes = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.collectCoincident assigned'
  getColor color = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ flip RTB.mapMaybe assigned' $ \case
    Five.Strum   c -> guard (c == color) >> Just True
    Five.HOPO    c -> guard (c == color) >> Just True
    Five.NoteOff c -> guard (c == color) >> Just False
  green  = getColor Five.Green
  red    = getColor Five.Red
  yellow = getColor Five.Yellow
  blue   = getColor Five.Blue
  orange = getColor Five.Orange
  getColor' = \case
    Five.Green  -> green
    Five.Red    -> red
    Five.Yellow -> yellow
    Five.Blue   -> blue
    Five.Orange -> orange
  in Five notes $ \secs color -> case Map.lookupLE secs (getColor' color) of
    Nothing     -> False
    Just (_, b) -> b

drawFive :: (Int -> U.Seconds) -> (U.Seconds -> Int) -> Point V2 Int -> Five -> Draw ()
drawFive pxToSecs secsToPx targetP@(P (V2 targetX _)) five wind rend getImage = do
  V2 _ windowH <- SDL.get $ SDL.windowSize wind
  -- Highway
  let tex = getImage Image_highway_grybo
  forM_ [0 .. fromIntegral windowH - 1] $ \y -> draw1x rend tex $ P $ V2 targetX y
  -- Target
  draw1x rend (getImage Image_highway_grybo_target) targetP
  -- Sustains
  let sust = isSustain five
  forM_ [0 .. fromIntegral windowH - 1] $ \y -> do
    when (sust (pxToSecs y) Five.Green ) $ draw1x rend (getImage Image_sustain_green ) $ P $ V2 (targetX + 1  ) y
    when (sust (pxToSecs y) Five.Red   ) $ draw1x rend (getImage Image_sustain_red   ) $ P $ V2 (targetX + 37 ) y
    when (sust (pxToSecs y) Five.Yellow) $ draw1x rend (getImage Image_sustain_yellow) $ P $ V2 (targetX + 73 ) y
    when (sust (pxToSecs y) Five.Blue  ) $ draw1x rend (getImage Image_sustain_blue  ) $ P $ V2 (targetX + 109) y
    when (sust (pxToSecs y) Five.Orange) $ draw1x rend (getImage Image_sustain_orange) $ P $ V2 (targetX + 145) y
  -- Notes
  let maxSecs = pxToSecs (-100)
      minSecs = pxToSecs $ fromIntegral windowH + 100
      zoom = fst $ Map.split maxSecs $ snd $ Map.split minSecs $ fiveNotes five
  forM_ (Map.toDescList zoom) $ \(secs, evts) -> do
    let y = secsToPx secs
    forM_ evts $ \case
      Five.NoteOff Five.Green -> draw1x rend (getImage Image_sustain_end) $ P $ V2 (targetX + 1) y
      Five.NoteOff Five.Red -> draw1x rend (getImage Image_sustain_end) $ P $ V2 (targetX + 37) y
      Five.NoteOff Five.Yellow -> draw1x rend (getImage Image_sustain_end) $ P $ V2 (targetX + 73) y
      Five.NoteOff Five.Blue -> draw1x rend (getImage Image_sustain_end) $ P $ V2 (targetX + 109) y
      Five.NoteOff Five.Orange -> draw1x rend (getImage Image_sustain_end) $ P $ V2 (targetX + 145) y
      Five.Strum Five.Green -> draw1x rend (getImage Image_gem_green) $ P $ V2 (targetX + 1) $ y - 5
      Five.Strum Five.Red -> draw1x rend (getImage Image_gem_red) $ P $ V2 (targetX + 37) $ y - 5
      Five.Strum Five.Yellow -> draw1x rend (getImage Image_gem_yellow) $ P $ V2 (targetX + 73) $ y - 5
      Five.Strum Five.Blue -> draw1x rend (getImage Image_gem_blue) $ P $ V2 (targetX + 109) $ y - 5
      Five.Strum Five.Orange -> draw1x rend (getImage Image_gem_orange) $ P $ V2 (targetX + 145) $ y - 5
      Five.HOPO Five.Green -> draw1x rend (getImage Image_gem_green_hopo) $ P $ V2 (targetX + 1) $ y - 5
      Five.HOPO Five.Red -> draw1x rend (getImage Image_gem_red_hopo) $ P $ V2 (targetX + 37) $ y - 5
      Five.HOPO Five.Yellow -> draw1x rend (getImage Image_gem_yellow_hopo) $ P $ V2 (targetX + 73) $ y - 5
      Five.HOPO Five.Blue -> draw1x rend (getImage Image_gem_blue_hopo) $ P $ V2 (targetX + 109) $ y - 5
      Five.HOPO Five.Orange -> draw1x rend (getImage Image_gem_orange_hopo) $ P $ V2 (targetX + 145) $ y - 5

main :: IO ()
main = do
  dir <- getArgs >>= \case
    [dir] -> return dir
    _ -> error "Usage: onyxeditor path/to/song/dir/"
  mid <- Load.fromFile $ dir </> "gen/plan/album/2p/notes.mid"
  song <- case runStackTrace $ RB.readMIDIFile mid of
    (Right song, warns) -> mapM_ printMessage warns >> return song
    (Left errs, _) -> mapM_ printMessage errs >> error "Error when reading MIDI file"
  let gtr = foldr RTB.merge RTB.empty [ t | RB.PartGuitar t <- RB.s_tracks song ]
      gtr' = processFive (RB.s_tempos song) (170 / 480) gtr
  -- print gtr

  bracket_ (SDL.initialize [SDL.InitTimer, SDL.InitVideo, SDL.InitAudio]) SDL.quit $ do
  bracket (SDL.createWindow "Onyx Editor" SDL.defaultWindow) SDL.destroyWindow $ \wind -> do
  bracket (SDL.createRenderer wind (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \rend -> do
  withImgInit [IMG_INIT_PNG] $ \_ -> do
  withImages rend $ \getImage -> do
  withMixer [MIX_INIT_OGG] $ do
  withMixerAudio 44100 mixDefaultFormat 2 1024 $ do
  mus <- withCString (dir </> "gen/plan/album/song-countin.ogg") mixLoadMUS

  let draw f = f wind rend getImage
  
  let pxToSecs now px = let
        secs = fromIntegral (500 - px) * 0.005 + now :: Rational
        in if secs < 0 then 0 else realToFrac secs
      secsToPx now px = round (negate $ (realToFrac px - now) / 0.005 - 500 :: Rational)

  zero $ mixPlayMusic mus 1
  SDL.rendererDrawColor rend $= V4 54 59 123 255
  start <- SDL.time
  let isQuit = \case
        SDL.Event _ SDL.QuitEvent -> True
        _ -> False
  fix $ \loop -> do
    SDL.clear rend
    now <- fmap (subtract start) SDL.time
    draw $ drawFive (pxToSecs now) (secsToPx now) (P $ V2 100 500) gtr'
    SDL.present rend
    threadDelay 5000
    evts <- SDL.pollEvents
    if any isQuit evts
      then return ()
      else loop
