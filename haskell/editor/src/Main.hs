{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TemplateHaskell          #-}
module Main where

import           Control.Concurrent               (threadDelay)
import           Control.Exception                (bracket, bracket_)
import           Control.Monad                    (forM_, guard, when)
import           Control.Monad.Fix                (fix)
import           Control.Monad.Trans.StackTrace   (printMessage, runStackTrace)
import qualified Data.ByteString                  as B
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.FileEmbed                   (embedDir)
import           Data.List                        (stripPrefix)
import qualified Data.Map                         as Map
import           Foreign
import           Foreign.C                        (withCString)
import           Linear                           (V2 (..), V4 (..))
import           Linear.Affine                    (Point (..))
import           RockBand.Common                  (Difficulty (..))
import qualified RockBand.Beat                    as Beat
import qualified RockBand.Drums                   as Drums
import qualified RockBand.File                    as RB
import qualified RockBand.FiveButton              as Five
import           SDL                              (($=))
import qualified SDL
import           SDLBindings
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.Util                  as U
import           System.Environment               (getArgs)
import           System.FilePath                  ((</>))

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

data FiveEvent
  = SustainEnd
  | Strum
  | HOPO
  | StrumSustain
  | HOPOSustain
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Five = Five
  { notesGreen  :: Map.Map U.Seconds FiveEvent
  , notesRed    :: Map.Map U.Seconds FiveEvent
  , notesYellow :: Map.Map U.Seconds FiveEvent
  , notesBlue   :: Map.Map U.Seconds FiveEvent
  , notesOrange :: Map.Map U.Seconds FiveEvent
  } deriving (Eq, Ord, Show)

data Drums = Drums
  { notesDrums :: Map.Map U.Seconds [Drums.Gem Drums.ProType]
  } deriving (Eq, Ord, Show)

processFive :: Bool -> U.TempoMap -> U.Beats -> RTB.T U.Beats Five.Event -> Five
processFive hasHOPOs tmap threshold trk = let
  expert = flip RTB.mapMaybe trk $ \case Five.DiffEvent Expert e -> Just e; _ -> Nothing
  assigned = if hasHOPOs
    then Five.assignHOPO threshold expert
    else flip RTB.mapMaybe expert $ \case
      Five.Note True color -> Just $ Five.Strum color
      Five.Note False color -> Just $ Five.NoteOff color
      _ -> Nothing
  getColor color = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ U.applyTempoTrack tmap $ removeStubs $ RTB.normalize $ flip RTB.mapMaybe assigned $ \case
    Five.NoteOff c -> guard (c == color) >> Just SustainEnd
    Five.Strum   c -> guard (c == color) >> Just StrumSustain
    Five.HOPO    c -> guard (c == color) >> Just HOPOSustain
  removeStubs rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, e), rtb') -> case e of
      Strum -> RTB.cons dt Strum $ removeStubs rtb'
      HOPO -> RTB.cons dt HOPO $ removeStubs rtb'
      StrumSustain -> case RTB.viewL rtb' of
        Nothing -> RTB.empty
        Just ((dt', SustainEnd), rtb'') -> if dt' <= 1/4
          then RTB.cons dt Strum $ RTB.delay dt' $ removeStubs rtb''
          else RTB.cons dt StrumSustain $ RTB.cons dt' SustainEnd $ removeStubs rtb''
        _ -> error "processFive: double note-on"
      HOPOSustain -> case RTB.viewL rtb' of
        Nothing -> RTB.empty
        Just ((dt', SustainEnd), rtb'') -> if dt' <= 1/4
          then RTB.cons dt HOPO $ RTB.delay dt' $ removeStubs rtb''
          else RTB.cons dt HOPOSustain $ RTB.cons dt' SustainEnd $ removeStubs rtb''
        _ -> error "processFive: double note-on"
      SustainEnd -> RTB.delay dt $ removeStubs rtb'
  in Five (getColor Five.Green) (getColor Five.Red) (getColor Five.Yellow) (getColor Five.Blue) (getColor Five.Orange)

processDrums :: U.TempoMap -> RTB.T U.Beats Drums.Event -> Drums
processDrums tmap trk = Drums $ Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $
  U.applyTempoTrack tmap $ RTB.collectCoincident $ flip RTB.mapMaybe (Drums.assignToms trk) $ \case
    (Expert, gem) -> Just gem
    _ -> Nothing

type Beats = Map.Map U.Seconds Beat

data Beat
  = Bar
  | Beat
  | HalfBeat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

processBeat :: U.TempoMap -> RTB.T U.Beats Beat.Event -> Beats
processBeat tmap rtb = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0
  $ U.applyTempoTrack tmap $ flip fmap rtb $ \case
    Beat.Bar -> Bar
    Beat.Beat -> Beat

drawFive :: (Int -> U.Seconds) -> (U.Seconds -> Int) -> Point V2 Int -> Five -> Beats -> Draw ()
drawFive pxToSecs secsToPx targetP@(P (V2 targetX _)) five beats wind rend getImage = do
  V2 _ windowH <- SDL.get $ SDL.windowSize wind
  let maxSecs = pxToSecs (-100)
      minSecs = pxToSecs $ fromIntegral windowH + 100
      zoom = fst . Map.split maxSecs . snd . Map.split minSecs
  -- Highway
  let tex = getImage Image_highway_grybo
  forM_ [0 .. fromIntegral windowH - 1] $ \y -> draw1x rend tex $ P $ V2 targetX y
  -- Beats
  forM_ (Map.toDescList $ zoom beats) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      Bar -> draw1x rend (getImage Image_highway_grybo_bar) $ P $ V2 targetX (y - 1)
      Beat -> draw1x rend (getImage Image_highway_grybo_beat) $ P $ V2 targetX (y - 1)
      HalfBeat -> draw1x rend (getImage Image_highway_grybo_halfbeat) $ P $ V2 targetX y
  -- Target
  draw1x rend (getImage Image_highway_grybo_target) targetP
  -- Sustains
  let sust secs color = let
        colormap = case color of
          Five.Green -> notesGreen five
          Five.Red -> notesRed five
          Five.Yellow -> notesYellow five
          Five.Blue -> notesBlue five
          Five.Orange -> notesOrange five
        in case Map.lookupLE secs colormap of
          Just (_, StrumSustain) -> True
          Just (_, HOPOSustain) -> True
          _ -> False
  forM_ [0 .. fromIntegral windowH - 1] $ \y -> do
    when (sust (pxToSecs y) Five.Green ) $ draw1x rend (getImage Image_sustain_green ) $ P $ V2 (targetX + 1  ) y
    when (sust (pxToSecs y) Five.Red   ) $ draw1x rend (getImage Image_sustain_red   ) $ P $ V2 (targetX + 37 ) y
    when (sust (pxToSecs y) Five.Yellow) $ draw1x rend (getImage Image_sustain_yellow) $ P $ V2 (targetX + 73 ) y
    when (sust (pxToSecs y) Five.Blue  ) $ draw1x rend (getImage Image_sustain_blue  ) $ P $ V2 (targetX + 109) y
    when (sust (pxToSecs y) Five.Orange) $ draw1x rend (getImage Image_sustain_orange) $ P $ V2 (targetX + 145) y
  -- Notes
  forM_ (Map.toDescList $ zoom $ notesGreen five) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      SustainEnd -> draw1x rend (getImage Image_sustain_end) $ P $ V2 (targetX + 1) y
      Strum -> draw1x rend (getImage Image_gem_green) $ P $ V2 (targetX + 1) $ y - 5
      StrumSustain -> draw1x rend (getImage Image_gem_green) $ P $ V2 (targetX + 1) $ y - 5
      HOPO -> draw1x rend (getImage Image_gem_green_hopo) $ P $ V2 (targetX + 1) $ y - 5
      HOPOSustain -> draw1x rend (getImage Image_gem_green_hopo) $ P $ V2 (targetX + 1) $ y - 5
  forM_ (Map.toDescList $ zoom $ notesRed five) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      SustainEnd -> draw1x rend (getImage Image_sustain_end) $ P $ V2 (targetX + 37) y
      Strum -> draw1x rend (getImage Image_gem_red) $ P $ V2 (targetX + 37) $ y - 5
      StrumSustain -> draw1x rend (getImage Image_gem_red) $ P $ V2 (targetX + 37) $ y - 5
      HOPO -> draw1x rend (getImage Image_gem_red_hopo) $ P $ V2 (targetX + 37) $ y - 5
      HOPOSustain -> draw1x rend (getImage Image_gem_red_hopo) $ P $ V2 (targetX + 37) $ y - 5
  forM_ (Map.toDescList $ zoom $ notesYellow five) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      SustainEnd -> draw1x rend (getImage Image_sustain_end) $ P $ V2 (targetX + 73) y
      Strum -> draw1x rend (getImage Image_gem_yellow) $ P $ V2 (targetX + 73) $ y - 5
      StrumSustain -> draw1x rend (getImage Image_gem_yellow) $ P $ V2 (targetX + 73) $ y - 5
      HOPO -> draw1x rend (getImage Image_gem_yellow_hopo) $ P $ V2 (targetX + 73) $ y - 5
      HOPOSustain -> draw1x rend (getImage Image_gem_yellow_hopo) $ P $ V2 (targetX + 73) $ y - 5
  forM_ (Map.toDescList $ zoom $ notesBlue five) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      SustainEnd -> draw1x rend (getImage Image_sustain_end) $ P $ V2 (targetX + 109) y
      Strum -> draw1x rend (getImage Image_gem_blue) $ P $ V2 (targetX + 109) $ y - 5
      StrumSustain -> draw1x rend (getImage Image_gem_blue) $ P $ V2 (targetX + 109) $ y - 5
      HOPO -> draw1x rend (getImage Image_gem_blue_hopo) $ P $ V2 (targetX + 109) $ y - 5
      HOPOSustain -> draw1x rend (getImage Image_gem_blue_hopo) $ P $ V2 (targetX + 109) $ y - 5
  forM_ (Map.toDescList $ zoom $ notesOrange five) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      SustainEnd -> draw1x rend (getImage Image_sustain_end) $ P $ V2 (targetX + 145) y
      Strum -> draw1x rend (getImage Image_gem_orange) $ P $ V2 (targetX + 145) $ y - 5
      StrumSustain -> draw1x rend (getImage Image_gem_orange) $ P $ V2 (targetX + 145) $ y - 5
      HOPO -> draw1x rend (getImage Image_gem_orange_hopo) $ P $ V2 (targetX + 145) $ y - 5
      HOPOSustain -> draw1x rend (getImage Image_gem_orange_hopo) $ P $ V2 (targetX + 145) $ y - 5

drawDrums :: (Int -> U.Seconds) -> (U.Seconds -> Int) -> Point V2 Int -> Drums -> Beats -> Draw ()
drawDrums pxToSecs secsToPx targetP@(P (V2 targetX _)) drums beats wind rend getImage = do
  V2 _ windowH <- SDL.get $ SDL.windowSize wind
  let maxSecs = pxToSecs (-100)
      minSecs = pxToSecs $ fromIntegral windowH + 100
      zoom = fst . Map.split maxSecs . snd . Map.split minSecs
  -- Highway
  let tex = getImage Image_highway_drums
  forM_ [0 .. fromIntegral windowH - 1] $ \y -> draw1x rend tex $ P $ V2 targetX y
  -- Beats
  forM_ (Map.toDescList $ zoom beats) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      Bar -> draw1x rend (getImage Image_highway_drums_bar) $ P $ V2 targetX (y - 1)
      Beat -> draw1x rend (getImage Image_highway_drums_beat) $ P $ V2 targetX (y - 1)
      HalfBeat -> draw1x rend (getImage Image_highway_drums_halfbeat) $ P $ V2 targetX y
  -- Target
  draw1x rend (getImage Image_highway_drums_target) targetP
  -- Notes
  forM_ (Map.toDescList $ zoom $ notesDrums drums) $ \(secs, evts) -> do
    let y = secsToPx secs
    forM_ evts $ \evt -> do
      case evt of
        Drums.Kick -> draw1x rend (getImage Image_gem_kick) $ P $ V2 (targetX + 1) (y - 3)
        Drums.Red -> draw1x rend (getImage Image_gem_red) $ P $ V2 (targetX + 1) (y - 5)
        Drums.Pro Drums.Yellow Drums.Tom -> draw1x rend (getImage Image_gem_yellow) $ P $ V2 (targetX + 37) (y - 5)
        Drums.Pro Drums.Yellow Drums.Cymbal -> draw1x rend (getImage Image_gem_yellow_cymbal) $ P $ V2 (targetX + 37) (y - 5)
        Drums.Pro Drums.Blue Drums.Tom -> draw1x rend (getImage Image_gem_blue) $ P $ V2 (targetX + 73) (y - 5)
        Drums.Pro Drums.Blue Drums.Cymbal -> draw1x rend (getImage Image_gem_blue_cymbal) $ P $ V2 (targetX + 73) (y - 5)
        Drums.Pro Drums.Green Drums.Tom -> draw1x rend (getImage Image_gem_green) $ P $ V2 (targetX + 109) (y - 5)
        Drums.Pro Drums.Green Drums.Cymbal -> draw1x rend (getImage Image_gem_green_cymbal) $ P $ V2 (targetX + 109) (y - 5)

main :: IO ()
main = do
  dir <- getArgs >>= \case
    [dir] -> return dir
    _ -> error "Usage: onyxeditor path/to/song/dir/"
  mid <- Load.fromFile $ dir </> "gen/plan/album/2p/notes.mid"
  song <- case runStackTrace $ RB.readMIDIFile mid of
    (Right song, warns) -> mapM_ printMessage warns >> return song
    (Left errs, _) -> mapM_ printMessage errs >> error "Error when reading MIDI file"
  let gtr = processFive True (RB.s_tempos song) (170 / 480)
        $ foldr RTB.merge RTB.empty [ t | RB.PartGuitar t <- RB.s_tracks song ]
      bass = processFive True (RB.s_tempos song) (170 / 480)
        $ foldr RTB.merge RTB.empty [ t | RB.PartBass t <- RB.s_tracks song ]
      keys = processFive False (RB.s_tempos song) (170 / 480)
        $ foldr RTB.merge RTB.empty [ t | RB.PartKeys t <- RB.s_tracks song ]
      drums = processDrums (RB.s_tempos song)
        $ foldr RTB.merge RTB.empty [ t | RB.PartDrums t <- RB.s_tracks song ]
      beat = processBeat (RB.s_tempos song)
        $ foldr RTB.merge RTB.empty [ t | RB.Beat t <- RB.s_tracks song ]

  bracket_ (SDL.initialize [SDL.InitTimer, SDL.InitVideo, SDL.InitAudio]) SDL.quit $ do
  bracket (SDL.createWindow "Onyx Editor" SDL.defaultWindow{ SDL.windowInitialSize = V2 1000 600 }) SDL.destroyWindow $ \wind -> do
  bracket (SDL.createRenderer wind (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \rend -> do
  withImgInit [IMG_INIT_PNG] $ \_ -> do
  withImages rend $ \getImage -> do
  withMixer [MIX_INIT_OGG] $ do
  withMixerAudio 44100 mixDefaultFormat 2 1024 $ do
  mus <- withCString (dir </> "gen/plan/album/song-countin.ogg") mixLoadMUS

  let draw f = f wind rend getImage

  let pxToSecs now px = let
        secs = fromIntegral (550 - px) * 0.003 + now :: Rational
        in if secs < 0 then 0 else realToFrac secs
      secsToPx now px = round (negate $ (realToFrac px - now) / 0.003 - 550 :: Rational)

  zero $ mixPlayMusic mus 1
  SDL.rendererDrawColor rend $= V4 54 59 123 255
  start <- SDL.time
  let isQuit = \case
        SDL.Event _ SDL.QuitEvent -> True
        _ -> False
  fix $ \loop -> do
    SDL.clear rend
    now <- fmap (subtract start) SDL.time
    draw $ drawFive (pxToSecs now) (secsToPx now) (P $ V2 50 550) gtr beat
    draw $ drawFive (pxToSecs now) (secsToPx now) (P $ V2 275 550) bass beat
    draw $ drawDrums (pxToSecs now) (secsToPx now) (P $ V2 500 550) drums beat
    draw $ drawFive (pxToSecs now) (secsToPx now) (P $ V2 689 550) keys beat
    SDL.present rend
    -- threadDelay 2000
    evts <- SDL.pollEvents
    if any isQuit evts
      then return ()
      else loop
