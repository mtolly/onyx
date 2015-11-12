{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE RecordWildCards          #-}
module Main where

import           Control.Concurrent               (threadDelay)
import           Control.Exception                (bracket, bracket_)
import           Control.Monad                    (forM_, guard, unless, when)
import           Control.Monad.Trans.StackTrace   (printMessage, runStackTrace)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (inits)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromJust, fromMaybe)
import           Foreign.C                        (withCString)
import           Linear                           (V2 (..), V4 (..))
import           Linear.Affine                    (Point (..))
import           Numeric.NonNegative.Class        ((-|))
import qualified RockBand.Beat                    as Beat
import           RockBand.Common                  (Difficulty (..), Key (..))
import qualified RockBand.Drums                   as Drums
import qualified RockBand.File                    as RB
import qualified RockBand.FiveButton              as Five
import qualified RockBand.ProKeys                 as PK
import           SDL                              (($=))
import qualified SDL
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.Util                  as U
import           System.Environment               (getArgs)
import           System.FilePath                  ((</>))
import qualified Data.Vector.Storable as V

import           Images
import           SDLBindings

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

data Sustainable a
  = SustainEnd
  | Note a
  | Sustain a
  deriving (Eq, Ord, Show, Read)

data GuitarNoteType = Strum | HOPO
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Five = Five
  { fiveNotes  :: Map.Map Five.Color (Map.Map U.Seconds (Sustainable GuitarNoteType))
  , fiveSolo   :: Map.Map U.Seconds Bool
  , fiveEnergy :: Map.Map U.Seconds Bool
  } deriving (Eq, Ord, Show)

data Drums = Drums
  { drumNotes  :: Map.Map U.Seconds [Drums.Gem Drums.ProType]
  , drumSolo   :: Map.Map U.Seconds Bool
  , drumEnergy :: Map.Map U.Seconds Bool
  } deriving (Eq, Ord, Show)

data ProKeys = ProKeys
  { proKeysNotes  :: Map.Map PK.Pitch (Map.Map U.Seconds (Sustainable ()))
  , proKeysRanges :: Map.Map U.Seconds PK.LaneRange
  , proKeysSolo   :: Map.Map U.Seconds Bool
  , proKeysEnergy :: Map.Map U.Seconds Bool
  } deriving (Eq, Ord, Show)

removeStubs :: (Ord a) => RTB.T U.Beats (Sustainable a) -> RTB.T U.Beats (Sustainable a)
removeStubs = go . RTB.normalize where
  go rtb = case RTB.viewL rtb of
    Nothing              -> RTB.empty
    Just ((dt, e), rtb') -> case e of
      Note    nt -> RTB.cons dt (Note nt) $ go rtb'
      Sustain nt -> case RTB.viewL rtb' of
        Nothing                         -> RTB.empty
        Just ((dt', SustainEnd), rtb'') -> if dt' <= 1/4
          then RTB.cons dt (Note    nt) $ RTB.delay dt' $ go rtb''
          else RTB.cons dt (Sustain nt) $ RTB.cons  dt' SustainEnd $ go rtb''
        _                               -> error "removeStubs: double note-on"
      SustainEnd -> RTB.delay dt $ go rtb'

trackToMap :: (Ord a) => U.TempoMap -> RTB.T U.Beats a -> Map.Map U.Seconds a
trackToMap tmap = Map.fromList . ATB.toPairList . RTB.toAbsoluteEventList 0 . U.applyTempoTrack tmap . RTB.normalize

processFive :: Maybe U.Beats -> U.TempoMap -> RTB.T U.Beats Five.Event -> Five
processFive hopoThreshold tmap trk = let
  expert = flip RTB.mapMaybe trk $ \case Five.DiffEvent Expert e -> Just e; _ -> Nothing
  assigned = case hopoThreshold of
    Just threshold -> Five.assignHOPO threshold expert
    Nothing -> flip RTB.mapMaybe expert $ \case
      Five.Note True  color -> Just $ Five.Strum   color
      Five.Note False color -> Just $ Five.NoteOff color
      _ -> Nothing
  getColor color = trackToMap tmap $ removeStubs $ flip RTB.mapMaybe assigned $ \case
    Five.NoteOff c -> guard (c == color) >> Just SustainEnd
    Five.Strum   c -> guard (c == color) >> Just (Sustain Strum)
    Five.HOPO    c -> guard (c == color) >> Just (Sustain HOPO )
  notes = Map.fromList $ do
    color <- [minBound .. maxBound]
    return (color, getColor color)
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Five.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Five.Overdrive b -> Just b; _ -> Nothing
  in Five notes solo energy

processDrums :: U.TempoMap -> RTB.T U.Beats Drums.Event -> Drums
processDrums tmap trk = let
  notes = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $
    U.applyTempoTrack tmap $ RTB.collectCoincident $ flip RTB.mapMaybe (Drums.assignToms trk) $ \case
      (Expert, gem) -> Just gem
      _             -> Nothing
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Drums.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Drums.Overdrive b -> Just b; _ -> Nothing
  in Drums notes solo energy

processProKeys :: U.TempoMap -> RTB.T U.Beats PK.Event -> ProKeys
processProKeys tmap trk = let
  notesForPitch p = trackToMap tmap $ removeStubs $ flip RTB.mapMaybe trk $ \case
    PK.Note p' b | p == p' -> Just $ if b then Sustain () else SustainEnd
    _                      -> Nothing
  notes = Map.fromList [ (p, notesForPitch p) | p <- allPitches ]
  allPitches
    =  map PK.RedYellow [minBound .. maxBound]
    ++ map PK.BlueGreen [minBound .. maxBound]
    ++ [PK.OrangeC]
  ranges = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.LaneShift r -> Just r; _ -> Nothing
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.Overdrive b -> Just b; _ -> Nothing
  in ProKeys notes ranges solo energy

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
    -- TODO: add half-beats

drawFive :: (Int -> U.Seconds) -> (U.Seconds -> Int) -> Point V2 Int -> Five -> Beats -> Draw ()
drawFive pxToSecs secsToPx targetP@(P (V2 targetX _)) five beats wind rend getImage = do
  V2 _ windowH <- SDL.get $ SDL.windowSize wind
  let maxSecs = pxToSecs (-100)
      minSecs = pxToSecs $ fromIntegral windowH + 100
      zoom = fst . Map.split maxSecs . snd . Map.split minSecs
  -- Highway
  let x = fromIntegral targetX
  SDL.rendererDrawColor rend $= V4 126 126 150 255
  SDL.fillRect rend $ Just $ SDL.Rectangle (P $ V2 x 0) (V2 182 windowH)
  SDL.rendererDrawColor rend $= V4 184 185 204 255
  SDL.fillRects rend $ V.fromList $ do
    offsetX <- [0, 36, 72, 108, 144, 180]
    return $ SDL.Rectangle (P $ V2 (x + offsetX) 0) (V2 1 windowH)
  SDL.rendererDrawColor rend $= V4 0 0 0 255
  SDL.fillRects rend $ V.fromList $ do
    offsetX <- [1, 37, 73, 109, 145, 181]
    return $ SDL.Rectangle (P $ V2 (x + offsetX) 0) (V2 1 windowH)
  -- Solo highway
  SDL.rendererDrawColor rend $= V4 91 137 185 255
  let soloEdges
        = Map.insert minSecs (fromMaybe False $ fmap snd $ Map.lookupLE minSecs $ fiveSolo five)
        $ Map.insert maxSecs False
        $ zoom $ fiveSolo five
      go []  = return ()
      go [_] = return ()
      go ((s1, b1) : rest@((s2, _) : _)) = do
        let y1 = secsToPx s1
            y2 = secsToPx s2
        when b1 $ SDL.fillRects rend $ V.fromList $ do
          offsetX <- [2, 38, 74, 110, 146]
          return $ SDL.Rectangle (P $ V2 (x + offsetX) $ fromIntegral y2) (V2 34 $ fromIntegral $ y1 - y2)
        go rest
    in do
      -- print soloEdges
      go $ Map.toAscList soloEdges
  -- Solo edges
  let texSoloEdge = getImage Image_highway_grybo_solo_edge
  forM_ (Map.toDescList $ zoom $ fiveSolo five) $ \(secs, _) -> do
    draw1x rend texSoloEdge $ P $ V2 targetX $ secsToPx secs
  -- Beats
  forM_ (Map.toDescList $ zoom beats) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      Bar      -> draw1x rend (getImage Image_highway_grybo_bar     ) $ P $ V2 targetX (y - 1)
      Beat     -> draw1x rend (getImage Image_highway_grybo_beat    ) $ P $ V2 targetX (y - 1)
      HalfBeat -> draw1x rend (getImage Image_highway_grybo_halfbeat) $ P $ V2 targetX y
  -- Target
  draw1x rend (getImage Image_highway_grybo_target) targetP
  -- Sustains
  let colors =
        [ (Five.Green , 1  , Image_gem_green , Image_gem_green_hopo )
        , (Five.Red   , 37 , Image_gem_red   , Image_gem_red_hopo   )
        , (Five.Yellow, 73 , Image_gem_yellow, Image_gem_yellow_hopo)
        , (Five.Blue  , 109, Image_gem_blue  , Image_gem_blue_hopo  )
        , (Five.Orange, 145, Image_gem_orange, Image_gem_orange_hopo)
        ]
  forM_ colors $ \(color, offsetX, _, _) -> do
    let thisColor = fromJust $ Map.lookup color $ fiveNotes five
        isEnergy secs = fromMaybe False $ fmap snd $ Map.lookupLE secs $ fiveEnergy five
        drawSustainBlock ystart yend energy = do
          let (shadeLight, shadeNormal, shadeDark) = if energy
                then             (V4 137 235 204 255, V4 138 192 175 255, V4 124 158 149 255)
                else case color of
                  Five.Green  -> (V4 135 247 126 255, V4  21 218   2 255, V4  13 140   2 255)
                  Five.Red    -> (V4 247 127 158 255, V4 218   2  62 255, V4 140   2  40 255)
                  Five.Yellow -> (V4 247 228 127 255, V4 218 180   2 255, V4 140 115   3 255)
                  Five.Blue   -> (V4 119 189 255 255, V4   2 117 218 255, V4   3  76 140 255)
                  Five.Orange -> (V4 255 183 119 255, V4 218  97   4 255, V4 140  63   3 255)
              h = yend - ystart + 1
          SDL.rendererDrawColor rend $= V4 0 0 0 255
          SDL.fillRect rend $ Just $ fmap fromIntegral $ SDL.Rectangle (P $ V2 (targetX + offsetX + 14) ystart) (V2 1 h)
          SDL.fillRect rend $ Just $ fmap fromIntegral $ SDL.Rectangle (P $ V2 (targetX + offsetX + 22) ystart) (V2 1 h)
          SDL.rendererDrawColor rend $= shadeLight
          SDL.fillRect rend $ Just $ fmap fromIntegral $ SDL.Rectangle (P $ V2 (targetX + offsetX + 15) ystart) (V2 1 h)
          SDL.rendererDrawColor rend $= shadeNormal
          SDL.fillRect rend $ Just $ fmap fromIntegral $ SDL.Rectangle (P $ V2 (targetX + offsetX + 16) ystart) (V2 5 h)
          SDL.rendererDrawColor rend $= shadeDark
          SDL.fillRect rend $ Just $ fmap fromIntegral $ SDL.Rectangle (P $ V2 (targetX + offsetX + 21) ystart) (V2 1 h)
        go False ((secsEnd, SustainEnd) : rest) = case Map.lookupLT secsEnd thisColor of
          Just (secsStart, Sustain _) -> do
            drawSustainBlock (secsToPx secsEnd) (fromIntegral windowH) $ isEnergy secsStart
            go False rest
          _ -> error "during grybo drawing: found a sustain end not preceded by sustain start"
        go True ((_, SustainEnd) : rest) = go False rest
        go False ((_, Note _) : rest) = go False rest
        go True ((_, Note _) : _) = error "during grybo drawing: found a note in middle of a sustain"
        go True ((_, Sustain _) : _) = error "during grybo drawing: found a sustain in middle of a sustain"
        go False ((secsStart, Sustain _) : rest) = do
          let pxEnd = case rest of
                [] -> 0
                (secsEnd, SustainEnd) : _ -> secsToPx secsEnd
                _ -> error "during grybo drawing: found a sustain not followed by sustain end"
          drawSustainBlock pxEnd (secsToPx secsStart) $ isEnergy secsStart
          go True rest
        go _ [] = return ()
    case Map.toAscList $ zoom thisColor of
      [] -> case Map.lookupLT (pxToSecs $ fromIntegral windowH) thisColor of
        -- handle the case where the entire screen is the middle of a sustain
        Just (secsStart, Sustain _) ->
          drawSustainBlock 0 (fromIntegral windowH) $ isEnergy secsStart
        _ -> return ()
      events -> go False events
  -- Notes
  forM_ colors $ \(color, offsetX, strumImage, hopoImage) -> do
    forM_ (Map.toDescList $ zoom $ fromJust $ Map.lookup color $ fiveNotes five) $ \(secs, evt) -> do
      let y = secsToPx secs
          isEnergy = fromMaybe False $ fmap snd $ Map.lookupLE secs $ fiveEnergy five
      case evt of
        SustainEnd    -> draw1x rend (getImage Image_sustain_end                                      ) $ P $ V2 (targetX + offsetX) y
        Note    Strum -> draw1x rend (getImage $ if isEnergy then Image_gem_energy else strumImage    ) $ P $ V2 (targetX + offsetX) $ y - 5
        Sustain Strum -> draw1x rend (getImage $ if isEnergy then Image_gem_energy else strumImage    ) $ P $ V2 (targetX + offsetX) $ y - 5
        Note    HOPO  -> draw1x rend (getImage $ if isEnergy then Image_gem_energy_hopo else hopoImage) $ P $ V2 (targetX + offsetX) $ y - 5
        Sustain HOPO  -> draw1x rend (getImage $ if isEnergy then Image_gem_energy_hopo else hopoImage) $ P $ V2 (targetX + offsetX) $ y - 5

drawDrums :: (Int -> U.Seconds) -> (U.Seconds -> Int) -> Point V2 Int -> Drums -> Beats -> Draw ()
drawDrums pxToSecs secsToPx targetP@(P (V2 targetX _)) drums beats wind rend getImage = do
  V2 _ windowH <- SDL.get $ SDL.windowSize wind
  let maxSecs = pxToSecs (-100)
      minSecs = pxToSecs $ fromIntegral windowH + 100
      zoom    = fst . Map.split maxSecs . snd . Map.split minSecs
  -- Highway
  let x = fromIntegral targetX
  SDL.rendererDrawColor rend $= V4 126 126 150 255
  SDL.fillRect rend $ Just $ SDL.Rectangle (P $ V2 x 0) (V2 146 windowH)
  SDL.rendererDrawColor rend $= V4 184 185 204 255
  SDL.fillRects rend $ V.fromList $ do
    offsetX <- [0, 36, 72, 108, 144]
    return $ SDL.Rectangle (P $ V2 (x + offsetX) 0) (V2 1 windowH)
  SDL.rendererDrawColor rend $= V4 0 0 0 255
  SDL.fillRects rend $ V.fromList $ do
    offsetX <- [1, 37, 73, 109, 145]
    return $ SDL.Rectangle (P $ V2 (x + offsetX) 0) (V2 1 windowH)
  -- Solo highway
  SDL.rendererDrawColor rend $= V4 91 137 185 255
  let soloEdges
        = Map.insert minSecs (fromMaybe False $ fmap snd $ Map.lookupLE minSecs $ drumSolo drums)
        $ Map.insert maxSecs False
        $ zoom $ drumSolo drums
      go []  = return ()
      go [_] = return ()
      go ((s1, b1) : rest@((s2, _) : _)) = do
        let y1 = secsToPx s1
            y2 = secsToPx s2
        when b1 $ SDL.fillRects rend $ V.fromList $ do
          offsetX <- [2, 38, 74, 110]
          return $ SDL.Rectangle (P $ V2 (x + offsetX) $ fromIntegral y2) (V2 34 $ fromIntegral $ y1 - y2)
        go rest
    in do
      -- print soloEdges
      go $ Map.toAscList soloEdges
  -- Solo edges
  let texSoloEdge = getImage Image_highway_drums_solo_edge
  forM_ (Map.toDescList $ zoom $ drumSolo drums) $ \(secs, _) -> do
    draw1x rend texSoloEdge $ P $ V2 targetX $ secsToPx secs
  -- Beats
  forM_ (Map.toDescList $ zoom beats) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      Bar      -> draw1x rend (getImage Image_highway_drums_bar     ) $ P $ V2 targetX (y - 1)
      Beat     -> draw1x rend (getImage Image_highway_drums_beat    ) $ P $ V2 targetX (y - 1)
      HalfBeat -> draw1x rend (getImage Image_highway_drums_halfbeat) $ P $ V2 targetX y
  -- Target
  draw1x rend (getImage Image_highway_drums_target) targetP
  -- Notes
  forM_ (Map.toDescList $ zoom $ drumNotes drums) $ \(secs, evts) -> do
    let y = secsToPx secs
        isEnergy = fromMaybe False $ fmap snd $ Map.lookupLE secs $ drumEnergy drums
    forM_ evts $ \evt -> do
      case evt of
        Drums.Kick                          -> draw1x rend (getImage $ if isEnergy then Image_gem_kick_energy   else Image_gem_kick         ) $ P $ V2 (targetX + 1  ) (y - 3)
        Drums.Red                           -> draw1x rend (getImage $ if isEnergy then Image_gem_energy        else Image_gem_red          ) $ P $ V2 (targetX + 1  ) (y - 5)
        Drums.Pro Drums.Yellow Drums.Tom    -> draw1x rend (getImage $ if isEnergy then Image_gem_energy        else Image_gem_yellow       ) $ P $ V2 (targetX + 37 ) (y - 5)
        Drums.Pro Drums.Yellow Drums.Cymbal -> draw1x rend (getImage $ if isEnergy then Image_gem_energy_cymbal else Image_gem_yellow_cymbal) $ P $ V2 (targetX + 37 ) (y - 8)
        Drums.Pro Drums.Blue   Drums.Tom    -> draw1x rend (getImage $ if isEnergy then Image_gem_energy        else Image_gem_blue         ) $ P $ V2 (targetX + 73 ) (y - 5)
        Drums.Pro Drums.Blue   Drums.Cymbal -> draw1x rend (getImage $ if isEnergy then Image_gem_energy_cymbal else Image_gem_blue_cymbal  ) $ P $ V2 (targetX + 73 ) (y - 8)
        Drums.Pro Drums.Green  Drums.Tom    -> draw1x rend (getImage $ if isEnergy then Image_gem_energy        else Image_gem_green        ) $ P $ V2 (targetX + 109) (y - 5)
        Drums.Pro Drums.Green  Drums.Cymbal -> draw1x rend (getImage $ if isEnergy then Image_gem_energy_cymbal else Image_gem_green_cymbal ) $ P $ V2 (targetX + 109) (y - 8)

drawProKeys :: (Int -> U.Seconds) -> (U.Seconds -> Int) -> Point V2 Int -> ProKeys -> Beats -> Draw ()
drawProKeys pxToSecs secsToPx targetP@(P (V2 targetX _)) prokeys beats wind rend getImage = do
  V2 _ windowH <- SDL.get $ SDL.windowSize wind
  let maxSecs = pxToSecs (-100)
      minSecs = pxToSecs $ fromIntegral windowH + 100
      zoom    = fst . Map.split maxSecs . snd . Map.split minSecs
  -- Highway
  let tex         = getImage Image_highway_prokeys
      texSolo     = getImage Image_highway_prokeys_solo
      texSoloEdge = getImage Image_highway_prokeys_solo_edge
  forM_ [0 .. fromIntegral windowH - 1] $ \y -> let
    isSolo = fromMaybe False $ fmap snd $ Map.lookupLE (pxToSecs y) $ proKeysSolo prokeys
    in draw1x rend (if isSolo then texSolo else tex) $ P $ V2 targetX y
  forM_ (Map.toDescList $ zoom $ proKeysSolo prokeys) $ \(secs, _) -> do
    draw1x rend texSoloEdge $ P $ V2 targetX $ secsToPx secs
  -- Beats
  forM_ (Map.toDescList $ zoom beats) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      Bar      -> draw1x rend (getImage Image_highway_prokeys_bar     ) $ P $ V2 targetX (y - 1)
      Beat     -> draw1x rend (getImage Image_highway_prokeys_beat    ) $ P $ V2 targetX (y - 1)
      HalfBeat -> draw1x rend (getImage Image_highway_prokeys_halfbeat) $ P $ V2 targetX y
  -- Target
  draw1x rend (getImage Image_highway_prokeys_target) targetP
  -- Ranges
  forM_ [0 .. fromIntegral windowH - 1] $ \y -> case Map.lookupLE (pxToSecs y) $ proKeysRanges prokeys of
    Nothing -> return ()
    Just (_, rng) -> do
      let texRange = getImage $ case rng of
            PK.RangeC -> Image_highway_prokeys_crange
            PK.RangeD -> Image_highway_prokeys_drange
            PK.RangeE -> Image_highway_prokeys_erange
            PK.RangeF -> Image_highway_prokeys_frange
            PK.RangeG -> Image_highway_prokeys_grange
            PK.RangeA -> Image_highway_prokeys_arange
      draw1x rend texRange $ P $ V2 targetX y
  -- Sustains
  let pitches = map PK.RedYellow keys ++ map PK.BlueGreen keys ++ [PK.OrangeC]
      keys = [minBound .. maxBound]
      isBlack = \case
        PK.RedYellow k -> isBlackKey k
        PK.BlueGreen k -> isBlackKey k
        PK.OrangeC     -> False
      isBlackKey = \case
        Cs -> True; Ds -> True; Fs -> True; Gs -> True; As -> True; _ -> False
      pitchList = do
        (pitch, lowerPitches) <- zip pitches $ inits pitches
        let offsetX = 1 + sum (map (\p -> if isBlack p then 10 else 12) lowerPitches)
        return (pitch, offsetX)
  forM_ pitchList $ \(pitch, offsetX) -> do
    let thisPitch = fromJust $ Map.lookup pitch $ proKeysNotes prokeys
        black = isBlack pitch
        isEnergy secs = fromMaybe False $ fmap snd $ Map.lookupLE secs $ proKeysEnergy prokeys
        drawSustain energy y = do
          let img = getImage $ case (energy, black) of
                (False, False) -> Image_sustain_whitekey
                (False, True ) -> Image_sustain_blackkey
                (True , False) -> Image_sustain_whitekey_energy
                (True , True ) -> Image_sustain_blackkey_energy
          draw1x rend img $ P $ V2 (targetX + offsetX) y
        go False ((secsEnd, SustainEnd) : rest) = case Map.lookupLT secsEnd thisPitch of
          Just (secsStart, Sustain _) -> do
            forM_ [secsToPx secsEnd .. fromIntegral windowH] $ drawSustain $ isEnergy secsStart
            go False rest
          _ -> error "during prokeys drawing: found a sustain end not preceded by sustain start"
        go True ((_, SustainEnd) : rest) = go False rest
        go False ((_, Note ()) : rest) = go False rest
        go True ((_, Note ()) : _) = error "during prokeys drawing: found a note in middle of a sustain"
        go True ((_, Sustain ()) : _) = error "during prokeys drawing: found a sustain in middle of a sustain"
        go False ((secsStart, Sustain ()) : rest) = do
          let pxEnd = case rest of
                [] -> 0
                (secsEnd, SustainEnd) : _ -> secsToPx secsEnd
                _ -> error "during prokeys drawing: found a sustain not followed by sustain end"
          forM_ [pxEnd .. secsToPx secsStart] $ drawSustain $ isEnergy secsStart
          go True rest
        go _ [] = return ()
    case Map.toAscList $ zoom thisPitch of
      [] -> case Map.lookupLT (pxToSecs $ fromIntegral windowH) thisPitch of
        -- handle the case where the entire screen is the middle of a sustain
        Just (secsStart, Sustain ()) -> forM_ [0 .. fromIntegral windowH] $ drawSustain $ isEnergy secsStart
        _ -> return ()
      events -> go False events
  -- Notes
  forM_ pitchList $ \(pitch, offsetX) -> do
    forM_ (Map.toDescList $ zoom $ fromJust $ Map.lookup pitch $ proKeysNotes prokeys) $ \(secs, evt) -> do
      let y = secsToPx secs
          black = isBlack pitch
          isEnergy = fromMaybe False $ fmap snd $ Map.lookupLE secs $ proKeysEnergy prokeys
          img = getImage $ case (isEnergy, black) of
            (False, False) -> Image_gem_whitekey
            (False, True ) -> Image_gem_blackkey
            (True , False) -> Image_gem_whitekey_energy
            (True , True ) -> Image_gem_blackkey_energy
      case evt of
        SustainEnd -> draw1x rend (getImage Image_sustain_key_end) $ P $ V2 (targetX + offsetX - if black then 1 else 0) y
        -- TODO: why is the above black sustain end hack needed?
        Note    () -> draw1x rend img                              $ P $ V2 (targetX + offsetX) $ y - 5
        Sustain () -> draw1x rend img                              $ P $ V2 (targetX + offsetX) $ y - 5

data App
  = Paused
    { pausedSongTime :: U.Seconds
    }
  | Playing
    { startedSDLTime  :: U.Seconds
    , startedSongTime :: U.Seconds
    }
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  dir <- getArgs >>= \case
    [dir] -> return dir
    _ -> error "Usage: onyxeditor path/to/song/dir/"
  mid <- Load.fromFile $ dir </> "gen/plan/album/2p/notes.mid"
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
  mus <- withCString (dir </> "gen/plan/album/song-countin.ogg") mixLoadMUS

  let draw f = f wind rend getImage

  let pxToSecs targetY now px = let
        secs = fromIntegral (targetY - px) * 0.003 + realToFrac now :: Rational
        in if secs < 0 then 0 else realToFrac secs
      secsToPx targetY now px = round (negate $ (realToFrac px - realToFrac now) / 0.003 - targetY :: Rational)

  let fiveNull five = all Map.null $ Map.elems $ fiveNotes five
      drumsNull = Map.null $ drumNotes drums
      proKeysNull = Map.null $ proKeysNotes prokeys
      drawFrame :: U.Seconds -> IO ()
      drawFrame t = do
        -- frameStart <- SDL.ticks
        SDL.rendererDrawColor rend $= V4 54 59 123 255
        SDL.clear rend
        V2 _ windowH <- SDL.get $ SDL.windowSize wind
        let targetY :: (Num a) => a
            targetY = fromIntegral windowH - 50
        unless (fiveNull gtr ) $ draw $ drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 50  targetY) gtr     beat
        unless (fiveNull bass) $ draw $ drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 275 targetY) bass    beat
        unless drumsNull       $ draw $ drawDrums   (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 500 targetY) drums   beat
        unless (fiveNull keys) $ draw $ drawFive    (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 689 targetY) keys    beat
        unless proKeysNull     $ draw $ drawProKeys (pxToSecs targetY t) (secsToPx targetY t) (P $ V2 914 targetY) prokeys beat
        -- frameEnd <- SDL.ticks
        -- print $ frameEnd - frameStart
        SDL.present rend
  drawFrame 0
  firstSDLTime <- SDL.time
  zero $ mixPlayMusic mus 1
  let loop state = do
        currentSDLTime <- SDL.time
        let currentSongTime = case state of
              Paused {..} -> pausedSongTime
              Playing{..} -> startedSongTime + (currentSDLTime - startedSDLTime)
        drawFrame currentSongTime
        let applyEvents s []                   = threadDelay 1000 >> loop s
            applyEvents s (SDL.Event _ e : es) = case e of
              SDL.QuitEvent -> return ()
              KeyPress SDL.ScancodeSpace -> case s of
                Paused{..} -> do
                  -- TODO: resuming fails if the audio ended, need to call mixPlayMusic instead
                  err <- mixSetMusicPosition $ realToFrac currentSongTime
                  when (err == 0) mixResumeMusic
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
                  err <- mixSetMusicPosition $ realToFrac newSongTime
                  when (err == 0) mixResumeMusic
                  applyEvents (Playing{ startedSDLTime = currentSDLTime, startedSongTime = newSongTime }) es
              KeyPress SDL.ScancodeRight -> case s of
                Paused{..} -> do
                  applyEvents (Paused{ pausedSongTime = pausedSongTime + 5 }) es
                Playing{..} -> do
                  let currentSongTime' = startedSongTime + (currentSDLTime - startedSDLTime)
                      newSongTime = currentSongTime' + 5
                  mixPauseMusic
                  err <- mixSetMusicPosition $ realToFrac newSongTime
                  when (err == 0) mixResumeMusic
                  applyEvents (Playing{ startedSDLTime = currentSDLTime, startedSongTime = newSongTime }) es
              _ -> applyEvents s es
        SDL.pollEvents >>= applyEvents state
  loop Playing{ startedSDLTime = firstSDLTime, startedSongTime = 0 }

pattern KeyPress scan <- SDL.KeyboardEvent SDL.KeyboardEventData
  { SDL.keyboardEventKeyMotion = SDL.Pressed
  , SDL.keyboardEventRepeat = False
  , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymScancode = scan }
  }
