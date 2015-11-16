{-# LANGUAGE LambdaCase #-}
module OnyxiteDisplay.Draw where

import           Control.Monad          (forM_, when)
import           Data.List              (inits, intercalate, sort)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Word              (Word8)
import           Linear                 (V2 (..), V4 (..))
import           Linear.Affine          (Point (..))
import           RockBand.Common        (Key (..))
import qualified RockBand.Drums         as Drums
import qualified RockBand.FiveButton    as Five
import qualified RockBand.ProKeys       as PK
import qualified Sound.MIDI.Util        as U

import           OnyxiteDisplay.Process

data ImageID
  = Image_gem_blackkey
  | Image_gem_blackkey_energy
  | Image_gem_blue
  | Image_gem_blue_cymbal
  | Image_gem_blue_hopo
  | Image_gem_energy
  | Image_gem_energy_cymbal
  | Image_gem_energy_hopo
  | Image_gem_green
  | Image_gem_green_cymbal
  | Image_gem_green_hopo
  | Image_gem_kick
  | Image_gem_kick_energy
  | Image_gem_orange
  | Image_gem_orange_hopo
  | Image_gem_red
  | Image_gem_red_cymbal
  | Image_gem_red_hopo
  | Image_gem_whitekey
  | Image_gem_whitekey_energy
  | Image_gem_yellow
  | Image_gem_yellow_cymbal
  | Image_gem_yellow_hopo
  | Image_highway_drums_bar
  | Image_highway_drums_beat
  | Image_highway_drums_halfbeat
  | Image_highway_drums_solo_edge
  | Image_highway_drums_target
  | Image_highway_grybo_bar
  | Image_highway_grybo_beat
  | Image_highway_grybo_halfbeat
  | Image_highway_grybo_solo_edge
  | Image_highway_grybo_target
  | Image_highway_prokeys_bar
  | Image_highway_prokeys_beat
  | Image_highway_prokeys_halfbeat
  | Image_highway_prokeys_solo_edge
  | Image_highway_prokeys_target
  | Image_sustain_key_end
  | Image_sustain_end
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

class (Monad m) => MonadDraw m where
  getDims   :: m (V2 Int)
  setColor  :: V4 Word8 -> m ()
  fillRect  :: Point V2 Int -> V2 Int -> m ()
  fillRects :: [(Point V2 Int, V2 Int)] -> m ()
  fillRects = mapM_ $ uncurry fillRect
  drawImage :: ImageID -> Point V2 Int -> m ()

drawFive :: (MonadDraw m) => (Int -> U.Seconds) -> (U.Seconds -> Int) -> Point V2 Int -> Five -> Beats -> Bool -> m ()
drawFive pxToSecs secsToPx (P (V2 targetX targetY)) five beats autoplay = do
  V2 _ windowH <- getDims
  let maxSecs = pxToSecs (-100)
      minSecs = pxToSecs $ windowH + 100
      zoom = fst . Map.split maxSecs . snd . Map.split minSecs
  -- Highway
  setColor $ V4 126 126 150 255
  fillRect (P $ V2 targetX 0) (V2 182 windowH)
  setColor $ V4 184 185 204 255
  fillRects $ do
    offsetX <- [0, 36, 72, 108, 144, 180]
    return (P $ V2 (targetX + offsetX) 0, V2 1 windowH)
  setColor $ V4 0 0 0 255
  fillRects $ do
    offsetX <- [1, 37, 73, 109, 145, 181]
    return (P $ V2 (targetX + offsetX) 0, V2 1 windowH)
  -- Solo highway
  setColor $ V4 91 137 185 255
  let soloEdges
        = Map.insert minSecs (fromMaybe False $ fmap snd $ Map.lookupLE minSecs $ fiveSolo five)
        $ Map.insert maxSecs False
        $ zoom $ fiveSolo five
      go []  = return ()
      go [_] = return ()
      go ((s1, b1) : rest@((s2, _) : _)) = do
        let y1 = secsToPx s1
            y2 = secsToPx s2
        when b1 $ fillRects $ do
          offsetX <- [2, 38, 74, 110, 146]
          return (P $ V2 (targetX + offsetX) y2, V2 34 $ y1 - y2)
        go rest
    in go $ Map.toAscList soloEdges
  -- Solo edges
  forM_ (Map.toDescList $ zoom $ fiveSolo five) $ \(secs, _) -> do
    drawImage Image_highway_grybo_solo_edge $ P $ V2 targetX $ secsToPx secs
  -- Beats
  forM_ (Map.toDescList $ zoom beats) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      Bar      -> drawImage Image_highway_grybo_bar      $ P $ V2 targetX (y - 1)
      Beat     -> drawImage Image_highway_grybo_beat     $ P $ V2 targetX (y - 1)
      HalfBeat -> drawImage Image_highway_grybo_halfbeat $ P $ V2 targetX y
  -- Target
  drawImage Image_highway_grybo_target $ P $ V2 targetX $ targetY - 5
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
        drawSustainBlock ystart yend energy = when (not autoplay || ystart < targetY || yend < targetY) $ do
          let ystart' = if autoplay then min ystart targetY else ystart
              yend'   = if autoplay then min yend   targetY else yend
              sustaining = autoplay && (targetY < ystart || targetY < yend)
              (shadeLight, shadeNormal, shadeDark) = if energy
                then             (V4 137 235 204 255, V4 138 192 175 255, V4 124 158 149 255)
                else case color of
                  Five.Green  -> (V4 135 247 126 255, V4  21 218   2 255, V4  13 140   2 255)
                  Five.Red    -> (V4 247 127 158 255, V4 218   2  62 255, V4 140   2  40 255)
                  Five.Yellow -> (V4 247 228 127 255, V4 218 180   2 255, V4 140 115   3 255)
                  Five.Blue   -> (V4 119 189 255 255, V4   2 117 218 255, V4   3  76 140 255)
                  Five.Orange -> (V4 255 183 119 255, V4 218  97   4 255, V4 140  63   3 255)
              h = yend' - ystart' + 1
          setColor $ V4 0 0 0 255
          fillRect (P $ V2 (targetX + offsetX + 14) ystart') (V2 1 h)
          fillRect (P $ V2 (targetX + offsetX + 22) ystart') (V2 1 h)
          setColor shadeLight
          fillRect (P $ V2 (targetX + offsetX + 15) ystart') (V2 1 h)
          setColor shadeNormal
          fillRect (P $ V2 (targetX + offsetX + 16) ystart') (V2 5 h)
          setColor shadeDark
          fillRect (P $ V2 (targetX + offsetX + 21) ystart') (V2 1 h)
          when sustaining $ do
            setColor shadeLight
            fillRect (P $ V2 (targetX + offsetX + 1) (targetY - 4)) $ V2 35 8
        go False ((secsEnd, SustainEnd) : rest) = case Map.lookupLT secsEnd thisColor of
          Just (secsStart, Sustain _) -> do
            drawSustainBlock (secsToPx secsEnd) windowH $ isEnergy secsStart
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
      [] -> case Map.lookupLT (pxToSecs windowH) thisColor of
        -- handle the case where the entire screen is the middle of a sustain
        Just (secsStart, Sustain _) ->
          drawSustainBlock 0 windowH $ isEnergy secsStart
        _ -> return ()
      events -> go False events
  -- Notes
  let nowSecs = pxToSecs targetY
  forM_ colors $ \(color, offsetX, strumImage, hopoImage) -> do
    forM_ (Map.toDescList $ zoom $ fromJust $ Map.lookup color $ fiveNotes five) $ \(secs, evt) -> do
      let futureSecs = toRational secs - toRational nowSecs
      if autoplay && futureSecs <= 0
        then do
          -- note is in the past or being hit now
          if (-0.1) < futureSecs
            then do
              let opacity = round $ ((futureSecs + 0.1) / 0.05) * 255 :: Int
                  opacity' = fromIntegral $ if opacity < 0 then 0 else if opacity > 255 then 255 else opacity
              when (evt /= SustainEnd) $ do
                setColor $ case color of
                  Five.Green  -> V4 190 255 192 opacity'
                  Five.Red    -> V4 255 188 188 opacity'
                  Five.Yellow -> V4 255 244 151 opacity'
                  Five.Blue   -> V4 190 198 255 opacity'
                  Five.Orange -> V4 231 196 112 opacity'
                fillRect (P $ V2 (targetX + offsetX + 1) (targetY - 4)) $ V2 35 8
            else return ()
        else do
          let y = secsToPx secs
              isEnergy = fromMaybe False $ fmap snd $ Map.lookupLE secs $ fiveEnergy five
          case evt of
            SustainEnd    -> drawImage Image_sustain_end                                        $ P $ V2 (targetX + offsetX) y
            Note    Strum -> drawImage (if isEnergy then Image_gem_energy      else strumImage) $ P $ V2 (targetX + offsetX) $ y - 5
            Sustain Strum -> drawImage (if isEnergy then Image_gem_energy      else strumImage) $ P $ V2 (targetX + offsetX) $ y - 5
            Note    HOPO  -> drawImage (if isEnergy then Image_gem_energy_hopo else hopoImage ) $ P $ V2 (targetX + offsetX) $ y - 5
            Sustain HOPO  -> drawImage (if isEnergy then Image_gem_energy_hopo else hopoImage ) $ P $ V2 (targetX + offsetX) $ y - 5

drawDrums :: (MonadDraw m) => (Int -> U.Seconds) -> (U.Seconds -> Int) -> Point V2 Int -> Drums -> Beats -> Bool -> m ()
drawDrums pxToSecs secsToPx (P (V2 targetX targetY)) drums beats autoplay = do
  V2 _ windowH <- getDims
  let maxSecs = pxToSecs (-100)
      minSecs = pxToSecs $ windowH + 100
      zoom    = fst . Map.split maxSecs . snd . Map.split minSecs
  -- Highway
  setColor $ V4 126 126 150 255
  fillRect (P $ V2 targetX 0) (V2 146 windowH)
  setColor $ V4 184 185 204 255
  fillRects $ do
    offsetX <- [0, 36, 72, 108, 144]
    return (P $ V2 (targetX + offsetX) 0, V2 1 windowH)
  setColor $ V4 0 0 0 255
  fillRects $ do
    offsetX <- [1, 37, 73, 109, 145]
    return (P $ V2 (targetX + offsetX) 0, V2 1 windowH)
  -- Solo highway
  setColor $ V4 91 137 185 255
  let soloEdges
        = Map.insert minSecs (fromMaybe False $ fmap snd $ Map.lookupLE minSecs $ drumSolo drums)
        $ Map.insert maxSecs False
        $ zoom $ drumSolo drums
      go []  = return ()
      go [_] = return ()
      go ((s1, b1) : rest@((s2, _) : _)) = do
        let y1 = secsToPx s1
            y2 = secsToPx s2
        when b1 $ fillRects $ do
          offsetX <- [2, 38, 74, 110]
          return (P $ V2 (targetX + offsetX) y2, V2 34 $ y1 - y2)
        go rest
    in go $ Map.toAscList soloEdges
  -- Solo edges
  forM_ (Map.toDescList $ zoom $ drumSolo drums) $ \(secs, _) -> do
    drawImage Image_highway_drums_solo_edge $ P $ V2 targetX $ secsToPx secs
  -- Beats
  forM_ (Map.toDescList $ zoom beats) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      Bar      -> drawImage Image_highway_drums_bar      $ P $ V2 targetX (y - 1)
      Beat     -> drawImage Image_highway_drums_beat     $ P $ V2 targetX (y - 1)
      HalfBeat -> drawImage Image_highway_drums_halfbeat $ P $ V2 targetX y
  -- Target
  drawImage Image_highway_drums_target $ P $ V2 targetX $ targetY - 5
  -- Notes
  let nowSecs = pxToSecs targetY
  forM_ (Map.toDescList $ zoom $ drumNotes drums) $ \(secs, evts) -> do
    let futureSecs = toRational secs - toRational nowSecs
    if autoplay && futureSecs <= 0
      then do
        -- note is in the past or being hit now
        if (-0.1) < futureSecs
          then do
            let opacity = round $ ((futureSecs + 0.1) / 0.05) * 255 :: Int
                opacity' = fromIntegral $ if opacity < 0 then 0 else if opacity > 255 then 255 else opacity
            forM_ (sort evts) $ \case
              Drums.Kick               -> do setColor $ V4 231 196 112 opacity'; fillRect (P $ V2 (targetX + 2  ) (targetY - 5)) $ V2 143 1; fillRect (P $ V2 (targetX + 2  ) (targetY + 4)) $ V2 143 1
              Drums.Red                -> do setColor $ V4 255 188 188 opacity'; fillRect (P $ V2 (targetX + 2  ) (targetY - 4)) $ V2 35  8
              Drums.Pro Drums.Yellow _ -> do setColor $ V4 255 244 151 opacity'; fillRect (P $ V2 (targetX + 38 ) (targetY - 4)) $ V2 35  8
              Drums.Pro Drums.Blue   _ -> do setColor $ V4 190 198 255 opacity'; fillRect (P $ V2 (targetX + 74 ) (targetY - 4)) $ V2 35  8
              Drums.Pro Drums.Green  _ -> do setColor $ V4 190 255 192 opacity'; fillRect (P $ V2 (targetX + 110) (targetY - 4)) $ V2 35  8
          else return ()
      else do
        -- note is in the future
        let y = secsToPx secs
            isEnergy = fromMaybe False $ fmap snd $ Map.lookupLE secs $ drumEnergy drums
        forM_ evts $ \case
          Drums.Kick                          -> drawImage (if isEnergy then Image_gem_kick_energy   else Image_gem_kick         ) $ P $ V2 (targetX + 1  ) (y - 3)
          Drums.Red                           -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_red          ) $ P $ V2 (targetX + 1  ) (y - 5)
          Drums.Pro Drums.Yellow Drums.Tom    -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_yellow       ) $ P $ V2 (targetX + 37 ) (y - 5)
          Drums.Pro Drums.Yellow Drums.Cymbal -> drawImage (if isEnergy then Image_gem_energy_cymbal else Image_gem_yellow_cymbal) $ P $ V2 (targetX + 37 ) (y - 8)
          Drums.Pro Drums.Blue   Drums.Tom    -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_blue         ) $ P $ V2 (targetX + 73 ) (y - 5)
          Drums.Pro Drums.Blue   Drums.Cymbal -> drawImage (if isEnergy then Image_gem_energy_cymbal else Image_gem_blue_cymbal  ) $ P $ V2 (targetX + 73 ) (y - 8)
          Drums.Pro Drums.Green  Drums.Tom    -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_green        ) $ P $ V2 (targetX + 109) (y - 5)
          Drums.Pro Drums.Green  Drums.Cymbal -> drawImage (if isEnergy then Image_gem_energy_cymbal else Image_gem_green_cymbal ) $ P $ V2 (targetX + 109) (y - 8)

data PKHighway
  = RailingLight
  | RailingDark
  | WhiteKey
  | WhiteKeyShort
  | BlackKey
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

pkHighway :: [PKHighway]
pkHighway = let
  rail = [RailingLight, RailingDark]
  w = WhiteKey
  b = BlackKey
  three = [w, b, w, b, WhiteKeyShort]
  four = [w, b, w, b, w, b, WhiteKeyShort]
  in intercalate rail [[], three, four, three, four, [WhiteKeyShort], []]

drawProKeys :: (MonadDraw m) => (Int -> U.Seconds) -> (U.Seconds -> Int) -> Point V2 Int -> ProKeys -> Beats -> Bool -> m ()
drawProKeys pxToSecs secsToPx (P (V2 targetX targetY)) prokeys beats autoplay = do
  V2 _ windowH <- getDims
  let maxSecs = pxToSecs (-100)
      minSecs = pxToSecs $ windowH + 100
      zoom    = fst . Map.split maxSecs . snd . Map.split minSecs
  -- Highway
  let drawHighway _    []               = return ()
      drawHighway xpos (chunk : chunks) = do
        let (color, width) = case chunk of
              RailingLight  -> (V4 184 185 204 255, 1 )
              RailingDark   -> (V4   0   0   0 255, 1 )
              WhiteKey      -> (V4 126 126 150 255, 11)
              WhiteKeyShort -> (V4 126 126 150 255, 10)
              BlackKey      -> (V4 105 105 129 255, 11)
        setColor color
        fillRect (P $ V2 xpos 0) (V2 width windowH)
        drawHighway (xpos + width) chunks
  drawHighway targetX pkHighway
  -- Solo highway
  let soloEdges
        = Map.insert minSecs (fromMaybe False $ fmap snd $ Map.lookupLE minSecs $ proKeysSolo prokeys)
        $ Map.insert maxSecs False
        $ zoom $ proKeysSolo prokeys
      drawSoloHighway _    _  _  []               = return ()
      drawSoloHighway xpos y1 y2 (chunk : chunks) = do
        let (color, width) = case chunk of
              RailingLight  -> (V4 184 185 204   0, 1 )
              RailingDark   -> (V4   0   0   0   0, 1 )
              WhiteKey      -> (V4  91 137 185 255, 11)
              WhiteKeyShort -> (V4  91 137 185 255, 10)
              BlackKey      -> (V4  73 111 149 255, 11)
        setColor color
        fillRect (P $ V2 xpos y1) (V2 width $ y2 - y1)
        drawSoloHighway (xpos + width) y1 y2 chunks
      go []  = return ()
      go [_] = return ()
      go ((s1, b1) : rest@((s2, _) : _)) = do
        when b1 $ drawSoloHighway targetX (secsToPx s1) (secsToPx s2) pkHighway
        go rest
    in go $ Map.toAscList soloEdges
  -- Solo edges
  forM_ (Map.toDescList $ zoom $ proKeysSolo prokeys) $ \(secs, _) -> do
    drawImage Image_highway_prokeys_solo_edge $ P $ V2 targetX $ secsToPx secs
  -- Beats
  forM_ (Map.toDescList $ zoom beats) $ \(secs, evt) -> do
    let y = secsToPx secs
    case evt of
      Bar      -> drawImage Image_highway_prokeys_bar      $ P $ V2 targetX (y - 1)
      Beat     -> drawImage Image_highway_prokeys_beat     $ P $ V2 targetX (y - 1)
      HalfBeat -> drawImage Image_highway_prokeys_halfbeat $ P $ V2 targetX y
  -- Target
  drawImage Image_highway_prokeys_target $ P $ V2 targetX $ targetY - 5
  -- Ranges
  setColor $ V4 0 0 0 $ round (0.3 * 255 :: Double)
  let rangeEdges
        = Map.insert minSecs (fmap snd $ Map.lookupLE minSecs $ proKeysRanges prokeys)
        $ Map.insert maxSecs Nothing
        $ fmap Just $ zoom $ proKeysRanges prokeys
      go []  = return ()
      go [_] = return ()
      go ((s1, rng) : rest@((s2, _) : _)) = do
        case rng of
          Nothing -> return ()
          Just r -> fillRects $ case r of
            PK.RangeC -> [(P $ V2 (targetX + 192) y, V2 90 h)]
            PK.RangeD -> [(P $ V2 (targetX + 2) y, V2 22 h), (P $ V2 (targetX + 203) y, V2 79 h)]
            PK.RangeE -> [(P $ V2 (targetX + 2) y, V2 44 h), (P $ V2 (targetX + 225) y, V2 57 h)]
            PK.RangeF -> [(P $ V2 (targetX + 2) y, V2 56 h), (P $ V2 (targetX + 247) y, V2 35 h)]
            PK.RangeG -> [(P $ V2 (targetX + 2) y, V2 78 h), (P $ V2 (targetX + 270) y, V2 12 h)]
            PK.RangeA -> [(P $ V2 (targetX + 2) y, V2 100 h)]
            where y = secsToPx s1
                  h = secsToPx s2 - y
        go rest
    in go $ Map.toAscList rangeEdges
  -- Sustains
  let pitches = [minBound .. maxBound]
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
        drawSustainBlock ystart yend energy = when (not autoplay || ystart < targetY || yend < targetY) $ do
          let ystart' = if autoplay then min ystart targetY else ystart
              yend'   = if autoplay then min yend   targetY else yend
              sustaining = autoplay && (targetY < ystart || targetY < yend)
              (shadeLight, shadeNormal, shadeDark) = case (energy, black) of
                (True , False) -> (V4 137 235 204 255, V4 138 192 175 255, V4 124 158 149 255)
                (True , True ) -> (V4  52 148 117 255, V4  71 107  95 255, V4  69  83  79 255)
                (False, False) -> (V4 199 134 218 255, V4 184 102 208 255, V4 178  86 204 255)
                (False, True ) -> (V4 175  83 201 255, V4 147  49 175 255, V4 123  42 150 255)
              h = yend' - ystart' + 1
              offsetX' = offsetX + if black then 0 else 1
          setColor $ V4 0 0 0 255
          fillRect (P $ V2 (targetX + offsetX' + 2) ystart') (V2 1 h)
          fillRect (P $ V2 (targetX + offsetX' + 8) ystart') (V2 1 h)
          setColor shadeLight
          fillRect (P $ V2 (targetX + offsetX' + 3) ystart') (V2 1 h)
          setColor shadeNormal
          fillRect (P $ V2 (targetX + offsetX' + 4) ystart') (V2 3 h)
          setColor shadeDark
          fillRect (P $ V2 (targetX + offsetX' + 7) ystart') (V2 1 h)
          when sustaining $ do
            setColor shadeLight
            fillRect (P $ V2 (targetX + offsetX + 1) (targetY - 4)) $ V2 (if black then 9 else 11) 8
        go False ((secsEnd, SustainEnd) : rest) = case Map.lookupLT secsEnd thisPitch of
          Just (secsStart, Sustain _) -> do
            drawSustainBlock (secsToPx secsEnd) windowH $ isEnergy secsStart
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
          drawSustainBlock pxEnd (secsToPx secsStart) $ isEnergy secsStart
          go True rest
        go _ [] = return ()
    case Map.toAscList $ zoom thisPitch of
      [] -> case Map.lookupLT (pxToSecs windowH) thisPitch of
        -- handle the case where the entire screen is the middle of a sustain
        Just (secsStart, Sustain ()) ->
          drawSustainBlock 0 windowH $ isEnergy secsStart
        _ -> return ()
      events -> go False events
  -- Notes
  let nowSecs = pxToSecs targetY
  forM_ pitchList $ \(pitch, offsetX) -> do
    let black = isBlack pitch
    forM_ (Map.toDescList $ zoom $ fromJust $ Map.lookup pitch $ proKeysNotes prokeys) $ \(secs, evt) -> do
      let futureSecs = toRational secs - toRational nowSecs
      if autoplay && futureSecs <= 0
        then do
          -- note is in the past or being hit now
          if (-0.1) < futureSecs
            then do
              let opacity = round $ ((futureSecs + 0.1) / 0.05) * 255 :: Int
                  opacity' = fromIntegral $ if opacity < 0 then 0 else if opacity > 255 then 255 else opacity
              when (evt /= SustainEnd) $ do
                setColor $ V4 227 193 238 opacity'
                fillRect (P $ V2 (targetX + offsetX + 1) (targetY - 4)) $ V2 (if black then 9 else 11) 8
            else return ()
        else do
          let y = secsToPx secs
              isEnergy = fromMaybe False $ fmap snd $ Map.lookupLE secs $ proKeysEnergy prokeys
              img = case (isEnergy, black) of
                (False, False) -> Image_gem_whitekey
                (False, True ) -> Image_gem_blackkey
                (True , False) -> Image_gem_whitekey_energy
                (True , True ) -> Image_gem_blackkey_energy
          case evt of
            SustainEnd -> drawImage Image_sustain_key_end $ P $ V2 (targetX + offsetX - if black then 1 else 0) y
            Note    () -> drawImage img                   $ P $ V2 (targetX + offsetX) $ y - 5
            Sustain () -> drawImage img                   $ P $ V2 (targetX + offsetX) $ y - 5

drawVocals :: (MonadDraw m) => (Int -> U.Seconds) -> (U.Seconds -> Int) -> Point V2 Int -> Int -> Vocals -> m ()
drawVocals pxToSecs secsToPx (P (V2 targetX targetY)) height vox = return ()
