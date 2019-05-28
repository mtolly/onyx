module Draw.Protar (drawProtar, eachChordsWidth) where

import           Prelude

import           Data.Array              (cons, index, range, snoc, unsnoc, uncons, take)
import           Data.Foldable           (for_)
import           Data.Int                (ceil, round, toNumber)
import           Data.List               as L
import           Data.Maybe              (Maybe (..), fromMaybe, isNothing)
import           Data.Time.Duration      (Seconds, negateDuration)
import           Data.Traversable        (for, maximum, sum)
import           Data.Tuple              (Tuple (..), fst, snd)
import           Effect                  (Effect)
import           Graphics.Canvas         as C

import           Draw.Common             (Draw, drawImage, drawLane, fillRect,
                                          secToNum, setFillStyle, drawBeats, BadgeInfo, drawBadgeVertical)
import           Images
import           OnyxMap                 as Map
import           Song                    (ChordLine (..), Flex (..),
                                          GuitarNoteType (..), Protar (..),
                                          ProtarNote (..), Song (..),
                                          Sustainable (..))
import           Style                   (customize)

getChordsWidth
  :: C.Context2D -> Protar -> Effect Protar
getChordsWidth ctx (Protar pg) = do
  widths <- for (Map.values pg.chords) $ \x -> let
    f xs = map sum $ for xs \(Tuple line str) -> do
      case line of
        Baseline    -> C.setFont ctx customize.proChordNameFont
        Superscript -> C.setFont ctx customize.proChordNameFontSuperscript
      metrics <- C.measureText ctx str
      pure metrics.width
    in case x of
      SustainEnd -> pure 0.0
      Sustain ps -> f ps
      Note    ps -> f ps
  pure $ Protar pg { chordsWidth = ceil $ fromMaybe 0.0 $ maximum widths }

eachChordsWidth
  :: C.Context2D -> Song -> Effect Song
eachChordsWidth ctx (Song o) = do
  parts <- for o.parts \(Tuple s f@(Flex flex)) -> case flex.protar of
    Nothing -> pure $ Tuple s f
    Just pg -> do
      pg' <- for pg \(Tuple d pgd) -> do
        pgd' <- getChordsWidth ctx pgd
        pure $ Tuple d pgd'
      pure $ Tuple s $ Flex flex { protar = Just pg' }
  pure $ Song o { parts = parts }

drawProtar :: Protar -> BadgeInfo -> Int -> Draw Int
drawProtar (Protar protar) badge startX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let targetX = if stuff.app.settings.leftyFlip
        then startX
        else startX + protar.chordsWidth
      pxToSecsVert px = stuff.pxToSecsVert (windowH - px) <> stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs <> negateDuration stuff.time)
      widthFret = customize.widthProtarFret
      widthHighway = widthFret * protar.strings + 2
      maxSecs = pxToSecsVert $ stuff.minY - 50
      minSecs = pxToSecsVert $ stuff.maxY + 50
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
      handedness n = if stuff.app.settings.leftyFlip then protar.strings - 1 - n else n
      lowStrings = if protar.strings <= 6 then 0 else protar.strings - 6
      drawH = stuff.maxY - stuff.minY
      phantomOpacity = 0.5 -- TODO move this to customize
  -- Chord names
  let drawChord = drawChord' $ if stuff.app.settings.leftyFlip
        then toNumber $ startX + widthHighway + 5
        else toNumber $ targetX - 5
      removePiece name = if stuff.app.settings.leftyFlip
        then map (\o -> {piece: o.head, pieces: o.tail}) (uncons name)
        else map (\o -> {piece: o.last, pieces: o.init}) (unsnoc name)
      drawChord' x secs name = case removePiece name of
        Nothing -> pure unit
        Just o -> do
          let ctx = stuff.context
          y <- case fst o.piece of
            Baseline -> do
              C.setFont ctx customize.proChordNameFont
              C.setFillStyle ctx customize.proChordNameColor
              pure $ toNumber $ secsToPxVert secs + 5
            Superscript -> do
              C.setFont ctx customize.proChordNameFontSuperscript
              C.setFillStyle ctx customize.proChordNameColor
              pure $ toNumber $ secsToPxVert secs - 3
          void $ C.setTextAlign ctx $ if stuff.app.settings.leftyFlip
            then C.AlignLeft
            else C.AlignRight
          void $ C.fillText ctx (snd o.piece) x y
          metrics <- C.measureText ctx (snd o.piece)
          let x' = if stuff.app.settings.leftyFlip
                then x + metrics.width
                else x - metrics.width
          drawChord' x' secs o.pieces
  -- 1. draw chords that end before now
  Map.zoomDescDo minSecs stuff.time protar.chords \secs e -> case e of
    Sustain _ -> pure unit
    Note c -> drawChord secs c
    SustainEnd -> case Map.lookupLT secs protar.chords of
      -- TODO process beforehand to remove the lookup here
      Just { value: Sustain c } -> drawChord secs c
      _ -> pure unit -- shouldn't happen
  -- 2. draw chords sticking at now
  case Map.lookupLE stuff.time protar.chords of
    Just { value: Sustain c } -> drawChord stuff.time c
    _ -> pure unit
  -- 3. draw chords that start after now
  Map.zoomDescDo stuff.time maxSecs protar.chords \secs e -> case e of
    SustainEnd -> pure unit
    Sustain c  -> drawChord secs c
    Note    c  -> drawChord secs c
  -- Highway
  setFillStyle customize.highway stuff
  fillRect { x: toNumber targetX, y: toNumber stuff.minY, width: toNumber widthHighway, height: toNumber drawH } stuff
  -- Solo highway
  setFillStyle customize.highwaySolo stuff
  let startsAsTrue bools = case Map.lookupLE minSecs bools of
        Nothing           -> false
        Just { value: v } -> v
      zoomEdges bools
        = L.fromFoldable
        $ cons (Tuple minSecs $ startsAsTrue bools)
        $ flip snoc (Tuple maxSecs false)
        $ Map.doTupleArray (zoomAsc bools)
      drawSolos L.Nil            = pure unit
      drawSolos (L.Cons _ L.Nil) = pure unit
      drawSolos (L.Cons (Tuple s1 b1) rest@(L.Cons (Tuple s2 _) _)) = do
        let y1 = secsToPxVert s1
            y2 = secsToPxVert s2
        when b1 $ for_ (map (\i -> i * widthFret + 2) $ range 0 (protar.strings - 1)) \offsetX -> do
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, width: toNumber $ widthFret - 2, height: toNumber $ y1 - y2 } stuff
        drawSolos rest
  drawSolos $ zoomEdges protar.solo
  -- Solo edges
  zoomDesc protar.solo \secs _ -> do
    let y = secsToPxVert secs
    setFillStyle customize.highwaySoloEdge stuff
    fillRect { x: toNumber targetX, y: toNumber y, width: toNumber widthHighway, height: 1.0 } stuff
  -- Arpeggios
  setFillStyle "rgba(255,255,255,0.3)" stuff -- TODO move color to customize
  drawSolos $ zoomEdges protar.arpeggio
  -- Beats
  drawBeats secsToPxVert
    { x: targetX
    , width: widthHighway
    , minSecs: minSecs
    , maxSecs: maxSecs
    } stuff
  -- Railings
  setFillStyle customize.highwayRailing stuff
  for_ (map (\i -> i * widthFret) $ range 0 protar.strings) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: toNumber stuff.minY, width: 1.0, height: toNumber drawH } stuff
  setFillStyle customize.highwayDivider stuff
  for_ (map (\i -> i * widthFret + 1) $ range 0 protar.strings) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: toNumber stuff.minY, width: 1.0, height: toNumber drawH } stuff
  -- Lanes
  let colors' =
        [ { c: _.s6, lane: _.s6, x: handedness (0    + lowStrings) * widthFret + 1, strum: image_gem_red_pro   , hopo: image_gem_red_pro_hopo   , tap: image_gem_red_pro_tap
          , shades: customize.sustainRed, target: image_highway_protar_target_red
          }
        , { c: _.s5, lane: _.s5, x: handedness (1    + lowStrings) * widthFret + 1, strum: image_gem_green_pro , hopo: image_gem_green_pro_hopo , tap: image_gem_green_pro_tap
          , shades: customize.sustainGreen, target: image_highway_protar_target_green
          }
        , { c: _.s4, lane: _.s4, x: handedness (2    + lowStrings) * widthFret + 1, strum: image_gem_orange_pro, hopo: image_gem_orange_pro_hopo, tap: image_gem_orange_pro_tap
          , shades: customize.sustainOrange, target: image_highway_protar_target_orange
          }
        , { c: _.s3, lane: _.s3, x: handedness (3    + lowStrings) * widthFret + 1, strum: image_gem_blue_pro  , hopo: image_gem_blue_pro_hopo  , tap: image_gem_blue_pro_tap
          , shades: customize.sustainBlue, target: image_highway_protar_target_blue
          }
        , { c: _.s2, lane: _.s2, x: handedness (4    + lowStrings) * widthFret + 1, strum: image_gem_yellow_pro, hopo: image_gem_yellow_pro_hopo, tap: image_gem_yellow_pro_tap
          , shades: customize.sustainYellow, target: image_highway_protar_target_yellow
          }
        , { c: _.s1, lane: _.s1, x: handedness (5    + lowStrings) * widthFret + 1, strum: image_gem_purple_pro, hopo: image_gem_purple_pro_hopo, tap: image_gem_purple_pro_tap
          , shades: customize.sustainPurple, target: image_highway_protar_target_purple
          }
        , { c: _.s7, lane: _.s7, x: handedness ((-1) + lowStrings) * widthFret + 1, strum: image_gem_purple_pro, hopo: image_gem_purple_pro_hopo, tap: image_gem_purple_pro_tap
          , shades: customize.sustainPurple, target: image_highway_protar_target_purple
          }
        , { c: _.s8, lane: _.s8, x: handedness ((-2) + lowStrings) * widthFret + 1, strum: image_gem_yellow_pro, hopo: image_gem_yellow_pro_hopo, tap: image_gem_yellow_pro_tap
          , shades: customize.sustainYellow, target: image_highway_protar_target_yellow
          }
        ]
      colors = take protar.strings colors'
  for_ colors \{x: offsetX, lane: gem} -> let
    thisLane = Map.union protar.bre $ gem protar.lanes
    startsAsLane = case Map.lookupLE minSecs thisLane of
      Nothing           -> false
      Just { value: v } -> v
    laneEdges
      = L.fromFoldable
      $ cons (Tuple minSecs startsAsLane)
      $ flip snoc (Tuple maxSecs false)
      $ Map.doTupleArray (zoomAsc thisLane)
    drawLanes L.Nil            = pure unit
    drawLanes (L.Cons _ L.Nil) = pure unit
    drawLanes (L.Cons (Tuple s1 b1) rest@(L.Cons (Tuple s2 _) _)) = do
      let y1 = secsToPxVert s1
          y2 = secsToPxVert s2
      when b1 $ drawLane
        { x: targetX + offsetX + 1
        , y: y2
        , width: widthFret - 2
        , height: y1 - y2
        } stuff
      drawLanes rest
    in drawLanes laneEdges
  -- Target
  for_ colors \{x: offsetX, target: targetImage} -> do
    drawImage targetImage (toNumber $ targetX + offsetX) (toNumber targetY - 5.0) stuff
  -- Sustains
  for_ colors \{ c: getColor, x: offsetX, shades: normalShades } -> do
    let thisColor = getColor protar.notes
        isEnergy secs = case Map.lookupLE secs protar.energy of
          Nothing           -> false
          Just { value: v } -> v
        hitAtY = if stuff.app.settings.autoplay then targetY else stuff.maxY + 50
        drawSustainBlock ystart yend energy note = when ((ystart < hitAtY || yend < hitAtY) && not note.phantom) do
          let ystart' = min ystart hitAtY
              yend'   = min yend   hitAtY
              sustaining = stuff.app.settings.autoplay && (targetY < ystart || targetY < yend)
              shades =
                if energy                   then customize.sustainEnergy
                else if isNothing note.fret then customize.sustainProtarMute
                else                             normalShades
              h = yend' - ystart' + 1
          setFillStyle shades.light stuff
          fillRect { x: toNumber $ targetX + offsetX + 12, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          setFillStyle shades.normal stuff
          fillRect { x: toNumber $ targetX + offsetX + 13, y: toNumber ystart', width: 5.0, height: toNumber h } stuff
          setFillStyle shades.dark stuff
          fillRect { x: toNumber $ targetX + offsetX + 18, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          setFillStyle customize.sustainBorder stuff
          fillRect { x: toNumber $ targetX + offsetX + 11, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          fillRect { x: toNumber $ targetX + offsetX + 19, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          fillRect
            { x: toNumber $ targetX + offsetX + 11
            , y: toNumber ystart'
            , width: 9.0
            , height: 1.0
            } stuff
          when sustaining do
            setFillStyle shades.light stuff
            fillRect { x: toNumber $ targetX + offsetX + 1, y: toNumber $ targetY - 4, width: toNumber $ widthFret - 1, height: 8.0 } stuff

        go Nothing L.Nil = pure unit
        go sust (L.Cons (Tuple secs v) rest) = do
          case sust of
            Nothing -> pure unit
            Just (Tuple secsStart o) -> drawSustainBlock (secsToPxVert secs) (secsToPxVert secsStart) (isEnergy secsStart) o
          let newSustain = case v of
                Sustain (ProtarNote o) -> Just (Tuple secs o)
                _                      -> Nothing
          go newSustain rest
        go (Just (Tuple secsStart o)) L.Nil = do
          drawSustainBlock stuff.minY (secsToPxVert secsStart) (isEnergy secsStart) o

        startSustain = case Map.lookupLT (pxToSecsVert stuff.maxY) thisColor of
          Just { key: secsStart, value: Sustain (ProtarNote o) } -> Just (Tuple secsStart o)
          _                                                      -> Nothing

    go startSustain $ L.fromFoldable $ Map.doTupleArray (zoomAsc thisColor)
  -- Phantom notes held during arpeggio (now)
  let withNoteStart (Note    (ProtarNote o)) f = f o
      withNoteStart (Sustain (ProtarNote o)) f = f o
      withNoteStart SustainEnd               _ = pure unit

      fretImage i = case index protarFrets i of
        Just x  -> x
        Nothing -> image_pro_fret_00 -- whatever

      getImages color isEnergy note = case note.fret of
        Just fret -> let
          base = case note.noteType of
            Strum -> if isEnergy then image_gem_energy_pro      else color.strum
            HOPO  -> if isEnergy then image_gem_energy_pro_hopo else color.hopo
            Tap   -> if isEnergy then image_gem_energy_pro_tap  else color.tap
          in [base, fretImage fret]
        Nothing -> let
          base = case note.noteType of
            Strum -> if isEnergy then image_gem_energy_mute      else image_gem_mute
            HOPO  -> if isEnergy then image_gem_energy_mute_hopo else image_gem_mute_hopo
            Tap   -> if isEnergy then image_gem_energy_mute_tap  else image_gem_mute_tap
          in [base]
  case Map.lookupLE stuff.time protar.arpeggio of
    Just { key: arpTime, value: true } -> do
      C.setGlobalAlpha stuff.context phantomOpacity
      for_ colors \color -> do
        case Map.lookup arpTime $ color.c protar.notes of
          Just evt -> withNoteStart evt \obj -> do
            for_ (getImages color false obj) \img -> do
              drawImage img (toNumber $ targetX + color.x) (toNumber $ secsToPxVert stuff.time - 10) stuff
          Nothing -> pure unit
      C.setGlobalAlpha stuff.context 1.0
    _ -> pure unit
  -- Notes
  for_ colors \color -> do
    let offsetX = color.x
    zoomDesc (color.c protar.notes) \secs evt -> withNoteStart evt \obj -> do
      let futureSecs = secToNum $ secs <> negateDuration stuff.time
      unless (obj.phantom && futureSecs <= 0.0) do
        when obj.phantom $ C.setGlobalAlpha stuff.context phantomOpacity
        if stuff.app.settings.autoplay && futureSecs <= 0.0
          then do
            -- note is in the past or being hit now
            if (-0.1) < futureSecs
              then do
                setFillStyle (color.shades.hit $ (futureSecs + 0.1) / 0.05) stuff
                fillRect { x: toNumber $ targetX + offsetX + 1, y: toNumber $ targetY - 4, width: toNumber $ widthFret - 1, height: 8.0 } stuff
              else pure unit
          else do
            let y = secsToPxVert secs
                isEnergy = not obj.phantom && case Map.lookupLE secs protar.energy of
                  Just {value: bool} -> bool
                  Nothing            -> false
            for_ (getImages color isEnergy obj) \img -> do
              drawImage img (toNumber $ targetX + offsetX) (toNumber $ y - 10) stuff
        when obj.phantom $ C.setGlobalAlpha stuff.context 1.0
  -- Draw badge below target
  drawBadgeVertical badge targetX widthHighway stuff
  -- Return targetX of next track
  pure $ startX + protar.chordsWidth + widthHighway + customize.marginWidth
