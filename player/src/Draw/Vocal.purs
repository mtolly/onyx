module Draw.Vocal (drawVocal) where

import           Prelude

import           Data.Either             (either)
import           Data.Foldable           (for_)
import           Data.Int                (round, toNumber)
import           Data.List               as L
import           Data.Maybe              (Maybe (..), isNothing)
import           Data.String.Regex       as R
import           Data.String.Regex.Flags (noFlags)
import           Data.Time.Duration      (Seconds (..), negateDuration)
import           Data.Tuple              (Tuple (..), fst)
import           Effect                  (Effect)
import           Effect.Exception.Unsafe (unsafeThrow)
import           Graphics.Canvas         as C

import           Draw.Common             (Draw, fillCircle, fillRect,
                                          measureText, onContext, secToNum,
                                          setFillStyle, slide, zoomAscDoPadding,
                                          zoomDescDoPadding)
import           OnyxMap                 as Map
import           Song                    (Vocal (..), VocalNote (..),
                                          VocalRange (..))
import           Style                   (customize)

drawVocal :: Vocal -> Int -> Draw Int
drawVocal (Vocal v) targetY stuff = do
  windowW <- map round $ C.getCanvasWidth stuff.canvas
  let pxToSecsHoriz px = stuff.pxToSecsHoriz px <> stuff.time
      secsToPxHoriz secs = stuff.secsToPxHoriz $ secs <> negateDuration stuff.time
      minSecs = pxToSecsHoriz (-100)
      maxSecs = pxToSecsHoriz $ windowW + 100
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = zoomDescDoPadding minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = zoomAscDoPadding minSecs maxSecs
      targetX = secsToPxHoriz stuff.time
  setFillStyle customize.vocalNoteArea stuff
  fillRect { x: 0.0, y: toNumber targetY + 25.0, width: toNumber windowW, height: 130.0 } stuff
  setFillStyle customize.lyricLaneBottom stuff
  fillRect { x: 0.0, y: toNumber targetY + 155.0, width: toNumber windowW, height: 25.0 } stuff
  setFillStyle customize.lyricLaneTop stuff
  fillRect { x: 0.0, y: toNumber targetY, width: toNumber windowW, height: 25.0 } stuff
  -- Draw note pitches
  -- TODO: draw all pitch lines before talkies
  -- TODO: draw harmony unisons better
  let thisRange = case Map.lookupLE stuff.time v.ranges of
        Nothing -> { min: 36.0, max: 84.0 }
        Just { value: VocalRange rmin rmax } -> { min: toNumber rmin, max: toNumber rmax }
        Just { key: t1, value: VocalRangeShift } -> case Map.lookupGT stuff.time v.ranges of
          Just { key: t2, value: VocalRange bmin bmax } -> case Map.lookupLT t1 v.ranges of
            Just { value: VocalRange amin amax } ->
              { min: slide (secToNum t1) (secToNum t2) (secToNum stuff.time) (toNumber amin) (toNumber bmin)
              , max: slide (secToNum t1) (secToNum t2) (secToNum stuff.time) (toNumber amax) (toNumber bmax)
              }
            _ -> unsafeThrow "not a valid range shift"
          _ -> unsafeThrow "not a valid range shift"
      pitchToY p = toNumber targetY + slide thisRange.min thisRange.max (toNumber p) 143.0 37.0
      drawLines :: Maybe (Tuple Seconds Int) -> L.List (Tuple Seconds VocalNote) -> Effect Unit
      drawLines (Just (Tuple t1 p1)) evts@(L.Cons (Tuple t2 (VocalStart lyric (Just p2))) _) | lyric == "+" || lyric == "+$" = do
        -- draw line from (t1,p1) to (t2,p2)
        onContext (\ctx -> C.moveTo ctx (toNumber $ secsToPxHoriz t1) (pitchToY p1)) stuff
        onContext (\ctx -> C.lineTo ctx (toNumber $ secsToPxHoriz t2) (pitchToY p2)) stuff
        drawLines Nothing evts
      drawLines (Just _) evts = drawLines Nothing evts -- ignore last note-off because no slide
      drawLines Nothing (L.Cons (Tuple t1 (VocalStart _ (Just p))) (L.Cons (Tuple t2 VocalEnd) rest)) = do
        -- draw line from (t1,p) to (t2,p)
        onContext (\ctx -> C.moveTo ctx (toNumber $ secsToPxHoriz t1) (pitchToY p)) stuff
        onContext (\ctx -> C.lineTo ctx (toNumber $ secsToPxHoriz t2) (pitchToY p)) stuff
        drawLines (Just (Tuple t2 p)) rest
      drawLines Nothing (L.Cons (Tuple t1 (VocalStart _ (Just p))) rest@(L.Cons (Tuple t2 (VocalStart _ _)) _)) = do
        -- draw line from (t1,p) to (t2,p)
        -- this case only happens in sloppy vox charts with no gap between notes
        onContext (\ctx -> C.moveTo ctx (toNumber $ secsToPxHoriz t1) (pitchToY p)) stuff
        onContext (\ctx -> C.lineTo ctx (toNumber $ secsToPxHoriz t2) (pitchToY p)) stuff
        drawLines (Just (Tuple t2 p)) rest
      drawLines Nothing (L.Cons (Tuple t1 (VocalStart _ Nothing)) (L.Cons (Tuple t2 VocalEnd) rest)) = do
        -- draw talky from t1 to t2
        fillRect { x: toNumber $ secsToPxHoriz t1, y: toNumber targetY + 25.0, width: toNumber $ secsToPxHoriz t2 - secsToPxHoriz t1, height: 130.0 } stuff
        drawLines Nothing rest
      drawLines Nothing (L.Cons (Tuple t1 (VocalStart _ Nothing)) rest@(L.Cons (Tuple t2 (VocalStart _ _)) _)) = do
        -- draw talky from t1 to t2
        -- this case only happens in sloppy vox charts with no gap between notes
        fillRect { x: toNumber $ secsToPxHoriz t1, y: toNumber targetY + 25.0, width: toNumber $ secsToPxHoriz t2 - secsToPxHoriz t1, height: 130.0 } stuff
        drawLines Nothing rest
      drawLines Nothing (L.Cons (Tuple _ (VocalStart _ _)) L.Nil) = pure unit -- off-screen
      drawLines _ L.Nil = pure unit
      drawLines Nothing (L.Cons (Tuple _ VocalEnd) rest) = drawLines Nothing rest
      lineParts =
        [ { part: v.harm2, line: customize.harm2Pitch, talky: customize.harm2Talky, width: 6.0 }
        , { part: v.harm3, line: customize.harm3Pitch, talky: customize.harm3Talky, width: 5.0 }
        , { part: v.harm1, line: customize.harm1Pitch, talky: customize.harm1Talky, width: 4.0 }
        ]
  onContext (\ctx -> C.setLineCap ctx C.Round) stuff
  for_ lineParts \o -> do
    onContext C.beginPath stuff
    onContext (\ctx -> C.setStrokeStyle ctx o.line) stuff
    onContext (\ctx -> C.setLineWidth ctx o.width) stuff
    onContext (\ctx -> C.setFillStyle ctx o.talky) stuff
    drawLines Nothing $ L.fromFoldable $ Map.doTupleArray (zoomAsc o.part)
    onContext C.stroke stuff
    onContext C.closePath stuff
  -- Draw text
  let lyricParts =
        [ { part: v.harm1, y: targetY + 174, isHarm3: false }
        , { part: v.harm2, y: targetY + 20, isHarm3: false }
        , { part: v.harm3, y: targetY + 20, isHarm3: true }
        ]
      harm2Lyric t = case Map.lookup t v.harm2 of
        Nothing -> Nothing
        Just VocalEnd -> Nothing
        Just (VocalStart lyric _) -> Just lyric
      -- TODO: support §
      getLyrics
        :: Boolean
        -> L.List (Tuple Seconds VocalNote)
        -> L.List {time :: Seconds, end :: Seconds, lyric :: String, isTalky :: Boolean}
      getLyrics isHarm3 evs = case L.uncons evs of
        Nothing -> L.Nil
        Just ht -> let
          Tuple t vn = ht.head
          processed = case vn of
            VocalEnd -> Nothing
            VocalStart lyric pitch
              | lyric == "+" -> Nothing
              | R.test (either unsafeThrow identity $ R.regex "\\$$" noFlags) lyric -> Nothing
              | isHarm3 && harm2Lyric t == Just lyric -> Nothing
              | otherwise -> Just
                { time: t
                , end: case L.uncons ht.tail of
                  -- TODO need to handle slides
                  Nothing -> t <> Seconds 100.0
                  Just o  -> fst o.head
                , lyric: R.replace (either unsafeThrow identity $ R.regex "=$" noFlags) "-" lyric
                , isTalky: isNothing pitch
                }
          in case processed of
            Nothing -> getLyrics isHarm3 ht.tail
            Just p  -> L.Cons p $ getLyrics isHarm3 ht.tail
      drawLyrics
        :: Number
        -> Number
        -> L.List {time :: Seconds, end :: Seconds, lyric :: String, isTalky :: Boolean}
        -> Effect Unit
      drawLyrics _    _     L.Nil           = pure unit
      drawLyrics minX textY (L.Cons o rest) = do
        let textX = max minX $ toNumber $ secsToPxHoriz o.time
        onContext (\ctx -> C.setFont ctx $ if o.isTalky
          then customize.lyricFontTalky
          else customize.lyricFont
          ) stuff
        setFillStyle (if o.time <= stuff.time && stuff.time <= o.end
          then customize.lyricColorActive
          else case Map.lookupLE o.time v.energy of
            Nothing -> customize.lyricColor
            Just { value: isEnergy } ->
              if isEnergy then customize.lyricColorEnergy else customize.lyricColor
          ) stuff
        metric <- measureText o.lyric stuff
        onContext (\ctx -> C.fillText ctx o.lyric textX textY) stuff
        drawLyrics (textX + metric.width + 5.0) textY rest
      mergeTime
        :: forall a t. (Ord t)
        => L.List {time :: t | a}
        -> L.List {time :: t | a}
        -> L.List {time :: t | a}
      mergeTime L.Nil ly = ly
      mergeTime lx L.Nil = lx
      mergeTime lx@(L.Cons x tx) ly@(L.Cons y ty) = if x.time <= y.time
        then L.Cons x $ mergeTime tx ly
        else L.Cons y $ mergeTime lx ty
  onContext (\ctx -> C.setTextAlign ctx C.AlignLeft) stuff
  drawLyrics (-999.0) (toNumber targetY + 174.0) $
    getLyrics false $ L.fromFoldable $ Map.doTupleArray (zoomAsc v.harm1)
  drawLyrics (-999.0) (toNumber targetY + 20.0) $ mergeTime
    (getLyrics false $ L.fromFoldable $ Map.doTupleArray (zoomAsc v.harm2))
    (getLyrics true  $ L.fromFoldable $ Map.doTupleArray (zoomAsc v.harm3))
  -- Draw percussion notes
  zoomDesc v.percussion \t (_ :: Unit) -> if t > stuff.time
    then do
      setFillStyle customize.percussionOuter stuff
      fillCircle { x: toNumber $ secsToPxHoriz t, y: toNumber targetY + 90.0, r: 11.0 } stuff
      setFillStyle customize.percussionInner stuff
      fillCircle { x: toNumber $ secsToPxHoriz t, y: toNumber targetY + 90.0, r: 9.0 } stuff
    else do
      let opacity = (secToNum (t <> negateDuration stuff.time) + 0.1) / 0.05
      when (opacity > 0.0) $ do
        setFillStyle (customize.percussionHit opacity) stuff
        fillCircle { x: toNumber $ secsToPxHoriz stuff.time, y: toNumber targetY + 90.0, r: 11.0 } stuff
  -- Draw phrase ends
  setFillStyle customize.vocalPhraseEnd stuff
  zoomDesc v.phrases \t (_ :: Unit) -> do
    fillRect { x: toNumber (secsToPxHoriz t) - 1.0, y: toNumber targetY + 25.0, width: 3.0, height: 130.0 } stuff
  -- Draw target line
  setFillStyle customize.vocalTargetLine stuff
  fillRect { x: toNumber targetX - 1.0, y: toNumber targetY + 25.0, width: 3.0, height: 130.0 } stuff
  pure $ targetY + 180 + customize.marginWidth
