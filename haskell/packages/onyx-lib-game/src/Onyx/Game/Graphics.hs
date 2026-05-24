{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NegativeLiterals      #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
module Onyx.Game.Graphics where

import           Codec.Picture
import qualified Codec.Picture.STBIR          as IR
import qualified Codec.Wavefront              as Obj
import           Control.Arrow                (second)
import           Control.Monad                (forM, forM_, guard, unless, void,
                                               when)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Trans.Resource (register, runResourceT)
import           Data.Bifunctor               (bimap)
import qualified Data.ByteString              as B
import           Data.Fixed                   (Fixed (..), Milli)
import           Data.Foldable                (toList, traverse_)
import           Data.Function                ((&))
import           Data.Functor                 ((<&>))
import qualified Data.HashMap.Strict          as HM
import           Data.IORef                   (IORef, newIORef, readIORef,
                                               writeIORef)
import           Data.List.Extra              (findIndex, isPrefixOf, nubOrd,
                                               partition, sort)
import           Data.List.HT                 (partitionMaybe)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (catMaybes, fromMaybe, isJust,
                                               listToMaybe, mapMaybe)
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as MV
import           Foreign                      hiding (void)
import           Foreign.C
import           FreeType
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear                       (M44, V2 (..), V3 (..), V4 (..),
                                               (!*!))
import qualified Linear                       as L
import           Onyx.Amplitude.PS2.MIDI      (AmpInstrument (..))
import           Onyx.Build.RB3CH             (BasicTiming (..))
import           Onyx.Codec.JSON              (loadYaml)
import           Onyx.Game.Audio              (AudioHandle (..))
import qualified Onyx.Game.Graphics.Config    as C
import           Onyx.Game.Graphics.Video
import           Onyx.Game.Time
import           Onyx.Game.Track
import           Onyx.MIDI.Common             (StrumHOPOTap (..), each,
                                               showPosition)
import           Onyx.MIDI.Track.Beat
import qualified Onyx.MIDI.Track.Drums        as D
import           Onyx.MIDI.Track.Drums.Elite  (EliteDrumNote (..))
import qualified Onyx.MIDI.Track.Drums.Elite  as ED
import qualified Onyx.MIDI.Track.FiveFret     as Five
import qualified Onyx.MIDI.Track.ProGuitar    as PG
import           Onyx.MIDI.Track.Venue        (PostProcess3)
import           Onyx.MIDI.Track.Vocal
import           Onyx.PhaseShift.Dance        (NoteType (..))
import           Onyx.Preferences             (EliteDrumLayoutHint (..),
                                               Preferences (..),
                                               readPreferences)
import           Onyx.Project                 (DrumMode (..), ManiaChart (..),
                                               ManiaStyle (..), VideoInfo (..))
import           Onyx.Resources               (getResourcesPath)
import           Onyx.StackTrace              (QueueLog, SendMessage,
                                               StackTraceT, errorToWarning,
                                               fatal, getQueueLog, inside,
                                               mapStackTraceT, stackIO, warn)
import           Onyx.Util.Text.Transform     (showTimestamp)
import           Optics                       ((.~))
import qualified Sound.MIDI.Util              as U
import           System.Directory             (doesFileExist)
import           System.FilePath              ((<.>), (</>))

data Object
  = Box
  | Flat
  | Model ModelID

data ObjectPosition
  = ObjectStretch (V3 Float) (V3 Float)
  -- ^ translate+scale so the area -0.5 to 0.5 in all 3 dimensions is scaled
  -- to the bounds between the two given corners
  | ObjectMove (V3 Float)
  -- ^ translate, no scale

data LightPosition
  = LightGlobal C.Light
  | LightOffset C.Light

data ColorSource
  = CSColor (V4 Float)
  | CSImage TextureID
  | CSImage2 TextureID TextureID
  | CSImage3 TextureID TextureID TextureID

drawObject :: GLStuff -> Object -> ObjectPosition -> ColorSource -> Float -> LightPosition -> IO ()
drawObject gl obj posn colorSource alpha lightOffset = do
  let colorType    = 1 :: GLuint
      texture1Type = 2 :: GLuint
      texture2Type = 3 :: GLuint
      texture3Type = 4 :: GLuint
      thisObject = case obj of
        Box           -> gl.boxObject
        Flat          -> gl.flatObject
        Model modelID -> fromMaybe (gl.flatObject { objVertexCount = 0 })
          $ lookup modelID gl.models
  glBindVertexArray thisObject.objVAO
  sendUniformName gl.objectShader "model" case posn of
    ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2) -> let
      yScale = case obj of
        Flat -> 1 -- so we don't try to scale by 0 since y1 == y2
        _    -> abs $ y2 - y1
      in translate4 (V3 ((x1 + x2) / 2) ((y1 + y2) / 2) ((z1 + z2) / 2))
        !*! L.scaled (V4 (abs $ x2 - x1) yScale (abs $ z2 - z1) 1)
    ObjectMove xyz -> translate4 xyz
  sendUniformName gl.objectShader "alpha" alpha
  case lightOffset of
    LightGlobal g -> do
      sendUniformName gl.objectShader "light.position" g.position
      sendUniformName gl.objectShader "light.ambient"  g.ambient
      sendUniformName gl.objectShader "light.diffuse"  g.diffuse
      sendUniformName gl.objectShader "light.specular" g.specular
    LightOffset off -> do
      let center = case posn of
            ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)
              -> V3 ((x1 + x2) / 2) (max y1 y2) ((z1 + z2) / 2)
            ObjectMove xyz -> xyz
      sendUniformName gl.objectShader "light.position" $ center + off.position
      sendUniformName gl.objectShader "light.ambient"  off.ambient
      sendUniformName gl.objectShader "light.diffuse"  off.diffuse
      sendUniformName gl.objectShader "light.specular" off.specular
  let missingTexture = do
        sendUniformName gl.objectShader "material.diffuse.type" colorType
        sendUniformName gl.objectShader "material.diffuse.color" (V4 1 0 1 1 :: V4 Float) -- magenta for missing texture
  case colorSource of
    CSColor color -> do
      sendUniformName gl.objectShader "material.diffuse.type" colorType
      sendUniformName gl.objectShader "material.diffuse.color" color
    CSImage texid -> do
      case lookup texid gl.textures of
        Just tex -> do
          glActiveTexture GL_TEXTURE0
          glBindTexture GL_TEXTURE_2D tex.textureGL
          sendUniformName gl.objectShader "material.diffuse.type" texture1Type
          sendUniformName gl.objectShader "material.diffuse.image" (0 :: GLint)
        Nothing -> missingTexture
    CSImage2 t1 t2 -> do
      case (lookup t1 gl.textures, lookup t2 gl.textures) of
        (Just tex1, Just tex2) -> do
          glActiveTexture GL_TEXTURE0
          glBindTexture GL_TEXTURE_2D tex1.textureGL
          glActiveTexture GL_TEXTURE1
          glBindTexture GL_TEXTURE_2D tex2.textureGL
          sendUniformName gl.objectShader "material.diffuse.type" texture2Type
          sendUniformName gl.objectShader "material.diffuse.image"  (0 :: GLint)
          sendUniformName gl.objectShader "material.diffuse.image2" (1 :: GLint)
        _ -> missingTexture
    CSImage3 t1 t2 t3 -> do
      case (lookup t1 gl.textures, lookup t2 gl.textures, lookup t3 gl.textures) of
        (Just tex1, Just tex2, Just tex3) -> do
          glActiveTexture GL_TEXTURE0
          glBindTexture GL_TEXTURE_2D tex1.textureGL
          glActiveTexture GL_TEXTURE1
          glBindTexture GL_TEXTURE_2D tex2.textureGL
          glActiveTexture GL_TEXTURE2
          glBindTexture GL_TEXTURE_2D tex3.textureGL
          sendUniformName gl.objectShader "material.diffuse.type" texture3Type
          sendUniformName gl.objectShader "material.diffuse.image"  (0 :: GLint)
          sendUniformName gl.objectShader "material.diffuse.image2" (1 :: GLint)
          sendUniformName gl.objectShader "material.diffuse.image3" (2 :: GLint)
        _ -> missingTexture
  sendUniformName gl.objectShader "material.specular.type" colorType
  sendUniformName gl.objectShader "material.specular.color" (V4 0.5 0.5 0.5 1 :: V4 Float) -- CONFIGME
  sendUniformName gl.objectShader "material.shininess" (32 :: Float)
  glDrawArrays GL_TRIANGLES 0 thisObject.objVertexCount

makeToggleBounds :: t -> t -> Map.Map t Toggle -> [(t, t, Bool)]
makeToggleBounds t1 t2 m = let
  togs = Map.toAscList m
  initBool = case togs of
    []           -> False
    (_, tog) : _ -> tog /= ToggleEmpty && tog /= ToggleStart ()
  togFuture tog = tog /= ToggleEmpty && tog /= ToggleEnd ()
  zipped = map (\((chunkStart, b), chunkEnd) -> (chunkStart, chunkEnd, b)) $ zip
    ((t1, initBool) : map (second togFuture) togs)
    (map fst togs ++ [t2])
  simplify [] = []
  simplify ((tx, _, b1) : (_, ty, b2) : rest) | b1 == b2
    = simplify $ (tx, ty, b1) : rest
  simplify (x : xs) = x : simplify xs
  in simplify zipped

drawRailings :: GLStuff -> Float -> Float -> Float -> Float -> IO ()
drawRailings gl notesLeft notesRight nearZ farZ = do
  let rail = gl.gfxConfig.track.railings
  drawObject gl Box
    (ObjectStretch
      (V3
        (notesLeft - rail.x_width)
        rail.y_top
        nearZ
      )
      (V3
        notesLeft
        rail.y_bottom
        farZ
      )
    )
    (CSColor rail.color) 1 (LightGlobal gl.gfxConfig.track.light)
  drawObject gl Box
    (ObjectStretch
      (V3
        notesRight
        rail.y_top
        nearZ
      )
      (V3
        (notesRight + rail.x_width)
        rail.y_bottom
        farZ
      )
    )
    (CSColor rail.color) 1 (LightGlobal gl.gfxConfig.track.light)

drawRailingsDefault :: GLStuff -> Float -> Float -> IO ()
drawRailingsDefault gl = drawRailings gl
  gl.gfxConfig.track.note_area.x_left
  gl.gfxConfig.track.note_area.x_right

drawDrums :: GLStuff -> Double -> Double -> DrumMode -> Map.Map Double (CommonState (DrumState (D.Gem D.ProType, D.DrumVelocity) (D.Gem D.ProType))) -> IO ()
drawDrums gl nowTime speed mode trk = drawDrumPlay gl nowTime speed mode DrumPlayState
  { events = do
    (cst, cs) <- Map.toDescList $ fst $ Map.split nowTime trk
    pad <- Set.toList cs.inner.notes
    let res = EventResult
          { hit = Just (pad, Just cst)
          , missed = []
          }
    return (cst, (res, initialState)) -- score/combo state not used
  , track = trk
  , noteTimes = Set.empty -- not used
  }

drawEliteDrums :: GLStuff -> Double -> Double -> [EliteDrumLayoutHint] -> Map.Map Double (CommonState (EliteDrumState Double (EliteDrumNote ED.FlamStatus) (ED.EliteGem ()))) -> IO ()
drawEliteDrums gl nowTime speed layout trk = drawEliteDrumPlay gl nowTime speed layout EliteDrumPlayState
  { events = let
    -- dummy game state with no inputs, but all notes marked as hit on time
    hitResults = do
      (cst, cs) <- Map.toDescList $ fst $ Map.split nowTime trk
      let notes = Set.toList cs.inner.notes
      guard $ not $ null notes
      return (cst, Map.fromList $ map (, EDHit cst) notes)
    in [(nowTime, (Nothing, initialEDState & #noteResults .~ hitResults))]
  , track = trk
  , noteTimes = Set.empty -- not used
  }

drawEliteDrumPlay :: GLStuff -> Double -> Double -> [EliteDrumLayoutHint] -> EliteDrumPlayState Double -> IO ()
drawEliteDrumPlay gl nowTime speed layout tdps = do
  glUseProgram gl.objectShader
  -- view and projection matrices should already have been set
  let draw = drawObject gl
      globalLight = LightGlobal gl.gfxConfig.track.light
      nearZ = gl.gfxConfig.track.time.z_past
      nowZ = gl.gfxConfig.track.time.z_now
      farZ = gl.gfxConfig.track.time.z_future
      farTime = nowTime + speed * realToFrac gl.gfxConfig.track.time.secs_future :: Double
      timeToZ t = nowZ + (farZ - nowZ) * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + (farTime - nowTime) * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      zoomed = zoomMap nearTime farTime tdps.track
      adjustedLeft = gl.gfxConfig.track.note_area.x_left * 1.3
      adjustedRight = gl.gfxConfig.track.note_area.x_right * 1.3
      trackWidth = adjustedRight - adjustedLeft
      fracToX f = adjustedLeft + trackWidth * f
      drawBeat t cs = case cs.beats of
        Nothing -> return ()
        Just e -> let
          tex = case e of
            Just Bar  -> TextureLine1
            Just Beat -> TextureLine2
            Nothing   -> TextureLine3
          z = timeToZ t
          xyz1 = V3
            (fracToX 0)
            gl.gfxConfig.track.y
            (z + gl.gfxConfig.track.beats.z_past)
          xyz2 = V3
            (fracToX 1)
            gl.gfxConfig.track.y
            (z + gl.gfxConfig.track.beats.z_future)
          in draw Flat (ObjectStretch xyz1 xyz2) (CSImage tex) 1 globalLight
      drawTargetSquare x1 x2 tex alpha = let
        y = gl.gfxConfig.track.y
        z1 = gl.gfxConfig.track.targets.z_past
        z2 = gl.gfxConfig.track.targets.z_future
        in draw Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) alpha globalLight
      layoutLeftOpenHand = fromMaybe False $ listToMaybe $ flip mapMaybe layout \case
        EDLeftCrossHand -> Just False
        EDLeftOpenHand  -> Just True
        _               -> Nothing
      layoutRightNearCrash = fromMaybe False $ listToMaybe $ flip mapMaybe layout \case
        EDRightFarCrash  -> Just False
        EDRightNearCrash -> Just True
        _                -> Nothing
      highwayParts = (if gl.leftyFlip then reverse else id) $ concat
        [ if layoutLeftOpenHand then [ED.CrashL, ED.Hihat, ED.Snare] else [ED.Snare, ED.Hihat, ED.CrashL]
        , [ED.Tom1, ED.Tom2, ED.Tom3]
        , if layoutRightNearCrash then [ED.CrashR, ED.Ride] else [ED.Ride, ED.CrashR]
        ]
      partWidth = \case
        ED.Snare  -> 0.15
        ED.Hihat  -> 0.125
        ED.CrashL -> 0.125
        ED.CrashR -> 0.125
        ED.Ride   -> 0.125
        ED.Tom1   -> 0.116666
        ED.Tom2   -> 0.116666
        ED.Tom3   -> 0.116666
        _         -> 1 -- not used
      widthSum = sum $ map partWidth highwayParts
      lookupGemBounds, gemBounds :: ED.EliteGem () -> (Float, Float)
      lookupGemBounds g = let
        onLeft = sum $ map partWidth $ takeWhile (/= g) highwayParts
        in (fracToX $ onLeft / widthSum, fracToX $ (onLeft + partWidth g) / widthSum)
      gemBounds = \case
        ED.Kick ()   -> (fracToX 0, fracToX 1)
        ED.HihatFoot -> lookupGemBounds ED.Hihat
        gem          -> lookupGemBounds gem
      hihatZoneBounds = map gemBounds [ED.Snare, ED.Hihat, ED.CrashL]
      hihatZoneX1 = minimum $ map fst hihatZoneBounds
      hihatZoneX2 = maximum $ map snd hihatZoneBounds
      -- drawGem _ _ note _ | note.gem == ED.HihatFoot = return ()
      drawGem t _od note alpha = let
        (texid, obj) = case (note.gem, note.type_) of
          (ED.Kick ()  , _                ) -> (TextureLongKick    , Model ModelDrumKick      )
          (ED.Snare    , ED.GemRim        ) -> (TextureRedGem      , Model ModelDrumRim       )
          (ED.Snare    , _                ) -> (TextureRedGem      , Model ModelDrumTom       )
          (ED.Hihat    , ED.GemHihatOpen  ) -> (TextureYellowCymbal, Model ModelDrumHihatOpen )
          (ED.Hihat    , ED.GemHihatClosed) -> (TextureYellowCymbal, Model ModelDrumCymbalFlat)
          (ED.Hihat    , _                ) -> (TextureYellowCymbal, Model ModelDrumCymbal    )
          (ED.HihatFoot, _                ) -> (TextureHihatFoot   , Model ModelDrumHihatFoot )
          (ED.CrashL   , _                ) -> (TexturePurpleCymbal, Model ModelDrumCymbal    )
          (ED.Tom1     , ED.GemRim        ) -> (TextureOrangeGem   , Model ModelDrumRim       )
          (ED.Tom1     , _                ) -> (TextureOrangeGem   , Model ModelDrumTom       )
          (ED.Tom2     , ED.GemRim        ) -> (TextureOrangeGem   , Model ModelDrumRim       )
          (ED.Tom2     , _                ) -> (TextureOrangeGem   , Model ModelDrumTom       )
          (ED.Tom3     , ED.GemRim        ) -> (TextureOrangeGem   , Model ModelDrumRim       )
          (ED.Tom3     , _                ) -> (TextureOrangeGem   , Model ModelDrumTom       )
          (ED.Ride     , _                ) -> (TextureBlueCymbal  , Model ModelDrumCymbal    )
          (ED.CrashR   , _                ) -> (TextureGreenCymbal , Model ModelDrumCymbal    )
        shade = case alpha of
          Nothing -> case note.velocity of
            D.VelocityNormal -> CSImage texid
            D.VelocityGhost  -> CSImage2 texid TextureOverlayGhost
            D.VelocityAccent -> CSImage2 texid TextureOverlayAccent
          Just _  -> CSColor gl.gfxConfig.objects.gems.color_hit
        (x1, x2) = gemBounds note.gem
        xCenter = x1 + (x2 - x1) / 2
        (x1', x2') = case note.velocity of
          D.VelocityGhost -> let
            adjustX v = xCenter + (v - xCenter) * 0.8
            in (adjustX x1, adjustX x2)
          _ -> (x1, x2)
        reference = case note.gem of
          ED.Kick () -> (x2' - x1') / 2
          _          -> 0.5 / 2
        xPairs = case note.extra of
          ED.Flam -> map (, obj) case note.gem of
            ED.Kick () -> [(x1', x1' + (x2' - x1') * (1/3)), (x1' + (x2' - x1') * (2/3), x2')]
            _          -> let
              -- make 2 slightly narrower notes, and adjust to keep it within the track
              flamWidth = (x2' - x1') * 0.75
              noteLeft = (xCenter - flamWidth, xCenter)
              noteRight = (xCenter, xCenter + flamWidth)
              xAdjustment
                | fst noteLeft < adjustedLeft = adjustedLeft - fst noteLeft
                | snd noteRight > adjustedRight = adjustedRight - snd noteRight
                | otherwise = 0
              in map (bimap (+ xAdjustment) (+ xAdjustment)) [noteLeft, noteRight]
          ED.NotFlam -> case note.gem of
            ED.HihatFoot ->
              [ (gemBounds ED.Snare , Model ModelDrumHihatFootWings)
              , ((x1', x2'), obj)
              , (gemBounds ED.CrashL, Model ModelDrumHihatFootWings)
              ]
            _            -> [((x1', x2'), obj)]
        (y1, y2) = (y - reference, y + reference)
        y = gl.gfxConfig.track.y
        (z1, z2) = (z - reference, z + reference)
        z = timeToZ t
        in forM_ xPairs \((thisX1, thisX2), obj') -> draw obj'
          (ObjectStretch (V3 thisX1 y1 z1) (V3 thisX2 y2 z2))
          shade
          (fromMaybe 1 alpha)
          (LightOffset gl.gfxConfig.objects.gems.light)
      drawNotes t cs = let
        od = case cs.overdrive of
          ToggleEmpty  -> False
          ToggleEnd () -> False
          _            -> True
        fadeTime = gl.gfxConfig.objects.gems.secs_fade
        in forM_ cs.inner.notes \gem ->
          case eliteNoteStatus t gem tdps.events of
            NoteFuture -> drawGem t od gem Nothing
            NoteMissed -> drawGem t od gem Nothing
            NoteHitAt hitTime -> if nowTime - hitTime < realToFrac fadeTime
              then drawGem nowTime od gem $ Just $ 1 - realToFrac (nowTime - hitTime) / fadeTime
              else return ()
      targets =
        [ (     [ED.Snare ], TextureTargetRed         , TextureTargetRedLight         , Just $ V4 0 0 0 0.2)
        , (     [ED.Hihat ], TextureTargetYellow      , TextureTargetYellowLight      , Just $ V4 1 1 1 0.2)
        , (     [ED.CrashL], TextureTargetPurple      , TextureTargetPurpleLight      , Just $ V4 0 0 0 0.2)
        , if gl.leftyFlip
          then ([ED.Tom1  ], TextureTargetOrangeRight , TextureTargetOrangeRightLight , Just $ V4 1 1 1 0.2)
          else ([ED.Tom1  ], TextureTargetOrangeLeft  , TextureTargetOrangeLeftLight  , Just $ V4 1 1 1 0.2)
        , (     [ED.Tom2  ], TextureTargetOrangeCenter, TextureTargetOrangeCenterLight, Just $ V4 1 1 1 0.15)
        , if gl.leftyFlip
          then ([ED.Tom3  ], TextureTargetOrangeLeft  , TextureTargetOrangeLeftLight  , Just $ V4 1 1 1 0.1)
          else ([ED.Tom3  ], TextureTargetOrangeRight , TextureTargetOrangeRightLight , Just $ V4 1 1 1 0.1)
        , (     [ED.Ride  ], TextureTargetBlue        , TextureTargetBlueLight        , Just if layoutRightNearCrash then V4 1 1 1 0.2 else V4 0 0 0 0.2)
        , (     [ED.CrashR], TextureTargetGreen       , TextureTargetGreenLight       , Just if layoutRightNearCrash then V4 0 0 0 0.2 else V4 1 1 1 0.2)
        ]
  -- draw highway
  forM_ (makeToggleBounds nearTime farTime $ fmap (.solo) zoomed) \(t1, t2, isSolo) -> do
    let highwayColor
          = (if isSolo then (.solo) else (.normal))
          $ gl.gfxConfig.track.color
    draw
      Flat
      (ObjectStretch
        (V3
          adjustedLeft
          gl.gfxConfig.track.y
          (timeToZ t1)
        )
        (V3
          adjustedRight
          gl.gfxConfig.track.y
          (timeToZ t2)
        )
      )
      (CSColor highwayColor)
      1
      globalLight
  glDepthFunc GL_ALWAYS
  forM_ targets \(pads, _, _, mtint) -> forM_ mtint \tint -> do
    let x1 = minimum $ map (fst . gemBounds) pads
        x2 = maximum $ map (snd . gemBounds) pads
    draw
      Flat
      (ObjectStretch
        (V3 x1 gl.gfxConfig.track.y nearZ)
        (V3 x2 gl.gfxConfig.track.y farZ)
      )
      (CSColor tint)
      1
      globalLight
  -- draw hihat open zones
  -- TODO this could be more efficient by drawing sections of zones at a time
  let hihatZones = nubOrd do
        (_, cs) <- Map.toList zoomed
        let zone = cs.inner.hihatZone
        toList (getPast zone) <> toList (getFuture zone)
  forM_ hihatZones \zone -> let
    (tex, zoneT1, zoneT2) = case zone of
      EliteHihatZoneSolid t1 t2 -> (TextureHihatZoneSolid, t1, t2)
      EliteHihatZoneFade  t1 t2 -> (TextureHihatZoneFade , t1, t2)
    in draw
      Flat
      (ObjectStretch
        (V3 hihatZoneX1 gl.gfxConfig.track.y $ timeToZ zoneT2)
        (V3 hihatZoneX2 gl.gfxConfig.track.y $ timeToZ zoneT1)
      )
      (CSImage tex)
      1
      globalLight
  glDepthFunc GL_LESS
  -- draw railings
  drawRailings gl adjustedLeft adjustedRight nearZ farZ
  -- draw beat lines
  glDepthFunc GL_ALWAYS
  void $ Map.traverseWithKey drawBeat zoomed
  -- draw lanes
  let drawLane startTime endTime gem = let
        (x1, x2) = gemBounds gem
        y = gl.gfxConfig.track.y
        z1 = timeToZ startTime
        z2 = timeToZ endTime
        tex = case gem of
          ED.Kick ()   -> TextureLaneOrange
          ED.Snare     -> TextureLaneRed
          ED.Hihat     -> TextureLaneYellow
          ED.HihatFoot -> TextureLaneYellow
          ED.CrashL    -> TextureLanePurple
          ED.Tom1      -> TextureLaneOrange
          ED.Tom2      -> TextureLaneOrange
          ED.Tom3      -> TextureLaneOrange
          ED.Ride      -> TextureLaneBlue
          ED.CrashR    -> TextureLaneGreen
        in draw Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) 1 globalLight
      drawLanes _        []                      = return ()
      drawLanes nextTime ((thisTime, cs) : rest) = do
        let lanes = Map.toList cs.inner.lanes
            bre = cs.bre
        -- draw following lanes
        forM_ lanes \(pad, tog) -> when (elem tog [ToggleStart (), ToggleRestart () (), ToggleOn ()]) do
          drawLane thisTime nextTime pad
        when (elem bre [ToggleStart (), ToggleRestart () (), ToggleOn ()]) do
          mapM_ (drawLane thisTime nextTime) [ED.Snare, ED.Hihat, ED.CrashL, ED.Tom1, ED.Tom2, ED.Tom3, ED.CrashR, ED.Ride]
        -- draw past lanes if rest is empty
        when (null rest) do
          forM_ lanes \(pad, tog) -> when (elem tog [ToggleEnd (), ToggleRestart () (), ToggleOn ()]) do
            drawLane nearTime thisTime pad
          when (elem bre [ToggleEnd (), ToggleRestart () (), ToggleOn ()]) do
            mapM_ (drawLane nearTime thisTime) [ED.Snare, ED.Hihat, ED.CrashL, ED.Tom1, ED.Tom2, ED.Tom3, ED.CrashR, ED.Ride]
        drawLanes thisTime rest
  drawLanes farTime $ Map.toDescList zoomed
  -- draw target
  forM_ targets \(pads, tex, _, _) -> do
    let x1 = minimum $ map (fst . gemBounds) pads
        x2 = maximum $ map (snd . gemBounds) pads
    drawTargetSquare x1 x2 tex 1
  let drawLights [] _ = return ()
      drawLights _ [] = return ()
      drawLights ((t, note) : states) colors = let
        pad = note.gem
        (colorsYes, colorsNo) = partition (\(pads, _, _, _) -> elem pad pads) colors
        alpha = 1 - realToFrac (nowTime - t) / gl.gfxConfig.track.targets.secs_light
        in when (t > nearTime) do
          forM_ colorsYes \(pads, _, light, _) -> let
            x1 = minimum $ map (fst . gemBounds) pads
            x2 = minimum $ map (snd . gemBounds) pads
            in drawTargetSquare x1 x2 light alpha
          drawLights states colorsNo
  drawLights [ (t, hit) | (t, (Just (EDInputHit hit), _)) <- tdps.events ] targets
  glDepthFunc GL_LESS
  -- draw notes
  traverseDescWithKey_ drawNotes zoomed
  -- draw side hihat stomp indicators (disabled at the moment)
  {-
  let stompWidth = case gemBounds ED.Hihat of
        (x1, x2) -> (x2 - x1) / 2
      outsideRailLeft  = adjustedLeft  - rail.x_width
      outsideRailRight = adjustedRight + rail.x_width
      drawStomps t cs = forM_ cs.commonState.tdHihatStomp \stomp -> do
        let stompColor = if t >= nowTime
              then case stomp of
                EliteHihatStompNotated  -> V4 1 0.8 0.6 1
                EliteHihatStompImplicit -> V4 0.7 0.7 1 1
              else V4 0 0 0 1
            stompZ = timeToZ $ max nowTime t
            stompTypeAlpha = case stomp of
              EliteHihatStompNotated  -> 1
              EliteHihatStompImplicit -> 0.5
            stompAlpha = stompTypeAlpha * if t >= nowTime
              then 1
              else max 0 $ 1 - realToFrac (nowTime - t) / gl.gfxConfig.objects.gems.secs_fade
        draw Box
          (ObjectStretch
            (V3
              (outsideRailLeft - stompWidth)
              rail.y_top
              (stompZ - stompWidth / 2)
            )
            (V3
              outsideRailLeft
              (rail.y_top - stompWidth)
              (stompZ + stompWidth / 2)
            )
          )
          (CSColor stompColor) stompAlpha (LightOffset gl.gfxConfig.objects.gems.light)
        draw Box
          (ObjectStretch
            (V3
              outsideRailRight
              rail.y_top
              (stompZ - stompWidth / 2)
            )
            (V3
              (outsideRailRight + stompWidth)
              (rail.y_top - stompWidth)
              (stompZ + stompWidth / 2)
            )
          )
          (CSColor stompColor) stompAlpha (LightOffset gl.gfxConfig.objects.gems.light)
  traverseDescWithKey_ drawStomps zoomed
  -}

drawDrumPlay :: GLStuff -> Double -> Double -> DrumMode -> DrumPlayState Double (D.Gem D.ProType, D.DrumVelocity) (D.Gem D.ProType) -> IO ()
drawDrumPlay gl nowTime speed mode dps = do
  glUseProgram gl.objectShader
  -- view and projection matrices should already have been set
  let draw = drawObject gl
      globalLight = LightGlobal gl.gfxConfig.track.light
      nearZ = gl.gfxConfig.track.time.z_past
      nowZ = gl.gfxConfig.track.time.z_now
      farZ = gl.gfxConfig.track.time.z_future
      farTime = nowTime + speed * realToFrac gl.gfxConfig.track.time.secs_future :: Double
      timeToZ t = nowZ + (farZ - nowZ) * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + (farTime - nowTime) * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      zoomed = zoomMap nearTime farTime dps.track
      trackWidth
        = gl.gfxConfig.track.note_area.x_right
        - gl.gfxConfig.track.note_area.x_left
      fracToX f = gl.gfxConfig.track.note_area.x_left + trackWidth * f
      laneCount = case mode of
        Drums5 -> 5
        _      -> 4
      drawGem t od (gem, velocity) alpha = let
        obj = case gem of
          D.Kick           -> Model ModelDrumKick
          D.Red            -> Model ModelDrumTom
          D.Pro _ D.Tom    -> Model ModelDrumTom
          D.Pro _ D.Cymbal -> Model ModelDrumCymbal
          D.Orange         -> Model ModelDrumCymbal
        texid = if od
          then case gem of
            D.Kick           -> TextureLongEnergy
            D.Red            -> TextureEnergyGem
            D.Pro _ D.Tom    -> TextureEnergyGem
            D.Pro _ D.Cymbal -> TextureEnergyCymbal
            D.Orange         -> TextureEnergyCymbal
          else case gem of
            D.Kick                  -> TextureLongKick
            D.Red                   -> if gl.leftyFlip then TextureGreenGem                    else TextureRedGem
            D.Pro D.Yellow D.Tom    -> if gl.leftyFlip then TextureBlueGem                     else TextureYellowGem
            D.Pro D.Blue   D.Tom    -> if gl.leftyFlip && mode /= Drums5 then TextureYellowGem else TextureBlueGem
            D.Pro D.Green  D.Tom    -> if gl.leftyFlip then TextureRedGem                      else TextureGreenGem
            D.Pro D.Yellow D.Cymbal -> case (gl.leftyFlip, mode) of
              (True , Drums5) -> TextureOrangeCymbal
              (True , _     ) -> TextureBlueCymbal
              (False, _     ) -> TextureYellowCymbal
            D.Pro D.Blue   D.Cymbal -> if gl.leftyFlip then TextureYellowCymbal                else TextureBlueCymbal
            D.Pro D.Green  D.Cymbal -> if gl.leftyFlip then TextureRedCymbal                   else TextureGreenCymbal
            D.Orange                -> if gl.leftyFlip then TextureYellowCymbal                else TextureOrangeCymbal
        shade = case alpha of
          Nothing -> case velocity of
            D.VelocityNormal -> CSImage texid
            D.VelocityGhost  -> CSImage2 texid TextureOverlayGhost
            D.VelocityAccent -> CSImage2 texid TextureOverlayAccent
          Just _  -> CSColor gl.gfxConfig.objects.gems.color_hit
        (x1, x2) = case mode of
          Drums5 -> case gem of
            D.Kick           -> (-1, 1)
            D.Red            -> if gl.leftyFlip then (0.6, 1) else (-1, -0.6)
            D.Pro D.Yellow _ -> if gl.leftyFlip then (0.2, 0.6) else (-0.6, -0.2)
            D.Pro D.Blue _   -> if gl.leftyFlip then (-0.2, 0.2) else (-0.2, 0.2)
            D.Orange         -> if gl.leftyFlip then (-0.6, -0.2) else (0.2, 0.6)
            D.Pro D.Green _  -> if gl.leftyFlip then (-1, -0.6) else (0.6, 1)
          _ -> case gem of
            D.Kick           -> (-1, 1)
            D.Red            -> if gl.leftyFlip then (0.5, 1) else (-1, -0.5)
            D.Pro D.Yellow _ -> if gl.leftyFlip then (0, 0.5) else (-0.5, 0)
            D.Pro D.Blue _   -> if gl.leftyFlip then (-0.5, 0) else (0, 0.5)
            D.Pro D.Green _  -> if gl.leftyFlip then (-1, -0.5) else (0.5, 1)
            D.Orange         -> (0.25, 0.75) -- shouldn't be used
        xCenter = x1 + (x2 - x1) / 2
        (x1', x2') = case velocity of
          D.VelocityGhost -> let
            adjustX v = xCenter + (v - xCenter) * 0.7
            in (adjustX x1, adjustX x2)
          _ -> (x1, x2)
        reference = case gem of
          D.Kick -> (x2' - x1') / 2
          _      -> 0.5 / 2
        (y1, y2) = (y - reference, y + reference)
        y = gl.gfxConfig.track.y
        (z1, z2) = (z - reference, z + reference)
        z = timeToZ t
        posn = ObjectStretch (V3 x1' y1 z1) (V3 x2' y2 z2)
        in draw obj posn shade (fromMaybe 1 alpha)
          $ LightOffset gl.gfxConfig.objects.gems.light
      drawNotes t cs = let
        od = case cs.overdrive of
          ToggleEmpty  -> False
          ToggleEnd () -> False
          _            -> True
        fadeTime = gl.gfxConfig.objects.gems.secs_fade
        in forM_ cs.inner.notes \gem ->
          case noteStatus t gem dps.events of
            NoteFuture -> drawGem t od gem Nothing
            NoteMissed -> drawGem t od gem Nothing
            NoteHitAt hitTime -> if nowTime - hitTime < realToFrac fadeTime
              then drawGem nowTime od gem $ Just $ 1 - realToFrac (nowTime - hitTime) / fadeTime
              else return ()
      drawBeat t cs = case cs.beats of
        Nothing -> return ()
        Just e -> let
          tex = case e of
            Just Bar  -> TextureLine1
            Just Beat -> TextureLine2
            Nothing   -> TextureLine3
          z = timeToZ t
          xyz1 = V3
            gl.gfxConfig.track.note_area.x_left
            gl.gfxConfig.track.y
            (z + gl.gfxConfig.track.beats.z_past)
          xyz2 = V3
            gl.gfxConfig.track.note_area.x_right
            gl.gfxConfig.track.y
            (z + gl.gfxConfig.track.beats.z_future)
          in draw Flat (ObjectStretch xyz1 xyz2) (CSImage tex) 1 globalLight
      drawTargetSquare i tex alpha = let
        x1 = fracToX $ i       / laneCount
        x2 = fracToX $ (i + 1) / laneCount
        y = gl.gfxConfig.track.y
        z1 = gl.gfxConfig.track.targets.z_past
        z2 = gl.gfxConfig.track.targets.z_future
        in draw Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) alpha globalLight
  -- draw highway
  forM_ (makeToggleBounds nearTime farTime $ fmap (.solo) zoomed) \(t1, t2, isSolo) -> do
    let highwayColor
          = (if isSolo then (.solo) else (.normal))
          $ gl.gfxConfig.track.color
    draw
      Flat
      (ObjectStretch
        (V3
          gl.gfxConfig.track.note_area.x_left
          gl.gfxConfig.track.y
          (timeToZ t1)
        )
        (V3
          gl.gfxConfig.track.note_area.x_right
          gl.gfxConfig.track.y
          (timeToZ t2)
        )
      )
      (CSColor highwayColor)
      1
      globalLight
  -- draw railings
  drawRailingsDefault gl nearZ farZ
  -- draw beat lines
  glDepthFunc GL_ALWAYS
  void $ Map.traverseWithKey drawBeat zoomed
  -- draw lanes
  let drawLane startTime endTime pad = let
        i = case mode of
          Drums5 -> case pad of
            D.Red            -> if gl.leftyFlip then 4 else 0
            D.Pro D.Yellow _ -> if gl.leftyFlip then 3 else 1
            D.Pro D.Blue   _ ->                             2
            D.Orange         -> if gl.leftyFlip then 1 else 3
            D.Pro D.Green  _ -> if gl.leftyFlip then 0 else 4
            _                -> 0
          _ -> case pad of
            D.Red            -> if gl.leftyFlip then 3 else 0
            D.Pro D.Yellow _ -> if gl.leftyFlip then 2 else 1
            D.Pro D.Blue   _ -> if gl.leftyFlip then 1 else 2
            D.Pro D.Green  _ -> if gl.leftyFlip then 0 else 3
            _                -> 0
        x1 = fracToX $ i       / laneCount
        x2 = fracToX $ (i + 1) / laneCount
        y = gl.gfxConfig.track.y
        z1 = timeToZ startTime
        z2 = timeToZ endTime
        tex = case pad of
          D.Red            -> if gl.leftyFlip then TextureLaneGreen else TextureLaneRed
          D.Pro D.Yellow _ -> case (gl.leftyFlip, mode) of
            (True , Drums5) -> TextureLaneOrange
            (True , _     ) -> TextureLaneBlue
            (False, _     ) -> TextureLaneYellow
          D.Pro D.Blue   _ -> if gl.leftyFlip && mode /= Drums5 then TextureLaneYellow else TextureLaneBlue
          D.Orange         -> if gl.leftyFlip then TextureLaneYellow else TextureLaneOrange
          D.Pro D.Green  _ -> if gl.leftyFlip then TextureLaneRed else TextureLaneGreen
          _                -> TextureLaneOrange
        in draw Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) 1 globalLight
      drawLanes _        []                      = return ()
      drawLanes nextTime ((thisTime, cs) : rest) = do
        let lanes = Map.toList cs.inner.lanes
            bre = cs.bre
        -- draw following lanes
        forM_ lanes \(pad, tog) -> when (elem tog [ToggleStart (), ToggleRestart () (), ToggleOn ()]) do
          drawLane thisTime nextTime pad
        when (elem bre [ToggleStart (), ToggleRestart () (), ToggleOn ()]) do
          mapM_ (drawLane thisTime nextTime) [D.Red, D.Pro D.Yellow D.Tom, D.Pro D.Blue D.Tom, D.Pro D.Green D.Tom]
        -- draw past lanes if rest is empty
        when (null rest) do
          forM_ lanes \(pad, tog) -> when (elem tog [ToggleEnd (), ToggleRestart () (), ToggleOn ()]) do
            drawLane nearTime thisTime pad
          when (elem bre [ToggleEnd (), ToggleRestart () (), ToggleOn ()]) do
            mapM_ (drawLane nearTime thisTime) [D.Red, D.Pro D.Yellow D.Tom, D.Pro D.Blue D.Tom, D.Pro D.Green D.Tom]
        drawLanes thisTime rest
  drawLanes farTime $ Map.toDescList zoomed
  -- draw target
  mapM_ (\(i, tex) -> drawTargetSquare i tex 1) $ zip [0..] case mode of
    Drums5 -> [TextureTargetRed, TextureTargetYellow, TextureTargetBlue, TextureTargetOrange, TextureTargetGreen]
    _      -> [TextureTargetRed, TextureTargetYellow, TextureTargetBlue, TextureTargetGreen]
  let drawLights [] _ = return ()
      drawLights _ [] = return ()
      drawLights ((t, pad) : states) colors = let
        (colorsYes, colorsNo) = partition (\(_, _, pads) -> elem pad pads) colors
        alpha = 1 - realToFrac (nowTime - t) / gl.gfxConfig.track.targets.secs_light
        in when (t > nearTime) do
          forM_ colorsYes \(i, light, _) -> drawTargetSquare i light alpha
          drawLights states colorsNo
  drawLights [ (t, pad) | (t, (res, _)) <- dps.events, Just ((pad, _vel), _) <- [res.hit] ] if gl.leftyFlip
    then case mode of
      Drums5 ->
        [ (0, TextureTargetRedLight   , [D.Pro D.Green  D.Tom, D.Pro D.Green  D.Cymbal])
        , (1, TextureTargetYellowLight, [D.Orange                                     ])
        , (2, TextureTargetBlueLight  , [D.Pro D.Blue   D.Tom, D.Pro D.Blue   D.Cymbal])
        , (3, TextureTargetOrangeLight, [D.Pro D.Yellow D.Tom, D.Pro D.Yellow D.Cymbal])
        , (4, TextureTargetGreenLight , [D.Red                                        ])
        ]
      _ ->
        [ (0, TextureTargetRedLight   , [D.Pro D.Green  D.Tom, D.Pro D.Green  D.Cymbal])
        , (1, TextureTargetYellowLight, [D.Pro D.Blue   D.Tom, D.Pro D.Blue   D.Cymbal])
        , (2, TextureTargetBlueLight  , [D.Pro D.Yellow D.Tom, D.Pro D.Yellow D.Cymbal])
        , (3, TextureTargetGreenLight , [D.Red                                        ])
        ]
    else case mode of
      Drums5 ->
        [ (0, TextureTargetRedLight   , [D.Red                                        ])
        , (1, TextureTargetYellowLight, [D.Pro D.Yellow D.Tom, D.Pro D.Yellow D.Cymbal])
        , (2, TextureTargetBlueLight  , [D.Pro D.Blue   D.Tom, D.Pro D.Blue   D.Cymbal])
        , (3, TextureTargetOrangeLight, [D.Orange                                     ])
        , (4, TextureTargetGreenLight , [D.Pro D.Green  D.Tom, D.Pro D.Green  D.Cymbal])
        ]
      _ ->
        [ (0, TextureTargetRedLight   , [D.Red                                        ])
        , (1, TextureTargetYellowLight, [D.Pro D.Yellow D.Tom, D.Pro D.Yellow D.Cymbal])
        , (2, TextureTargetBlueLight  , [D.Pro D.Blue   D.Tom, D.Pro D.Blue   D.Cymbal])
        , (3, TextureTargetGreenLight , [D.Pro D.Green  D.Tom, D.Pro D.Green  D.Cymbal])
        ]
  glDepthFunc GL_LESS
  -- draw notes
  traverseDescWithKey_ drawNotes zoomed

traverseDescWithKey_ :: (Applicative t) => (k -> a -> t ()) -> Map.Map k a -> t ()
traverseDescWithKey_ f = traverse_ (uncurry f) . Map.toDescList

zoomMap :: (TimeState a, Fractional t, Ord t) => t -> t -> Map.Map t a -> Map.Map t a
zoomMap t1 t2 m = let
  zoomed = fst $ Map.split t2 $ snd $ Map.split t1 m
  generated = case Map.lookupGE t2 m of
    Just (_, s) -> Just $ before s
    Nothing -> case Map.lookupLE t1 m of
      Just (_, s) -> Just $ after s
      Nothing     -> Nothing
  in if Map.null zoomed
    then maybe Map.empty (Map.singleton $ t1 + (t2 + t1) / 2) generated
    else zoomed

mapSingleState :: (TimeState a, Ord t) => t -> Map.Map t a -> a
mapSingleState t m = case Map.lookupGE t m of
  Just (t', x) -> if t == t'
    then x
    else before x
  Nothing -> case Map.lookupLT t m of
    Just (_, x) -> after x
    Nothing     -> empty

drawFive :: GLStuff -> Double -> Double -> Map.Map Double (CommonState (GuitarState Double (Maybe Five.Color))) -> IO ()
drawFive glStuff nowTime speed trk = drawFivePlay glStuff nowTime speed GuitarPlayState
  { events = let
    hitResults = do
      (cst, cs) <- Map.toDescList $ fst $ Map.split nowTime trk
      guard $ any (isJust . getNow) cs.inner.notes
      return (cst, GuitarHitStrum cst)
    -- TODO we need to generate fretting events for proper target light-up (especially sustains)
    in [(nowTime, (Nothing, initialGuitarState & #noteResults .~ hitResults))]
  , track = trk
  , noteTimes = Set.empty -- not used
  }

drawFivePlay :: GLStuff -> Double -> Double -> GuitarPlayState Double -> IO ()
drawFivePlay gl nowTime speed gps = do
  glUseProgram gl.objectShader
  -- view and projection matrices should already have been set
  let draw = drawObject gl
      globalLight = LightGlobal gl.gfxConfig.track.light
      nearZ = gl.gfxConfig.track.time.z_past
      nowZ = gl.gfxConfig.track.time.z_now
      farZ = gl.gfxConfig.track.time.z_future
      farTime = nowTime + speed * realToFrac gl.gfxConfig.track.time.secs_future :: Double
      timeToZ t = nowZ + (farZ - nowZ) * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + (farTime - nowTime) * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      zoomed = zoomMap nearTime farTime gps.track
      currentState = case gps.events of
        (_, (_, s)) : _ -> s
        _               -> initialGuitarState
      trackWidth
        = gl.gfxConfig.track.note_area.x_right
        - gl.gfxConfig.track.note_area.x_left
      fracToX f = gl.gfxConfig.track.note_area.x_left + trackWidth * f
      colorCenterX = \case
        Nothing          -> fracToX 0.5
        Just Five.Green  -> fracToX if gl.leftyFlip then 9 / 10 else 1 / 10
        Just Five.Red    -> fracToX if gl.leftyFlip then 7 / 10 else 3 / 10
        Just Five.Yellow -> fracToX $                                  5 / 10
        Just Five.Blue   -> fracToX if gl.leftyFlip then 3 / 10 else 7 / 10
        Just Five.Orange -> fracToX if gl.leftyFlip then 1 / 10 else 9 / 10
      drawSustain t1 t2 od color
        | t2 <= nowTime = return ()
        | otherwise     = let
          sc = gl.gfxConfig.objects.sustains.colors
          boxColor = if od
            then sc.energy
            else case color of
              Nothing          -> sc.open
              Just Five.Green  -> sc.green
              Just Five.Red    -> sc.red
              Just Five.Yellow -> sc.yellow
              Just Five.Blue   -> sc.blue
              Just Five.Orange -> sc.orange
          (x1, x2) = let
            center = colorCenterX color
            halfWidth = 0.5 * case color of
              Nothing -> gl.gfxConfig.objects.sustains.width.open
              Just _  -> gl.gfxConfig.objects.sustains.width.fret
            in (center - halfWidth, center + halfWidth)
          y2 = gl.gfxConfig.track.y
          y1 = y2 + gl.gfxConfig.objects.sustains.height
          (z1, z2) = (timeToZ $ max nowTime t1, timeToZ t2)
          in draw Box (ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)) (CSColor boxColor) 1 globalLight
      drawGem t od color sht alpha = let
        (texid, obj) = case (color, sht) of
          (Nothing         , Strum) -> (if od then TextureLongEnergy     else TextureLongOpen    , Model ModelGuitarOpen   )
          (Just Five.Green , Strum) -> (if od then TextureEnergyGem      else TextureGreenGem    , Model ModelGuitarStrum  )
          (Just Five.Red   , Strum) -> (if od then TextureEnergyGem      else TextureRedGem      , Model ModelGuitarStrum  )
          (Just Five.Yellow, Strum) -> (if od then TextureEnergyGem      else TextureYellowGem   , Model ModelGuitarStrum  )
          (Just Five.Blue  , Strum) -> (if od then TextureEnergyGem      else TextureBlueGem     , Model ModelGuitarStrum  )
          (Just Five.Orange, Strum) -> (if od then TextureEnergyGem      else TextureOrangeGem   , Model ModelGuitarStrum  )
          (Nothing         , HOPO ) -> (if od then TextureLongEnergyHopo else TextureLongOpenHopo, Model ModelGuitarOpen   )
          (Just Five.Green , HOPO ) -> (if od then TextureEnergyHopo     else TextureGreenHopo   , Model ModelGuitarHOPOTap)
          (Just Five.Red   , HOPO ) -> (if od then TextureEnergyHopo     else TextureRedHopo     , Model ModelGuitarHOPOTap)
          (Just Five.Yellow, HOPO ) -> (if od then TextureEnergyHopo     else TextureYellowHopo  , Model ModelGuitarHOPOTap)
          (Just Five.Blue  , HOPO ) -> (if od then TextureEnergyHopo     else TextureBlueHopo    , Model ModelGuitarHOPOTap)
          (Just Five.Orange, HOPO ) -> (if od then TextureEnergyHopo     else TextureOrangeHopo  , Model ModelGuitarHOPOTap)
          (Nothing         , Tap  ) -> (if od then TextureLongEnergyTap  else TextureLongOpenTap , Model ModelGuitarOpen   )
          (Just Five.Green , Tap  ) -> (if od then TextureEnergyTap      else TextureGreenTap    , Model ModelGuitarHOPOTap)
          (Just Five.Red   , Tap  ) -> (if od then TextureEnergyTap      else TextureRedTap      , Model ModelGuitarHOPOTap)
          (Just Five.Yellow, Tap  ) -> (if od then TextureEnergyTap      else TextureYellowTap   , Model ModelGuitarHOPOTap)
          (Just Five.Blue  , Tap  ) -> (if od then TextureEnergyTap      else TextureBlueTap     , Model ModelGuitarHOPOTap)
          (Just Five.Orange, Tap  ) -> (if od then TextureEnergyTap      else TextureOrangeTap   , Model ModelGuitarHOPOTap)
        shade = case alpha of
          Nothing -> CSImage texid
          Just _  -> CSColor gl.gfxConfig.objects.gems.color_hit
        posn = ObjectMove $ V3 (colorCenterX color) gl.gfxConfig.track.y z
        z = timeToZ t
        in draw obj posn shade (fromMaybe 1 alpha) $ LightOffset
          $ gl.gfxConfig.objects.gems.light
      drawNotes _        []                      = return ()
      drawNotes nextTime ((thisTime, cs) : rest) = do
        let notes = Map.toList cs.inner.notes
        -- draw following sustain
        forM_ notes \(color, pnf) -> forM_ (getFuture pnf) \sust -> do
          drawSustain thisTime nextTime sust.overdrive color
        -- draw note
        let thisOD = case cs.overdrive of
              ToggleEmpty  -> False
              ToggleEnd () -> False
              _            -> True
            fadeTime = gl.gfxConfig.objects.gems.secs_fade
        forM_ notes \(color, pnf) -> forM_ (getNow pnf) \sht ->
          case guitarNoteStatus thisTime gps.events of
            NoteFuture        -> drawGem thisTime thisOD color sht Nothing
            NoteMissed        -> drawGem thisTime thisOD color sht Nothing
            NoteHitAt hitTime -> if nowTime - hitTime < realToFrac fadeTime
              then drawGem nowTime thisOD color sht $ Just $ 1 - realToFrac (nowTime - hitTime) / fadeTime
              else return ()
        -- draw past sustain if rest is empty
        when (null rest) do
          forM_ notes \(color, pnf) -> forM_ (getPast pnf) \sust -> do
            drawSustain nearTime thisTime sust.overdrive color
        drawNotes thisTime rest
      drawBeat t cs = case cs.beats of
        Nothing -> return ()
        Just e -> let
          tex = case e of
            Just Bar  -> TextureLine1
            Just Beat -> TextureLine2
            Nothing   -> TextureLine3
          z = timeToZ t
          xyz1 = V3
            gl.gfxConfig.track.note_area.x_left
            gl.gfxConfig.track.y
            (z + gl.gfxConfig.track.beats.z_past)
          xyz2 = V3
            gl.gfxConfig.track.note_area.x_right
            gl.gfxConfig.track.y
            (z + gl.gfxConfig.track.beats.z_future)
          in draw Flat (ObjectStretch xyz1 xyz2) (CSImage tex) 1 globalLight
      drawTargetSquare i tex alpha = let
        x1 = fracToX $ i       / 5
        x2 = fracToX $ (i + 1) / 5
        y = gl.gfxConfig.track.y
        z1 = gl.gfxConfig.track.targets.z_past
        z2 = gl.gfxConfig.track.targets.z_future
        in draw Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) alpha globalLight
  -- draw highway
  forM_ (makeToggleBounds nearTime farTime $ fmap (.solo) zoomed) \(t1, t2, isSolo) -> do
    let highwayColor
          = (if isSolo then (.solo) else (.normal))
          $ gl.gfxConfig.track.color
    draw
      Flat
      (ObjectStretch
        (V3
          gl.gfxConfig.track.note_area.x_left
          gl.gfxConfig.track.y
          (timeToZ t1)
        )
        (V3
          gl.gfxConfig.track.note_area.x_right
          gl.gfxConfig.track.y
          (timeToZ t2)
        )
      )
      (CSColor highwayColor)
      1
      globalLight
  -- draw railings
  drawRailingsDefault gl nearZ farZ
  -- draw beat lines
  glDepthFunc GL_ALWAYS
  void $ Map.traverseWithKey drawBeat zoomed
  -- draw lanes
  let drawLane startTime endTime color = let
        i = case color :: Maybe Five.Color of
          Nothing          -> 2
          Just Five.Green  -> if gl.leftyFlip then 4 else 0
          Just Five.Red    -> if gl.leftyFlip then 3 else 1
          Just Five.Yellow ->                             2
          Just Five.Blue   -> if gl.leftyFlip then 1 else 3
          Just Five.Orange -> if gl.leftyFlip then 0 else 4
        x1 = fracToX $ i       / 5
        x2 = fracToX $ (i + 1) / 5
        y = gl.gfxConfig.track.y
        z1 = timeToZ startTime
        z2 = timeToZ endTime
        tex = case color of
          Nothing          -> TextureLanePurple
          Just Five.Green  -> TextureLaneGreen
          Just Five.Red    -> TextureLaneRed
          Just Five.Yellow -> TextureLaneYellow
          Just Five.Blue   -> TextureLaneBlue
          Just Five.Orange -> TextureLaneOrange
        in draw Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) 1 globalLight
      drawLanes _        []                      = return ()
      drawLanes nextTime ((thisTime, cs) : rest) = do
        let tremolo = Map.toList cs.inner.tremolo
            trill   = Map.toList cs.inner.trill
            bre     = cs.bre
            _ = cs :: CommonState (GuitarState Double (Maybe Five.Color))
        -- draw following lanes
        forM_ tremolo \(color, tog) -> when (elem tog [ToggleStart (), ToggleRestart () (), ToggleOn ()]) do
          drawLane thisTime nextTime color
        forM_ trill \(color, tog) -> when (elem tog [ToggleStart (), ToggleRestart () (), ToggleOn ()]) do
          drawLane thisTime nextTime color
        when (elem bre [ToggleStart (), ToggleRestart () (), ToggleOn ()]) do
          mapM_ (drawLane thisTime nextTime . Just) each
        -- draw past lanes if rest is empty
        when (null rest) do
          forM_ tremolo \(color, tog) -> when (elem tog [ToggleEnd (), ToggleRestart () (), ToggleOn ()]) do
            drawLane nearTime thisTime color
          forM_ trill \(color, tog) -> when (elem tog [ToggleEnd (), ToggleRestart () (), ToggleOn ()]) do
            drawLane nearTime thisTime color
          when (elem bre [ToggleEnd (), ToggleRestart () (), ToggleOn ()]) do
            mapM_ (drawLane nearTime thisTime . Just) each
        drawLanes thisTime rest
  drawLanes farTime $ Map.toDescList zoomed
  -- draw target
  let targets =
        [ (if gl.leftyFlip then 4 else 0, Five.Green , TextureTargetGreen , TextureTargetGreenLight )
        , (if gl.leftyFlip then 3 else 1, Five.Red   , TextureTargetRed   , TextureTargetRedLight   )
        , (                            2, Five.Yellow, TextureTargetYellow, TextureTargetYellowLight)
        , (if gl.leftyFlip then 1 else 3, Five.Blue  , TextureTargetBlue  , TextureTargetBlueLight  )
        , (if gl.leftyFlip then 0 else 4, Five.Orange, TextureTargetOrange, TextureTargetOrangeLight)
        ]
  forM_ targets \(i, fret, dark, light) -> if testBit currentState.heldFrets $ fromEnum fret
    then drawTargetSquare i light 1
    else drawTargetSquare i dark  1 -- TODO fade out light texture
  glDepthFunc GL_LESS
  -- draw notes
  drawNotes farTime $ Map.toDescList zoomed

-- TODO this needs lefty flip support
drawPG :: GLStuff -> Double -> Double -> PG.GtrTuning -> Map.Map Double (CommonState (PGState Double)) -> IO ()
drawPG gl nowTime speed tuning trk = do
  glUseProgram gl.objectShader
  -- view and projection matrices should already have been set
  let draw = drawObject gl
      globalLight = LightGlobal gl.gfxConfig.track.light
      nearZ = gl.gfxConfig.track.time.z_past
      nowZ = gl.gfxConfig.track.time.z_now
      farZ = gl.gfxConfig.track.time.z_future
      farTime = nowTime + speed * realToFrac gl.gfxConfig.track.time.secs_future :: Double
      timeToZ t = nowZ + (farZ - nowZ) * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + (farTime - nowTime) * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      zoomed = zoomMap nearTime farTime trk
      trackWidth
        = gl.gfxConfig.track.note_area.x_right
        - gl.gfxConfig.track.note_area.x_left
      fracToX f = gl.gfxConfig.track.note_area.x_left + trackWidth * f
      sc = gl.gfxConfig.objects.sustains.colors
      strR = (sc.red   , TextureTargetRed   , TextureTargetRedLight   , TextureRSRed   )
      strY = (sc.yellow, TextureTargetYellow, TextureTargetYellowLight, TextureRSYellow)
      strB = (sc.blue  , TextureTargetBlue  , TextureTargetBlueLight  , TextureRSBlue  )
      strO = (sc.orange, TextureTargetOrange, TextureTargetOrangeLight, TextureRSOrange)
      strG = (sc.green , TextureTargetGreen , TextureTargetGreenLight , TextureRSGreen )
      strP = (sc.purple, TextureTargetPurple, TextureTargetPurpleLight, TextureRSPurple)
      allStrings = case tuning.gtrBase of
        PG.Guitar6      -> zip [PG.S6 ..] [            strR, strY, strB, strO, strG, strP]
        PG.Guitar7      -> zip [PG.S7 ..] [      strP, strR, strY, strB, strO, strG, strP]
        PG.Guitar8      -> zip [PG.S8 ..] [strG, strP, strR, strY, strB, strO, strG, strP]
        PG.Bass4        -> zip [PG.S6 ..] [            strR, strY, strB, strO            ]
        PG.Bass5        -> zip [PG.S6 ..] [      strP, strR, strY, strB, strO            ]
        PG.Bass6        -> zip [PG.S6 ..] [      strP, strR, strY, strB, strO, strG      ]
        PG.GtrCustom ns -> case length ns of
          n | n <= 6 -> take n $ zip [PG.S6 ..] if PG.lowBassTuning tuning
            then [strP, strR, strY, strB, strO, strG]
            else [strR, strY, strB, strO, strG, strP]
          7          -> zip [PG.S7 ..] [      strP, strR, strY, strB, strO, strG, strP]
          _          -> zip [PG.S8 ..] [strG, strP, strR, strY, strB, strO, strG, strP]
      numStrings = fromIntegral $ length allStrings :: Float
      -- TODO probably should center strings when less than 6, instead of stretching them
      stringCenterX str = fracToX $ maybe 0 fromIntegral (findIndex ((== str) . fst) allStrings) / numStrings + 1 / (numStrings * 2)
      drawSustain t1 t2 od str
        | t2 <= nowTime = return ()
        | otherwise     = let
          boxColor = if od
            then sc.energy
            else case lookup str allStrings of
              Nothing                   -> sc.red -- shouldn't happen
              Just (sustColor, _, _, _) -> sustColor
          (x1, x2) = let
            center = stringCenterX str
            halfWidth = 0.5 * gl.gfxConfig.objects.sustains.width.fret
            in (center - halfWidth, center + halfWidth)
          y2 = gl.gfxConfig.track.y
          y1 = y2 + gl.gfxConfig.objects.sustains.height
          (z1, z2) = (timeToZ $ max nowTime t1, timeToZ t2)
          in draw Box (ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)) (CSColor boxColor) 1 globalLight
      drawGem t _od str note alpha = let
        obj = Model ModelPGNote
        fretWidth = 2 / numStrings
        halfWidth = fretWidth / 2
        (x1, x2) = let
          center = stringCenterX str
          in (center - halfWidth, center + halfWidth)
        y1 = gl.gfxConfig.track.y - halfWidth
        y2 = gl.gfxConfig.track.y + halfWidth
        z = timeToZ t
        (z1, z2) = (z - halfWidth, z + halfWidth)
        stretch = ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)
        texid1 = case lookup str allStrings of
          Nothing             -> TextureRSRed -- shouldn't happen
          Just (_, _, _, tex) -> tex
        texid2 = toEnum $ fromEnum TextureRS0 + note.fret
        texid3 = case note.sht of
          Strum -> Nothing
          HOPO  -> Just TextureRSHopo
          Tap   -> Just TextureRSTap
        shade = case alpha of
          Nothing -> maybe (CSImage2 texid1 texid2) (CSImage3 texid1 texid2) texid3
          Just _  -> CSColor gl.gfxConfig.objects.gems.color_hit
        in draw obj stretch shade (fromMaybe 1 alpha) $ LightOffset let
          normalLight = gl.gfxConfig.objects.gems.light
          in normalLight & #position .~ V3 0 0 0.5
      drawNotes _        []                      = return ()
      drawNotes nextTime ((thisTime, cs) : rest) = do
        let notes = Map.toList cs.inner.notes
        -- draw following sustain
        forM_ notes \(str, pnf) -> forM_ (getFuture pnf) \sust -> do
          drawSustain thisTime nextTime sust.overdrive str
        -- draw note
        let thisOD = case cs.overdrive of
              ToggleEmpty  -> False
              ToggleEnd () -> False
              _            -> True
            fadeTime = gl.gfxConfig.objects.gems.secs_fade
        forM_ notes \(str, pnf) -> forM_ (getNow pnf) \note -> if nowTime <= thisTime
          then drawGem thisTime thisOD str note Nothing
          else if nowTime - thisTime < realToFrac fadeTime
            then drawGem nowTime thisOD str note $ Just $ 1 - realToFrac (nowTime - thisTime) / fadeTime
            else return ()
        -- draw past sustain if rest is empty
        when (null rest) do
          forM_ notes \(str, pnf) -> forM_ (getPast pnf) \sust -> do
            drawSustain nearTime thisTime sust.overdrive str
        drawNotes thisTime rest
      drawBeat t cs = case cs.beats of
        Nothing -> return ()
        Just e -> let
          tex = case e of
            Just Bar  -> TextureLine1
            Just Beat -> TextureLine2
            Nothing   -> TextureLine3
          z = timeToZ t
          xyz1 = V3
            gl.gfxConfig.track.note_area.x_left
            gl.gfxConfig.track.y
            (z + gl.gfxConfig.track.beats.z_past)
          xyz2 = V3
            gl.gfxConfig.track.note_area.x_right
            gl.gfxConfig.track.y
            (z + gl.gfxConfig.track.beats.z_future)
          in draw Flat (ObjectStretch xyz1 xyz2) (CSImage tex) 1 globalLight
      drawTargetSquare i tex alpha = let
        x1 = fracToX $ i       / numStrings
        x2 = fracToX $ (i + 1) / numStrings
        y = gl.gfxConfig.track.y
        z1 = gl.gfxConfig.track.targets.z_past
        z2 = gl.gfxConfig.track.targets.z_future
        in draw Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) alpha globalLight
  -- draw highway
  forM_ (makeToggleBounds nearTime farTime $ fmap (.solo) zoomed) \(t1, t2, isSolo) -> do
    let highwayColor
          = (if isSolo then (.solo) else (.normal))
          $ gl.gfxConfig.track.color
    draw
      Flat
      (ObjectStretch
        (V3
          gl.gfxConfig.track.note_area.x_left
          gl.gfxConfig.track.y
          (timeToZ t1)
        )
        (V3
          gl.gfxConfig.track.note_area.x_right
          gl.gfxConfig.track.y
          (timeToZ t2)
        )
      )
      (CSColor highwayColor)
      1
      globalLight
  -- draw railings
  drawRailingsDefault gl nearZ farZ
  -- draw beat lines
  glDepthFunc GL_ALWAYS
  void $ Map.traverseWithKey drawBeat zoomed
  -- draw target
  mapM_ (\(i, (_, (_, tex, _, _))) -> drawTargetSquare i tex 1) $ zip [0..] allStrings
  let drawLights [] _ = return ()
      drawLights _ [] = return ()
      drawLights ((t, cs) : states) colors = let
        getLightAlpha str = do
          pnf <- Map.lookup str cs.inner.notes
          case getFuture pnf of
            Just _ -> Just 1 -- str is being sustained
            Nothing -> do
              guard $ isJust (getNow pnf) || isJust (getPast pnf)
              Just alpha
        (colorsYes, colorsNo) = flip partitionMaybe colors \(i, (str, (_, _, light, _))) ->
          fmap (\thisAlpha -> (i, light, thisAlpha))
            $ getLightAlpha str
        alpha = 1 - realToFrac (nowTime - t) / gl.gfxConfig.track.targets.secs_light
        in do
          forM_ colorsYes \(i, light, thisAlpha) -> drawTargetSquare i light thisAlpha
          drawLights states colorsNo
      lookPast = case Map.split nowTime zoomed of
        (past, future)
          | Map.null past -> case Map.lookupMin future of
            Nothing      -> []
            Just (_, cs) -> [(nowTime, before cs)]
          | otherwise     -> Map.toDescList past
  drawLights lookPast $ zip [0..] allStrings
  glDepthFunc GL_LESS
  -- draw notes
  drawNotes farTime $ Map.toDescList zoomed

data ManiaColor = ManiaRed | ManiaWhite | ManiaBlack

drawMania :: GLStuff -> Double -> Double -> ManiaChart -> Map.Map Double (CommonState ManiaState) -> IO ()
drawMania gl nowTime speed mchart trk = do
  glUseProgram gl.objectShader
  -- view and projection matrices should already have been set
  let draw = drawObject gl
      globalLight = LightGlobal gl.gfxConfig.track.light
      nearZ = gl.gfxConfig.track.time.z_past
      nowZ = gl.gfxConfig.track.time.z_now
      farZ = gl.gfxConfig.track.time.z_future
      farTime = nowTime + speed * realToFrac gl.gfxConfig.track.time.secs_future :: Double
      timeToZ t = nowZ + (farZ - nowZ) * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + (farTime - nowTime) * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      zoomed = zoomMap nearTime farTime trk
      trackWidth
        = gl.gfxConfig.track.note_area.x_right
        - gl.gfxConfig.track.note_area.x_left
      fracToX f = gl.gfxConfig.track.note_area.x_left + trackWidth * f
      sc = gl.gfxConfig.objects.sustains.colors
      drawBeat t cs = case cs.beats of
        Nothing -> return ()
        Just e -> let
          tex = case e of
            Just Bar  -> TextureLine1
            Just Beat -> TextureLine2
            Nothing   -> TextureLine3
          z = timeToZ t
          xyz1 = V3
            gl.gfxConfig.track.note_area.x_left
            gl.gfxConfig.track.y
            (z + gl.gfxConfig.track.beats.z_past)
          xyz2 = V3
            gl.gfxConfig.track.note_area.x_right
            gl.gfxConfig.track.y
            (z + gl.gfxConfig.track.beats.z_future)
          in draw Flat (ObjectStretch xyz1 xyz2) (CSImage tex) 1 globalLight
      keys = take mchart.keys case mchart.style of
        ManiaTurntable -> ManiaRed : cycle [ManiaWhite, ManiaBlack]
        ManiaEncore    -> [ManiaWhite, ManiaWhite] <> repeat ManiaBlack
        _              -> cycle [ManiaWhite, ManiaBlack]
      keyColor i = case drop i keys of
        color : _ -> color
        []        -> ManiaRed -- shouldn't happen
      keyWidth = 1 / fromIntegral mchart.keys
      keyBounds :: Int -> (Float, Float)
      keyBounds n = (fracToX $ fromIntegral n * keyWidth, fracToX $ fromIntegral (n + 1) * keyWidth)
      drawSustain t1 t2 color key
        | t2 <= nowTime = return ()
        | otherwise     = let
          boxColor = case mchart.style of
            ManiaAmplitude inst -> case inst of
              AmpDrums  -> sc.red
              AmpBass   -> sc.blue
              AmpSynth  -> sc.yellow
              AmpVocal  -> sc.green
              AmpGuitar -> sc.orange
              AmpFX     -> sc.purple
            _ -> case color of
              ManiaRed   -> sc.red
              ManiaWhite -> sc.blue
              ManiaBlack -> sc.purple
          (x1, x2) = let
            (leftBound, rightBound) = keyBounds key
            center = (leftBound + rightBound) / 2
            halfWidth = 0.5 * gl.gfxConfig.objects.sustains.width.fret
            in (center - halfWidth, center + halfWidth)
          y2 = gl.gfxConfig.track.y
          y1 = y2 + gl.gfxConfig.objects.sustains.height
          (z1, z2) = (timeToZ $ max nowTime t1, timeToZ t2)
          in draw Box (ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)) (CSColor boxColor) 1 globalLight
      drawGem t ntype color key alpha = let
        obj = case ntype of
          NoteLift -> Model ModelDrumCymbal
          _        -> Model ModelPGNote
          -- TODO mine
        isLift = ntype == NoteLift
        halfWidth = keyWidth / 2
        reference = if isLift then 0.2 else halfWidth
        (x1, x2) = let
          pair@(orig1, orig2) = keyBounds key
          half = (orig2 - orig1) * 0.5
          center = orig1 + half
          in if isLift
            then (center - half * 0.7, center + half * 0.7)
            else pair
        y1 = gl.gfxConfig.track.y - reference
        y2 = gl.gfxConfig.track.y + reference
        z = timeToZ t
        (z1, z2) = (z - reference, z + reference)
        stretch = ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)
        texid = case mchart.style of
          ManiaAmplitude inst -> case inst of
            AmpDrums  -> TextureRedGem
            AmpBass   -> TextureBlueGem
            AmpSynth  -> TextureYellowGem
            AmpVocal  -> TextureGreenGem
            AmpGuitar -> TextureOrangeGem
            AmpFX     -> TexturePurpleGem
          _ -> case color of
            ManiaRed   -> TextureRedGem
            ManiaWhite -> TextureBlueGem
            ManiaBlack -> TexturePurpleGem
        shade = case alpha of
          Nothing -> if isLift
            then CSColor $ V4 0.8 0.8 0.8 1
            else CSImage texid
          Just _  -> CSColor gl.gfxConfig.objects.gems.color_hit
        in draw obj stretch shade (fromMaybe 1 alpha) $ LightOffset let
          normalLight = gl.gfxConfig.objects.gems.light
          in normalLight & #position .~ V3 0 0 0.5
      drawNotes _        []                      = return ()
      drawNotes nextTime ((thisTime, cs) : rest) = do
        let notes = Map.toList cs.inner.notes
        -- draw following sustain
        forM_ notes \(key, pnf) -> forM_ (getFuture pnf) \_ntype -> do
          drawSustain thisTime nextTime (keyColor key) key
        -- draw note
        let fadeTime = gl.gfxConfig.objects.gems.secs_fade
        forM_ notes \(key, pnf) -> forM_ (getNow pnf) \ntype -> if nowTime <= thisTime
          then drawGem thisTime ntype (keyColor key) key Nothing
          else if nowTime - thisTime < realToFrac fadeTime
            then drawGem nowTime ntype (keyColor key) key $ Just $ 1 - realToFrac (nowTime - thisTime) / fadeTime
            else return ()
        -- draw past sustain if rest is empty
        when (null rest) do
          forM_ notes \(key, pnf) -> forM_ (getPast pnf) \_ntype -> do
            drawSustain nearTime thisTime (keyColor key) key
        drawNotes thisTime rest
      drawTargetSquare i tex alpha = let
        (x1, x2) = keyBounds i
        y = gl.gfxConfig.track.y
        z1 = gl.gfxConfig.track.targets.z_past
        z2 = gl.gfxConfig.track.targets.z_future
        in draw Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) alpha globalLight
  -- draw highway
  forM_ (makeToggleBounds nearTime farTime $ fmap (.solo) zoomed) \(t1, t2, isSolo) -> do
    let highwayColor
          = (if isSolo then (.solo) else (.normal))
          $ gl.gfxConfig.track.color
    draw
      Flat
      (ObjectStretch
        (V3
          gl.gfxConfig.track.note_area.x_left
          gl.gfxConfig.track.y
          (timeToZ t1)
        )
        (V3
          gl.gfxConfig.track.note_area.x_right
          gl.gfxConfig.track.y
          (timeToZ t2)
        )
      )
      (CSColor highwayColor)
      1
      globalLight
  -- draw railings
  drawRailingsDefault gl nearZ farZ
  -- draw beat lines
  glDepthFunc GL_ALWAYS
  void $ Map.traverseWithKey drawBeat zoomed
  -- draw target
  forM_ (zip [0..] keys) \(i, color) -> let
    tex = case mchart.style of
      ManiaAmplitude inst -> case inst of
        AmpDrums  -> TextureTargetRed
        AmpBass   -> TextureTargetBlue
        AmpSynth  -> TextureTargetYellow
        AmpVocal  -> TextureTargetGreen
        AmpGuitar -> TextureTargetOrange
        AmpFX     -> TextureTargetPurple
      _ -> case color of
        ManiaRed   -> TextureTargetRed
        ManiaWhite -> TextureTargetBlue
        ManiaBlack -> TextureTargetPurple
    in drawTargetSquare i tex 1
  let drawLights [] _ = return ()
      drawLights _ [] = return ()
      drawLights ((t, cs) : states) colors = let
        getLightAlpha i = do
          pnf <- Map.lookup i cs.inner.notes
          case getFuture pnf of
            Just _ -> Just 1 -- key is being sustained
            Nothing -> do
              guard $ isJust (getNow pnf) || isJust (getPast pnf)
              Just alpha
        (colorsYes, colorsNo) = flip partitionMaybe colors \(i, color) -> let
          light = case mchart.style of
            ManiaAmplitude inst -> case inst of
              AmpDrums  -> TextureTargetRedLight
              AmpBass   -> TextureTargetBlueLight
              AmpSynth  -> TextureTargetYellowLight
              AmpVocal  -> TextureTargetGreenLight
              AmpGuitar -> TextureTargetOrangeLight
              AmpFX     -> TextureTargetPurpleLight
            _ -> case color of
              ManiaRed   -> TextureTargetRedLight
              ManiaWhite -> TextureTargetBlueLight
              ManiaBlack -> TextureTargetPurpleLight
          in fmap (\thisAlpha -> (i, light, thisAlpha)) $ getLightAlpha i
        alpha = 1 - realToFrac (nowTime - t) / gl.gfxConfig.track.targets.secs_light
        in do
          forM_ colorsYes \(i, light, thisAlpha) -> drawTargetSquare i light thisAlpha
          drawLights states colorsNo
      lookPast = case Map.split nowTime zoomed of
        (past, future)
          | Map.null past -> case Map.lookupMin future of
            Nothing      -> []
            Just (_, cs) -> [(nowTime, before cs)]
          | otherwise     -> Map.toDescList past
  drawLights lookPast $ zip [0..] keys
  glDepthFunc GL_LESS
  -- draw notes
  drawNotes farTime $ Map.toDescList zoomed

drawLyrics
  :: GLStuff
  -> WindowDims
  -> (Int, Int, Int, Int)
  -> Double
  -> Map.Map Double (CommonState (VocalState Double))
  -> IO ()
drawLyrics gl dims (x, y, w, h) nowTime vocals = let
  common = mapSingleState nowTime vocals
  currentPhrase = case common.inner.phrases.now of
    ToggleStart     p -> Just p
    ToggleRestart _ p -> Just p
    ToggleOn        p -> Just p
    _                 -> Nothing
  currentSyllables :: [LyricSyllable Double]
  currentSyllables = case currentPhrase of
    Nothing -> []
    Just (phraseStart, phraseEnd) -> concat
      [ reverse $ takeWhile (\syl -> phraseStart <= syl.timeStart) common.inner.lyrics.past
      , toList common.inner.lyrics.now
      , takeWhile (\syl -> syl.timeStart < phraseEnd) common.inner.lyrics.future
      ]
  (past, future) = span (\syl -> syl.timeStart <= nowTime) currentSyllables
  joinText syls = T.concat do
    syl <- syls
    syl.lyric.lyricText : if syl.lyric.lyricContinues then [] else [" "]
  futureText = T.strip $ joinText future
  pastText = (if T.null futureText then T.strip else T.stripStart) $ joinText past
  in unless (T.null pastText && T.null futureText) do

    pastTextures <- prepareText gl (.lyrics) pastText
    futureTextures <- prepareText gl (.lyrics) futureText

    let pastColor = V4 (255 / 255) (231 / 255) (83 / 255) 1
        coloredTextures = map (, Just pastColor) pastTextures <> map (, Nothing) futureTextures

        lineWidth = fromIntegral $ sum (map (vX . gsrAdvance . fst . fst) coloredTextures) `shiftR` 6 :: Int
        boxWidth  = 2 * margin + lineWidth
        boxHeight = 2 * margin + fontSize - 5
        boxX      = x + quot (w - boxWidth) 2
        boxY      = y + quot (h - boxHeight) 2
        textX     = boxX + margin
        textY     = boxY + margin
        margin    = round $ fromIntegral gl.gfxConfig.text.lyrics.margin * gl.scaleUI
        fontSize  = round $ fromIntegral gl.gfxConfig.text.lyrics.size   * gl.scaleUI

    drawBackgroundBox gl dims
      (V2 boxX boxY)
      (V2 boxWidth boxHeight)
      (V4 0 0 0 0.8)
      backgroundBoxRadius
      [TL, TR, BL, BR]
    drawTextLine gl dims coloredTextures (V2 textX textY)

fillPtr :: (Storable a, MonadIO m) => (Ptr a -> IO ()) -> m a
fillPtr f = liftIO $ alloca \p -> f p >> peek p

getStringGL :: (MonadIO m) => GLenum -> m String
getStringGL name = let
  cast :: Ptr GLubyte -> Ptr CChar
  cast = castPtr
  -- actually should always decode utf-8 instead of system encoding, but probably fine
  -- "String queries return pointers to UTF-8 encoded, NULL-terminated static strings"
  in liftIO $ glGetString name >>= peekCString . cast

-- Some Chromebook model (ChromeOS, loading Linux build) failed shader compile with:
-- "GLSL 3.30 is not supported. Supported versions: 1.10, 1.20, 1.30, 1.40, 1.00 es and 3.00 es"
-- 3.00 es is based on 3.30 core, just with extra precision statement needed
hackShaderGLES :: B.ByteString -> B.ByteString
hackShaderGLES bs = case B.stripPrefix "#version" bs of
  Just bs' -> "#version 300 es\nprecision highp float;\n" <> B.dropWhile (\c -> c /= 0xA && c /= 0xD) bs'
  Nothing  -> bs

loadProgram :: (MonadIO m, SendMessage m) => [(GLenum, FilePath)] -> StackTraceT m GLuint
loadProgram shaderPairs = do
  versionGL <- getStringGL GL_VERSION
  let isGLES = "OpenGL ES" `isPrefixOf` versionGL
  when isGLES $ warn "Loading shaders as GLSL ES. Please report if an error occurs"
  mapStackTraceT (liftIO . runResourceT) do
    shaders <- forM shaderPairs \(shaderType, f) -> do
      inside ("compiling shader: " <> f) do
        source <- stackIO $ B.readFile f
        let source' = if isGLES then hackShaderGLES source else source
        shader <- stackIO (compileShader shaderType source') >>= either fatal return
        void $ register $ glDeleteShader shader
        return shader
    stackIO (compileProgram shaders) >>= either fatal return

compileShader :: GLenum -> B.ByteString -> IO (Either String GLuint)
compileShader shaderType source = do
  shader <- glCreateShader shaderType
  B.useAsCString source \cs' -> with cs' \cs -> do
    glShaderSource shader 1 cs nullPtr
    glCompileShader shader
    fillPtr (glGetShaderiv shader GL_COMPILE_STATUS) >>= \case
      GL_FALSE -> do
        errLen <- fillPtr $ glGetShaderiv shader GL_INFO_LOG_LENGTH
        allocaArray (fromIntegral errLen) \infoLog -> do
          glGetShaderInfoLog shader errLen nullPtr infoLog
          peekCString infoLog <&> Left
      _ -> return $ Right shader

compileProgram :: [GLuint] -> IO (Either String GLuint)
compileProgram shaders = do
  program <- glCreateProgram
  mapM_ (glAttachShader program) shaders
  glLinkProgram program
  fillPtr (glGetProgramiv program GL_LINK_STATUS) >>= \case
    GL_FALSE -> do
      errLen <- fillPtr $ glGetProgramiv program GL_INFO_LOG_LENGTH
      allocaArray (fromIntegral errLen) \infoLog -> do
        glGetProgramInfoLog program errLen nullPtr infoLog
        peekCString infoLog <&> Left
    _ -> return $ Right program

withArrayBytes :: (Storable a, Num len) => [a] -> (len -> Ptr a -> IO b) -> IO b
withArrayBytes xs f = withArray xs \p -> let
  bytes = fromIntegral $ length xs * sizeOf (head xs)
  in f bytes p

data Vertex = Vertex
  { vertexPosition  :: V3 CFloat
  , vertexNormal    :: V3 CFloat
  , vertexTexCoords :: V2 CFloat
  } deriving (Eq, Ord, Show)

instance Storable Vertex where
  sizeOf _ = 8 * sizeOf (undefined :: CFloat)
  alignment _ = alignment (undefined :: CFloat)
  peek = undefined -- not implemented
  poke p v = do
    poke (castPtr p) v.vertexPosition
    poke (castPtr p `plusPtr` (3 * sizeOf (undefined :: CFloat))) v.vertexNormal
    poke (castPtr p `plusPtr` (6 * sizeOf (undefined :: CFloat))) v.vertexTexCoords

simpleBox :: [Vertex]
simpleBox = let
  frontTopRight    = V3  0.5  0.5  0.5
  backTopRight     = V3  0.5  0.5 -0.5
  frontBottomRight = V3  0.5 -0.5  0.5
  backBottomRight  = V3  0.5 -0.5 -0.5
  frontTopLeft     = V3 -0.5  0.5  0.5
  backTopLeft      = V3 -0.5  0.5 -0.5
  frontBottomLeft  = V3 -0.5 -0.5  0.5
  backBottomLeft   = V3 -0.5 -0.5 -0.5
  in

    -- top face
    [ Vertex frontTopRight (V3 0.0 1.0 0.0) (V2 1.0 0.0)
    , Vertex backTopRight  (V3 0.0 1.0 0.0) (V2 1.0 1.0)
    , Vertex backTopLeft   (V3 0.0 1.0 0.0) (V2 0.0 1.0)
    , Vertex backTopLeft   (V3 0.0 1.0 0.0) (V2 0.0 1.0)
    , Vertex frontTopLeft  (V3 0.0 1.0 0.0) (V2 0.0 0.0)
    , Vertex frontTopRight (V3 0.0 1.0 0.0) (V2 1.0 0.0)

    -- left face
    , Vertex frontTopLeft    (V3 -1.0 0.0 0.0) (V2 1.0 1.0)
    , Vertex backTopLeft     (V3 -1.0 0.0 0.0) (V2 0.0 1.0)
    , Vertex backBottomLeft  (V3 -1.0 0.0 0.0) (V2 0.0 0.0)
    , Vertex backBottomLeft  (V3 -1.0 0.0 0.0) (V2 0.0 0.0)
    , Vertex frontBottomLeft (V3 -1.0 0.0 0.0) (V2 1.0 0.0)
    , Vertex frontTopLeft    (V3 -1.0 0.0 0.0) (V2 1.0 1.0)

    -- front face
    , Vertex frontBottomLeft  (V3 0.0 0.0 1.0) (V2 0.0 0.0)
    , Vertex frontBottomRight (V3 0.0 0.0 1.0) (V2 1.0 0.0)
    , Vertex frontTopRight    (V3 0.0 0.0 1.0) (V2 1.0 1.0)
    , Vertex frontTopRight    (V3 0.0 0.0 1.0) (V2 1.0 1.0)
    , Vertex frontTopLeft     (V3 0.0 0.0 1.0) (V2 0.0 1.0)
    , Vertex frontBottomLeft  (V3 0.0 0.0 1.0) (V2 0.0 0.0)

    -- right face
    , Vertex frontTopRight    (V3 1.0 0.0 0.0) (V2 0.0 1.0)
    , Vertex frontBottomRight (V3 1.0 0.0 0.0) (V2 0.0 0.0)
    , Vertex backBottomRight  (V3 1.0 0.0 0.0) (V2 1.0 0.0)
    , Vertex backBottomRight  (V3 1.0 0.0 0.0) (V2 1.0 0.0)
    , Vertex backTopRight     (V3 1.0 0.0 0.0) (V2 1.0 1.0)
    , Vertex frontTopRight    (V3 1.0 0.0 0.0) (V2 0.0 1.0)
    ]

simpleFlat :: [Vertex]
simpleFlat = let
  frontTopRight    = Vertex (V3  0.5 0  0.5) (V3 0.0 1.0 0.0) (V2 1.0 0.0)
  backTopRight     = Vertex (V3  0.5 0 -0.5) (V3 0.0 1.0 0.0) (V2 1.0 1.0)
  frontTopLeft     = Vertex (V3 -0.5 0  0.5) (V3 0.0 1.0 0.0) (V2 0.0 0.0)
  backTopLeft      = Vertex (V3 -0.5 0 -0.5) (V3 0.0 1.0 0.0) (V2 0.0 1.0)
  in
    [ frontTopRight
    , backTopRight
    , backTopLeft
    , backTopLeft
    , frontTopLeft
    , frontTopRight
    ]

quadVertices :: [CFloat]
quadVertices =
  -- positions    texture coords
  [  1,  1, 0,    1, 1   -- top right
  ,  1, -1, 0,    1, 0   -- bottom right
  , -1, -1, 0,    0, 0   -- bottom left
  , -1,  1, 0,    0, 1   -- top left
  ]

quadIndices :: [GLuint]
quadIndices =
  [ 0, 3, 1 -- first triangle
  , 1, 3, 2 -- second triangle
  ]

data Corner = TL | TR | BL | BR
  deriving (Eq)

-- https://chatgpt.com/share/68aa8c7b-ae74-8013-86cf-ff2038ccc09d
makeCorner :: Corner -> Int -> ([CFloat], [GLuint])
makeCorner corner numArcPoints = (vertices, indices) where

  -- center position and angle range
  -- (fixed these from chatgpt)
  (cx, cy, start, end) = case corner of
    TL -> ( 1, -1,   pi/2,   pi  )
    TR -> (-1, -1,    0  ,   pi/2)
    BR -> (-1,  1, 3*pi/2, 2*pi  )
    BL -> ( 1,  1,   pi  , 3*pi/2)

  r = 2 :: CFloat  -- radius so arc spans [-1,1] square

  arcAngles =
    [ start + (end - start) * fromIntegral i / fromIntegral numArcPoints
    | i <- [0..numArcPoints]
    ]

  -- arc points
  arcVerts = concat
    [ [ cx + r * cos angle
      , cy + r * sin angle
      , 0
      , (cos angle + 1) / 2  -- texcoord u (simple mapping)
      , (sin angle + 1) / 2  -- texcoord v
      ]
    | angle <- arcAngles
    ]

  -- center vertex
  centerVert = [cx, cy, 0, 0.5, 0.5]

  vertices = centerVert <> arcVerts

  -- indices for triangle fan
  indices = concat
    [ [0, fromIntegral i, fromIntegral (i + 1)]
    | i <- [1..numArcPoints]
    ]

translate4 :: (Num a) => V3 a -> M44 a
translate4 (V3 x y z) = V4
  (V4 1 0 0 x)
  (V4 0 1 0 y)
  (V4 0 0 1 z)
  (V4 0 0 0 1)

degrees :: (Floating a) => a -> a
degrees d = (d / 180) * pi

class SendUniform a where
  sendUniform :: GLint -> a -> IO ()

sendUniformName :: (MonadIO m, SendUniform a) => GLuint -> String -> a -> m ()
sendUniformName prog name x = liftIO do
  uni <- withCString name $ glGetUniformLocation prog
  sendUniform uni x

instance SendUniform (M44 Float) where
  sendUniform uni mat = with mat
    $ glUniformMatrix4fv uni 1 GL_TRUE {- <- this means row major order -}
    . castPtr

instance SendUniform (V2 Float) where
  sendUniform uni (V2 x y) = glUniform2f uni x y

instance SendUniform (V3 Float) where
  sendUniform uni (V3 x y z) = glUniform3f uni x y z

instance SendUniform (V4 Float) where
  sendUniform uni (V4 x y z w) = glUniform4f uni x y z w

instance SendUniform GLint where
  sendUniform = glUniform1i

instance SendUniform GLuint where
  sendUniform = glUniform1ui

instance SendUniform Float where
  sendUniform = glUniform1f

instance SendUniform Bool where
  sendUniform uni b = glUniform1i uni if b then 1 else 0

class (Pixel a) => GLPixel a where
  glPixelInternalFormat :: a -> GLint
  glPixelFormat :: a -> GLenum
  glPixelType :: a -> GLenum

instance GLPixel PixelRGBA8 where
  glPixelInternalFormat _ = GL_RGBA
  glPixelFormat _ = GL_RGBA
  glPixelType _ = GL_UNSIGNED_BYTE

data Texture = Texture
  { textureGL     :: GLuint
  , textureWidth  :: Int
  , textureHeight :: Int
  } deriving (Show)

flipVertical :: (Pixel a) => Image a -> Image a
flipVertical img = let
  lineElements = componentCount (pixelAt img 0 0) * imageWidth img
  splitLines v = let
    len = VS.length v
    in if len > lineElements
      then case VS.splitAt (len - lineElements) v of
        (x, y) -> y : splitLines x
      else [v | not $ VS.null v]
  in img { imageData = VS.concat $ splitLines $ imageData img }

loadTexture :: (GLPixel a, MonadIO m) => Bool -> Image a -> m Texture
loadTexture linear img = liftIO do
  let pixelProp :: (a -> b) -> Image a -> b
      pixelProp f _ = f undefined
      flippedVert = flipVertical img
      -- Old implementation, this eats a ton of memory for some reason!
      -- flippedVert = generateImage
      --   (\x y -> pixelAt img x $ imageHeight img - y - 1)
      --   (imageWidth img)
      --   (imageHeight img)
  texture <- fillPtr $ glGenTextures 1
  glBindTexture GL_TEXTURE_2D texture
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
  let filtering = if linear then GL_LINEAR else GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER filtering
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER filtering
  VS.unsafeWith (imageData flippedVert) \p -> do
    glTexImage2D GL_TEXTURE_2D 0 (pixelProp glPixelInternalFormat img)
      (fromIntegral $ imageWidth flippedVert)
      (fromIntegral $ imageHeight flippedVert)
      0
      (pixelProp glPixelFormat img)
      (pixelProp glPixelType img)
      (castPtr p)
  -- when linear $ glGenerateMipmap GL_TEXTURE_2D
  return $ Texture texture (imageWidth img) (imageHeight img)

updateTexture :: (GLPixel a, MonadIO m) => Image a -> Texture -> m ()
updateTexture img tex = liftIO do
  let pixelProp :: (a -> b) -> Image a -> b
      pixelProp f _ = f undefined
      flippedVert = generateImage
        (\x y -> pixelAt img x $ imageHeight img - y - 1)
        (imageWidth img)
        (imageHeight img)
  glBindTexture GL_TEXTURE_2D tex.textureGL
  VS.unsafeWith (imageData flippedVert) \p -> do
    glTexSubImage2D GL_TEXTURE_2D 0 0 0
      (fromIntegral $ imageWidth flippedVert)
      (fromIntegral $ imageHeight flippedVert)
      (pixelProp glPixelFormat img)
      (pixelProp glPixelType img)
      (castPtr p)

data RenderObject = RenderObject
  { objVAO         :: GLuint
  , objVertexCount :: GLint
  } deriving (Show)

data GLStuff = GLStuff
  { objectShader  :: GLuint
  , boxObject     :: RenderObject
  , flatObject    :: RenderObject
  , quadShader    :: GLuint
  , quadObject    :: RenderObject
  , cornerObjects :: [(Corner, RenderObject)]
  , textures      :: [(TextureID, Texture)]
  , models        :: [(ModelID, RenderObject)]
  , gfxConfig     :: C.Config
  , framebuffers  :: Framebuffers
  , fxaaEnabled   :: Bool
  , videoBGs      :: Map.Map (VideoInfo FilePath) VideoHandle
  , imageBGs      :: Map.Map FilePath Texture
  , fontLib       :: FT_Library
  , fontFaces     :: Map.Map (T.Text, Int) FontFace
  , previewSong   :: Maybe PreviewSong
  , scaleUI       :: Float -- TODO allow changing this during runtime
  , waveform      :: Bool
  , leftyFlip     :: Bool
  }

data FontFace = FontFace
  { fontFace   :: FT_Face
  , fontSlot   :: FT_GlyphSlot
  , fontGlyphs :: IORef (HM.HashMap Char (FT_GlyphSlotRec, Texture))
  }

data VideoHandle = VideoHandle
  { frameLoader :: FrameLoader
  , texture     :: IORef (Maybe (Double, Texture))
  , filePath    :: FilePath
  }

instance Show VideoHandle where
  show vh = "VideoHandle[" <> show vh.filePath <> "]"

data Framebuffers
  = SimpleFramebuffer
    { simpleFBO       :: GLuint
    , simpleFBOTex    :: GLuint
    , simpleFBORender :: GLuint
    }
  | MSAAFramebuffers
    { msaaFBO            :: GLuint
    , msaaFBOTex         :: GLuint
    , msaaFBORender      :: GLuint
    , intermediateFBO    :: GLuint
    , intermediateFBOTex :: GLuint
    , multisamples       :: GLsizei
    }
  deriving (Show)

data TextureID
  = TextureLongKick
  | TextureLongOpen
  | TextureLongOpenHopo
  | TextureLongOpenTap
  | TextureLongEnergy
  | TextureLongEnergyHopo
  | TextureLongEnergyTap
  | TextureHihatFoot
  | TextureHihatZoneSolid
  | TextureHihatZoneFade
  | TextureGreenGem
  | TextureRedGem
  | TextureYellowGem
  | TextureBlueGem
  | TextureOrangeGem
  | TexturePurpleGem
  | TextureEnergyGem
  | TextureGreenHopo
  | TextureRedHopo
  | TextureYellowHopo
  | TextureBlueHopo
  | TextureOrangeHopo
  | TextureEnergyHopo
  | TextureGreenTap
  | TextureRedTap
  | TextureYellowTap
  | TextureBlueTap
  | TextureOrangeTap
  | TextureEnergyTap
  | TextureRedCymbal
  | TextureYellowCymbal
  | TextureBlueCymbal
  | TextureGreenCymbal
  | TexturePurpleCymbal
  | TextureOrangeCymbal
  | TextureEnergyCymbal
  | TextureLine1
  | TextureLine2
  | TextureLine3
  | TextureTargetGreen
  | TextureTargetRed
  | TextureTargetYellow
  | TextureTargetBlue
  | TextureTargetOrange
  | TextureTargetPurple
  | TextureTargetGreenLight
  | TextureTargetRedLight
  | TextureTargetYellowLight
  | TextureTargetBlueLight
  | TextureTargetOrangeLight
  | TextureTargetPurpleLight
  | TextureTargetOrangeLeft
  | TextureTargetOrangeLeftLight
  | TextureTargetOrangeCenter
  | TextureTargetOrangeCenterLight
  | TextureTargetOrangeRight
  | TextureTargetOrangeRightLight
  | TextureNumber0
  | TextureNumber1
  | TextureNumber2
  | TextureNumber3
  | TextureNumber4
  | TextureNumber5
  | TextureNumber6
  | TextureNumber7
  | TextureNumber8
  | TextureNumber9
  | TextureLaneGreen
  | TextureLaneRed
  | TextureLaneYellow
  | TextureLaneBlue
  | TextureLaneOrange
  | TextureLanePurple
  | TextureRS0
  | TextureRS1
  | TextureRS2
  | TextureRS3
  | TextureRS4
  | TextureRS5
  | TextureRS6
  | TextureRS7
  | TextureRS8
  | TextureRS9
  | TextureRS10
  | TextureRS11
  | TextureRS12
  | TextureRS13
  | TextureRS14
  | TextureRS15
  | TextureRS16
  | TextureRS17
  | TextureRS18
  | TextureRS19
  | TextureRS20
  | TextureRS21
  | TextureRS22
  | TextureRS23
  | TextureRS24
  | TextureRSRed
  | TextureRSYellow
  | TextureRSBlue
  | TextureRSOrange
  | TextureRSGreen
  | TextureRSPurple
  | TextureRSHopo
  | TextureRSTap
  | TextureRSPalmMute
  | TextureRSFretHandMute
  | TextureOverlayGhost
  | TextureOverlayAccent
  deriving (Eq, Show, Enum, Bounded)

data ModelID
  = ModelDrumTom
  | ModelDrumRim
  | ModelDrumCymbal
  | ModelDrumCymbalFlat
  | ModelDrumHihatOpen
  | ModelDrumKick
  | ModelDrumHihatFoot
  | ModelDrumHihatFootWings
  | ModelGuitarStrum
  | ModelGuitarHOPOTap
  | ModelGuitarOpen
  | ModelPGNote
  deriving (Eq, Show, Enum, Bounded)

loadObj :: (MonadIO m) => FilePath -> StackTraceT m [Vertex]
loadObj f = inside ("loading model: " <> f) do
  obj <- stackIO (Obj.fromFile f) >>= either fatal return
  return do
    Obj.Face a b c rest <- map Obj.elValue $ V.toList $ Obj.objFaces obj
    let triangulate v1 v2 v3 vs = [v1, v2, v3] ++ case vs of
          []    -> []
          h : t -> triangulate v1 v3 h t
    faceIndex <- triangulate a b c rest
    let loc      = Obj.objLocations obj V.! (Obj.faceLocIndex faceIndex - 1)
        texCoord = Obj.objTexCoords obj V.! (fromMaybe 0 (Obj.faceTexCoordIndex faceIndex) - 1)
        nor      = Obj.objNormals   obj V.! (fromMaybe 0 (Obj.faceNorIndex      faceIndex) - 1)
    return $ Vertex
      { vertexPosition   = CFloat <$> V3 (Obj.locX loc) (Obj.locY loc) (Obj.locZ loc)
      , vertexNormal     = CFloat <$> V3 (Obj.norX nor) (Obj.norY nor) (Obj.norZ nor)
      , vertexTexCoords  = CFloat <$> V2 (Obj.texcoordR texCoord) (Obj.texcoordS texCoord)
      }

load3DConfig :: (MonadIO m, SendMessage m) => StackTraceT m C.Config
load3DConfig = stackIO (getResourcesPath "3d-config.yml") >>= loadYaml

sortVertices :: [Vertex] -> [Vertex]
sortVertices = let
  getTris (v1 : v2 : v3 : rest) = [v1, v2, v3] : getTris rest
  getTris _                     = []
  getZ (V3 _ _ z) = z
  sumZ = sum . map (getZ . (.vertexPosition))
  in concatMap snd . sort . map (\tri -> (sumZ tri, tri)) . getTris

loadGLStuff :: (MonadIO m) => Float -> Maybe PreviewSong -> StackTraceT (QueueLog m) GLStuff
loadGLStuff scaleUI previewSong = do

  gfxConfig <- load3DConfig

  glEnable GL_DEPTH_TEST
  glEnable GL_CULL_FACE -- default CCW = front
  glEnable GL_BLEND
  glBlendFuncSeparate GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA GL_ONE GL_ONE

  -- format of the Vertex type
  let vertexParts =
        [ (3, GL_FLOAT, 3 * sizeOf (undefined :: CFloat))
        , (3, GL_FLOAT, 3 * sizeOf (undefined :: CFloat))
        , (2, GL_FLOAT, 2 * sizeOf (undefined :: CFloat))
        ]
      totalSize = sum [ size | (_, _, size) <- vertexParts ]
      writeParts _ _      []                         = return ()
      writeParts i offset ((n, gltype, size) : rest) = do
        glVertexAttribPointer i n gltype GL_FALSE
          (fromIntegral totalSize)
          (intPtrToPtr $ fromIntegral offset)
        glEnableVertexAttribArray i
        writeParts (i + 1) (offset + size) rest

  -- object stuff

  objectVS <- stackIO $ getResourcesPath "shaders/object.vert"
  objectFS <- stackIO $ getResourcesPath "shaders/object.frag"
  objectShader <- loadProgram
    [ (GL_VERTEX_SHADER  , objectVS)
    , (GL_FRAGMENT_SHADER, objectFS)
    ]

  let loadObject (sortVertices -> vertices) = liftIO do
        vao <- fillPtr $ glGenVertexArrays 1
        vbo <- fillPtr $ glGenBuffers 1
        glBindVertexArray vao
        glBindBuffer GL_ARRAY_BUFFER vbo
        withArrayBytes vertices \size p -> do
          glBufferData GL_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW
        writeParts 0 0 vertexParts
        glBindVertexArray 0
        -- commented out line worked on all platforms except AMD on Windows,
        -- where it breaks drawing and causes out of memory errors (???)
        -- withArrayLen [vbo] $ glDeleteBuffers . fromIntegral
        return RenderObject
          { objVAO         = vao
          , objVertexCount = fromIntegral $ length vertices
          }

  boxObject <- loadObject simpleBox
  flatObject <- loadObject simpleFlat

  -- quad stuff

  quadVS <- stackIO $ getResourcesPath "shaders/quad.vert"
  quadFS <- stackIO $ getResourcesPath "shaders/quad.frag"
  quadShader <- loadProgram
    [ (GL_VERTEX_SHADER  , quadVS)
    , (GL_FRAGMENT_SHADER, quadFS)
    ]

  let load2DShape vertices indices = do

        vao <- stackIO $ fillPtr $ glGenVertexArrays 1
        vbo <- stackIO $ fillPtr $ glGenBuffers 1
        ebo <- stackIO $ fillPtr $ glGenBuffers 1

        glBindVertexArray vao

        glBindBuffer GL_ARRAY_BUFFER vbo
        stackIO $ withArrayBytes vertices \size p -> do
          glBufferData GL_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW

        glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
        stackIO $ withArrayBytes indices \size p -> do
          glBufferData GL_ELEMENT_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW

        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
          (fromIntegral $ 5 * sizeOf (undefined :: CFloat))
          nullPtr
        glEnableVertexAttribArray 0
        glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE
          (fromIntegral $ 5 * sizeOf (undefined :: CFloat))
          (intPtrToPtr $ fromIntegral $ 3 * sizeOf (undefined :: CFloat))
        glEnableVertexAttribArray 1

        glUseProgram quadShader
        sendUniformName quadShader "inTexture" (0 :: GLint)
        let object = RenderObject vao $ fromIntegral $ length indices
        glBindVertexArray 0
        -- see note in loadObject
        -- stackIO $ withArrayLen [vbo, ebo] $ glDeleteBuffers . fromIntegral

        return object

  quadObject <- load2DShape quadVertices quadIndices
  cornerObjects <- forM [TL, TR, BL, BR] \corner -> do
    let (vertices, indices) = makeCorner corner 16
    object <- load2DShape vertices indices
    return (corner, object)

  -- textures

  textures <- fmap catMaybes $ forM [minBound .. maxBound] \texID -> do
    let imageName = textureToPath texID
    base <- stackIO $ getResourcesPath $ "textures" </> imageName
    let isLinear = case texID of
          TextureNumber0 -> False
          TextureNumber1 -> False
          TextureNumber2 -> False
          TextureNumber3 -> False
          TextureNumber4 -> False
          TextureNumber5 -> False
          TextureNumber6 -> False
          TextureNumber7 -> False
          TextureNumber8 -> False
          TextureNumber9 -> False
          _              -> True
        readExt [] = fatal $ "No file found for texture " <> imageName
        readExt (ext : rest) = stackIO (doesFileExist $ base <.> ext) >>= \case
          True -> stackIO (readImage $ base <.> ext) >>= either fatal return
            >>= loadTexture isLinear . convertRGBA8
          False -> readExt rest
    fmap (texID,) <$> errorToWarning (readExt ["png", "jpg", "jpeg"])

  -- models

  models <- forM [minBound .. maxBound] \modID -> do
    path <- liftIO $ getResourcesPath case modID of
      ModelDrumTom            -> "models/drum-tom.obj"
      ModelDrumRim            -> "models/drum-rim.obj"
      ModelDrumCymbal         -> "models/drum-cymbal.obj"
      ModelDrumCymbalFlat     -> "models/drum-cymbal-flat.obj"
      ModelDrumHihatOpen      -> "models/drum-hihat-open.obj"
      ModelDrumKick           -> "models/drum-kick.obj"
      ModelDrumHihatFoot      -> "models/drum-hihat-foot.obj"
      ModelDrumHihatFootWings -> "models/drum-hihat-foot-wings.obj"
      ModelGuitarStrum        -> "models/gtr-strum.obj"
      ModelGuitarHOPOTap      -> "models/gtr-hopotap.obj"
      ModelGuitarOpen         -> "models/gtr-open.obj"
      ModelPGNote             -> "models/pg-note.obj"
    obj <- loadObj path >>= loadObject
    return (modID, obj)

  -- framebuffers

  let setupMSAA n = do

        glEnable GL_MULTISAMPLE

        -- configure MSAA framebuffer
        msaaFBO <- fillPtr $ glGenFramebuffers 1
        checkGL "fbuf 0" $ glBindFramebuffer GL_FRAMEBUFFER msaaFBO
        -- create a multisampled color attachment texture
        -- create a (also multisampled) renderbuffer object for depth and stencil attachments
        msaaFBOTex    <- fillPtr $ glGenTextures      1
        msaaFBORender <- fillPtr $ glGenRenderbuffers 1

        -- intermediate framebuffer
        intermediateFBO <- fillPtr $ glGenFramebuffers 1
        checkGL "interm 0" $ glBindFramebuffer GL_FRAMEBUFFER intermediateFBO
        intermediateFBOTex <- fillPtr $ glGenTextures 1

        let multisamples = fromIntegral n
            fbufs = MSAAFramebuffers{..}

        void glGetError
        setFramebufferSize fbufs 1920 1080 -- dummy size, changed later
        glGetError >>= \case
          GL_NO_ERROR -> do

            checkGL "fbuf 0" $ glBindFramebuffer GL_FRAMEBUFFER msaaFBO
            checkGL "fbuf 1" $ glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D_MULTISAMPLE msaaFBOTex 0
            checkGL "fbuf 2" $ glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_STENCIL_ATTACHMENT GL_RENDERBUFFER msaaFBORender
            checkGL "interm 0" $ glBindFramebuffer GL_FRAMEBUFFER intermediateFBO
            checkGL "interm 1" $ glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D intermediateFBOTex 0

            glBindFramebuffer GL_FRAMEBUFFER 0
            return fbufs

          err -> do

            warn $ concat
              [ "Couldn't enable MSAA ("
              , case err of
                GL_INVALID_ENUM      -> "Invalid enum"
                GL_INVALID_VALUE     -> "Invalid value"
                GL_INVALID_OPERATION -> "Invalid operation"
                GL_OUT_OF_MEMORY     -> "Out of memory"
                _                    -> "Unknown error"
              , "). See 'Edit > Preferences' to configure anti-aliasing"
              ]
            liftIO $ withArrayLen [msaaFBO, intermediateFBO] $ glDeleteFramebuffers . fromIntegral
            liftIO $ withArrayLen [msaaFBOTex, intermediateFBOTex] $ glDeleteTextures . fromIntegral
            liftIO $ withArrayLen [msaaFBORender] $ glDeleteRenderbuffers . fromIntegral
            setupSimple

      setupSimple = do

        glDisable GL_MULTISAMPLE

        simpleFBO <- fillPtr $ glGenFramebuffers 1
        glBindFramebuffer GL_FRAMEBUFFER simpleFBO
        simpleFBOTex    <- fillPtr $ glGenTextures      1
        simpleFBORender <- fillPtr $ glGenRenderbuffers 1
        let fbufs = SimpleFramebuffer{..}
        setFramebufferSize fbufs 1920 1080 -- dummy size, changed later
        glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D simpleFBOTex 0
        glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_STENCIL_ATTACHMENT GL_RENDERBUFFER simpleFBORender

        glBindFramebuffer GL_FRAMEBUFFER 0
        return fbufs

  prefs <- readPreferences
  framebuffers <- case prefs.prefMSAA of
    Just n | n > 1 -> setupMSAA n
    _              -> setupSimple
  let fxaaEnabled = prefs.prefFXAA

  videoBGs <- fmap (Map.fromList . catMaybes) $ forM (maybe [] (map snd . previewBG) previewSong) \case
    PreviewBGVideo vi -> do
      writeMsg <- getQueueLog
      frameLoader <- stackIO $ forkFrameLoader writeMsg vi
      videoTexRef <- stackIO $ newIORef Nothing
      return $ Just (vi, VideoHandle
        { frameLoader = frameLoader
        , texture     = videoTexRef
        , filePath    = vi.fileVideo
        })
    _ -> return Nothing
  imageBGs <- fmap (Map.fromList . catMaybes) $ forM (maybe [] (map snd . previewBG) previewSong) \case
    PreviewBGImage f -> stackIO (readImage f) >>= \case
      Left _err -> do
        warn "Failed to load song image background"
        return Nothing
      Right img -> do
        tex <- loadTexture True $ convertRGBA8 img
        return $ Just (f, tex)
    _ -> return Nothing

  -- fonts
  fontLib <- stackIO ft_Init_FreeType
  let allFonts = Map.fromList do
        fontConfig <- [gfxConfig.text.timeBox, gfxConfig.text.trackLabel, gfxConfig.text.lyrics]
        return ((fontConfig.font, fontConfig.size), ())
  fontFaces <- flip Map.traverseWithKey allFonts \(filename, size) () -> do
    fontPath <- stackIO $ getResourcesPath $ "fonts" </> T.unpack filename
    fontFace <- stackIO $ ft_New_Face fontLib fontPath 0
    stackIO $ ft_Set_Char_Size fontFace (fromIntegral size * 64) 0 (round $ 72 * scaleUI) 0
    fontSlot <- stackIO $ frGlyph <$> peek fontFace
    fontGlyphs <- stackIO $ newIORef HM.empty
    return FontFace{..}

  let waveform  = prefs.prefWaveform
      leftyFlip = prefs.prefLeftyFlip

  return GLStuff{..}

textureToPath :: TextureID -> FilePath
textureToPath = \case
  TextureLongKick                -> "long-kick"
  TextureLongOpen                -> "long-open"
  TextureLongOpenHopo            -> "long-open-hopo"
  TextureLongOpenTap             -> "long-open-tap"
  TextureLongEnergy              -> "long-energy"
  TextureLongEnergyHopo          -> "long-energy-hopo"
  TextureLongEnergyTap           -> "long-energy-tap"
  TextureHihatFoot               -> "hihat-foot"
  TextureHihatZoneSolid          -> "hihat-zone-solid"
  TextureHihatZoneFade           -> "hihat-zone-fade"
  TextureGreenGem                -> "box-green"
  TextureRedGem                  -> "box-red"
  TextureYellowGem               -> "box-yellow"
  TextureBlueGem                 -> "box-blue"
  TextureOrangeGem               -> "box-orange"
  TexturePurpleGem               -> "box-purple"
  TextureEnergyGem               -> "box-energy"
  TextureGreenHopo               -> "hopo-green"
  TextureRedHopo                 -> "hopo-red"
  TextureYellowHopo              -> "hopo-yellow"
  TextureBlueHopo                -> "hopo-blue"
  TextureOrangeHopo              -> "hopo-orange"
  TextureEnergyHopo              -> "hopo-energy"
  TextureGreenTap                -> "tap-green"
  TextureRedTap                  -> "tap-red"
  TextureYellowTap               -> "tap-yellow"
  TextureBlueTap                 -> "tap-blue"
  TextureOrangeTap               -> "tap-orange"
  TextureEnergyTap               -> "tap-energy"
  TextureRedCymbal               -> "cymbal-red"
  TextureYellowCymbal            -> "cymbal-yellow"
  TextureBlueCymbal              -> "cymbal-blue"
  TextureGreenCymbal             -> "cymbal-green"
  TexturePurpleCymbal            -> "cymbal-purple"
  TextureOrangeCymbal            -> "cymbal-orange"
  TextureEnergyCymbal            -> "cymbal-energy"
  TextureLine1                   -> "line-1"
  TextureLine2                   -> "line-2"
  TextureLine3                   -> "line-3"
  TextureTargetGreen             -> "target-green"
  TextureTargetRed               -> "target-red"
  TextureTargetYellow            -> "target-yellow"
  TextureTargetBlue              -> "target-blue"
  TextureTargetOrange            -> "target-orange"
  TextureTargetPurple            -> "target-purple"
  TextureTargetGreenLight        -> "target-green-light"
  TextureTargetRedLight          -> "target-red-light"
  TextureTargetYellowLight       -> "target-yellow-light"
  TextureTargetBlueLight         -> "target-blue-light"
  TextureTargetOrangeLight       -> "target-orange-light"
  TextureTargetPurpleLight       -> "target-purple-light"
  TextureTargetOrangeLeft        -> "target-orange-left"
  TextureTargetOrangeLeftLight   -> "target-orange-left-light"
  TextureTargetOrangeCenter      -> "target-orange-center"
  TextureTargetOrangeCenterLight -> "target-orange-center-light"
  TextureTargetOrangeRight       -> "target-orange-right"
  TextureTargetOrangeRightLight  -> "target-orange-right-light"
  TextureNumber0                 -> "number-0"
  TextureNumber1                 -> "number-1"
  TextureNumber2                 -> "number-2"
  TextureNumber3                 -> "number-3"
  TextureNumber4                 -> "number-4"
  TextureNumber5                 -> "number-5"
  TextureNumber6                 -> "number-6"
  TextureNumber7                 -> "number-7"
  TextureNumber8                 -> "number-8"
  TextureNumber9                 -> "number-9"
  TextureLaneGreen               -> "lane-green"
  TextureLaneRed                 -> "lane-red"
  TextureLaneYellow              -> "lane-yellow"
  TextureLaneBlue                -> "lane-blue"
  TextureLaneOrange              -> "lane-orange"
  TextureLanePurple              -> "lane-purple"
  TextureRS0                     -> "rs-0"
  TextureRS1                     -> "rs-1"
  TextureRS2                     -> "rs-2"
  TextureRS3                     -> "rs-3"
  TextureRS4                     -> "rs-4"
  TextureRS5                     -> "rs-5"
  TextureRS6                     -> "rs-6"
  TextureRS7                     -> "rs-7"
  TextureRS8                     -> "rs-8"
  TextureRS9                     -> "rs-9"
  TextureRS10                    -> "rs-10"
  TextureRS11                    -> "rs-11"
  TextureRS12                    -> "rs-12"
  TextureRS13                    -> "rs-13"
  TextureRS14                    -> "rs-14"
  TextureRS15                    -> "rs-15"
  TextureRS16                    -> "rs-16"
  TextureRS17                    -> "rs-17"
  TextureRS18                    -> "rs-18"
  TextureRS19                    -> "rs-19"
  TextureRS20                    -> "rs-20"
  TextureRS21                    -> "rs-21"
  TextureRS22                    -> "rs-22"
  TextureRS23                    -> "rs-23"
  TextureRS24                    -> "rs-24"
  TextureRSRed                   -> "rs-red"
  TextureRSYellow                -> "rs-yellow"
  TextureRSBlue                  -> "rs-blue"
  TextureRSOrange                -> "rs-orange"
  TextureRSGreen                 -> "rs-green"
  TextureRSPurple                -> "rs-purple"
  TextureRSHopo                  -> "rs-hopo"
  TextureRSTap                   -> "rs-tap"
  TextureRSPalmMute              -> "rs-palm-mute"
  TextureRSFretHandMute          -> "rs-fret-hand-mute"
  TextureOverlayGhost            -> "overlay-ghost"
  TextureOverlayAccent           -> "overlay-accent"

setFramebufferSize :: (MonadIO m) => Framebuffers -> GLsizei -> GLsizei -> m ()
setFramebufferSize fbufs w h = case fbufs of
  SimpleFramebuffer{..} -> do
    glBindTexture GL_TEXTURE_2D simpleFBOTex
    glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE nullPtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glBindRenderbuffer GL_RENDERBUFFER simpleFBORender
    glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH24_STENCIL8 w h
  MSAAFramebuffers{..} -> do
    glBindTexture GL_TEXTURE_2D_MULTISAMPLE msaaFBOTex
    glTexImage2DMultisample GL_TEXTURE_2D_MULTISAMPLE multisamples GL_RGBA w h GL_TRUE
    glBindRenderbuffer GL_RENDERBUFFER msaaFBORender
    glRenderbufferStorageMultisample GL_RENDERBUFFER multisamples GL_DEPTH24_STENCIL8 w h
    glBindTexture GL_TEXTURE_2D intermediateFBOTex
    glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE nullPtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR

deleteGLStuff :: (MonadIO m) => GLStuff -> m ()
deleteGLStuff gl = liftIO do
  glDeleteProgram gl.objectShader
  glDeleteProgram gl.quadShader
  withArrayLen (map (.objVAO) $ [gl.boxObject, gl.flatObject, gl.quadObject] ++ map snd gl.models)
    $ glDeleteVertexArrays . fromIntegral
  case gl.framebuffers of
    MSAAFramebuffers{..} -> do
      withArrayLen [msaaFBO, intermediateFBO] $ glDeleteFramebuffers . fromIntegral
      withArrayLen [msaaFBOTex, intermediateFBOTex] $ glDeleteTextures . fromIntegral
      withArrayLen [msaaFBORender] $ glDeleteRenderbuffers . fromIntegral
    SimpleFramebuffer{..} -> do
      withArrayLen [simpleFBO] $ glDeleteFramebuffers . fromIntegral
      withArrayLen [simpleFBOTex] $ glDeleteTextures . fromIntegral
      withArrayLen [simpleFBORender] $ glDeleteRenderbuffers . fromIntegral
  mapM_ (freeTexture . snd) gl.textures
  mapM_ freeTexture $ Map.elems gl.imageBGs
  forM_ gl.fontFaces \font -> do
    readIORef font.fontGlyphs >>= mapM_ (freeTexture . snd) . HM.elems
    ft_Done_Face font.fontFace
  ft_Done_FreeType gl.fontLib
  stopVideoLoaders gl

stopVideoLoaders :: (MonadIO m) => GLStuff -> m ()
stopVideoLoaders gl = liftIO do
  forM_ (Map.elems gl.videoBGs) \vh -> frameMessage vh.frameLoader CloseLoader

data WindowDims = WindowDims Int Int

data QuadDrawSettings = QuadDrawSettings
  { fxaa        :: Bool
  , fadeHorizon :: Bool
  , postProcess :: Maybe (PostProcess3, Double, PostProcess3)
  }

drawTexture :: GLStuff -> QuadDrawSettings -> WindowDims -> Texture -> Maybe (V4 Float) -> V2 Int -> Int -> IO ()
drawTexture gl settings (WindowDims screenW screenH) (Texture tex w h) maybeColor (V2 x y) scale = do

  let fadeBottom = if settings.fadeHorizon then gl.gfxConfig.view.track_fade.bottom else 1
      fadeTop    = if settings.fadeHorizon then gl.gfxConfig.view.track_fade.top    else 1

  glUseProgram gl.quadShader
  glActiveTexture GL_TEXTURE0
  checkGL "glBindTexture" $ glBindTexture GL_TEXTURE_2D tex
  glBindVertexArray gl.quadObject.objVAO
  let scaleX = fromIntegral (w * scale) / fromIntegral screenW
      scaleY = fromIntegral (h * scale) / fromIntegral screenH
      translateX = (fromIntegral x / fromIntegral screenW) * 2 - 1 + scaleX
      translateY = (fromIntegral y / fromIntegral screenH) * 2 - 1 + scaleY
  sendUniformName gl.quadShader "transform"
    (   translate4 (V3 translateX translateY 0)
    !*! L.scaled (V4 scaleX scaleY 1 1)
    :: M44 Float
    )
  sendUniformName gl.quadShader "inResolution" $ V2
    (fromIntegral (w * scale) :: Float)
    (fromIntegral (h * scale) :: Float)
  sendUniformName gl.quadShader "startFade" fadeBottom
  sendUniformName gl.quadShader "endFade" fadeTop
  sendUniformName gl.quadShader "doFXAA" $ gl.fxaaEnabled && settings.fxaa
  case maybeColor of
    Nothing -> sendUniformName gl.quadShader "colorMode" (2 :: GLuint)
    Just color -> do
      sendUniformName gl.quadShader "colorMode" (1 :: GLuint)
      sendUniformName gl.quadShader "color" color
  case settings.postProcess of
    Nothing -> sendUniformName gl.quadShader "doPostProcess" False
    Just (start, frac, end) -> do
      sendUniformName gl.quadShader "doPostProcess"       True
      sendUniformName gl.quadShader "postProcessStart"    (fromIntegral $ fromEnum start :: GLuint)
      sendUniformName gl.quadShader "postProcessFraction" (realToFrac frac :: Float)
      sendUniformName gl.quadShader "postProcessEnd"      (fromIntegral $ fromEnum end   :: GLuint)
  checkGL "glDrawElements" $ glDrawElements GL_TRIANGLES gl.quadObject.objVertexCount GL_UNSIGNED_INT nullPtr

drawColor :: GLStuff -> WindowDims -> RenderObject -> V2 Int -> V2 Int -> V4 Float -> IO ()
drawColor gl (WindowDims screenW screenH) object (V2 x y) (V2 w h) color = do
  glUseProgram gl.quadShader
  glBindVertexArray object.objVAO
  let scaleX = fromIntegral w / fromIntegral screenW
      scaleY = fromIntegral h / fromIntegral screenH
      translateX = (fromIntegral x / fromIntegral screenW) * 2 - 1 + scaleX
      translateY = (fromIntegral y / fromIntegral screenH) * 2 - 1 + scaleY
  sendUniformName gl.quadShader "transform"
    (   translate4 (V3 translateX translateY 0)
    !*! L.scaled (V4 scaleX scaleY 1 1)
    :: M44 Float
    )
  sendUniformName gl.quadShader "inResolution" $ V2
    (fromIntegral w :: Float)
    (fromIntegral h :: Float)
  sendUniformName gl.quadShader "startFade" (1 :: Float)
  sendUniformName gl.quadShader "endFade" (1 :: Float)
  sendUniformName gl.quadShader "colorMode" (0 :: GLuint)
  sendUniformName gl.quadShader "color" color
  checkGL "glDrawElements" $ glDrawElements GL_TRIANGLES object.objVertexCount GL_UNSIGNED_INT nullPtr

backgroundBoxRadius :: Int
backgroundBoxRadius = 10

-- Box with some rounded corners
drawBackgroundBox :: GLStuff -> WindowDims -> V2 Int -> V2 Int -> V4 Float -> Int -> [Corner] -> IO ()
drawBackgroundBox glStuff dims (V2 x y) (V2 w h) color radius roundedCorners = do
  let withCorner :: Corner -> (RenderObject -> IO ()) -> IO ()
      withCorner corner fn = if elem corner roundedCorners
        then mapM_ fn $ lookup corner glStuff.cornerObjects
        else fn glStuff.quadObject
  -- top left corner
  withCorner TL \object -> drawColor glStuff dims object
    (V2 x (y + h - radius))
    (V2 radius radius)
    color
  -- top edge
  drawColor glStuff dims glStuff.quadObject
    (V2 (x + radius) (y + h - radius))
    (V2 (w - radius * 2) radius)
    color
  -- top right corner
  withCorner TR \object -> drawColor glStuff dims object
    (V2 (x + w - radius) (y + h - radius))
    (V2 radius radius)
    color
  -- center chunk
  drawColor glStuff dims glStuff.quadObject
    (V2 x (y + radius))
    (V2 w (h - radius * 2))
    color
  -- bottom left corner
  withCorner BL \object -> drawColor glStuff dims object
    (V2 x y)
    (V2 radius radius)
    color
  -- bottom edge
  drawColor glStuff dims glStuff.quadObject
    (V2 (x + radius) y)
    (V2 (w - radius * 2) radius)
    color
  -- bottom right corner
  withCorner BR \object -> drawColor glStuff dims object
    (V2 (x + w - radius) y)
    (V2 radius radius)
    color

data BackgroundMode
  = BackgroundFill -- preserve aspect ratio, texture is drawn at least as big as screen (may be clipped)
  | BackgroundFit  -- preserve aspect ratio, texture is drawn no bigger than screen (may be boxed)

drawBackground :: GLStuff -> WindowDims -> BackgroundMode -> Texture -> IO ()
drawBackground gl (WindowDims screenW screenH) mode (Texture tex w h) = do
  glUseProgram gl.quadShader
  glActiveTexture GL_TEXTURE0
  checkGL "glBindTexture" $ glBindTexture GL_TEXTURE_2D tex
  glBindVertexArray gl.quadObject.objVAO
  let textureRatio = fromIntegral w       / fromIntegral h       :: Float
      screenRatio  = fromIntegral screenW / fromIntegral screenH :: Float
      scaleX = case mode of
        BackgroundFill -> if textureRatio > screenRatio
          then textureRatio / screenRatio -- texture may clip left/right
          else 1                          -- texture may clip top/bottom
        BackgroundFit -> if textureRatio > screenRatio
          then 1
          else textureRatio / screenRatio -- TODO check
      scaleY = case mode of
        BackgroundFill -> if textureRatio > screenRatio
          then 1                          -- texture may clip left/right
          else screenRatio / textureRatio -- texture may clip top/bottom
        BackgroundFit -> if textureRatio > screenRatio
          then screenRatio / textureRatio -- TODO check
          else 1
  sendUniformName gl.quadShader "transform"
    (L.scaled (V4 scaleX scaleY 1 1) :: M44 Float)
  sendUniformName gl.quadShader "startFade" (1 :: Float)
  sendUniformName gl.quadShader "endFade" (1 :: Float)
  sendUniformName gl.quadShader "doFXAA" False
  sendUniformName gl.quadShader "colorMode" (2 :: GLuint)
  checkGL "glDrawElements" $ glDrawElements GL_TRIANGLES gl.quadObject.objVertexCount GL_UNSIGNED_INT nullPtr

freeTexture :: (MonadIO m) => Texture -> m ()
freeTexture (Texture tex _ _) = liftIO $ with tex $ glDeleteTextures 1

-- | Split up the available space to show tracks at the largest possible size.
-- Returns [(x, y, w, h)] in GL space (so bottom left corner is (0, 0)).
splitHighwaySpace :: Int -> Float -> WindowDims -> [(Int, Int, Int, Int)]
splitHighwaySpace 0 _                _                = []
splitHighwaySpace n heightWidthRatio (WindowDims w h) = let
  fi :: Int -> Float
  fi = fromIntegral
  heightScore rows = let
    cols = ceiling $ fi n / fi rows
    colWidth = fi w / fi cols
    colHeight = fi h / fi rows
    colHeightUsed = min colHeight $ colWidth * heightWidthRatio
    in colHeightUsed
  bestRows = snd $ maximum [ ((heightScore rows, -rows), rows) | rows <- [1..n] ]
  bestCols = ceiling $ fi n / fi bestRows
  pieces :: Int -> Int -> [(Int, Int)]
  pieces p whole = let
    edges = [ round $ fi whole * (fi i / fi p) | i <- [0 .. p] ]
    in zip edges $ tail edges
  makeRows _      []                = []
  makeRows spaces ((y1, y2) : rows) = let
    cols = min spaces bestCols
    thisRow = do
      (x1, x2) <- pieces cols w
      let maxHeight = round $ fi (x2 - x1) * heightWidthRatio
      return (x1, y1, x2 - x1, min maxHeight $ y2 - y1)
    in thisRow ++ makeRows (spaces - cols) rows
  in makeRows n $ reverse $ pieces bestRows h

splitSpace
  :: GLStuff
  -> [(T.Text, PreviewTrack)]
  -> WindowDims
  -> ([((Int, Int, Int, Int), (T.Text, PreviewTrack))], [((Int, Int, Int, Int), (T.Text, PreviewTrack))])
splitSpace _  []    _                = ([], [])
splitSpace gl trks  (WindowDims w h) = let
  (vocals, highways) = flip partition trks \case
    (_, PreviewVocal _) -> True
    _                   -> False
  defaultVocalHeight = round $ 80 * gl.scaleUI
  vocalHeight = case length vocals of
    0   -> defaultVocalHeight
    len -> min defaultVocalHeight $ quot (fromIntegral h - defaultVocalHeight) len
  vocalSpaces = [ (0, h - vocalHeight * i, w, vocalHeight) | i <- [1..] ]
  highwaySpaces = splitHighwaySpace (length highways)
    gl.gfxConfig.view.height_width_ratio
    (WindowDims w $ h - vocalHeight * length vocals)
  in (zip vocalSpaces vocals, zip highwaySpaces highways)

setUpTrackView :: GLStuff -> WindowDims -> IO (M44 Float)
setUpTrackView gl (WindowDims w h) = do
  glClear GL_DEPTH_BUFFER_BIT
  glUseProgram gl.objectShader
  let viewPosn = gl.gfxConfig.view.camera.position
      tiltDown = gl.gfxConfig.view.camera.rotate
      view, projection :: M44 Float
      view
        = L.mkTransformation (L.axisAngle (V3 1 0 0) (degrees tiltDown)) 0
        !*! translate4 (negate viewPosn)
        -- note, this translates then rotates (can't just give V3 to mkTransformation)
      projection = L.perspective
        (degrees gl.gfxConfig.view.camera.fov)
        (fromIntegral w / fromIntegral h)
        gl.gfxConfig.view.camera.near
        gl.gfxConfig.view.camera.far
  sendUniformName gl.objectShader "view" view
  sendUniformName gl.objectShader "projection" projection
  sendUniformName gl.objectShader "viewPos" viewPosn
  return $ projection !*! view

drawDrumPlayFull
  :: GLStuff
  -> WindowDims
  -> Double
  -> Double
  -> [EliteDrumLayoutHint]
  -> EliteDrumPlayState Double
  -> IO ()
drawDrumPlayFull gl dims@(WindowDims wWhole hWhole) time speed layout dps = do
  glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
  case gl.gfxConfig.view.background of
    V4 r g b a -> glClearColor r g b a
  glClear GL_COLOR_BUFFER_BIT

  glDepthFunc GL_LESS
  void $ setUpTrackView gl dims
  drawEliteDrumPlay gl time speed layout dps

  glDepthFunc GL_ALWAYS
  let tdgs = case dps.events of
        (_, (_, s)) : _ -> s
        _               -> initialEDState
  drawTimeBox gl dims
    [ "Score: " <> T.pack (show tdgs.score)
    , "Combo: " <> T.pack (show tdgs.combo)
    ]

drawFivePlayFull
  :: GLStuff
  -> WindowDims
  -> Double
  -> Double
  -> GuitarPlayState Double
  -> IO ()
drawFivePlayFull gl dims@(WindowDims wWhole hWhole) time speed gps = do
  glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
  case gl.gfxConfig.view.background of
    V4 r g b a -> glClearColor r g b a
  glClear GL_COLOR_BUFFER_BIT

  glDepthFunc GL_LESS
  void $ setUpTrackView gl dims
  drawFivePlay gl time speed gps

  glDepthFunc GL_ALWAYS
  let ggs = case gps.events of
        (_, (_, s)) : _ -> s
        _               -> initialGuitarState
  drawTimeBox gl dims
    [ "Score: " <> T.pack (show ggs.score)
    , "Combo: " <> T.pack (show ggs.combo)
    , "Accuracy: " <> let
      offsets = take 50 $ ggs.noteResults >>= \case
        (t1, GuitarHitStrum t2) -> return $ t2 - t1
        (t1, GuitarHitHOPO  t2) -> return $ t2 - t1
        _                       -> []
      in case offsets of
        []    -> ""
        _ : _ -> let
          acc = sum offsets / fromIntegral (length offsets)
          in T.pack (show (round (acc * 1000) :: Int)) <> " ms"
    ]

getGlyph :: GLStuff -> (C.TextConfig -> C.FontConfig) -> Char -> IO (Maybe (FT_GlyphSlotRec, Texture))
getGlyph gl getFontConfig originalChar = let
  fontConfig = getFontConfig gl.gfxConfig.text
  in case Map.lookup (fontConfig.font, fontConfig.size) gl.fontFaces of
    Nothing   -> return Nothing
    Just font -> do
      table <- readIORef font.fontGlyphs
      case HM.lookup originalChar table of
        Just pair -> return $ Just pair
        Nothing   -> do

          (c, smile) <- case originalChar of
            '‿' -> do
              -- Check if there is actually an undertie in the font
              index <- ft_Get_Char_Index font.fontFace $ fromIntegral $ fromEnum originalChar
              if index == 0
                then return ('_', True) -- no undertie, we'll make a fake one
                else return (originalChar, False)
            _ -> return (originalChar, False)
          let applySmile = if smile
                then underscoreToUndertie
                else id

          ft_Load_Char font.fontFace (fromIntegral $ fromEnum c) FT_LOAD_RENDER
          gsr <- peek font.fontSlot
          case bPixel_mode $ gsrBitmap gsr of
            FT_PIXEL_MODE_GRAY -> do
              let w = bWidth $ gsrBitmap gsr
                  h = bRows $ gsrBitmap gsr
              fptr <- newForeignPtr_ $ bBuffer $ gsrBitmap gsr
              v <- VS.freeze $ MV.unsafeFromForeignPtr0 fptr $ fromIntegral $ w * h
              tex <- loadTexture False $ pixelMap (PixelRGBA8 255 255 255) $ applySmile $ Image
                { imageWidth  = fromIntegral w
                , imageHeight = fromIntegral h
                , imageData   = v
                }
              writeIORef font.fontGlyphs $ HM.insert originalChar (gsr, tex) table
              return $ Just (gsr, tex)
            _ -> do
              putStrLn "freetype output isn't FT_PIXEL_MODE_GRAY"
              return Nothing

underscoreToUndertie :: Image Word8 -> Image Word8
underscoreToUndertie img = let
  newWidth    = img.imageWidth
  newHeight   = round (fromIntegral img.imageHeight * 1.5 :: Double)
  scaleUp     = 100
  largeWidth  = newWidth  * scaleUp
  largeHeight = newHeight * scaleUp
  largeImage  = generateImage generator largeWidth largeHeight
  -- Claude Code wrote this
  generator :: Int -> Int -> Word8
  generator x y = let
    -- Convert to normalized coordinates [0, 1]
    nx, ny :: Double
    nx = fromIntegral x / fromIntegral (largeWidth  - 1)
    ny = fromIntegral y / fromIntegral (largeHeight - 1)
    -- First arc: parabola from top left (0,0) through center (0.5, 0.5) to top right (1,0)
    arc1Y = 2 * nx * (1 - nx)
    -- Second arc: parabola from top left (0,0) through bottom mid (0.5, 1) to top right (1,0)
    arc2Y = 4 * nx * (1 - nx)
    -- Check if point is between the two arcs
    inSmile = ny >= arc1Y && ny <= arc2Y
    in if inSmile then 255 else 0
  in IR.resize IR.defaultOptions newWidth newHeight largeImage

prepareText :: GLStuff -> (C.TextConfig -> C.FontConfig) -> T.Text -> IO [(FT_GlyphSlotRec, Texture)]
prepareText glStuff getFontConfig = fmap catMaybes . mapM (getGlyph glStuff getFontConfig) . T.unpack

drawTextLine
  :: GLStuff
  -> WindowDims
  -> [((FT_GlyphSlotRec, Texture), Maybe (V4 Float))]
  -> V2 Int
  -> IO ()
drawTextLine glStuff dims chars penStart = let
  drawLoop _   [] = return ()
  drawLoop pen (((gsr, tex), maybeColor) : rest) = do
    let penOffset  = fromIntegral <$> V2 (gsrBitmap_left gsr) (gsrBitmap_top gsr)
        penOffsetH = V2 0 (negate tex.textureHeight)
        penAdvance = fromIntegral . (`shiftR` 6) <$> V2 (vX $ gsrAdvance gsr) (vY $ gsrAdvance gsr)
    drawTexture glStuff QuadDrawSettings
      { fadeHorizon = False
      , fxaa = False
      , postProcess = Nothing
      } dims tex maybeColor (pen + penOffset + penOffsetH) 1
    drawLoop (pen + penAdvance) rest
  in drawLoop penStart chars

drawTimeBox
  :: GLStuff
  -> WindowDims
  -> [T.Text]
  -> IO ()
drawTimeBox gl dims@(WindowDims _ hWhole) timeLines = do
  texLines <- mapM (prepareText gl (.timeBox)) timeLines
  let maxTextWidth = foldr max 0 $ map (sum . map (vX . gsrAdvance . fst)) texLines
      boxWidth = 2 * margin + fromIntegral (maxTextWidth `shiftR` 6)
      boxHeight = (margin + fontSize) * length texLines + margin
      margin   = round $ fromIntegral gl.gfxConfig.text.timeBox.margin * gl.scaleUI
      fontSize = round $ fromIntegral gl.gfxConfig.text.timeBox.size   * gl.scaleUI
      backgroundColor = V4 0 0 0 0.5
  drawBackgroundBox gl dims
    (V2 0 (hWhole - boxHeight))
    (V2 boxWidth boxHeight)
    backgroundColor
    backgroundBoxRadius
    [BR]
  forM_ (zip [1..] texLines) \(i, texLine) -> do
    drawTextLine gl dims (map (, Nothing) texLine) $ V2 margin $ hWhole - (margin + fontSize) * i

drawVenueDebug :: GLStuff -> WindowDims -> Double -> VenueState Double -> IO ()
drawVenueDebug gl dims@(WindowDims _wWhole hWhole) _time venueState = do
  texLines <- mapM (prepareText gl (.timeBox))
    [ T.pack $ show venueState.lighting
    , T.pack $ show venueState.postProcessing
    ]
  forM_ (zip [1..] texLines) \(i, texLine) -> do
    drawTextLine gl dims (map (, Nothing) texLine) $ V2 300 $ hWhole - 50 * i

drawViaFramebuffer :: GLStuff -> WindowDims -> QuadDrawSettings -> (Int, Int, Int, Int) -> IO a -> IO a
drawViaFramebuffer gl dims@(WindowDims wWhole hWhole) settings (x, y, w, h) drawFn = do
  glBindFramebuffer GL_FRAMEBUFFER case gl.framebuffers of
    SimpleFramebuffer{..} -> simpleFBO
    MSAAFramebuffers{..}  -> msaaFBO
  setFramebufferSize gl.framebuffers
    (fromIntegral w) (fromIntegral h)
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glClearColor 0 0 0 0
  glClear GL_COLOR_BUFFER_BIT

  result <- drawFn

  case gl.framebuffers of
    SimpleFramebuffer{..} -> do

      glBindFramebuffer GL_FRAMEBUFFER 0
      glClear GL_DEPTH_BUFFER_BIT
      glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
      drawTexture gl settings dims (Texture simpleFBOTex w h) Nothing (V2 x y) 1

    MSAAFramebuffers{..} -> do

      glBindFramebuffer GL_READ_FRAMEBUFFER msaaFBO
      glBindFramebuffer GL_DRAW_FRAMEBUFFER intermediateFBO
      glBlitFramebuffer
        0 0 (fromIntegral w) (fromIntegral h)
        0 0 (fromIntegral w) (fromIntegral h)
        GL_COLOR_BUFFER_BIT GL_NEAREST

      glBindFramebuffer GL_FRAMEBUFFER 0
      glClear GL_DEPTH_BUFFER_BIT
      glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
      drawTexture gl settings dims (Texture intermediateFBOTex w h) Nothing (V2 x y) 1

  return result

drawTracks
  :: GLStuff
  -> WindowDims
  -> Double
  -> Double
  -> Maybe PreviewBG
  -> [EliteDrumLayoutHint]
  -> [(T.Text, PreviewTrack)]
  -> Maybe AudioHandle
  -> IO ()
drawTracks gl dims@(WindowDims wWhole hWhole) time speed bg userLayout trks mAudioHandle = do
  glBindFramebuffer GL_FRAMEBUFFER 0
  glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
  case bg of
    Just _  -> glClearColor 0 0 0 255
    Nothing -> case gl.gfxConfig.view.background of
      V4 r g b a -> glClearColor r g b a
  glClear GL_COLOR_BUFFER_BIT

  glDepthFunc GL_ALWAYS -- turn off z to draw backgrounds + time box

  let waveform = when gl.waveform $ forM_ mAudioHandle \audioHandle -> checkGL "draw waveform" do
        waveformData <- audioHandle.audioWaveform
        unless (VS.null waveformData) do
          drawWaveform gl dims waveformData

  -- TODO should allow user choice between fit and fill background modes
  case bg of
    Just (PreviewBGVideo vi) -> case Map.lookup vi gl.videoBGs of
      Just videoHandle -> do
        -- TODO maybe separate out the timestamp updates from drawing
        frameMessage videoHandle.frameLoader $ RequestFrame time
        mtex <- getFrame videoHandle.frameLoader >>= \case
          Nothing -> return Nothing
          Just (timeNew, image) -> readIORef videoHandle.texture >>= \case
            Nothing -> do
              tex <- loadTexture True image
              writeIORef videoHandle.texture $ Just (timeNew, tex)
              return $ Just tex
            Just (timeCurrent, tex) -> do
              when (timeNew /= timeCurrent) do
                updateTexture image tex
                writeIORef videoHandle.texture $ Just (timeNew, tex)
              return $ Just tex
        forM_ mtex $ drawBackground gl dims BackgroundFit
      Nothing -> return ()
    Just (PreviewBGImage f) -> forM_ (Map.lookup f gl.imageBGs) $ drawBackground gl dims BackgroundFit
    Just (PreviewBGVenue venue) -> do
      let venueState = mapSingleState time venue
          postProc = case venueState.postProcessing of
            LightCut       _       y       -> (y, 0, y)
            LightStatic    x               -> (x, 0, x)
            LightFade      (t1, x) (t2, y) -> (x, (time - t1) / (t2 - t1), y)
            LightStartFade (_ , x) _       -> (x, 0, x)
            LightEndFade   _       (_ , y) -> (y, 0, y)
      drawViaFramebuffer gl dims QuadDrawSettings
        { fadeHorizon = False
        , fxaa = False
        , postProcess = Just postProc
        } (0, 0, wWhole, hWhole) do
          case gl.gfxConfig.view.background of
            V4 r g b a -> glClearColor r g b a
          glClear GL_COLOR_BUFFER_BIT
          -- drawVenueDebug gl dims time venueState
          waveform
    Nothing -> waveform

  glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
  forM_ gl.previewSong \psong -> let
    songLength = U.applyTempoMap (previewTempo psong) (previewTiming psong).timingEnd
    currentBeats = U.unapplyTempoMap (previewTempo psong) (realToFrac time :: U.Seconds)
    currentMB = showPosition (previewMeasures psong) currentBeats
    lengthMB = showPosition (previewMeasures psong) (previewTiming psong).timingEnd
    currentBPM = realToFrac (U.getTempoAtTime (previewTempo psong) currentBeats) * 60 * realToFrac speed :: Rational
    -- round into Fixed type, rather than realToFrac which uses floor
    bpmMilli = MkFixed $ round $ currentBPM * 1000 :: Milli
    bpmNumSpaces
      | bpmMilli < 10   = 3
      | bpmMilli < 100  = 2
      | bpmMilli < 1000 = 1
      | otherwise       = 0
    bpmString = "BPM:" <> replicate bpmNumSpaces ' ' <> show bpmMilli
    in drawTimeBox gl dims $
      [ showTimestamp (realToFrac time) <> " / " <> showTimestamp songLength
      , T.pack $ currentMB <> " / " <> lengthMB
      , T.pack bpmString
      ] <> toList (fmap snd $ Map.lookupLE time $ previewSections psong)

  let (vocalSpaces, highwaySpaces) = splitSpace gl trks dims

  forM_ vocalSpaces \(space, (_name, trk)) -> checkGL "draw vocals" case trk of
    PreviewVocal m -> drawLyrics gl dims space time m
    _              -> return () -- shouldn't happen

  glDepthFunc GL_LESS
  glClear GL_DEPTH_BUFFER_BIT

  mvps <- forM highwaySpaces \(xywh@(_x, _y, w, h), (_name, trk)) -> checkGL "draw" do
    drawViaFramebuffer gl dims QuadDrawSettings
      { fadeHorizon = True
      , fxaa = True
      , postProcess = Nothing
      } xywh do
        mvp <- setUpTrackView gl (WindowDims w h)
        case trk of
          PreviewDrums        mode m -> drawDrums      gl time speed mode                   m
          PreviewDrumsElite layout m -> drawEliteDrums gl time speed (userLayout <> layout) m
          PreviewFive              m -> drawFive       gl time speed                        m
          PreviewPG              t m -> drawPG         gl time speed t                      m
          PreviewMania           p m -> drawMania      gl time speed p                      m
          PreviewVocal             _ -> return () -- shouldn't happen
        return mvp

  glDepthFunc GL_ALWAYS -- turn off z to draw track labels

  glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
  forM_ (zip highwaySpaces mvps) \(((x, y, w, h), (name, _trk)), mvp) -> checkGL "draw track label" do
    texLine <- prepareText gl (.trackLabel) name

    -- use view/projection matrix to get pixel position of the closer edge of the strikeline
    let V4 _ nowY _ nowW = mvp L.!* V4 0 gl.gfxConfig.track.y gl.gfxConfig.track.targets.z_past 1
        ndcY             = nowY / nowW
        strikeY          = round $ (ndcY + 1) * 0.5 * fromIntegral h :: Int

        lineWidth = fromIntegral $ sum (map (vX . gsrAdvance . fst) texLine) `shiftR` 6 :: Int
        boxWidth  = 2 * margin + lineWidth
        boxHeight = 2 * margin + fontSize - 5
        boxX      = x + quot (w - boxWidth) 2
        boxY      = y + quot (strikeY - boxHeight) 2
        textX     = boxX + margin
        textY     = boxY + margin
        margin    = round $ fromIntegral gl.gfxConfig.text.trackLabel.margin * gl.scaleUI
        fontSize  = round $ fromIntegral gl.gfxConfig.text.trackLabel.size   * gl.scaleUI

    drawBackgroundBox gl dims
      (V2 boxX boxY)
      (V2 boxWidth boxHeight)
      (V4 0 0 0 0.5)
      backgroundBoxRadius
      [TL, TR, BL, BR]
    drawTextLine gl dims (map (, Nothing) texLine) (V2 textX textY)

drawWaveform :: GLStuff -> WindowDims -> VS.Vector Float -> IO ()
drawWaveform gl (WindowDims wWhole hWhole) waveformData = do

  let waveformWidth = wWhole
      waveformHeight = hWhole
      waveformX = 0
      waveformY = 0
      numSamples = VS.length waveformData

  -- Draw waveform samples as vertical lines
  when (numSamples > 0) do
    let centerY = waveformY + waveformHeight `div` 2
        maxHeight = fromIntegral waveformHeight / 2.0 - 5  -- Leave some margin

    forM_ [0 .. waveformWidth - 1] \i -> do
      let -- Map pixel position to sample position with interpolation
          samplePos = fromIntegral i * fromIntegral (numSamples - 1) / fromIntegral (waveformWidth - 1)
          sampleIdx = floor samplePos
          sampleFrac = samplePos - fromIntegral sampleIdx

          -- Get samples for interpolation
          sample1 = waveformData VS.! min sampleIdx (numSamples - 1)
          sample2 = if sampleIdx + 1 < numSamples
                   then waveformData VS.! (sampleIdx + 1)
                   else sample1

          -- Linear interpolation
          sample = sample1 + sampleFrac * (sample2 - sample1)
          sampleHeight = abs sample * maxHeight
          lineTop = centerY - round sampleHeight
          lineBottom = centerY + round sampleHeight
          lineX = waveformX + i

      -- Draw vertical line representing the waveform sample
      drawColor gl (WindowDims wWhole hWhole)
        gl.quadObject
        (V2 lineX lineTop)
        (V2 1 (lineBottom - lineTop + 1))
        (V4 0 0 0 0.7) -- Semi-transparent black

checkGL :: (MonadIO m) => String -> m a -> m a
checkGL s f = do
  void glGetError
  x <- f
  desc <- glGetError <&> \case
    GL_NO_ERROR          -> Nothing
    GL_INVALID_ENUM      -> Just "Invalid enum"
    GL_INVALID_VALUE     -> Just "Invalid value"
    GL_INVALID_OPERATION -> Just "Invalid operation"
    GL_OUT_OF_MEMORY     -> Just "Out of memory"
    _                    -> Just "Unknown error"
  liftIO $ forM_ desc \err -> putStrLn $ s <> ": " <> err
  return x
