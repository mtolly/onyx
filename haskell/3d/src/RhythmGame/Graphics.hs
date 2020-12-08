{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module RhythmGame.Graphics where

import           Build                          (loadYaml)
import           Codec.Picture
import qualified Codec.Wavefront                as Obj
import           Config                         (VideoInfo (..))
import           Control.Arrow                  (second)
import           Control.Monad                  (forM, forM_, guard, void, when)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Resource   (register, runResourceT)
import           Control.Monad.Trans.StackTrace (QueueLog, SendMessage,
                                                 StackTraceT, fatal,
                                                 getQueueLog, inside,
                                                 mapStackTraceT, stackIO, warn)
import qualified Data.ByteString                as B
import           Data.Foldable                  (traverse_)
import           Data.IORef                     (IORef, newIORef, readIORef,
                                                 writeIORef)
import           Data.List                      (findIndex, partition, sort)
import           Data.List.HT                   (partitionMaybe)
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (fromMaybe, isJust, catMaybes)
import qualified Data.Set                       as Set
import qualified Data.Vector                    as V
import qualified Data.Vector.Storable           as VS
import           Foreign                        hiding (void)
import           Foreign.C
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear                         (M44, V2 (..), V3 (..), V4 (..),
                                                 (!*!))
import qualified Linear                         as L
import           Preferences                    (Preferences (..),
                                                 readPreferences)
import           Resources                      (getResourcesPath)
import qualified RhythmGame.Graphics.Config     as C
import           RhythmGame.Graphics.Video
import           RhythmGame.PNF
import           RhythmGame.Track
import           RockBand.Codec.Beat
import qualified RockBand.Codec.Drums           as D
import qualified RockBand.Codec.Five            as F
import qualified RockBand.Codec.ProGuitar       as PG
import           RockBand.Common                (StrumHOPOTap (..), each)
import           System.Directory               (doesFileExist)
import           System.FilePath                ((<.>), (</>))

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
drawObject GLStuff{..} obj posn colorSource alpha lightOffset = do
  let colorType    = 1 :: GLuint
      texture1Type = 2 :: GLuint
      texture2Type = 3 :: GLuint
      texture3Type = 4 :: GLuint
      thisObject = case obj of
        Box           -> boxObject
        Flat          -> flatObject
        Model modelID -> fromMaybe (flatObject { objVertexCount = 0 })
          $ lookup modelID models
  glBindVertexArray $ objVAO thisObject
  sendUniformName objectShader "model" $ case posn of
    ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2) -> let
      yScale = case obj of
        Flat -> 1 -- so we don't try to scale by 0 since y1 == y2
        _    -> abs $ y2 - y1
      in translate4 (V3 ((x1 + x2) / 2) ((y1 + y2) / 2) ((z1 + z2) / 2))
        !*! L.scaled (V4 (abs $ x2 - x1) yScale (abs $ z2 - z1) 1)
    ObjectMove xyz -> translate4 xyz
  sendUniformName objectShader "alpha" alpha
  case lightOffset of
    LightGlobal g -> do
      sendUniformName objectShader "light.position" $ C.light_position g
      sendUniformName objectShader "light.ambient"  $ C.light_ambient  g
      sendUniformName objectShader "light.diffuse"  $ C.light_diffuse  g
      sendUniformName objectShader "light.specular" $ C.light_specular g
    LightOffset off -> do
      let center = case posn of
            ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)
              -> V3 ((x1 + x2) / 2) (max y1 y2) ((z1 + z2) / 2)
            ObjectMove xyz -> xyz
      sendUniformName objectShader "light.position" $ center + C.light_position off
      sendUniformName objectShader "light.ambient"  $ C.light_ambient  off
      sendUniformName objectShader "light.diffuse"  $ C.light_diffuse  off
      sendUniformName objectShader "light.specular" $ C.light_specular off
  let missingTexture = do
        sendUniformName objectShader "material.diffuse.type" colorType
        sendUniformName objectShader "material.diffuse.color" (V4 1 0 1 1 :: V4 Float) -- magenta for missing texture
  case colorSource of
    CSColor color -> do
      sendUniformName objectShader "material.diffuse.type" colorType
      sendUniformName objectShader "material.diffuse.color" color
    CSImage texid -> do
      case lookup texid textures of
        Just tex -> do
          glActiveTexture GL_TEXTURE0
          glBindTexture GL_TEXTURE_2D $ textureGL tex
          sendUniformName objectShader "material.diffuse.type" texture1Type
          sendUniformName objectShader "material.diffuse.image" (0 :: GLint)
        Nothing -> missingTexture
    CSImage2 t1 t2 -> do
      case (lookup t1 textures, lookup t2 textures) of
        (Just tex1, Just tex2) -> do
          glActiveTexture GL_TEXTURE0
          glBindTexture GL_TEXTURE_2D $ textureGL tex1
          glActiveTexture GL_TEXTURE1
          glBindTexture GL_TEXTURE_2D $ textureGL tex2
          sendUniformName objectShader "material.diffuse.type" texture2Type
          sendUniformName objectShader "material.diffuse.image"  (0 :: GLint)
          sendUniformName objectShader "material.diffuse.image2" (1 :: GLint)
        _ -> missingTexture
    CSImage3 t1 t2 t3 -> do
      case (lookup t1 textures, lookup t2 textures, lookup t3 textures) of
        (Just tex1, Just tex2, Just tex3) -> do
          glActiveTexture GL_TEXTURE0
          glBindTexture GL_TEXTURE_2D $ textureGL tex1
          glActiveTexture GL_TEXTURE1
          glBindTexture GL_TEXTURE_2D $ textureGL tex2
          glActiveTexture GL_TEXTURE2
          glBindTexture GL_TEXTURE_2D $ textureGL tex3
          sendUniformName objectShader "material.diffuse.type" texture3Type
          sendUniformName objectShader "material.diffuse.image"  (0 :: GLint)
          sendUniformName objectShader "material.diffuse.image2" (1 :: GLint)
          sendUniformName objectShader "material.diffuse.image3" (2 :: GLint)
        _ -> missingTexture
  sendUniformName objectShader "material.specular.type" colorType
  sendUniformName objectShader "material.specular.color" (V4 0.5 0.5 0.5 1 :: V4 Float) -- CONFIGME
  sendUniformName objectShader "material.shininess" (32 :: Float)
  glDrawArrays GL_TRIANGLES 0 $ objVertexCount thisObject

makeToggleBounds :: t -> t -> Map.Map t Toggle -> [(t, t, Bool)]
makeToggleBounds t1 t2 m = let
  togs = Map.toAscList m
  initBool = case togs of
    []           -> False
    (_, tog) : _ -> tog /= ToggleEmpty && tog /= ToggleStart
  togFuture tog = tog /= ToggleEmpty && tog /= ToggleEnd
  zipped = map (\((chunkStart, b), chunkEnd) -> (chunkStart, chunkEnd, b)) $ zip
    ((t1, initBool) : map (second togFuture) togs)
    (map fst togs ++ [t2])
  simplify [] = []
  simplify ((tx, _, b1) : (_, ty, b2) : rest) | b1 == b2
    = simplify $ (tx, ty, b1) : rest
  simplify (x : xs) = x : simplify xs
  in simplify zipped

drawDrums :: GLStuff -> Double -> Double -> Map.Map Double (CommonState (DrumState (D.Gem D.ProType))) -> IO ()
drawDrums glStuff nowTime speed trk = drawDrumPlay glStuff nowTime speed DrumPlayState
  { drumEvents = do
    (cst, cs) <- Map.toDescList $ fst $ Map.split nowTime trk
    pad <- Set.toList $ drumNotes $ commonState cs
    let res = EventResult
          { eventHit = Just (pad, Just cst)
          , eventMissed = []
          }
    return (cst, (res, initialState)) -- score/combo state not used
  , drumTrack = trk
  , drumNoteTimes = Set.empty -- not used
  }

drawDrumPlay :: GLStuff -> Double -> Double -> DrumPlayState Double (D.Gem D.ProType) -> IO ()
drawDrumPlay glStuff@GLStuff{..} nowTime speed dps = do
  glUseProgram objectShader
  -- view and projection matrices should already have been set
  let drawObject' = drawObject glStuff
      globalLight = LightGlobal $ C.trk_light $ C.cfg_track gfxConfig
      nearZ = C.tt_z_past $ C.trk_time $ C.cfg_track gfxConfig
      nowZ = C.tt_z_now $ C.trk_time $ C.cfg_track gfxConfig
      farZ = C.tt_z_future $ C.trk_time $ C.cfg_track gfxConfig
      farTime = nowTime + speed * realToFrac (C.tt_secs_future $ C.trk_time $ C.cfg_track gfxConfig) :: Double
      timeToZ t = nowZ + (farZ - nowZ) * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + (farTime - nowTime) * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      zoomed = zoomMap nearTime farTime $ drumTrack dps
      trackWidth
        = C.na_x_right (C.trk_note_area $ C.cfg_track gfxConfig)
        - C.na_x_left  (C.trk_note_area $ C.cfg_track gfxConfig)
      fracToX f = C.na_x_left (C.trk_note_area $ C.cfg_track gfxConfig) + trackWidth * f
      drawGem t od gem alpha = let
        (texid, obj) = case gem of
          D.Kick                  -> (if od then TextureLongEnergy   else TextureLongKick     , Model ModelDrumKick  )
          D.Red                   -> (if od then TextureEnergyGem    else TextureRedGem       , Model ModelDrumTom   )
          D.Pro D.Yellow D.Tom    -> (if od then TextureEnergyGem    else TextureYellowGem    , Model ModelDrumTom   )
          D.Pro D.Blue   D.Tom    -> (if od then TextureEnergyGem    else TextureBlueGem      , Model ModelDrumTom   )
          D.Pro D.Green  D.Tom    -> (if od then TextureEnergyGem    else TextureGreenGem     , Model ModelDrumTom   )
          D.Pro D.Yellow D.Cymbal -> (if od then TextureEnergyCymbal else TextureYellowCymbal , Model ModelDrumCymbal)
          D.Pro D.Blue   D.Cymbal -> (if od then TextureEnergyCymbal else TextureBlueCymbal   , Model ModelDrumCymbal)
          D.Pro D.Green  D.Cymbal -> (if od then TextureEnergyCymbal else TextureGreenCymbal  , Model ModelDrumCymbal)
          D.Orange                -> (if od then TextureEnergyCymbal else TextureGreenCymbal  , Model ModelDrumCymbal)
        shade = case alpha of
          Nothing -> CSImage texid
          Just _  -> CSColor $ C.gems_color_hit $ C.obj_gems $ C.cfg_objects gfxConfig
        posn = case gem of
          D.Kick           -> gemAtX $ fracToX 0.5
          D.Red            -> gemAtX $ fracToX $ 1 / 8
          D.Pro D.Yellow _ -> gemAtX $ fracToX $ 3 / 8
          D.Pro D.Blue _   -> gemAtX $ fracToX $ 5 / 8
          D.Pro D.Green _  -> gemAtX $ fracToX $ 7 / 8
          D.Orange         -> gemAtX $ fracToX 0.5 -- TODO
        gemAtX x = ObjectMove $ V3 x (C.trk_y $ C.cfg_track gfxConfig) z
        z = timeToZ t
        in drawObject' obj posn shade (fromMaybe 1 alpha) $ LightOffset
          $ C.gems_light $ C.obj_gems $ C.cfg_objects gfxConfig
      drawNotes t cs = let
        od = case commonOverdrive cs of
          ToggleEmpty -> False
          ToggleEnd   -> False
          _           -> True
        fadeTime = C.gems_secs_fade $ C.obj_gems $ C.cfg_objects gfxConfig
        in forM_ (drumNotes $ commonState cs) $ \gem ->
          case noteStatus t gem $ drumEvents dps of
            NoteFuture -> drawGem t od gem Nothing
            NoteMissed -> drawGem t od gem Nothing
            NoteHitAt hitTime -> if nowTime - hitTime < realToFrac fadeTime
              then drawGem nowTime od gem $ Just $ 1 - realToFrac (nowTime - hitTime) / fadeTime
              else return ()
      drawBeat t cs = case commonBeats cs of
        Nothing -> return ()
        Just e -> let
          tex = case e of
            Just Bar  -> TextureLine1
            Just Beat -> TextureLine2
            Nothing   -> TextureLine3
          z = timeToZ t
          xyz1 = V3
            (C.na_x_left $ C.trk_note_area $ C.cfg_track gfxConfig)
            (C.trk_y $ C.cfg_track gfxConfig)
            (z + C.beats_z_past (C.trk_beats $ C.cfg_track gfxConfig))
          xyz2 = V3
            (C.na_x_right $ C.trk_note_area $ C.cfg_track gfxConfig)
            (C.trk_y $ C.cfg_track gfxConfig)
            (z + C.beats_z_future (C.trk_beats $ C.cfg_track gfxConfig))
          in drawObject' Flat (ObjectStretch xyz1 xyz2) (CSImage tex) 1 globalLight
      drawTargetSquare i tex alpha = let
        x1 = fracToX $ i       / 4
        x2 = fracToX $ (i + 1) / 4
        y = C.trk_y $ C.cfg_track gfxConfig
        z1 = C.tgt_z_past $ C.trk_targets $ C.cfg_track gfxConfig
        z2 = C.tgt_z_future $ C.trk_targets $ C.cfg_track gfxConfig
        in drawObject' Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) alpha globalLight
  -- draw highway
  forM_ (makeToggleBounds nearTime farTime $ fmap commonSolo zoomed) $ \(t1, t2, isSolo) -> do
    let highwayColor = (if isSolo then C.tc_solo else C.tc_normal)
          $ C.trk_color $ C.cfg_track gfxConfig
    drawObject'
      Flat
      (ObjectStretch
        (V3
          (C.na_x_left $ C.trk_note_area $ C.cfg_track gfxConfig)
          (C.trk_y $ C.cfg_track gfxConfig)
          (timeToZ t1)
        )
        (V3
          (C.na_x_right $ C.trk_note_area $ C.cfg_track gfxConfig)
          (C.trk_y $ C.cfg_track gfxConfig)
          (timeToZ t2)
        )
      )
      (CSColor highwayColor)
      1
      globalLight
  -- draw railings
  let rail = C.trk_railings $ C.cfg_track gfxConfig
      noteArea = C.trk_note_area $ C.cfg_track gfxConfig
  drawObject' Box
    (ObjectStretch
      (V3
        (C.na_x_left noteArea - C.rail_x_width rail)
        (C.rail_y_top rail)
        nearZ
      )
      (V3
        (C.na_x_left noteArea)
        (C.rail_y_bottom rail)
        farZ
      )
    )
    (CSColor $ C.rail_color rail) 1 globalLight
  drawObject' Box
    (ObjectStretch
      (V3
        (C.na_x_right noteArea)
        (C.rail_y_top rail)
        nearZ
      )
      (V3
        (C.na_x_right noteArea + C.rail_x_width rail)
        (C.rail_y_bottom rail)
        farZ
      )
    )
    (CSColor $ C.rail_color rail) 1 globalLight
  -- draw beat lines
  glDepthFunc GL_ALWAYS
  void $ Map.traverseWithKey drawBeat zoomed
  -- draw lanes
  let drawLane startTime endTime pad = let
        i = case pad of
          D.Pro D.Yellow _ -> 1
          D.Pro D.Blue   _ -> 2
          D.Pro D.Green  _ -> 3
          _                -> 0
        x1 = fracToX $ i       / 4
        x2 = fracToX $ (i + 1) / 4
        y = C.trk_y $ C.cfg_track gfxConfig
        z1 = timeToZ startTime
        z2 = timeToZ endTime
        tex = case pad of
          D.Red            -> TextureLaneRed
          D.Pro D.Yellow _ -> TextureLaneYellow
          D.Pro D.Blue   _ -> TextureLaneBlue
          D.Pro D.Green  _ -> TextureLaneGreen
          _                -> TextureLaneOrange
        in drawObject' Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) 1 globalLight
      drawLanes _        []                      = return ()
      drawLanes nextTime ((thisTime, cs) : rest) = do
        let lanes = Map.toList $ drumLanes $ commonState cs
            bre = commonBRE cs
        -- draw following lanes
        forM_ lanes $ \(pad, tog) -> when (elem tog [ToggleStart, ToggleRestart, ToggleOn]) $ do
          drawLane thisTime nextTime pad
        when (elem bre [ToggleStart, ToggleRestart, ToggleOn]) $ do
          mapM_ (drawLane thisTime nextTime) [D.Red, D.Pro D.Yellow D.Tom, D.Pro D.Blue D.Tom, D.Pro D.Green D.Tom]
        -- draw past lanes if rest is empty
        when (null rest) $ do
          forM_ lanes $ \(pad, tog) -> when (elem tog [ToggleEnd, ToggleRestart, ToggleOn]) $ do
            drawLane nearTime thisTime pad
          when (elem bre [ToggleEnd, ToggleRestart, ToggleOn]) $ do
            mapM_ (drawLane nearTime thisTime) [D.Red, D.Pro D.Yellow D.Tom, D.Pro D.Blue D.Tom, D.Pro D.Green D.Tom]
        drawLanes thisTime rest
  drawLanes farTime $ Map.toDescList zoomed
  -- draw target
  mapM_ (\(i, tex) -> drawTargetSquare i tex 1) $
    zip [0..] [TextureTargetRed, TextureTargetYellow, TextureTargetBlue, TextureTargetGreen]
  let drawLights [] _ = return ()
      drawLights _ [] = return ()
      drawLights ((t, pad) : states) colors = let
        (colorsYes, colorsNo) = partition (\(_, _, pads) -> elem pad pads) colors
        alpha = 1 - realToFrac (nowTime - t) / C.tgt_secs_light
          (C.trk_targets $ C.cfg_track gfxConfig)
        in when (t > nearTime) $ do
          forM_ colorsYes $ \(i, light, _) -> drawTargetSquare i light alpha
          drawLights states colorsNo
  drawLights [ (t, pad) | (t, (res, _)) <- drumEvents dps, Just (pad, _) <- [eventHit res] ]
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

drawFive :: GLStuff -> Double -> Double -> Map.Map Double (CommonState (GuitarState (Maybe F.Color))) -> IO ()
drawFive glStuff@GLStuff{..} nowTime speed trk = do
  glUseProgram objectShader
  -- view and projection matrices should already have been set
  let drawObject' = drawObject glStuff
      globalLight = LightGlobal $ C.trk_light $ C.cfg_track gfxConfig
      nearZ = C.tt_z_past $ C.trk_time $ C.cfg_track gfxConfig
      nowZ = C.tt_z_now $ C.trk_time $ C.cfg_track gfxConfig
      farZ = C.tt_z_future $ C.trk_time $ C.cfg_track gfxConfig
      farTime = nowTime + speed * realToFrac (C.tt_secs_future $ C.trk_time $ C.cfg_track gfxConfig) :: Double
      timeToZ t = nowZ + (farZ - nowZ) * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + (farTime - nowTime) * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      zoomed = zoomMap nearTime farTime trk
      trackWidth
        = C.na_x_right (C.trk_note_area $ C.cfg_track gfxConfig)
        - C.na_x_left  (C.trk_note_area $ C.cfg_track gfxConfig)
      fracToX f = C.na_x_left (C.trk_note_area $ C.cfg_track gfxConfig) + trackWidth * f
      colorCenterX = \case
        Nothing       -> fracToX 0.5
        Just F.Green  -> fracToX $ 1 / 10
        Just F.Red    -> fracToX $ 3 / 10
        Just F.Yellow -> fracToX $ 5 / 10
        Just F.Blue   -> fracToX $ 7 / 10
        Just F.Orange -> fracToX $ 9 / 10
      drawSustain t1 t2 od color
        | t2 <= nowTime = return ()
        | otherwise     = let
          sc = C.sust_colors $ C.obj_sustains $ C.cfg_objects gfxConfig
          boxColor = if od
            then C.sc_energy sc
            else case color of
              Nothing       -> C.sc_open sc
              Just F.Green  -> C.sc_green sc
              Just F.Red    -> C.sc_red sc
              Just F.Yellow -> C.sc_yellow sc
              Just F.Blue   -> C.sc_blue sc
              Just F.Orange -> C.sc_orange sc
          (x1, x2) = let
            center = colorCenterX color
            halfWidth = 0.5 * case color of
              Nothing -> C.sw_open $ C.sust_width $ C.obj_sustains $ C.cfg_objects gfxConfig
              Just _  -> C.sw_fret $ C.sust_width $ C.obj_sustains $ C.cfg_objects gfxConfig
            in (center - halfWidth, center + halfWidth)
          y2 = C.trk_y $ C.cfg_track gfxConfig
          y1 = y2 + C.sust_height (C.obj_sustains $ C.cfg_objects gfxConfig)
          (z1, z2) = (timeToZ $ max nowTime t1, timeToZ t2)
          in drawObject' Box (ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)) (CSColor boxColor) 1 globalLight
      drawGem t od color sht alpha = let
        (texid, obj) = case (color, sht) of
          (Nothing      , Strum) -> (if od then TextureLongEnergy else TextureLongOpen , Model ModelGuitarOpen)
          (Just F.Green , Strum) -> (if od then TextureEnergyGem  else TextureGreenGem , Model ModelGuitarStrum)
          (Just F.Red   , Strum) -> (if od then TextureEnergyGem  else TextureRedGem   , Model ModelGuitarStrum)
          (Just F.Yellow, Strum) -> (if od then TextureEnergyGem  else TextureYellowGem, Model ModelGuitarStrum)
          (Just F.Blue  , Strum) -> (if od then TextureEnergyGem  else TextureBlueGem  , Model ModelGuitarStrum)
          (Just F.Orange, Strum) -> (if od then TextureEnergyGem  else TextureOrangeGem, Model ModelGuitarStrum)
          (Nothing      , HOPO) -> (if od then TextureLongEnergyHopo else TextureLongOpenHopo , Model ModelGuitarOpen)
          (Just F.Green , HOPO) -> (if od then TextureEnergyHopo  else TextureGreenHopo , Model ModelGuitarHOPOTap)
          (Just F.Red   , HOPO) -> (if od then TextureEnergyHopo  else TextureRedHopo   , Model ModelGuitarHOPOTap)
          (Just F.Yellow, HOPO) -> (if od then TextureEnergyHopo  else TextureYellowHopo, Model ModelGuitarHOPOTap)
          (Just F.Blue  , HOPO) -> (if od then TextureEnergyHopo  else TextureBlueHopo  , Model ModelGuitarHOPOTap)
          (Just F.Orange, HOPO) -> (if od then TextureEnergyHopo  else TextureOrangeHopo, Model ModelGuitarHOPOTap)
          (Nothing      , Tap) -> (if od then TextureLongEnergyTap else TextureLongOpenTap , Model ModelGuitarOpen)
          (Just F.Green , Tap) -> (if od then TextureEnergyTap  else TextureGreenTap , Model ModelGuitarHOPOTap)
          (Just F.Red   , Tap) -> (if od then TextureEnergyTap  else TextureRedTap   , Model ModelGuitarHOPOTap)
          (Just F.Yellow, Tap) -> (if od then TextureEnergyTap  else TextureYellowTap, Model ModelGuitarHOPOTap)
          (Just F.Blue  , Tap) -> (if od then TextureEnergyTap  else TextureBlueTap  , Model ModelGuitarHOPOTap)
          (Just F.Orange, Tap) -> (if od then TextureEnergyTap  else TextureOrangeTap, Model ModelGuitarHOPOTap)
        shade = case alpha of
          Nothing -> CSImage texid
          Just _  -> CSColor $ C.gems_color_hit $ C.obj_gems $ C.cfg_objects gfxConfig
        posn = ObjectMove $ V3 (colorCenterX color) (C.trk_y $ C.cfg_track gfxConfig) z
        z = timeToZ t
        in drawObject' obj posn shade (fromMaybe 1 alpha) $ LightOffset
          $ C.gems_light $ C.obj_gems $ C.cfg_objects gfxConfig
      drawNotes _        []                      = return ()
      drawNotes nextTime ((thisTime, cs) : rest) = do
        let notes = Map.toList $ guitarNotes $ commonState cs
        -- draw following sustain
        forM_ notes $ \(color, pnf) -> forM_ (getFuture pnf) $ \od -> do
          drawSustain thisTime nextTime od color
        -- draw note
        let thisOD = case commonOverdrive cs of
              ToggleEmpty -> False
              ToggleEnd   -> False
              _           -> True
            fadeTime = C.gems_secs_fade $ C.obj_gems $ C.cfg_objects gfxConfig
        forM_ notes $ \(color, pnf) -> forM_ (getNow pnf) $ \sht -> if nowTime <= thisTime
          then drawGem thisTime thisOD color sht Nothing
          else if nowTime - thisTime < realToFrac fadeTime
            then drawGem nowTime thisOD color sht $ Just $ 1 - realToFrac (nowTime - thisTime) / fadeTime
            else return ()
        -- draw past sustain if rest is empty
        when (null rest) $ do
          forM_ notes $ \(color, pnf) -> forM_ (getPast pnf) $ \od -> do
            drawSustain nearTime thisTime od color
        drawNotes thisTime rest
      drawBeat t cs = case commonBeats cs of
        Nothing -> return ()
        Just e -> let
          tex = case e of
            Just Bar  -> TextureLine1
            Just Beat -> TextureLine2
            Nothing   -> TextureLine3
          z = timeToZ t
          xyz1 = V3
            (C.na_x_left $ C.trk_note_area $ C.cfg_track gfxConfig)
            (C.trk_y $ C.cfg_track gfxConfig)
            (z + C.beats_z_past (C.trk_beats $ C.cfg_track gfxConfig))
          xyz2 = V3
            (C.na_x_right $ C.trk_note_area $ C.cfg_track gfxConfig)
            (C.trk_y $ C.cfg_track gfxConfig)
            (z + C.beats_z_future (C.trk_beats $ C.cfg_track gfxConfig))
          in drawObject' Flat (ObjectStretch xyz1 xyz2) (CSImage tex) 1 globalLight
      drawTargetSquare i tex alpha = let
        x1 = fracToX $ i       / 5
        x2 = fracToX $ (i + 1) / 5
        y = C.trk_y $ C.cfg_track gfxConfig
        z1 = C.tgt_z_past $ C.trk_targets $ C.cfg_track gfxConfig
        z2 = C.tgt_z_future $ C.trk_targets $ C.cfg_track gfxConfig
        in drawObject' Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) alpha globalLight
  -- draw highway
  forM_ (makeToggleBounds nearTime farTime $ fmap commonSolo zoomed) $ \(t1, t2, isSolo) -> do
    let highwayColor = (if isSolo then C.tc_solo else C.tc_normal)
          $ C.trk_color $ C.cfg_track gfxConfig
    drawObject'
      Flat
      (ObjectStretch
        (V3
          (C.na_x_left $ C.trk_note_area $ C.cfg_track gfxConfig)
          (C.trk_y $ C.cfg_track gfxConfig)
          (timeToZ t1)
        )
        (V3
          (C.na_x_right $ C.trk_note_area $ C.cfg_track gfxConfig)
          (C.trk_y $ C.cfg_track gfxConfig)
          (timeToZ t2)
        )
      )
      (CSColor highwayColor)
      1
      globalLight
  -- draw railings
  let rail = C.trk_railings $ C.cfg_track gfxConfig
      noteArea = C.trk_note_area $ C.cfg_track gfxConfig
  drawObject' Box
    (ObjectStretch
      (V3
        (C.na_x_left noteArea - C.rail_x_width rail)
        (C.rail_y_top rail)
        nearZ
      )
      (V3
        (C.na_x_left noteArea)
        (C.rail_y_bottom rail)
        farZ
      )
    )
    (CSColor $ C.rail_color rail) 1 globalLight
  drawObject' Box
    (ObjectStretch
      (V3
        (C.na_x_right noteArea)
        (C.rail_y_top rail)
        nearZ
      )
      (V3
        (C.na_x_right noteArea + C.rail_x_width rail)
        (C.rail_y_bottom rail)
        farZ
      )
    )
    (CSColor $ C.rail_color rail) 1 globalLight
  -- draw beat lines
  glDepthFunc GL_ALWAYS
  void $ Map.traverseWithKey drawBeat zoomed
  -- draw lanes
  let drawLane startTime endTime color = let
        i = maybe 2 (fromIntegral . fromEnum) (color :: Maybe F.Color)
        x1 = fracToX $ i       / 5
        x2 = fracToX $ (i + 1) / 5
        y = C.trk_y $ C.cfg_track gfxConfig
        z1 = timeToZ startTime
        z2 = timeToZ endTime
        tex = case color of
          Nothing       -> TextureLanePurple
          Just F.Green  -> TextureLaneGreen
          Just F.Red    -> TextureLaneRed
          Just F.Yellow -> TextureLaneYellow
          Just F.Blue   -> TextureLaneBlue
          Just F.Orange -> TextureLaneOrange
        in drawObject' Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) 1 globalLight
      drawLanes _        []                      = return ()
      drawLanes nextTime ((thisTime, cs) : rest) = do
        let tremolo = Map.toList $ guitarTremolo $ commonState cs
            trill   = Map.toList $ guitarTrill   $ commonState cs
            bre     = commonBRE cs
        -- draw following lanes
        forM_ tremolo $ \(color, tog) -> when (elem tog [ToggleStart, ToggleRestart, ToggleOn]) $ do
          drawLane thisTime nextTime color
        forM_ trill $ \(color, tog) -> when (elem tog [ToggleStart, ToggleRestart, ToggleOn]) $ do
          drawLane thisTime nextTime color
        when (elem bre [ToggleStart, ToggleRestart, ToggleOn]) $ do
          mapM_ (drawLane thisTime nextTime) $ map Just each
        -- draw past lanes if rest is empty
        when (null rest) $ do
          forM_ tremolo $ \(color, tog) -> when (elem tog [ToggleEnd, ToggleRestart, ToggleOn]) $ do
            drawLane nearTime thisTime color
          forM_ trill $ \(color, tog) -> when (elem tog [ToggleEnd, ToggleRestart, ToggleOn]) $ do
            drawLane nearTime thisTime color
          when (elem bre [ToggleEnd, ToggleRestart, ToggleOn]) $ do
            mapM_ (drawLane nearTime thisTime) $ map Just each
        drawLanes thisTime rest
  drawLanes farTime $ Map.toDescList zoomed
  -- draw target
  mapM_ (\(i, tex) -> drawTargetSquare i tex 1) $
    zip [0..] [TextureTargetGreen, TextureTargetRed, TextureTargetYellow, TextureTargetBlue, TextureTargetOrange]
  let drawLights [] _ = return ()
      drawLights _ [] = return ()
      drawLights ((t, cs) : states) colors = let
        getLightAlpha gem = do
          pnf <- Map.lookup gem $ guitarNotes $ commonState cs
          case getFuture pnf of
            Just _ -> Just 1 -- gem is being sustained
            Nothing -> do
              guard $ isJust (getNow pnf) || isJust (getPast pnf)
              Just alpha
        (colorsYes, colorsNo) = flip partitionMaybe colors $ \(i, light, gem) ->
          fmap (\thisAlpha -> (i, light, thisAlpha))
            $ getLightAlpha gem
        alpha = 1 - realToFrac (nowTime - t) / C.tgt_secs_light
          (C.trk_targets $ C.cfg_track gfxConfig)
        in do
          forM_ colorsYes $ \(i, light, thisAlpha) -> drawTargetSquare i light thisAlpha
          drawLights states colorsNo
      lookPast = case Map.split nowTime zoomed of
        (past, future)
          | Map.null past -> case Map.lookupMin future of
            Nothing      -> []
            Just (_, cs) -> [(nowTime, before cs)]
          | otherwise     -> Map.toDescList past
  drawLights lookPast
    [ (0, TextureTargetGreenLight , Just F.Green )
    , (1, TextureTargetRedLight   , Just F.Red   )
    , (2, TextureTargetYellowLight, Just F.Yellow)
    , (3, TextureTargetBlueLight  , Just F.Blue  )
    , (4, TextureTargetOrangeLight, Just F.Orange)
    ]
  glDepthFunc GL_LESS
  -- draw notes
  drawNotes farTime $ Map.toDescList zoomed

drawPG :: GLStuff -> Double -> Double -> Map.Map Double (CommonState (PGState Double)) -> IO ()
drawPG glStuff@GLStuff{..} nowTime speed trk = do
  glUseProgram objectShader
  -- view and projection matrices should already have been set
  let drawObject' = drawObject glStuff
      globalLight = LightGlobal $ C.trk_light $ C.cfg_track gfxConfig
      nearZ = C.tt_z_past $ C.trk_time $ C.cfg_track gfxConfig
      nowZ = C.tt_z_now $ C.trk_time $ C.cfg_track gfxConfig
      farZ = C.tt_z_future $ C.trk_time $ C.cfg_track gfxConfig
      farTime = nowTime + speed * realToFrac (C.tt_secs_future $ C.trk_time $ C.cfg_track gfxConfig) :: Double
      timeToZ t = nowZ + (farZ - nowZ) * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + (farTime - nowTime) * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      zoomed = zoomMap nearTime farTime trk
      trackWidth
        = C.na_x_right (C.trk_note_area $ C.cfg_track gfxConfig)
        - C.na_x_left  (C.trk_note_area $ C.cfg_track gfxConfig)
      fracToX f = C.na_x_left (C.trk_note_area $ C.cfg_track gfxConfig) + trackWidth * f
      sc = C.sust_colors $ C.obj_sustains $ C.cfg_objects gfxConfig
      allStrings =
        [ (PG.S6, (C.sc_red    sc, TextureTargetRed   , TextureTargetRedLight   , TextureRSRed   ))
        , (PG.S5, (C.sc_yellow sc, TextureTargetYellow, TextureTargetYellowLight, TextureRSYellow))
        , (PG.S4, (C.sc_blue   sc, TextureTargetBlue  , TextureTargetBlueLight  , TextureRSBlue  ))
        , (PG.S3, (C.sc_orange sc, TextureTargetOrange, TextureTargetOrangeLight, TextureRSOrange))
        , (PG.S2, (C.sc_green  sc, TextureTargetGreen , TextureTargetGreenLight , TextureRSGreen ))
        , (PG.S1, (C.sc_blue   sc, TextureTargetBlue  , TextureTargetBlueLight  , TextureRSPurple)) -- TODO should be purple
        ] -- TODO determine based on chart
      numStrings = fromIntegral $ length allStrings :: Float
      stringCenterX str = fracToX $ maybe 0 fromIntegral (findIndex ((== str) . fst) allStrings) / numStrings + 1 / (numStrings * 2)
      drawSustain t1 t2 od str
        | t2 <= nowTime = return ()
        | otherwise     = let
          boxColor = if od
            then C.sc_energy sc
            else case lookup str allStrings of
              Nothing                   -> C.sc_red sc -- shouldn't happen
              Just (sustColor, _, _, _) -> sustColor
          (x1, x2) = let
            center = stringCenterX str
            halfWidth = 0.5 * C.sw_fret (C.sust_width $ C.obj_sustains $ C.cfg_objects gfxConfig)
            in (center - halfWidth, center + halfWidth)
          y2 = C.trk_y $ C.cfg_track gfxConfig
          y1 = y2 + C.sust_height (C.obj_sustains $ C.cfg_objects gfxConfig)
          (z1, z2) = (timeToZ $ max nowTime t1, timeToZ t2)
          in drawObject' Box (ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)) (CSColor boxColor) 1 globalLight
      drawGem t od str note alpha = let
        obj = Model ModelPGNote
        fretWidth = 2 / numStrings
        halfWidth = fretWidth / 2
        (x1, x2) = let
          center = stringCenterX str
          in (center - halfWidth, center + halfWidth)
        y1 = C.trk_y (C.cfg_track gfxConfig) - halfWidth
        y2 = C.trk_y (C.cfg_track gfxConfig) + halfWidth
        z = timeToZ t
        (z1, z2) = (z - halfWidth, z + halfWidth)
        stretch = ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)
        texid1 = case lookup str allStrings of
          Nothing             -> TextureRSRed -- shouldn't happen
          Just (_, _, _, tex) -> tex
        texid2 = toEnum $ fromEnum TextureRS0 + pgFret note
        texid3 = case pgSHT note of
          Strum -> Nothing
          HOPO  -> Just TextureRSHopo
          Tap   -> Just TextureRSTap
        shade = case alpha of
          Nothing -> maybe (CSImage2 texid1 texid2) (CSImage3 texid1 texid2) texid3
          Just _  -> CSColor $ C.gems_color_hit $ C.obj_gems $ C.cfg_objects gfxConfig
        in drawObject' obj stretch shade (fromMaybe 1 alpha) $ LightOffset $ let
          normalLight = C.gems_light $ C.obj_gems $ C.cfg_objects gfxConfig
          in normalLight { C.light_position = V3 0 0 0.5 }
      drawNotes _        []                      = return ()
      drawNotes nextTime ((thisTime, cs) : rest) = do
        let notes = Map.toList $ pgNotes $ commonState cs
        -- draw following sustain
        forM_ notes $ \(str, pnf) -> forM_ (getFuture pnf) $ \sust -> do
          drawSustain thisTime nextTime (pgSustainOD sust) str
        -- draw note
        let thisOD = case commonOverdrive cs of
              ToggleEmpty -> False
              ToggleEnd   -> False
              _           -> True
            fadeTime = C.gems_secs_fade $ C.obj_gems $ C.cfg_objects gfxConfig
        forM_ notes $ \(str, pnf) -> forM_ (getNow pnf) $ \note -> if nowTime <= thisTime
          then drawGem thisTime thisOD str note Nothing
          else if nowTime - thisTime < realToFrac fadeTime
            then drawGem nowTime thisOD str note $ Just $ 1 - realToFrac (nowTime - thisTime) / fadeTime
            else return ()
        -- draw past sustain if rest is empty
        when (null rest) $ do
          forM_ notes $ \(str, pnf) -> forM_ (getPast pnf) $ \sust -> do
            drawSustain nearTime thisTime (pgSustainOD sust) str
        drawNotes thisTime rest
      drawBeat t cs = case commonBeats cs of
        Nothing -> return ()
        Just e -> let
          tex = case e of
            Just Bar  -> TextureLine1
            Just Beat -> TextureLine2
            Nothing   -> TextureLine3
          z = timeToZ t
          xyz1 = V3
            (C.na_x_left $ C.trk_note_area $ C.cfg_track gfxConfig)
            (C.trk_y $ C.cfg_track gfxConfig)
            (z + C.beats_z_past (C.trk_beats $ C.cfg_track gfxConfig))
          xyz2 = V3
            (C.na_x_right $ C.trk_note_area $ C.cfg_track gfxConfig)
            (C.trk_y $ C.cfg_track gfxConfig)
            (z + C.beats_z_future (C.trk_beats $ C.cfg_track gfxConfig))
          in drawObject' Flat (ObjectStretch xyz1 xyz2) (CSImage tex) 1 globalLight
      drawTargetSquare i tex alpha = let
        x1 = fracToX $ i       / numStrings
        x2 = fracToX $ (i + 1) / numStrings
        y = C.trk_y $ C.cfg_track gfxConfig
        z1 = C.tgt_z_past $ C.trk_targets $ C.cfg_track gfxConfig
        z2 = C.tgt_z_future $ C.trk_targets $ C.cfg_track gfxConfig
        in drawObject' Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (CSImage tex) alpha globalLight
  -- draw highway
  forM_ (makeToggleBounds nearTime farTime $ fmap commonSolo zoomed) $ \(t1, t2, isSolo) -> do
    let highwayColor = (if isSolo then C.tc_solo else C.tc_normal)
          $ C.trk_color $ C.cfg_track gfxConfig
    drawObject'
      Flat
      (ObjectStretch
        (V3
          (C.na_x_left $ C.trk_note_area $ C.cfg_track gfxConfig)
          (C.trk_y $ C.cfg_track gfxConfig)
          (timeToZ t1)
        )
        (V3
          (C.na_x_right $ C.trk_note_area $ C.cfg_track gfxConfig)
          (C.trk_y $ C.cfg_track gfxConfig)
          (timeToZ t2)
        )
      )
      (CSColor highwayColor)
      1
      globalLight
  -- draw railings
  let rail = C.trk_railings $ C.cfg_track gfxConfig
      noteArea = C.trk_note_area $ C.cfg_track gfxConfig
  drawObject' Box
    (ObjectStretch
      (V3
        (C.na_x_left noteArea - C.rail_x_width rail)
        (C.rail_y_top rail)
        nearZ
      )
      (V3
        (C.na_x_left noteArea)
        (C.rail_y_bottom rail)
        farZ
      )
    )
    (CSColor $ C.rail_color rail) 1 globalLight
  drawObject' Box
    (ObjectStretch
      (V3
        (C.na_x_right noteArea)
        (C.rail_y_top rail)
        nearZ
      )
      (V3
        (C.na_x_right noteArea + C.rail_x_width rail)
        (C.rail_y_bottom rail)
        farZ
      )
    )
    (CSColor $ C.rail_color rail) 1 globalLight
  -- draw beat lines
  glDepthFunc GL_ALWAYS
  void $ Map.traverseWithKey drawBeat zoomed
  -- draw target
  mapM_ (\(i, (_, (_, tex, _, _))) -> drawTargetSquare i tex 1) $ zip [0..] allStrings
  let drawLights [] _ = return ()
      drawLights _ [] = return ()
      drawLights ((t, cs) : states) colors = let
        getLightAlpha str = do
          pnf <- Map.lookup str $ pgNotes $ commonState cs
          case getFuture pnf of
            Just _ -> Just 1 -- str is being sustained
            Nothing -> do
              guard $ isJust (getNow pnf) || isJust (getPast pnf)
              Just alpha
        (colorsYes, colorsNo) = flip partitionMaybe colors $ \(i, (str, (_, _, light, _))) ->
          fmap (\thisAlpha -> (i, light, thisAlpha))
            $ getLightAlpha str
        alpha = 1 - realToFrac (nowTime - t) / C.tgt_secs_light
          (C.trk_targets $ C.cfg_track gfxConfig)
        in do
          forM_ colorsYes $ \(i, light, thisAlpha) -> drawTargetSquare i light thisAlpha
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

fillPtr :: (Storable a, MonadIO m) => (Ptr a -> IO ()) -> m a
fillPtr f = liftIO $ alloca $ \p -> f p >> peek p

loadProgram :: (MonadIO m) => [(GLenum, FilePath)] -> StackTraceT m GLuint
loadProgram shaderPairs = mapStackTraceT (liftIO . runResourceT) $ do
  shaders <- forM shaderPairs $ \(shaderType, f) -> do
    inside ("compiling shader: " <> f) $ do
      source <- stackIO $ B.readFile f
      shader <- stackIO (compileShader shaderType source) >>= either fatal return
      void $ register $ glDeleteShader shader
      return shader
  stackIO (compileProgram shaders) >>= either fatal return

compileShader :: GLenum -> B.ByteString -> IO (Either String GLuint)
compileShader shaderType source = do
  shader <- glCreateShader shaderType
  B.useAsCString source $ \cs' -> with cs' $ \cs -> do
    glShaderSource shader 1 cs nullPtr
    glCompileShader shader
    fillPtr (glGetShaderiv shader GL_COMPILE_STATUS) >>= \case
      GL_FALSE -> do
        errLen <- fillPtr $ glGetShaderiv shader GL_INFO_LOG_LENGTH
        allocaArray (fromIntegral errLen) $ \infoLog -> do
          glGetShaderInfoLog shader errLen nullPtr infoLog
          peekCString infoLog >>= return . Left
      _ -> return $ Right shader

compileProgram :: [GLuint] -> IO (Either String GLuint)
compileProgram shaders = do
  program <- glCreateProgram
  mapM_ (glAttachShader program) shaders
  glLinkProgram program
  fillPtr (glGetProgramiv program GL_LINK_STATUS) >>= \case
    GL_FALSE -> do
      errLen <- fillPtr $ glGetProgramiv program GL_INFO_LOG_LENGTH
      allocaArray (fromIntegral errLen) $ \infoLog -> do
        glGetProgramInfoLog program errLen nullPtr infoLog
        peekCString infoLog >>= return . Left
    _ -> return $ Right program

withArrayBytes :: (Storable a, Num len) => [a] -> (len -> Ptr a -> IO b) -> IO b
withArrayBytes xs f = withArray xs $ \p -> let
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
    poke (castPtr p) $ vertexPosition v
    poke (castPtr p `plusPtr` (3 * sizeOf (undefined :: CFloat))) $ vertexNormal v
    poke (castPtr p `plusPtr` (6 * sizeOf (undefined :: CFloat))) $ vertexTexCoords v

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
sendUniformName prog name x = liftIO $ do
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
  sendUniform uni b = glUniform1i uni $ if b then 1 else 0

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

loadTexture :: (GLPixel a, MonadIO m) => Bool -> Image a -> m Texture
loadTexture linear img = liftIO $ do
  let pixelProp :: (a -> b) -> Image a -> b
      pixelProp f _ = f undefined
      flippedVert = generateImage
        (\x y -> pixelAt img x $ imageHeight img - y - 1)
        (imageWidth img)
        (imageHeight img)
  texture <- fillPtr $ glGenTextures 1
  glBindTexture GL_TEXTURE_2D texture
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
  let filtering = if linear then GL_LINEAR else GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER filtering
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER filtering
  VS.unsafeWith (imageData flippedVert) $ \p -> do
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
updateTexture img tex = liftIO $ do
  let pixelProp :: (a -> b) -> Image a -> b
      pixelProp f _ = f undefined
      flippedVert = generateImage
        (\x y -> pixelAt img x $ imageHeight img - y - 1)
        (imageWidth img)
        (imageHeight img)
  let texture = textureGL tex
  glBindTexture GL_TEXTURE_2D texture
  VS.unsafeWith (imageData flippedVert) $ \p -> do
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
  { objectShader :: GLuint
  , boxObject    :: RenderObject
  , flatObject   :: RenderObject
  , quadShader   :: GLuint
  , quadObject   :: RenderObject
  , textures     :: [(TextureID, Texture)]
  , models       :: [(ModelID, RenderObject)]
  , gfxConfig    :: C.Config
  , framebuffers :: Framebuffers
  , fxaaEnabled  :: Bool
  , videoBGs     :: Map.Map (VideoInfo FilePath) VideoHandle
  , imageBGs     :: Map.Map FilePath Texture
  } deriving (Show)

data VideoHandle = VideoHandle
  { videoFrameLoader :: FrameLoader
  , videoTexture     :: IORef (Maybe (Double, Texture))
  , videoFilePath    :: FilePath
  }

instance Show VideoHandle where
  show vh = "VideoHandle[" <> show (videoFilePath vh) <> "]"

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
  | TextureGreenGem
  | TextureRedGem
  | TextureYellowGem
  | TextureBlueGem
  | TextureOrangeGem
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
  | TextureEnergyCymbal
  | TextureLine1
  | TextureLine2
  | TextureLine3
  | TextureTargetGreen
  | TextureTargetRed
  | TextureTargetYellow
  | TextureTargetBlue
  | TextureTargetOrange
  | TextureTargetGreenLight
  | TextureTargetRedLight
  | TextureTargetYellowLight
  | TextureTargetBlueLight
  | TextureTargetOrangeLight
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
  deriving (Eq, Show, Enum, Bounded)

data ModelID
  = ModelDrumTom
  | ModelDrumCymbal
  | ModelDrumKick
  | ModelGuitarStrum
  | ModelGuitarHOPOTap
  | ModelGuitarOpen
  | ModelPGNote
  deriving (Eq, Show, Enum, Bounded)

loadObj :: (MonadIO m) => FilePath -> StackTraceT m [Vertex]
loadObj f = inside ("loading model: " <> f) $ do
  obj <- stackIO (Obj.fromFile f) >>= either fatal return
  return $ do
    Obj.Face a b c rest <- map Obj.elValue $ V.toList $ Obj.objFaces obj
    let triangulate v1 v2 v3 vs = [v1, v2, v3] ++ case vs of
          []    -> []
          h : t -> triangulateExtra v1 v2 v3 h t
        triangulateExtra v1 v2 v3 v4 vs = [v1, v3, v4] ++ case vs of
          []    -> []
          h : t -> triangulateExtra v2 v3 v4 h t
        -- above is implemented according to https://stackoverflow.com/a/43422763
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
  sumZ = sum . map (getZ . vertexPosition)
  in concatMap snd . sort . map (\tri -> (sumZ tri, tri)) . getTris

loadGLStuff :: (MonadIO m) => [PreviewBG] -> StackTraceT (QueueLog m) GLStuff
loadGLStuff bgs = do

  gfxConfig <- load3DConfig

  glEnable GL_DEPTH_TEST
  glEnable GL_CULL_FACE -- default CCW = front
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

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

  let loadObject (sortVertices -> vertices) = liftIO $ do
        vao <- fillPtr $ glGenVertexArrays 1
        vbo <- fillPtr $ glGenBuffers 1
        glBindVertexArray vao
        glBindBuffer GL_ARRAY_BUFFER vbo
        withArrayBytes vertices $ \size p -> do
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

  quadVAO <- stackIO $ fillPtr $ glGenVertexArrays 1
  quadVBO <- stackIO $ fillPtr $ glGenBuffers 1
  quadEBO <- stackIO $ fillPtr $ glGenBuffers 1

  glBindVertexArray quadVAO

  glBindBuffer GL_ARRAY_BUFFER quadVBO
  stackIO $ withArrayBytes quadVertices $ \size p -> do
    glBufferData GL_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW

  glBindBuffer GL_ELEMENT_ARRAY_BUFFER quadEBO
  stackIO $ withArrayBytes quadIndices $ \size p -> do
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
  let quadObject = RenderObject quadVAO $ fromIntegral $ length quadIndices
  glBindVertexArray 0
  -- see note in loadObject
  -- stackIO $ withArrayLen [quadVBO, quadEBO] $ glDeleteBuffers . fromIntegral

  -- textures

  textures <- forM [minBound .. maxBound] $ \texID -> do
    let imageName = case texID of
          TextureLongKick          -> "long-kick"
          TextureLongOpen          -> "long-open"
          TextureLongOpenHopo      -> "long-open-hopo"
          TextureLongOpenTap       -> "long-open-tap"
          TextureLongEnergy        -> "long-energy"
          TextureLongEnergyHopo    -> "long-energy-hopo"
          TextureLongEnergyTap     -> "long-energy-tap"
          TextureGreenGem          -> "box-green"
          TextureRedGem            -> "box-red"
          TextureYellowGem         -> "box-yellow"
          TextureBlueGem           -> "box-blue"
          TextureOrangeGem         -> "box-orange"
          TextureEnergyGem         -> "box-energy"
          TextureGreenHopo         -> "hopo-green"
          TextureRedHopo           -> "hopo-red"
          TextureYellowHopo        -> "hopo-yellow"
          TextureBlueHopo          -> "hopo-blue"
          TextureOrangeHopo        -> "hopo-orange"
          TextureEnergyHopo        -> "hopo-energy"
          TextureGreenTap          -> "tap-green"
          TextureRedTap            -> "tap-red"
          TextureYellowTap         -> "tap-yellow"
          TextureBlueTap           -> "tap-blue"
          TextureOrangeTap         -> "tap-orange"
          TextureEnergyTap         -> "tap-energy"
          TextureRedCymbal         -> "cymbal-red"
          TextureYellowCymbal      -> "cymbal-yellow"
          TextureBlueCymbal        -> "cymbal-blue"
          TextureGreenCymbal       -> "cymbal-green"
          TextureEnergyCymbal      -> "cymbal-energy"
          TextureLine1             -> "line-1"
          TextureLine2             -> "line-2"
          TextureLine3             -> "line-3"
          TextureTargetGreen       -> "target-green"
          TextureTargetRed         -> "target-red"
          TextureTargetYellow      -> "target-yellow"
          TextureTargetBlue        -> "target-blue"
          TextureTargetOrange      -> "target-orange"
          TextureTargetGreenLight  -> "target-green-light"
          TextureTargetRedLight    -> "target-red-light"
          TextureTargetYellowLight -> "target-yellow-light"
          TextureTargetBlueLight   -> "target-blue-light"
          TextureTargetOrangeLight -> "target-orange-light"
          TextureNumber0           -> "number-0"
          TextureNumber1           -> "number-1"
          TextureNumber2           -> "number-2"
          TextureNumber3           -> "number-3"
          TextureNumber4           -> "number-4"
          TextureNumber5           -> "number-5"
          TextureNumber6           -> "number-6"
          TextureNumber7           -> "number-7"
          TextureNumber8           -> "number-8"
          TextureNumber9           -> "number-9"
          TextureLaneGreen         -> "lane-green"
          TextureLaneRed           -> "lane-red"
          TextureLaneYellow        -> "lane-yellow"
          TextureLaneBlue          -> "lane-blue"
          TextureLaneOrange        -> "lane-orange"
          TextureLanePurple        -> "lane-purple"
          TextureRS0               -> "rs-0"
          TextureRS1               -> "rs-1"
          TextureRS2               -> "rs-2"
          TextureRS3               -> "rs-3"
          TextureRS4               -> "rs-4"
          TextureRS5               -> "rs-5"
          TextureRS6               -> "rs-6"
          TextureRS7               -> "rs-7"
          TextureRS8               -> "rs-8"
          TextureRS9               -> "rs-9"
          TextureRS10              -> "rs-10"
          TextureRS11              -> "rs-11"
          TextureRS12              -> "rs-12"
          TextureRS13              -> "rs-13"
          TextureRS14              -> "rs-14"
          TextureRS15              -> "rs-15"
          TextureRS16              -> "rs-16"
          TextureRS17              -> "rs-17"
          TextureRS18              -> "rs-18"
          TextureRS19              -> "rs-19"
          TextureRS20              -> "rs-20"
          TextureRS21              -> "rs-21"
          TextureRS22              -> "rs-22"
          TextureRS23              -> "rs-23"
          TextureRS24              -> "rs-24"
          TextureRSRed             -> "rs-red"
          TextureRSYellow          -> "rs-yellow"
          TextureRSBlue            -> "rs-blue"
          TextureRSOrange          -> "rs-orange"
          TextureRSGreen           -> "rs-green"
          TextureRSPurple          -> "rs-purple"
          TextureRSHopo            -> "rs-hopo"
          TextureRSTap             -> "rs-tap"
          TextureRSPalmMute        -> "rs-palm-mute"
          TextureRSFretHandMute    -> "rs-fret-hand-mute"
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
    tex <- readExt ["png", "jpg", "jpeg"]
    return (texID, tex)

  -- models

  models <- forM [minBound .. maxBound] $ \modID -> do
    path <- liftIO $ getResourcesPath $ case modID of
      ModelDrumTom       -> "models/drum-tom.obj"
      ModelDrumCymbal    -> "models/drum-cymbal.obj"
      ModelDrumKick      -> "models/drum-kick.obj"
      ModelGuitarStrum   -> "models/gtr-strum.obj"
      ModelGuitarHOPOTap -> "models/gtr-hopotap.obj"
      ModelGuitarOpen    -> "models/gtr-open.obj"
      ModelPGNote        -> "models/pg-note.obj"
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
              , "). See onyx-resources/3d-config.yml to configure anti-aliasing"
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
  framebuffers <- case prefMSAA prefs of
    Just n | n > 1 -> setupMSAA n
    _              -> setupSimple
  let fxaaEnabled = prefFXAA prefs

  videoBGs <- fmap (Map.fromList . catMaybes) $ forM bgs $ \case
    PreviewBGVideo vi -> do
      writeMsg <- getQueueLog
      frameLoader <- stackIO $ forkFrameLoader writeMsg vi
      videoTexRef <- stackIO $ newIORef Nothing
      return $ Just (vi, VideoHandle
        { videoFrameLoader = frameLoader
        , videoTexture     = videoTexRef
        , videoFilePath    = _fileVideo vi
        })
    _ -> return Nothing
  imageBGs <- fmap (Map.fromList . catMaybes) $ forM bgs $ \case
    PreviewBGImage f -> do
      tex <- stackIO (readImage f) >>= either fatal return >>= loadTexture True . convertRGBA8
      return $ Just (f, tex)
    _ -> return Nothing

  return GLStuff{..}

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
deleteGLStuff GLStuff{..} = liftIO $ do
  glDeleteProgram objectShader
  glDeleteProgram quadShader
  withArrayLen (map objVAO $ [boxObject, flatObject, quadObject] ++ map snd models)
    $ glDeleteVertexArrays . fromIntegral
  case framebuffers of
    MSAAFramebuffers{..} -> do
      withArrayLen [msaaFBO, intermediateFBO] $ glDeleteFramebuffers . fromIntegral
      withArrayLen [msaaFBOTex, intermediateFBOTex] $ glDeleteTextures . fromIntegral
      withArrayLen [msaaFBORender] $ glDeleteRenderbuffers . fromIntegral
    SimpleFramebuffer{..} -> do
      withArrayLen [simpleFBO] $ glDeleteFramebuffers . fromIntegral
      withArrayLen [simpleFBOTex] $ glDeleteTextures . fromIntegral
      withArrayLen [simpleFBORender] $ glDeleteRenderbuffers . fromIntegral
  mapM_ (freeTexture . snd) textures

data WindowDims = WindowDims Int Int

drawTextureFade :: GLStuff -> WindowDims -> Texture -> V2 Int -> Int -> IO ()
drawTextureFade glstuff = drawTexture' glstuff $ let
  fade = C.view_track_fade $ C.cfg_view $ gfxConfig glstuff
  in (C.tf_bottom fade, C.tf_top fade)

drawTexture :: GLStuff -> WindowDims -> Texture -> V2 Int -> Int -> IO ()
drawTexture glstuff = drawTexture' glstuff (1, 1)

drawTexture' :: GLStuff -> (Float, Float) -> WindowDims -> Texture -> V2 Int -> Int -> IO ()
drawTexture' GLStuff{..} (fadeBottom, fadeTop) (WindowDims screenW screenH) (Texture tex w h) (V2 x y) scale = do
  glUseProgram quadShader
  glActiveTexture GL_TEXTURE0
  checkGL "glBindTexture" $ glBindTexture GL_TEXTURE_2D tex
  glBindVertexArray $ objVAO quadObject
  let scaleX = fromIntegral (w * scale) / fromIntegral screenW
      scaleY = fromIntegral (h * scale) / fromIntegral screenH
      translateX = (fromIntegral x / fromIntegral screenW) * 2 - 1 + scaleX
      translateY = (fromIntegral y / fromIntegral screenH) * 2 - 1 + scaleY
  sendUniformName quadShader "transform"
    (   translate4 (V3 translateX translateY 0)
    !*! L.scaled (V4 scaleX scaleY 1 1)
    :: M44 Float
    )
  sendUniformName quadShader "inResolution" $ V2
    (fromIntegral (w * scale) :: Float)
    (fromIntegral (h * scale) :: Float)
  sendUniformName quadShader "startFade" fadeBottom
  sendUniformName quadShader "endFade" fadeTop
  sendUniformName quadShader "doFXAA" fxaaEnabled
  checkGL "glDrawElements" $ glDrawElements GL_TRIANGLES (objVertexCount quadObject) GL_UNSIGNED_INT nullPtr

-- | Covers the screen with the texture, preserving aspect ratio and possibly clipping some of the texture.
drawBackground :: GLStuff -> WindowDims -> Texture -> IO ()
drawBackground GLStuff{..} (WindowDims screenW screenH) (Texture tex w h) = do
  glUseProgram quadShader
  glActiveTexture GL_TEXTURE0
  checkGL "glBindTexture" $ glBindTexture GL_TEXTURE_2D tex
  glBindVertexArray $ objVAO quadObject
  let textureRatio = fromIntegral w       / fromIntegral h       :: Float
      screenRatio  = fromIntegral screenW / fromIntegral screenH :: Float
      scaleX = if textureRatio > screenRatio
        then textureRatio / screenRatio -- texture may clip left/right
        else 1                          -- texture may clip top/bottom
      scaleY = if textureRatio > screenRatio
        then 1                          -- texture may clip left/right
        else screenRatio / textureRatio -- texture may clip top/bottom
  sendUniformName quadShader "transform"
    (L.scaled (V4 scaleX scaleY 1 1) :: M44 Float)
  sendUniformName quadShader "startFade" (1 :: Float)
  sendUniformName quadShader "endFade" (1 :: Float)
  sendUniformName quadShader "doFXAA" False
  checkGL "glDrawElements" $ glDrawElements GL_TRIANGLES (objVertexCount quadObject) GL_UNSIGNED_INT nullPtr

freeTexture :: (MonadIO m) => Texture -> m ()
freeTexture (Texture tex _ _) = liftIO $ with tex $ glDeleteTextures 1

-- | Split up the available space to show tracks at the largest possible size.
-- Returns [(x, y, w, h)] in GL space (so bottom left corner is (0, 0)).
splitSpace :: Int -> Float -> WindowDims -> [(Int, Int, Int, Int)]
splitSpace n heightWidthRatio (WindowDims w h) = let
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

setUpTrackView :: GLStuff -> WindowDims -> IO ()
setUpTrackView GLStuff{..} (WindowDims w h) = do
  glClear GL_DEPTH_BUFFER_BIT
  glUseProgram objectShader
  let viewPosn = C.cam_position $ C.view_camera $ C.cfg_view gfxConfig
      tiltDown = C.cam_rotate $ C.view_camera $ C.cfg_view gfxConfig
      view, projection :: M44 Float
      view
        = L.mkTransformation (L.axisAngle (V3 1 0 0) (degrees tiltDown)) 0
        !*! translate4 (negate viewPosn)
        -- note, this translates then rotates (can't just give V3 to mkTransformation)
      projection = L.perspective
        (degrees $ C.cam_fov $ C.view_camera $ C.cfg_view gfxConfig)
        (fromIntegral w / fromIntegral h)
        (C.cam_near $ C.view_camera $ C.cfg_view gfxConfig)
        (C.cam_far $ C.view_camera $ C.cfg_view gfxConfig)
  sendUniformName objectShader "view" view
  sendUniformName objectShader "projection" projection
  sendUniformName objectShader "viewPos" viewPosn

drawDrumPlayFull
  :: GLStuff
  -> WindowDims
  -> Double
  -> Double
  -> DrumPlayState Double (D.Gem D.ProType)
  -> IO ()
drawDrumPlayFull glStuff@GLStuff{..} dims@(WindowDims wWhole hWhole) time speed dps = do
  glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
  case C.view_background $ C.cfg_view gfxConfig of
    V4 r g b a -> glClearColor r g b a
  glClear GL_COLOR_BUFFER_BIT

  setUpTrackView glStuff dims
  drawDrumPlay glStuff time speed dps

  glClear GL_DEPTH_BUFFER_BIT
  let gps = case drumEvents dps of
        (_, (_, s)) : _ -> s
        _               -> initialState
      digitScale = 2
      digitWidth = 14
      digitHeight = 18
      drawNumber = drawNumber' True
      drawNumber' write0 n x y = case quotRem n 10 of
        (q, r) -> when (q /= 0 || r /= 0 || write0) $ do
          case drop r [TextureNumber0 .. TextureNumber9] of
            texid : _ -> case lookup texid textures of
              Just tex -> drawTexture glStuff dims tex (V2 x y) digitScale
              Nothing  -> return ()
            [] -> return ()
          drawNumber' False q (x - digitWidth * digitScale) y
  drawNumber (gameScore gps) (wWhole - digitWidth * digitScale) (hWhole - digitHeight * digitScale)
  drawNumber (gameCombo gps) (quot wWhole 2) 0

drawTracks
  :: GLStuff
  -> WindowDims
  -> Double
  -> Double
  -> (Maybe PreviewBG)
  -> [PreviewTrack]
  -> IO ()
drawTracks glStuff@GLStuff{..} dims@(WindowDims wWhole hWhole) time speed bg trks = do
  glBindFramebuffer GL_FRAMEBUFFER 0
  glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
  case bg of
    Just _  -> glClearColor 0 0 0 255
    Nothing -> case C.view_background $ C.cfg_view gfxConfig of
      V4 r g b a -> glClearColor r g b a
  glClear GL_COLOR_BUFFER_BIT

  forM_ bg $ \case
    PreviewBGVideo vi -> case Map.lookup vi videoBGs of
      Just VideoHandle{..} -> do
        -- TODO maybe separate out the timestamp updates from drawing
        frameMessage videoFrameLoader $ RequestFrame time
        mtex <- getFrame videoFrameLoader >>= \case
          Nothing -> return Nothing
          Just (timeNew, image) -> readIORef videoTexture >>= \case
            Nothing -> do
              tex <- loadTexture True image
              writeIORef videoTexture $ Just (timeNew, tex)
              return $ Just tex
            Just (timeCurrent, tex) -> do
              when (timeNew /= timeCurrent) $ do
                updateTexture image tex
                writeIORef videoTexture $ Just (timeNew, tex)
              return $ Just tex
        forM_ mtex $ \tex -> do
          glClear GL_DEPTH_BUFFER_BIT
          glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
          drawBackground glStuff dims tex
      Nothing -> return ()
    PreviewBGImage f -> case Map.lookup f imageBGs of
      Just tex -> do
        glClear GL_DEPTH_BUFFER_BIT
        glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
        drawBackground glStuff dims tex
      Nothing -> return ()

  let spaces = case trks of
        [] -> []
        _  -> splitSpace (length trks)
          (C.view_height_width_ratio $ C.cfg_view gfxConfig)
          dims

  forM_ (zip spaces trks) $ \((x, y, w, h), trk) -> checkGL "draw" $ do
    glBindFramebuffer GL_FRAMEBUFFER $ case framebuffers of
      SimpleFramebuffer{..} -> simpleFBO
      MSAAFramebuffers{..}  -> msaaFBO
    setFramebufferSize framebuffers
      (fromIntegral w) (fromIntegral h)
    glViewport 0 0 (fromIntegral w) (fromIntegral h)
    glClearColor 0 0 0 0
    glClear GL_COLOR_BUFFER_BIT
    setUpTrackView glStuff (WindowDims w h)
    case trk of
      PreviewDrums m -> drawDrums glStuff time speed m
      PreviewFive  m -> drawFive  glStuff time speed m
      PreviewPG    m -> drawPG    glStuff time speed m

    case framebuffers of
      SimpleFramebuffer{..} -> do

        glBindFramebuffer GL_FRAMEBUFFER 0
        glClear GL_DEPTH_BUFFER_BIT
        glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
        drawTextureFade glStuff dims (Texture simpleFBOTex w h) (V2 x y) 1

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
        drawTextureFade glStuff dims (Texture intermediateFBOTex w h) (V2 x y) 1

checkGL :: (MonadIO m) => String -> m a -> m a
checkGL s f = do
  void glGetError
  x <- f
  desc <- glGetError >>= return . \case
    GL_NO_ERROR          -> Nothing
    GL_INVALID_ENUM      -> Just "Invalid enum"
    GL_INVALID_VALUE     -> Just "Invalid value"
    GL_INVALID_OPERATION -> Just "Invalid operation"
    GL_OUT_OF_MEMORY     -> Just "Out of memory"
    _                    -> Just "Unknown error"
  liftIO $ forM_ desc $ \err -> putStrLn $ s <> ": " <> err
  return x
