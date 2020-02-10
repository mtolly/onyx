{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module RhythmGame.Graphics where

import           Codec.Picture
import qualified Codec.Wavefront        as Obj
import           Control.Arrow          (second)
import           Control.Exception      (bracket)
import           Control.Monad          (forM, forM_, guard, void, when)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString        as B
import           Data.FileEmbed         (embedFile, makeRelativeToProject)
import           Data.List              (partition)
import           Data.List.HT           (partitionMaybe)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe, isJust)
import qualified Data.Set               as Set
import qualified Data.Vector            as V
import qualified Data.Vector.Storable   as VS
import           Foreign                hiding (void)
import           Foreign.C
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear                 (M44, V2 (..), V3 (..), V4 (..), (!*!))
import qualified Linear                 as L
import           Resources              (getResourcesPath)
import           RhythmGame.PNF
import           RhythmGame.Track
import           RockBand.Codec.Beat
import qualified RockBand.Codec.Drums   as D
import qualified RockBand.Codec.Five    as F
import           RockBand.Common        (StrumHOPOTap (..))

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
  = LightGlobal (V3 Float)
  | LightOffset (V3 Float)

drawObject :: GLStuff -> (Int, Int) -> Object -> ObjectPosition -> Either TextureID (V4 Float) -> Float -> LightPosition -> IO ()
drawObject GLStuff{..} (viewY, viewH) obj posn texcolor alpha lightOffset = do
  let colorType   = 1 :: GLuint
      textureType = 2 :: GLuint
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
  sendUniformName objectShader "startFade" (fromIntegral viewY + fromIntegral viewH * (519 / 671) :: Float)
  sendUniformName objectShader "endFade" (fromIntegral viewY + fromIntegral viewH * (558 / 671) :: Float)
  case lightOffset of
    LightGlobal g -> sendUniformName objectShader "light.position" g
    LightOffset off -> do
      let center = case posn of
            ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)
              -> V3 ((x1 + x2) / 2) (max y1 y2) ((z1 + z2) / 2)
            ObjectMove xyz -> xyz
      sendUniformName objectShader "light.position" $ center + off
  case texcolor of
    Right color -> do
      sendUniformName objectShader "material.diffuse.type" colorType
      sendUniformName objectShader "material.diffuse.color" color
    Left texid -> do
      case lookup texid textures of
        Just tex -> do
          glActiveTexture GL_TEXTURE0
          glBindTexture GL_TEXTURE_2D $ textureGL tex
          sendUniformName objectShader "material.diffuse.type" textureType
          sendUniformName objectShader "material.diffuse.image" (0 :: GLint)
        Nothing -> do
          sendUniformName objectShader "material.diffuse.type" colorType
          sendUniformName objectShader "material.diffuse.color" (V4 1 0 1 1 :: V4 Float)
  sendUniformName objectShader "material.specular.type" colorType
  sendUniformName objectShader "material.specular.color" (V4 0.5 0.5 0.5 1 :: V4 Float)
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

drawDrums :: GLStuff -> (Int, Int) -> Double -> Double -> Map.Map Double (CommonState (DrumState (D.Gem D.ProType))) -> IO ()
drawDrums glStuff@GLStuff{..} ydims nowTime speed trk = do
  glUseProgram objectShader
  -- view and projection matrices should already have been set
  let drawObject' = drawObject glStuff ydims
      globalLight = LightGlobal $ V3 0 -0.5 0.5
      nearZ = 2 :: Float
      nowZ = 0 :: Float
      farZ = -12 :: Float
      farTime = nowTime + speed :: Double
      timeToZ t = nowZ + (farZ - nowZ) * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + (farTime - nowTime) * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      zoomed = zoomMap nearTime farTime trk
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
          Nothing -> Left texid
          Just _  -> Right $ V4 1 1 1 1
        posn = case gem of
          D.Kick           -> gemAtX 0
          D.Red            -> gemAtX -0.75
          D.Pro D.Yellow _ -> gemAtX -0.25
          D.Pro D.Blue _   -> gemAtX 0.25
          D.Pro D.Green _  -> gemAtX 0.75
          D.Orange         -> gemAtX 0.75 -- TODO
        gemAtX x = ObjectMove $ V3 x -1 z
        z = timeToZ t
        in drawObject' obj posn shade (fromMaybe 1 alpha) $ LightOffset $ V3 0 1 0
      drawNotes t cs = let
        od = case commonOverdrive cs of
          ToggleEmpty -> False
          ToggleEnd   -> False
          _           -> True
        in forM_ (drumNotes $ commonState cs) $ \gem -> if nowTime <= t
          then drawGem t od gem Nothing
          else if nowTime - t < 0.1
            then drawGem nowTime od gem $ Just $ realToFrac $ 1 - (nowTime - t) * 10
            else return ()
      drawBeat t cs = case commonBeats cs of
        Nothing -> return ()
        Just e -> let
          tex = case e of
            Just Bar  -> TextureLine1
            Just Beat -> TextureLine2
            Nothing   -> TextureLine3
          z = timeToZ t
          xyz1 = V3 -1 -1 (z + 0.05)
          xyz2 = V3  1 -1 (z - 0.05)
          in drawObject' Flat (ObjectStretch xyz1 xyz2) (Left tex) 1 globalLight
      drawTargetSquare i tex alpha = let
        pieceWidth = 0.5
        x1 = -1 + pieceWidth * i
        x2 = x1 + pieceWidth
        y = -1
        z1 = 0.17
        z2 = -0.17
        in drawObject' Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (Left tex) alpha globalLight
  -- draw highway
  forM_ (makeToggleBounds nearTime farTime $ fmap commonSolo zoomed) $ \(t1, t2, isSolo) -> do
    let highwayColor = if isSolo
          then V4 0.2 0.2 0.6 1
          else V4 0.2 0.2 0.2 1
    drawObject'
      Flat
      (ObjectStretch
        (V3 -1 -1 (timeToZ t1))
        (V3 1 -1 (timeToZ t2))
      )
      (Right highwayColor)
      1
      globalLight
  -- draw railings
  drawObject' Box
    (ObjectStretch (V3 -1.09 -0.85 nearZ) (V3 -1    -1.1 farZ))
    (Right $ V4 0.4 0.2 0.6 1) 1 globalLight
  drawObject' Box
    (ObjectStretch (V3  1    -0.85 nearZ) (V3  1.09 -1.1 farZ))
    (Right $ V4 0.4 0.2 0.6 1) 1 globalLight
  -- draw beat lines
  glDepthFunc GL_ALWAYS
  void $ Map.traverseWithKey drawBeat zoomed
  -- draw target
  mapM_ (\(i, tex) -> drawTargetSquare i tex 1) $
    zip [0..] [TextureTargetRed, TextureTargetYellow, TextureTargetBlue, TextureTargetGreen]
  let drawLights [] _ = return ()
      drawLights _ [] = return ()
      drawLights ((t, cs) : states) colors = let
        gemsHere = drumNotes $ commonState cs
        (colorsYes, colorsNo) = partition (\(_, _, pads) -> any (`Set.member` gemsHere) pads) colors
        alpha = realToFrac $ 1 - (nowTime - t) * 6
        in do
          forM_ colorsYes $ \(i, light, _) -> drawTargetSquare i light alpha
          drawLights states colorsNo
  drawLights (Map.toDescList $ fst $ Map.split nowTime zoomed)
    [ (0, TextureTargetRedLight   , [D.Red                                        ])
    , (1, TextureTargetYellowLight, [D.Pro D.Yellow D.Tom, D.Pro D.Yellow D.Cymbal])
    , (2, TextureTargetBlueLight  , [D.Pro D.Blue   D.Tom, D.Pro D.Blue   D.Cymbal])
    , (3, TextureTargetGreenLight , [D.Pro D.Green  D.Tom, D.Pro D.Green  D.Cymbal])
    ]
  glDepthFunc GL_LESS
  -- draw notes
  void $ Map.traverseWithKey drawNotes zoomed

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

drawFive :: GLStuff -> (Int, Int) -> Double -> Double -> Map.Map Double (CommonState (GuitarState (Maybe F.Color))) -> IO ()
drawFive glStuff@GLStuff{..} ydims nowTime speed trk = do
  glUseProgram objectShader
  -- view and projection matrices should already have been set
  let drawObject' = drawObject glStuff ydims
      globalLight = LightGlobal $ V3 0 -0.5 0.5
      nearZ = 2 :: Float
      nowZ = 0 :: Float
      farZ = -12 :: Float
      farTime = nowTime + speed :: Double
      timeToZ t = nowZ + (farZ - nowZ) * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + (farTime - nowTime) * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      zoomed = zoomMap nearTime farTime trk
      drawSustain t1 t2 od color
        | t2 <= nowTime = return ()
        | otherwise     = let
          boxColor = if od
            then V4 (108/255) (247/255) (198/255) 1
            else case color of
              Nothing       -> V4 (195/255) (110/255) (244/255) 1
              Just F.Green  -> V4 (75/255) (252/255) (90/255) 1
              Just F.Red    -> V4 (244/255) (95/255) (93/255) 1
              Just F.Yellow -> V4 (244/255) (224/255) (93/255) 1
              Just F.Blue   -> V4 (122/255) (116/255) (247/255) 1
              Just F.Orange -> V4 (244/255) (177/255) (83/255) 1
          (x1, x2) = case color of
            Nothing       -> (-0.4,  0.4)
            Just F.Green  -> (-0.9, -0.7)
            Just F.Red    -> (-0.5, -0.3)
            Just F.Yellow -> (-0.1,  0.1)
            Just F.Blue   -> ( 0.3,  0.5)
            Just F.Orange -> ( 0.7,  0.9)
          (y1, y2) = (-0.98, -1)
          (z1, z2) = (timeToZ $ max nowTime t1, timeToZ t2)
          in drawObject' Box (ObjectStretch (V3 x1 y1 z1) (V3 x2 y2 z2)) (Right boxColor) 1 globalLight
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
          Nothing -> Left texid
          Just _  -> Right $ V4 1 1 1 1
        posn = case color of
          Nothing       -> gemAtX 0
          Just F.Green  -> gemAtX -0.8
          Just F.Red    -> gemAtX -0.4
          Just F.Yellow -> gemAtX 0
          Just F.Blue   -> gemAtX 0.4
          Just F.Orange -> gemAtX 0.8
        gemAtX x = ObjectMove $ V3 x -1 z
        z = timeToZ t
        in drawObject' obj posn shade (fromMaybe 1 alpha) $ LightOffset $ V3 0 1 0
      drawNotes _        []                     = return ()
      drawNotes prevTime ((thisTime, cs) : rest) = do
        let notes = Map.toList $ guitarNotes $ commonState cs
        -- draw preceding sustain
        forM_ notes $ \(color, pnf) -> forM_ (getPast pnf) $ \od -> do
          drawSustain prevTime thisTime od color
        -- draw note
        let thisOD = case commonOverdrive cs of
              ToggleEmpty -> False
              ToggleEnd   -> False
              _           -> True
        forM_ notes $ \(color, pnf) -> forM_ (getNow pnf) $ \sht -> if nowTime <= thisTime
          then drawGem thisTime thisOD color sht Nothing
          else if nowTime - thisTime < 0.1
            then drawGem nowTime thisOD color sht $ Just $ realToFrac $ 1 - (nowTime - thisTime) * 10
            else return ()
        -- draw future sustain if rest is empty
        when (null rest) $ do
          forM_ notes $ \(color, pnf) -> forM_ (getFuture pnf) $ \od -> do
            drawSustain thisTime farTime od color
        drawNotes thisTime rest
      drawBeat t cs = case commonBeats cs of
        Nothing -> return ()
        Just e -> let
          tex = case e of
            Just Bar  -> TextureLine1
            Just Beat -> TextureLine2
            Nothing   -> TextureLine3
          z = timeToZ t
          xyz1 = V3 -1 -1 (z + 0.05)
          xyz2 = V3  1 -1 (z - 0.05)
          in drawObject' Flat (ObjectStretch xyz1 xyz2) (Left tex) 1 globalLight
      drawTargetSquare i tex alpha = let
        pieceWidth = 0.4
        x1 = -1 + pieceWidth * i
        x2 = x1 + pieceWidth
        y = -1
        z1 = 0.17
        z2 = -0.17
        in drawObject' Flat (ObjectStretch (V3 x1 y z1) (V3 x2 y z2)) (Left tex) alpha globalLight
  -- draw highway
  forM_ (makeToggleBounds nearTime farTime $ fmap commonSolo zoomed) $ \(t1, t2, isSolo) -> do
    let highwayColor = if isSolo
          then V4 0.2 0.2 0.6 1
          else V4 0.2 0.2 0.2 1
    drawObject'
      Flat
      (ObjectStretch
        (V3 -1 -1 (timeToZ t1))
        (V3 1 -1 (timeToZ t2))
      )
      (Right highwayColor)
      1
      globalLight
  -- draw railings
  drawObject' Box
    (ObjectStretch (V3 -1.09 -0.85 nearZ) (V3 -1    -1.1 farZ))
    (Right $ V4 0.4 0.2 0.6 1) 1 globalLight
  drawObject' Box
    (ObjectStretch (V3  1    -0.85 nearZ) (V3  1.09 -1.1 farZ))
    (Right $ V4 0.4 0.2 0.6 1) 1 globalLight
  -- draw beatlines
  glDepthFunc GL_ALWAYS
  void $ Map.traverseWithKey drawBeat zoomed
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
        alpha = realToFrac $ 1 - (nowTime - t) * 6
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
  drawNotes nearTime $ Map.toList zoomed

compileShader :: GLenum -> B.ByteString -> IO GLuint
compileShader shaderType source = do
  shader <- glCreateShader shaderType
  B.useAsCString source $ \cs' -> with cs' $ \cs -> do
    glShaderSource shader 1 cs nullPtr
    glCompileShader shader
    alloca $ \success -> do
      allocaArray 512 $ \infoLog -> do
        glGetShaderiv shader GL_COMPILE_STATUS success
        peek success >>= \case
          GL_FALSE -> do
            glGetShaderInfoLog shader 512 nullPtr infoLog
            peekCString infoLog >>= error
          _ -> return shader

compileProgram :: [GLuint] -> IO GLuint
compileProgram shaders = do
  program <- glCreateProgram
  mapM_ (glAttachShader program) shaders
  glLinkProgram program
  alloca $ \success -> do
    allocaArray 512 $ \infoLog -> do
      glGetProgramiv program GL_LINK_STATUS success
      peek success >>= \case
        GL_FALSE -> do
          glGetProgramInfoLog program 512 nullPtr infoLog
          peekCString infoLog >>= error
        _ -> return program

withArrayBytes :: (Storable a, Num len) => [a] -> (len -> Ptr a -> IO b) -> IO b
withArrayBytes xs f = withArray xs $ \p -> let
  bytes = fromIntegral $ length xs * sizeOf (head xs)
  in f bytes p

data Vertex = Vertex
  { vertexPosition  :: V3 CFloat
  , vertexNormal    :: V3 CFloat
  , vertexTexCoords :: V2 CFloat
  } deriving (Eq, Show)

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

objectVS, objectFS :: B.ByteString
objectVS = $(makeRelativeToProject "shaders/object.vert" >>= embedFile)
objectFS = $(makeRelativeToProject "shaders/object.frag" >>= embedFile)

quadVS, quadFS :: B.ByteString
quadVS = $(makeRelativeToProject "shaders/quad.vert" >>= embedFile)
quadFS = $(makeRelativeToProject "shaders/quad.frag" >>= embedFile)

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

loadTexture :: (GLPixel a) => Bool -> Image a -> IO Texture
loadTexture linear img = do
  let pixelProp :: (a -> b) -> Image a -> b
      pixelProp f _ = f undefined
      flippedVert = generateImage
        (\x y -> pixelAt img x $ imageHeight img - y - 1)
        (imageWidth img)
        (imageHeight img)
  texture <- alloca $ \p -> glGenTextures 1 p >> peek p
  glBindTexture GL_TEXTURE_2D texture
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
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
  } deriving (Show)

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
  deriving (Eq, Show, Enum, Bounded)

data ModelID
  = ModelDrumTom
  | ModelDrumCymbal
  | ModelDrumKick
  | ModelGuitarStrum
  | ModelGuitarHOPOTap
  | ModelGuitarOpen
  deriving (Eq, Show, Enum, Bounded)

loadObj :: FilePath -> IO [Vertex]
loadObj f = do
  obj <- Obj.fromFile f >>= either fail return
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

loadGLStuff :: IO GLStuff
loadGLStuff = do

  glEnable GL_DEPTH_TEST
  glEnable GL_CULL_FACE -- default CCW = front
  glEnable GL_BLEND
  glEnable GL_MULTISAMPLE
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

  objectShader <- bracket (compileShader GL_VERTEX_SHADER objectVS) glDeleteShader $ \vertexShader -> do
    bracket (compileShader GL_FRAGMENT_SHADER objectFS) glDeleteShader $ \fragmentShader -> do
      compileProgram [vertexShader, fragmentShader]

  let loadObject vertices = do
        vao <- alloca $ \p -> glGenVertexArrays 1 p >> peek p
        vbo <- alloca $ \p -> glGenBuffers 1 p >> peek p
        glBindVertexArray vao
        glBindBuffer GL_ARRAY_BUFFER vbo
        withArrayBytes vertices $ \size p -> do
          glBufferData GL_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW
        writeParts 0 0 vertexParts
        glBindVertexArray 0
        withArrayLen [vbo] $ glDeleteBuffers . fromIntegral
        return RenderObject
          { objVAO         = vao
          , objVertexCount = fromIntegral $ length vertices
          }

  boxObject <- loadObject simpleBox
  flatObject <- loadObject simpleFlat

  -- quad stuff

  quadShader <- bracket (compileShader GL_VERTEX_SHADER quadVS) glDeleteShader $ \vertexShader -> do
    bracket (compileShader GL_FRAGMENT_SHADER quadFS) glDeleteShader $ \fragmentShader -> do
      compileProgram [vertexShader, fragmentShader]

  quadVAO <- alloca $ \p -> glGenVertexArrays 1 p >> peek p
  quadVBO <- alloca $ \p -> glGenBuffers 1 p >> peek p
  quadEBO <- alloca $ \p -> glGenBuffers 1 p >> peek p

  glBindVertexArray quadVAO

  glBindBuffer GL_ARRAY_BUFFER quadVBO
  withArrayBytes quadVertices $ \size p -> do
    glBufferData GL_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW

  glBindBuffer GL_ELEMENT_ARRAY_BUFFER quadEBO
  withArrayBytes quadIndices $ \size p -> do
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
  sendUniformName quadShader "ourTexture" (0 :: GLint)
  let quadObject = RenderObject quadVAO $ fromIntegral $ length quadIndices
  glBindVertexArray 0
  withArrayLen [quadVBO, quadEBO] $ glDeleteBuffers . fromIntegral

  -- textures

  textures <- forM [minBound .. maxBound] $ \texID -> do
    path <- getResourcesPath $ case texID of
      TextureLongKick          -> "textures/long-kick.jpg"
      TextureLongOpen          -> "textures/long-open.jpg"
      TextureLongOpenHopo      -> "textures/long-open-hopo.jpg"
      TextureLongOpenTap       -> "textures/long-open-tap.jpg"
      TextureLongEnergy        -> "textures/long-energy.jpg"
      TextureLongEnergyHopo    -> "textures/long-energy-hopo.jpg"
      TextureLongEnergyTap     -> "textures/long-energy-tap.jpg"
      TextureGreenGem          -> "textures/box-green.jpg"
      TextureRedGem            -> "textures/box-red.jpg"
      TextureYellowGem         -> "textures/box-yellow.jpg"
      TextureBlueGem           -> "textures/box-blue.jpg"
      TextureOrangeGem         -> "textures/box-orange.jpg"
      TextureEnergyGem         -> "textures/box-energy.jpg"
      TextureGreenHopo         -> "textures/hopo-green.jpg"
      TextureRedHopo           -> "textures/hopo-red.jpg"
      TextureYellowHopo        -> "textures/hopo-yellow.jpg"
      TextureBlueHopo          -> "textures/hopo-blue.jpg"
      TextureOrangeHopo        -> "textures/hopo-orange.jpg"
      TextureEnergyHopo        -> "textures/hopo-energy.jpg"
      TextureGreenTap          -> "textures/tap-green.jpg"
      TextureRedTap            -> "textures/tap-red.jpg"
      TextureYellowTap         -> "textures/tap-yellow.jpg"
      TextureBlueTap           -> "textures/tap-blue.jpg"
      TextureOrangeTap         -> "textures/tap-orange.jpg"
      TextureEnergyTap         -> "textures/tap-energy.jpg"
      TextureRedCymbal         -> "textures/cymbal-red.jpg"
      TextureYellowCymbal      -> "textures/cymbal-yellow.jpg"
      TextureBlueCymbal        -> "textures/cymbal-blue.jpg"
      TextureGreenCymbal       -> "textures/cymbal-green.jpg"
      TextureEnergyCymbal      -> "textures/cymbal-energy.jpg"
      TextureLine1             -> "textures/line-1.png"
      TextureLine2             -> "textures/line-2.png"
      TextureLine3             -> "textures/line-3.png"
      TextureTargetGreen       -> "textures/target-green.jpg"
      TextureTargetRed         -> "textures/target-red.jpg"
      TextureTargetYellow      -> "textures/target-yellow.jpg"
      TextureTargetBlue        -> "textures/target-blue.jpg"
      TextureTargetOrange      -> "textures/target-orange.jpg"
      TextureTargetGreenLight  -> "textures/target-green-light.jpg"
      TextureTargetRedLight    -> "textures/target-red-light.jpg"
      TextureTargetYellowLight -> "textures/target-yellow-light.jpg"
      TextureTargetBlueLight   -> "textures/target-blue-light.jpg"
      TextureTargetOrangeLight -> "textures/target-orange-light.jpg"
    tex <- readImage path >>= either fail return >>= loadTexture True . convertRGBA8
    return (texID, tex)

  -- models

  models <- forM [minBound .. maxBound] $ \modID -> do
    path <- getResourcesPath $ case modID of
      ModelDrumTom       -> "models/drum-tom.obj"
      ModelDrumCymbal    -> "models/drum-cymbal.obj"
      ModelDrumKick      -> "models/drum-kick.obj"
      ModelGuitarStrum   -> "models/gtr-strum.obj"
      ModelGuitarHOPOTap -> "models/gtr-hopotap.obj"
      ModelGuitarOpen    -> "models/gtr-open.obj"
    obj <- loadObj path >>= loadObject
    return (modID, obj)

  return GLStuff{..}

deleteGLStuff :: GLStuff -> IO ()
deleteGLStuff GLStuff{..} = do
  glDeleteProgram objectShader
  glDeleteProgram quadShader
  withArrayLen (map objVAO $ [boxObject, flatObject, quadObject] ++ map snd models)
    $ glDeleteVertexArrays . fromIntegral
  mapM_ (freeTexture . snd) textures

data WindowDims = WindowDims Int Int

drawTexture :: GLStuff -> WindowDims -> Texture -> V2 Int -> Int -> IO ()
drawTexture GLStuff{..} (WindowDims screenW screenH) (Texture tex w h) (V2 x y) scale = do
  glUseProgram quadShader
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D tex
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
  glDrawElements GL_TRIANGLES (objVertexCount quadObject) GL_UNSIGNED_INT nullPtr

freeTexture :: Texture -> IO ()
freeTexture (Texture tex _ _) = with tex $ glDeleteTextures 1

-- | Split up the available space to show tracks at the largest possible size.
-- Returns [(x, y, w, h)] in GL space (so bottom left corner is (0, 0)).
splitSpace :: Int -> Double -> WindowDims -> [(Int, Int, Int, Int)]
splitSpace n heightWidthRatio (WindowDims w h) = let
  fi :: Int -> Double
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

drawTracks
  :: GLStuff
  -> WindowDims
  -> Double
  -> Double
  -> [PreviewTrack]
  -> IO ()
drawTracks glStuff@GLStuff{..} dims@(WindowDims wWhole hWhole) time speed trks = do
  glViewport 0 0 (fromIntegral wWhole) (fromIntegral hWhole)
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT

  let spaces = case trks of
        [] -> []
        _  -> splitSpace (length trks) (7/6) dims
  forM_ (zip spaces trks) $ \((x, y, w, h), trk) -> do
    glClear GL_DEPTH_BUFFER_BIT
    glUseProgram objectShader
    let viewPosn = V3 0 1.4 3 :: V3 Float
        view, projection :: M44 Float
        view
          = L.mkTransformation (L.axisAngle (V3 1 0 0) (degrees 25)) 0
          !*! translate4 (negate viewPosn)
          -- note, this translates then rotates (can't just give V3 to mkTransformation)
        projection = L.perspective (degrees 45) (fromIntegral w / fromIntegral h) 0.1 100
    sendUniformName objectShader "view" view
    sendUniformName objectShader "projection" projection
    -- light.position gets sent later
    sendUniformName objectShader "light.ambient" (V3 0.2 0.2 0.2 :: V3 Float)
    sendUniformName objectShader "light.diffuse" (V3 1 1 1 :: V3 Float)
    sendUniformName objectShader "light.specular" (V3 1 1 1 :: V3 Float)
    sendUniformName objectShader "viewPos" viewPosn
    glViewport (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
    case trk of
      PreviewDrums m -> drawDrums glStuff (y, h) time speed m
      PreviewFive m  -> drawFive  glStuff (y, h) time speed m
