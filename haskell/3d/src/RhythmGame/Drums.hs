{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module RhythmGame.Drums where

import           Codec.Picture
import           Control.Exception        (bracket)
import           Control.Monad            (forM_, when)
import           Control.Monad.IO.Class   (MonadIO (..))
import qualified Data.ByteString          as B
import           Data.FileEmbed           (embedFile, makeRelativeToProject)
import           Data.List                (partition)
import qualified Data.Map.Strict          as Map
import           Data.Map.Strict.Internal (Map (..))
import           Data.Maybe               (fromMaybe)
import qualified Data.Vector.Storable     as VS
import           Foreign
import           Foreign.C
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear                   (M44, V2 (..), V3 (..), V4 (..),
                                           (!*!))
import qualified Linear                   as L
import qualified RockBand.Codec.Drums     as D
import qualified Assimp as A
import Data.Foldable (toList)
import qualified Data.Text as T

data Note t a
  = Upcoming a
  | Hit t a
  | Missed a
  | Autoplay a
  deriving (Eq, Ord, Show, Read)

data Track t a = Track
  { trackNotes    :: Map t [Note t a] -- ^ lists must be non-empty
  , trackOverhits :: Map t [a] -- ^ lists must be non-empty
  , trackTime     :: t -- ^ the latest timestamp we have reached
  , trackWindow   :: t -- ^ the half-window of time on each side of a note
  } deriving (Eq, Ord, Show, Read)

-- | Efficiently updates the range @(k1, k2)@ of values.
updateRange :: (Ord k) => (k -> a -> a) -> k -> k -> Map k a -> Map k a
updateRange f k1 k2 = let
  go = \case
    Tip -> Tip
    Bin size k v mL mR -> if k <= k1
      then Bin size k v mL (go mR)
      else if k2 <= k
        then Bin size k v (go mL) mR
        else let
          v' = f k v
          in v' `seq` Bin size k v' (go mL) (go mR)
  in go

traverseRange_ :: (Applicative f, Ord k) => (k -> a -> f ()) -> Bool -> k -> k -> Map k a -> f ()
traverseRange_ f asc k1 k2 = let
  go = \case
    Tip -> pure ()
    Bin _ k v mL mR -> if k <= k1
      then go mR
      else if k2 <= k
        then go mL
        else if asc
          then go mL *> f k v *> go mR
          else go mR *> f k v *> go mL
  in go

updateTime :: (Num t, Ord t) => t -> Track t a -> Track t a
updateTime t trk = let
  f _ = map $ \case
    Upcoming x -> Missed x
    evt        -> evt
  in trk
    { trackTime = t
    , trackNotes = updateRange f (trackTime trk - trackWindow trk) (t - trackWindow trk) $ trackNotes trk
    }

hitPad :: (Num t, Ord t, Eq a) => t -> a -> Track t a -> Track t a
hitPad t x trk = let
  p1 = Map.lookupLE t $ trackNotes trk
  p2 = Map.lookupGE t $ trackNotes trk
  closestNotes = case p1 of
    Nothing -> case p2 of
      Nothing      -> Nothing
      Just (k2, _) -> if k2 - t < trackWindow trk then p2 else Nothing
    Just (k1, _) -> case p2 of
      Nothing -> if t - k1 < trackWindow trk then p1 else Nothing
      Just (k2, _) -> let
        distance1 = t - k1
        distance2 = k2 - t
        in if distance1 < distance2
          then if distance1 < trackWindow trk then p1 else Nothing
          else if distance2 < trackWindow trk then p2 else Nothing
  overhit = trk { trackOverhits = Map.alter newOverhit t $ trackOverhits trk }
  newOverhit = Just . (x :) . fromMaybe []
  in case closestNotes of
    Nothing -> overhit
    Just (k, notes) -> case partition (== Upcoming x) notes of
      ([], _) -> overhit
      (_ : _, notes') ->
        trk { trackNotes = Map.insert k (Hit t x : notes') $ trackNotes trk }

drawDrums :: GLStuff -> Track Double (D.Gem a) -> IO ()
drawDrums GLStuff{..} trk = do
  let drawCube :: V3 Float -> V3 Float -> V3 Float -> IO ()
      drawCube (V3 x1 y1 z1) (V3 x2 y2 z2) color = do
        sendUniformName cubeShader "model"
          $ translate4 (V3 ((x1 + x2) / 2) ((y1 + y2) / 2) ((z1 + z2) / 2))
          !*! L.scaled (V4 (x2 - x1) (y2 - y1) (z2 - z1) 1)
        sendUniformName cubeShader "material.ambient" color
        sendUniformName cubeShader "material.diffuse" color
        sendUniformName cubeShader "material.specular" (V3 0.5 0.5 0.5 :: V3 Float)
        sendUniformName cubeShader "material.shininess" (32 :: Float)
        glDrawArrays GL_TRIANGLES 0 36
      nearZ = 2 :: Float
      nowZ = 0 :: Float
      farZ = -12 :: Float
      nowTime = trackTime trk :: Double
      farTime = nowTime + 1 :: Double
      timeToZ t = nowZ + farZ * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + farTime * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      drawGem t gem colorFn = let
        color = case gem of
          D.Kick           -> V3 153 106 6
          D.Red            -> V3 202 25 7
          D.Pro D.Yellow _ -> V3 207 180 57
          D.Pro D.Blue   _ -> V3 71 110 222
          D.Pro D.Green  _ -> V3 58 207 68
          D.Orange         -> V3 58 207 68 -- TODO
        (x1, x2) = case gem of
          D.Kick           -> (-1, 1)
          D.Red            -> (-1, -0.5)
          D.Pro D.Yellow _ -> (-0.5, 0)
          D.Pro D.Blue   _ -> (0, 0.5)
          D.Pro D.Green  _ -> (0.5, 1)
          D.Orange         -> (0.5, 1) -- TODO
        (y1, y2) = case gem of
          D.Kick -> (-0.9, -1.1)
          _      -> (-0.8, -1.1)
        (z1, z2) = case gem of
          D.Kick -> (z + 0.1, z - 0.1)
          _      -> (z + 0.2, z - 0.2)
        z = timeToZ t
        in drawCube (V3 x1 y1 z1) (V3 x2 y2 z2)
          $ fmap (\chan -> colorFn $ fromInteger chan / 255) color
      drawNotes t notes = forM_ notes $ \case
        Autoplay gem -> if nowTime < t
          then drawGem t gem id
          else if nowTime - t < 0.1
            then drawGem nowTime gem sqrt
            else return ()
        Upcoming gem -> drawGem t gem id
        Hit t' gem -> if nowTime - t' < 0.1
          then drawGem nowTime gem sqrt
          else return ()
        Missed gem -> drawGem t gem (** 3)
      drawOverhits t notes = if nowTime - t < 0.1
        then forM_ notes $ \gem -> drawGem nowTime gem (** 3)
        else return ()
  drawCube (V3 -1 -1 nearZ) (V3 1 -1.1 farZ) (V3 0.2 0.2 0.2)
  drawCube (V3 -1 -0.98 0.2) (V3 1 -1.05 -0.2) (V3 0.8 0.8 0.8)
  traverseRange_ drawNotes False nearTime farTime $ trackNotes trk
  traverseRange_ drawOverhits False nearTime farTime $ trackOverhits trk

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

cubeVertices :: [CFloat]
cubeVertices = [
  -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,
   0.5, -0.5, -0.5,  0.0,  0.0, -1.0,
   0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
   0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
  -0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
  -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,

  -0.5, -0.5,  0.5,  0.0,  0.0, 1.0,
   0.5, -0.5,  0.5,  0.0,  0.0, 1.0,
   0.5,  0.5,  0.5,  0.0,  0.0, 1.0,
   0.5,  0.5,  0.5,  0.0,  0.0, 1.0,
  -0.5,  0.5,  0.5,  0.0,  0.0, 1.0,
  -0.5, -0.5,  0.5,  0.0,  0.0, 1.0,

  -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,
  -0.5,  0.5, -0.5, -1.0,  0.0,  0.0,
  -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,
  -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,
  -0.5, -0.5,  0.5, -1.0,  0.0,  0.0,
  -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,

   0.5,  0.5,  0.5,  1.0,  0.0,  0.0,
   0.5,  0.5, -0.5,  1.0,  0.0,  0.0,
   0.5, -0.5, -0.5,  1.0,  0.0,  0.0,
   0.5, -0.5, -0.5,  1.0,  0.0,  0.0,
   0.5, -0.5,  0.5,  1.0,  0.0,  0.0,
   0.5,  0.5,  0.5,  1.0,  0.0,  0.0,

  -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,
   0.5, -0.5, -0.5,  0.0, -1.0,  0.0,
   0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
   0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
  -0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
  -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,

  -0.5,  0.5, -0.5,  0.0,  1.0,  0.0,
   0.5,  0.5, -0.5,  0.0,  1.0,  0.0,
   0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
   0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
  -0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
  -0.5,  0.5, -0.5,  0.0,  1.0,  0.0
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
  [ 0, 1, 3 -- first triangle
  , 1, 2, 3 -- second triangle
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

instance SendUniform Float where
  sendUniform = glUniform1f

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

instance GLPixel PixelRGB8 where
  glPixelInternalFormat _ = GL_RGB
  glPixelFormat _ = GL_RGB
  glPixelType _ = GL_UNSIGNED_BYTE

instance GLPixel PixelRGBA8 where
  glPixelInternalFormat _ = GL_RGBA
  glPixelFormat _ = GL_RGBA
  glPixelType _ = GL_UNSIGNED_BYTE

data Texture = Texture
  { textureGL     :: GLuint
  , textureWidth  :: Int
  , textureHeight :: Int
  }

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
  let filtering = if linear then GL_LINEAR_MIPMAP_LINEAR else GL_NEAREST
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
  when linear $ glGenerateMipmap GL_TEXTURE_2D
  return $ Texture texture (imageWidth img) (imageHeight img)

loadMesh :: A.AiMesh -> IO (GLuint, (Int, Int))
loadMesh mesh = do
  A.Mesh{..} <- A.readMesh mesh

  vao <- alloca $ \p -> glGenVertexArrays 1 p >> peek p
  vbo <- alloca $ \p -> glGenBuffers 1 p >> peek p
  ebo <- alloca $ \p -> glGenBuffers 1 p >> peek p

  glBindVertexArray vao

  glBindBuffer GL_ARRAY_BUFFER vbo
  let vertexData = map CFloat $
        zip meshVertices meshNormals >>= \(v3v, v3n) -> toList v3v <> toList v3n
  withArrayBytes vertexData $ \size p -> do
    glBufferData GL_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW

  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  let indices = map fromIntegral $ meshFaces >>= A.faceIndices :: [GLuint]
  withArrayBytes indices $ \size p -> do
    glBufferData GL_ELEMENT_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW

  -- vertex position
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
    (fromIntegral $ 6 * sizeOf (undefined :: CFloat))
    nullPtr
  glEnableVertexAttribArray 0
  -- normal
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE
    (fromIntegral $ 6 * sizeOf (undefined :: CFloat))
    (intPtrToPtr $ fromIntegral $ 3 * sizeOf (undefined :: CFloat))
  glEnableVertexAttribArray 1

  -- clean up
  glBindVertexArray 0
  withArray [vbo, ebo] $ glDeleteBuffers 2
  return (vao, (length meshVertices, meshMaterialIndex))

data GLStuff = GLStuff
  { cubeShader     :: GLuint
  , cubeVAO        :: GLuint
  , quadShader     :: GLuint
  , quadVAO        :: GLuint
  , meshVAOs       :: [(GLuint, (Int, Int))]
  , sceneLights    :: [A.Light]
  , sceneCameras   :: [A.Camera]
  , sceneNodes     :: [([Int], [M44 Float], T.Text)]
  , sceneMaterials :: [A.Material]
  } deriving (Show)

drawScene :: GLStuff -> WindowDims -> IO ()
drawScene GLStuff{..} (WindowDims w h) = do

  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glClearColor 0.2 0.3 0.3 1.0
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  glUseProgram cubeShader

  let camera = head sceneCameras
      (_, cameraTrans, _) = head $
        filter (\(_, _, name) -> name == A.cameraName camera) sceneNodes
      viewPosn = A.cameraPosition camera
      view, projection :: M44 Float
      -- TODO fix these
      view = foldr (!*!) L.identity cameraTrans
      projection = L.perspective
        (A.cameraHorizontalFOV camera)
        (fromIntegral w / fromIntegral h)
        (A.cameraClipPlaneNear camera)
        (A.cameraClipPlaneFar camera)
  sendUniformName cubeShader "view" view
  sendUniformName cubeShader "projection" projection

  let light = head sceneLights
  sendUniformName cubeShader "light.position" $ A.lightPosition light
  sendUniformName cubeShader "light.ambient" $ A.lightColorAmbient light
  sendUniformName cubeShader "light.diffuse" $ A.lightColorDiffuse light
  sendUniformName cubeShader "light.specular" $ A.lightColorSpecular light
  sendUniformName cubeShader "viewPos" viewPosn

  forM_ sceneNodes $ \(meshIndices, transMatrices, _) -> do
    forM_ (map (meshVAOs !!) meshIndices) $ \(mesh, (len, materialIndex)) -> do
      let material = sceneMaterials !! materialIndex
          forceProp = either (error . ("Material property missing: " <>) . show) id
          xyz (V4 x y z _) = V3 x y z
      glBindVertexArray mesh
      sendUniformName cubeShader "model" $ foldr (!*!) L.identity $ reverse transMatrices
      sendUniformName cubeShader "material.ambient" $ xyz $ forceProp $ A.material_COLOR_AMBIENT material
      sendUniformName cubeShader "material.diffuse" $ xyz $ forceProp $ A.material_COLOR_DIFFUSE material
      sendUniformName cubeShader "material.specular" $ xyz $ forceProp $ A.material_COLOR_SPECULAR material
      sendUniformName cubeShader "material.shininess" $ forceProp $ A.material_SHININESS material
      glDrawElements GL_TRIANGLES (fromIntegral len) GL_UNSIGNED_INT nullPtr

loadGLStuff :: IO GLStuff
loadGLStuff = do

  glEnable GL_DEPTH_TEST
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  -- load from assimp

  scene <- either error id <$> A.importFile "/Users/mtolly/Desktop/3d/drums.dae"
    [ A.AiProcess_Triangulate
    , A.AiProcess_FlipUVs
    ]
  meshVAOs <- A.sceneMeshes scene >>= mapM loadMesh
  sceneLights <- A.sceneLights scene >>= mapM A.readLight
  sceneCameras <- A.sceneCameras scene >>= mapM A.readCamera
  sceneNodes <- let
    loadNode trans node = do
      name <- A.nodeName node
      meshIndices <- map fromIntegral <$> A.nodeMeshes node
      thisTrans <- A.nodeTransformation node
      let trans' = thisTrans : trans
      children <- A.nodeChildren node
      ((meshIndices, trans', name) :) . concat <$> mapM (loadNode trans') children
    in A.rootNode scene >>= loadNode []
  sceneMaterials <- A.sceneMaterials scene >>= mapM A.readMaterial
  A.aiReleaseImport scene

  -- cube stuff

  cubeShader <- bracket (compileShader GL_VERTEX_SHADER objectVS) glDeleteShader $ \vertexShader -> do
    bracket (compileShader GL_FRAGMENT_SHADER objectFS) glDeleteShader $ \fragmentShader -> do
      compileProgram [vertexShader, fragmentShader]

  cubeVAO <- alloca $ \p -> glGenVertexArrays 1 p >> peek p
  cubeVBO <- alloca $ \p -> glGenBuffers 1 p >> peek p

  glBindVertexArray cubeVAO

  glBindBuffer GL_ARRAY_BUFFER cubeVBO
  withArrayBytes cubeVertices $ \size p -> do
    glBufferData GL_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW

  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
    (fromIntegral $ 6 * sizeOf (undefined :: CFloat))
    nullPtr
  glEnableVertexAttribArray 0
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE
    (fromIntegral $ 6 * sizeOf (undefined :: CFloat))
    (intPtrToPtr $ fromIntegral $ 3 * sizeOf (undefined :: CFloat))
  glEnableVertexAttribArray 1

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

  -- clean up

  glBindVertexArray 0
  withArray [cubeVBO, quadVBO, quadEBO] $ glDeleteBuffers 3

  return GLStuff{..}

deleteGLStuff :: GLStuff -> IO ()
deleteGLStuff GLStuff{..} = do
  glDeleteProgram cubeShader
  glDeleteProgram quadShader
  withArrayLen (cubeVAO : quadVAO : map fst meshVAOs) $ \len p ->
    glDeleteVertexArrays (fromIntegral len) p

data WindowDims = WindowDims Int Int

drawTexture :: GLStuff -> WindowDims -> Texture -> V2 Int -> Int -> IO ()
drawTexture GLStuff{..} (WindowDims screenW screenH) (Texture tex w h) (V2 x y) scale = do
  glUseProgram quadShader
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D tex
  glBindVertexArray quadVAO
  let scaleX = fromIntegral (w * scale) / fromIntegral screenW
      scaleY = fromIntegral (h * scale) / fromIntegral screenH
      translateX = (fromIntegral x / fromIntegral screenW) * 2 - 1 + scaleX
      translateY = (fromIntegral y / fromIntegral screenH) * 2 - 1 + scaleY
  sendUniformName quadShader "transform"
    (   translate4 (V3 translateX translateY 0)
    !*! L.scaled (V4 scaleX scaleY 1 1)
    :: M44 Float
    )
  glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr

freeTexture :: Texture -> IO ()
freeTexture (Texture tex _ _) = with tex $ glDeleteTextures 1

drawDrumsFull :: GLStuff -> WindowDims -> Track Double (D.Gem a) -> IO ()
drawDrumsFull glStuff@GLStuff{..} (WindowDims w h) trk' = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glClearColor 0.2 0.3 0.3 1.0
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  glUseProgram cubeShader
  glBindVertexArray cubeVAO
  let viewPosn = V3 0 1 3 :: V3 Float
      view, projection :: M44 Float
      view
        = L.mkTransformation (L.axisAngle (V3 1 0 0) (degrees 20)) 0
        !*! translate4 (negate viewPosn)
        -- note, this translates then rotates (can't just give V3 to mkTransformation)
      projection = L.perspective (degrees 45) (fromIntegral w / fromIntegral h) 0.1 100
  sendUniformName cubeShader "view" view
  sendUniformName cubeShader "projection" projection
  sendUniformName cubeShader "light.position" (V3 0 -0.5 0.5 :: V3 Float)
  sendUniformName cubeShader "light.ambient" (V3 0.2 0.2 0.2 :: V3 Float)
  sendUniformName cubeShader "light.diffuse" (V3 1 1 1 :: V3 Float)
  sendUniformName cubeShader "light.specular" (V3 1 1 1 :: V3 Float)
  sendUniformName cubeShader "viewPos" viewPosn
  drawDrums glStuff trk'
