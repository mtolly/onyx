{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module RhythmGame.Drums where

import           Codec.Picture
import           Control.Exception        (bracket)
import           Control.Monad            (forM, forM_)
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
import           Resources                (getResourcesPath)
import           RockBand.Codec.Beat
import qualified RockBand.Codec.Drums     as D
import           System.Info              (os)

data Note t a
  = Upcoming a
  | Hit t a
  | Missed a
  | Autoplay a
  deriving (Eq, Ord, Show, Read, Functor)

data Track t a = Track
  { trackNotes    :: Map t [Note t a] -- ^ lists must be non-empty
  , trackOverhits :: Map t [a] -- ^ lists must be non-empty
  , trackTime     :: t -- ^ the latest timestamp we have reached
  , trackWindow   :: t -- ^ the half-window of time on each side of a note
  , trackBeats    :: Map t (Maybe BeatEvent) -- ^ 'Nothing' is the auto 8th note interval lines
  } deriving (Eq, Ord, Show, Read, Functor)

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

data Object = Box (V2 Float) (V2 Float) | Cone

coneSegments :: GLuint
coneSegments = 15

drawDrums :: GLStuff -> WindowDims -> Track Double (D.Gem D.ProType) -> IO ()
drawDrums GLStuff{..} (WindowDims _w h) trk = do
  glUseProgram objectShader
  -- view and projection matrices should already have been set
  let colorType = 1 :: GLuint
      boxType = 2 :: GLuint
      coneType = 3 :: GLuint
      drawObject :: Object -> V3 Float -> V3 Float -> Either TextureID (V3 Float) -> Float -> Maybe (V3 Float) -> IO ()
      drawObject obj (V3 x1 y1 z1) (V3 x2 y2 z2) texcolor alpha lightOffset = do
        glBindVertexArray $ case obj of
          Box{} -> boxVAO
          Cone  -> coneVAO
        sendUniformName objectShader "model"
          $ translate4 (V3 ((x1 + x2) / 2) ((y1 + y2) / 2) ((z1 + z2) / 2))
          !*! L.scaled (V4 (abs $ x2 - x1) (abs $ y2 - y1) (abs $ z2 - z1) 1)
        sendUniformName objectShader "alpha" alpha
        sendUniformName objectShader "startFade" (fromIntegral h * (519 / 671) :: Float)
        sendUniformName objectShader "endFade" (fromIntegral h * (558 / 671) :: Float)
        forM_ lightOffset $ \off -> do
          let center = V3 ((x1 + x2) / 2) (max y1 y2) ((z1 + z2) / 2)
          sendUniformName objectShader "light.position" $ center + off
          -- TODO do this better, right now it loses the original light position
        case texcolor of
          Right color -> do
            sendUniformName objectShader "material.diffuse.type" colorType
            sendUniformName objectShader "material.diffuse.color" color
          Left texid -> do
            case lookup texid textures of
              Just tex -> do
                glActiveTexture GL_TEXTURE0
                glBindTexture GL_TEXTURE_2D $ textureGL tex
                case obj of
                  Box (V2 totalW totalH) (V2 cornerW cornerH) -> do
                    sendUniformName objectShader "material.diffuse.type" boxType
                    sendUniformName objectShader "material.diffuse.image" (0 :: GLint)
                    sendUniformName objectShader "material.diffuse.box.totalWidth" totalW
                    sendUniformName objectShader "material.diffuse.box.totalHeight" totalH
                    sendUniformName objectShader "material.diffuse.box.cornerWidth" cornerW
                    sendUniformName objectShader "material.diffuse.box.cornerHeight" cornerH
                  Cone -> do
                    sendUniformName objectShader "material.diffuse.type" coneType
                    sendUniformName objectShader "material.diffuse.image" (0 :: GLint)
                    sendUniformName objectShader "material.diffuse.cone.segments" coneSegments
              Nothing -> do
                sendUniformName objectShader "material.diffuse.type" colorType
                sendUniformName objectShader "material.diffuse.color" (V3 1 0 1 :: V3 Float)
        sendUniformName objectShader "material.specular.type" colorType
        sendUniformName objectShader "material.specular.color" (V3 0.5 0.5 0.5 :: V3 Float)
        sendUniformName objectShader "material.shininess" (32 :: Float)
        glDrawArrays GL_TRIANGLES 0 36
      nearZ = 2 :: Float
      nowZ = 0 :: Float
      farZ = -12 :: Float
      nowTime = trackTime trk :: Double
      farTime = nowTime + 1 :: Double
      timeToZ t = nowZ + farZ * realToFrac ((t - nowTime) / (farTime - nowTime))
      zToTime z = nowTime + farTime * realToFrac ((z - nowZ) / (farZ - nowZ))
      nearTime = zToTime nearZ
      drawGem t gem alpha = let
        (texid, obj) = case gem of
          D.Kick                  -> (TextureKick        , Box (V2 500  40) (V2   0  20))
          D.Red                   -> (TextureRedGem      , Box (V2 600 400) (V2 150 254))
          D.Pro D.Yellow D.Tom    -> (TextureYellowGem   , Box (V2 600 400) (V2 150 254))
          D.Pro D.Blue   D.Tom    -> (TextureBlueGem     , Box (V2 600 400) (V2 150 254))
          D.Pro D.Green  D.Tom    -> (TextureGreenGem    , Box (V2 600 400) (V2 150 254))
          D.Pro D.Yellow D.Cymbal -> (TextureYellowCymbal, Cone                         )
          D.Pro D.Blue   D.Cymbal -> (TextureBlueCymbal  , Cone                         )
          D.Pro D.Green  D.Cymbal -> (TextureGreenCymbal , Cone                         )
          D.Orange                -> (TextureGreenCymbal , Cone                         )
        (x1, x2) = case gem of
          D.Kick           -> (-1, 1)
          D.Red            -> (-1, -0.5)
          D.Pro D.Yellow _ -> (-0.5, 0)
          D.Pro D.Blue   _ -> (0, 0.5)
          D.Pro D.Green  _ -> (0.5, 1)
          D.Orange         -> (0.5, 1) -- TODO
        (y1, y2) = case gem of
          D.Kick           -> (-0.97, -1)
          D.Pro _ D.Cymbal -> (-0.8 , -1)
          _                -> (-0.9 , -1)
        (z1, z2) = case gem of
          D.Kick -> (z + 0.06, z - 0.06)
          _      -> (z + 0.17, z - 0.17)
        z = timeToZ t
        in drawObject obj (V3 x1 y1 z1) (V3 x2 y2 z2) (Left texid) alpha $ Just $ V3 0 1 0
      drawNotes t notes = forM_ notes $ \case
        Autoplay gem -> if nowTime < t
          then drawGem t gem 1
          else if nowTime - t < 0.1
            then drawGem nowTime gem $ realToFrac $ 1 - (nowTime - t) * 10
            else return ()
        Upcoming gem -> drawGem t gem 1
        Hit t' gem -> if nowTime - t' < 0.1
          then drawGem nowTime gem $ realToFrac $ 1 - (nowTime - t') * 10
          else return ()
        Missed gem -> drawGem t gem 1
  -- draw highway
  drawObject (Box (V2 0 0) (V2 0 0)) (V3 -1 -1 nearZ) (V3 1 -1  0.17) (Right $ V3 0.2 0.2 0.2) 1 Nothing
  drawObject (Box (V2 0 0) (V2 0 0)) (V3 -1 -1  0.17) (V3 1 -1 -0.17) (Right $ V3 0.8 0.8 0.8) 1 Nothing
  drawObject (Box (V2 0 0) (V2 0 0)) (V3 -1 -1 -0.17) (V3 1 -1  farZ) (Right $ V3 0.2 0.2 0.2) 1 Nothing
  -- draw railings
  drawObject (Box (V2 0 0) (V2 0 0)) (V3 -1.09 -0.85 nearZ) (V3 -1    -1.1 farZ) (Right $ V3 0.4 0.2 0.6) 1 Nothing
  drawObject (Box (V2 0 0) (V2 0 0)) (V3  1    -0.85 nearZ) (V3  1.09 -1.1 farZ) (Right $ V3 0.4 0.2 0.6) 1 Nothing
  -- draw notes
  traverseRange_ drawNotes False nearTime farTime $ trackNotes    trk

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
  { vertexPosition   :: V3 CFloat
  , vertexNormal     :: V3 CFloat
  , vertexTexCoords  :: V2 CFloat
  , vertexTexSegment :: CFloat
  } deriving (Eq, Show)

instance Storable Vertex where
  sizeOf _ = 8 * sizeOf (undefined :: CFloat) + sizeOf (undefined :: CFloat)
  alignment _ = lcm
    (alignment (undefined :: CFloat))
    (alignment (undefined :: CFloat))
  peek = undefined -- not implemented
  poke p v = do
    poke (castPtr p) $ vertexPosition v
    poke (castPtr p `plusPtr` (3 * sizeOf (undefined :: CFloat))) $ vertexNormal v
    poke (castPtr p `plusPtr` (6 * sizeOf (undefined :: CFloat))) $ vertexTexCoords v
    poke (castPtr p `plusPtr` (8 * sizeOf (undefined :: CFloat))) $ vertexTexSegment v

simpleCone :: Int -> [Vertex]
simpleCone n = let
  topCenter :: V3 CFloat
  topCenter = V3 0 0.5 0
  bottomIndex :: Int -> V3 CFloat
  bottomIndex i = let
    theta = (fromIntegral i / fromIntegral n) * 2 * pi
    -- theta 0 goes back (negative Z) behind the center point
    x = -0.5 * sin theta
    z = -0.5 * cos theta
    in V3 x -0.5 z
  in do
    i <- [0 .. n - 1]
    let a = topCenter
        b = bottomIndex i
        c = bottomIndex $ i + 1
        dir = (b - a) `L.cross` (c - a)
        normal = L.normalize dir
        u0 = fromIntegral i / fromIntegral n
        u1 = fromIntegral (i + 1) / fromIntegral n
        uhalf = (u0 + u1) / 2
    [   Vertex a normal (V2 uhalf 1) (fromIntegral i)
      , Vertex b normal (V2 u0    0) (fromIntegral i)
      , Vertex c normal (V2 u1    0) (fromIntegral i)
      ]

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
    [ Vertex frontTopRight (V3 0.0 1.0 0.0) (V2 1.0 0.0) 1
    , Vertex backTopRight  (V3 0.0 1.0 0.0) (V2 1.0 1.0) 1
    , Vertex backTopLeft   (V3 0.0 1.0 0.0) (V2 0.0 1.0) 1
    , Vertex backTopLeft   (V3 0.0 1.0 0.0) (V2 0.0 1.0) 1
    , Vertex frontTopLeft  (V3 0.0 1.0 0.0) (V2 0.0 0.0) 1
    , Vertex frontTopRight (V3 0.0 1.0 0.0) (V2 1.0 0.0) 1

    -- left face
    , Vertex frontTopLeft    (V3 -1.0 0.0 0.0) (V2 1.0 1.0) 2
    , Vertex backTopLeft     (V3 -1.0 0.0 0.0) (V2 0.0 1.0) 2
    , Vertex backBottomLeft  (V3 -1.0 0.0 0.0) (V2 0.0 0.0) 2
    , Vertex backBottomLeft  (V3 -1.0 0.0 0.0) (V2 0.0 0.0) 2
    , Vertex frontBottomLeft (V3 -1.0 0.0 0.0) (V2 1.0 0.0) 2
    , Vertex frontTopLeft    (V3 -1.0 0.0 0.0) (V2 1.0 1.0) 2

    -- front face
    , Vertex frontBottomLeft  (V3 0.0 0.0 1.0) (V2 0.0 0.0) 3
    , Vertex frontBottomRight (V3 0.0 0.0 1.0) (V2 1.0 0.0) 3
    , Vertex frontTopRight    (V3 0.0 0.0 1.0) (V2 1.0 1.0) 3
    , Vertex frontTopRight    (V3 0.0 0.0 1.0) (V2 1.0 1.0) 3
    , Vertex frontTopLeft     (V3 0.0 0.0 1.0) (V2 0.0 1.0) 3
    , Vertex frontBottomLeft  (V3 0.0 0.0 1.0) (V2 0.0 0.0) 3

    -- right face
    , Vertex frontTopRight    (V3 1.0 0.0 0.0) (V2 0.0 1.0) 4
    , Vertex frontBottomRight (V3 1.0 0.0 0.0) (V2 0.0 0.0) 4
    , Vertex backBottomRight  (V3 1.0 0.0 0.0) (V2 1.0 0.0) 4
    , Vertex backBottomRight  (V3 1.0 0.0 0.0) (V2 1.0 0.0) 4
    , Vertex backTopRight     (V3 1.0 0.0 0.0) (V2 1.0 1.0) 4
    , Vertex frontTopRight    (V3 1.0 0.0 0.0) (V2 0.0 1.0) 4
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

data GLStuff = GLStuff
  { objectShader :: GLuint
  , boxVAO       :: GLuint
  , coneVAO      :: GLuint
  , quadShader   :: GLuint
  , quadVAO      :: GLuint
  , textures     :: [(TextureID, Texture)]
  } deriving (Show)

data TextureID
  = TextureKick
  | TextureRedGem
  | TextureYellowGem
  | TextureBlueGem
  | TextureGreenGem
  | TextureRedCymbal
  | TextureYellowCymbal
  | TextureBlueCymbal
  | TextureGreenCymbal
  deriving (Eq, Show, Enum, Bounded)

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
        , (1, GL_FLOAT, sizeOf (undefined :: CFloat))
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

  boxVAO <- alloca $ \p -> glGenVertexArrays 1 p >> peek p
  boxVBO <- alloca $ \p -> glGenBuffers 1 p >> peek p
  glBindVertexArray boxVAO
  glBindBuffer GL_ARRAY_BUFFER boxVBO
  withArrayBytes simpleBox $ \size p -> do
    glBufferData GL_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW
  writeParts 0 0 vertexParts

  coneVAO <- alloca $ \p -> glGenVertexArrays 1 p >> peek p
  coneVBO <- alloca $ \p -> glGenBuffers 1 p >> peek p
  glBindVertexArray coneVAO
  glBindBuffer GL_ARRAY_BUFFER coneVBO
  withArrayBytes (simpleCone $ fromIntegral coneSegments) $ \size p -> do
    glBufferData GL_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW
  writeParts 0 0 vertexParts

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

  -- textures

  textures <- forM [minBound .. maxBound] $ \texID -> do
    path <- getResourcesPath $ case texID of
      TextureKick         -> "textures/kick.jpg"
      TextureRedGem       -> "textures/box-red.jpg"
      TextureYellowGem    -> "textures/box-yellow.jpg"
      TextureBlueGem      -> "textures/box-blue.jpg"
      TextureGreenGem     -> "textures/box-green.jpg"
      TextureRedCymbal    -> "textures/cymbal-red.jpg"
      TextureYellowCymbal -> "textures/cymbal-yellow.jpg"
      TextureBlueCymbal   -> "textures/cymbal-blue.jpg"
      TextureGreenCymbal  -> "textures/cymbal-green.jpg"
    tex <- readImage path >>= either fail return >>= loadTexture True . convertRGBA8
    return (texID, tex)

  -- clean up

  glBindVertexArray 0
  withArray [boxVBO, coneVBO, quadVBO, quadEBO] $ glDeleteBuffers 3

  return GLStuff{..}

deleteGLStuff :: GLStuff -> IO ()
deleteGLStuff GLStuff{..} = case os of
  "mingw32" -> return () -- TODO all the deletion calls cause a segfault on Windows, need to figure out why!
  _ -> do
    glDeleteProgram objectShader
    glDeleteProgram quadShader
    withArrayLen [boxVAO, coneVAO, quadVAO] $ \len p ->
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

drawDrumsFull :: GLStuff -> WindowDims -> Track Double (D.Gem D.ProType) -> IO ()
drawDrumsFull glStuff@GLStuff{..} dims@(WindowDims w h) trk' = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glClearColor 0.2 0.3 0.3 1.0
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  glUseProgram objectShader
  glBindVertexArray boxVAO
  let viewPosn = V3 0 1.4 3 :: V3 Float
      view, projection :: M44 Float
      view
        = L.mkTransformation (L.axisAngle (V3 1 0 0) (degrees 25)) 0
        !*! translate4 (negate viewPosn)
        -- note, this translates then rotates (can't just give V3 to mkTransformation)
      projection = L.perspective (degrees 45) (fromIntegral w / fromIntegral h) 0.1 100
  sendUniformName objectShader "view" view
  sendUniformName objectShader "projection" projection
  sendUniformName objectShader "light.position" (V3 0 -0.5 0.5 :: V3 Float)
  sendUniformName objectShader "light.ambient" (V3 0.2 0.2 0.2 :: V3 Float)
  sendUniformName objectShader "light.diffuse" (V3 1 1 1 :: V3 Float)
  sendUniformName objectShader "light.specular" (V3 1 1 1 :: V3 Float)
  sendUniformName objectShader "viewPos" viewPosn
  drawDrums glStuff dims trk'
