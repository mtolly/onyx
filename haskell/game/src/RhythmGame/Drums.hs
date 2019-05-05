{-# LANGUAGE LambdaCase #-}
module RhythmGame.Drums where

import           Codec.Picture
import           Control.Concurrent        (threadDelay)
import           Control.Exception         (bracket)
import           Control.Monad             (forM_, when)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State
import           Data.List                 (partition)
import qualified Data.Map.Strict           as Map
import           Data.Map.Strict.Internal  (Map (..))
import qualified Data.Matrix               as M
import           Data.Maybe                (fromMaybe)
import qualified Data.Vector               as V
import qualified Data.Vector.Storable      as VS
import           Foreign
import           Foreign.C
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Resources                 (onyxAlbum)
import qualified RockBand.Codec.Drums      as D
import           SDL                       (($=))
import qualified SDL

data Note t a
  = Upcoming a
  | Hit t a
  | Missed a
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

drawDrums :: SDL.Renderer -> SDL.Rectangle CInt -> Track Double (D.Gem ()) -> IO ()
drawDrums rend rect@(SDL.Rectangle (SDL.P (SDL.V2 rectX rectY)) (SDL.V2 rectW rectH)) trk = do
  SDL.rendererDrawColor rend $= SDL.V4 100 100 100 0xFF
  SDL.fillRect rend $ Just rect
  let nowLineFrac = 0.8
      nowLineY = rectY + floor (fromIntegral rectH * nowLineFrac)
      futureSight = 0.7
      drawnSpan = futureSight / nowLineFrac -- time in seconds from top to bottom
      yToTime y = (trackTime trk + futureSight) - (fromIntegral (y - rectY) / fromIntegral rectH) * drawnSpan
      timeToY t = rectY + floor (((trackTime trk + futureSight - t) / drawnSpan) * fromIntegral rectH)
      timeAppear = yToTime $ rectY - 50
      timeDisappear = yToTime $ rectY + rectH + 50
      drawNotes t notes = let
        yCenter = timeToY t
        drawGem gem = let
          floor' :: Double -> CInt
          floor' = floor
          h = case gem of
            D.Kick -> 10
            _      -> 20
          color = case gem of
            D.Kick            -> SDL.V4 153 106 6 0xFF
            D.Red             -> SDL.V4 202 25 7 0xFF
            D.Pro D.Yellow () -> SDL.V4 207 180 57 0xFF
            D.Pro D.Blue ()   -> SDL.V4 71 110 222 0xFF
            D.Pro D.Green ()  -> SDL.V4 58 207 68 0xFF
            D.Orange          -> SDL.V4 58 207 68 0xFF -- TODO
          w = case gem of
            D.Kick -> rectW
            _      -> floor' $ fromIntegral rectW * 0.25
          x = case gem of
            D.Kick            -> rectX
            D.Red             -> rectX
            D.Pro D.Yellow () -> rectX + floor' (fromIntegral rectW * 0.25)
            D.Pro D.Blue ()   -> rectX + floor' (fromIntegral rectW * 0.5)
            D.Pro D.Green ()  -> rectX + floor' (fromIntegral rectW * 0.75)
            D.Orange          -> rectX + floor' (fromIntegral rectW * 0.75) -- TODO
          y = yCenter - quot h 2
          in do
            SDL.rendererDrawColor rend $= color
            SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 x y) $ SDL.V2 w h
        in forM_ notes $ \case
          Upcoming gem -> drawGem gem
          Hit _ _ -> return ()
          Missed gem -> drawGem gem
  SDL.rendererDrawColor rend $= SDL.V4 0 0 0 0xFF
  SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 rectX nowLineY) $ SDL.V2 rectW 10
  traverseRange_ drawNotes False timeDisappear timeAppear $ trackNotes trk

vertexShaderSource :: String
vertexShaderSource = unlines
  [ "#version 330 core"
  , "layout (location = 0) in vec3 aPos;"
  , "layout (location = 1) in vec2 aTexCoord;"

  , "out vec2 TexCoord;"

  , "uniform mat4 model;"
  , "uniform mat4 view;"
  , "uniform mat4 projection;"

  , "void main()"
  , "{"
  , "    gl_Position = projection * view * model * vec4(aPos, 1.0f);"
  , "    TexCoord = vec2(aTexCoord.x, aTexCoord.y);"
  , "}"
  ]

fragmentShaderSource :: String
fragmentShaderSource = unlines
  [ "#version 330 core"
  , "out vec4 FragColor;"

  , "in vec2 TexCoord;"

  , "uniform sampler2D ourTexture;"

  , "void main()"
  , "{"
  , "    FragColor = texture(ourTexture, TexCoord);"
  , "}"
  ]

compileShader :: GLenum -> String -> IO GLuint
compileShader shaderType source = do
  shader <- glCreateShader shaderType
  withCString source $ \cs' -> with cs' $ \cs -> do
    glShaderSource shader 1 cs nullPtr
    glCompileShader shader
    alloca $ \success -> do
      allocaArray 512 $ \infoLog -> do
        glGetShaderiv shader GL_COMPILE_STATUS success
        peek success >>= \case
          0 -> do
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
      glGetShaderiv program GL_LINK_STATUS success
      peek success >>= \case
        0 -> do
          glGetProgramInfoLog program 512 nullPtr infoLog
          peekCString infoLog >>= error
        _ -> return program

withArrayBytes :: (Storable a, Num len) => [a] -> (len -> Ptr a -> IO b) -> IO b
withArrayBytes xs f = withArray xs $ \p -> let
  bytes = fromIntegral $ length xs * sizeOf (head xs)
  in f bytes p

vertices :: [CFloat]
vertices = [
  -0.5, -0.5, -0.5,  0.0, 0.0,
   0.5, -0.5, -0.5,  1.0, 0.0,
   0.5,  0.5, -0.5,  1.0, 1.0,
   0.5,  0.5, -0.5,  1.0, 1.0,
  -0.5,  0.5, -0.5,  0.0, 1.0,
  -0.5, -0.5, -0.5,  0.0, 0.0,

  -0.5, -0.5,  0.5,  0.0, 0.0,
   0.5, -0.5,  0.5,  1.0, 0.0,
   0.5,  0.5,  0.5,  1.0, 1.0,
   0.5,  0.5,  0.5,  1.0, 1.0,
  -0.5,  0.5,  0.5,  0.0, 1.0,
  -0.5, -0.5,  0.5,  0.0, 0.0,

  -0.5,  0.5,  0.5,  1.0, 0.0,
  -0.5,  0.5, -0.5,  1.0, 1.0,
  -0.5, -0.5, -0.5,  0.0, 1.0,
  -0.5, -0.5, -0.5,  0.0, 1.0,
  -0.5, -0.5,  0.5,  0.0, 0.0,
  -0.5,  0.5,  0.5,  1.0, 0.0,

   0.5,  0.5,  0.5,  1.0, 0.0,
   0.5,  0.5, -0.5,  1.0, 1.0,
   0.5, -0.5, -0.5,  0.0, 1.0,
   0.5, -0.5, -0.5,  0.0, 1.0,
   0.5, -0.5,  0.5,  0.0, 0.0,
   0.5,  0.5,  0.5,  1.0, 0.0,

  -0.5, -0.5, -0.5,  0.0, 1.0,
   0.5, -0.5, -0.5,  1.0, 1.0,
   0.5, -0.5,  0.5,  1.0, 0.0,
   0.5, -0.5,  0.5,  1.0, 0.0,
  -0.5, -0.5,  0.5,  0.0, 0.0,
  -0.5, -0.5, -0.5,  0.0, 1.0,

  -0.5,  0.5, -0.5,  0.0, 1.0,
   0.5,  0.5, -0.5,  1.0, 1.0,
   0.5,  0.5,  0.5,  1.0, 0.0,
   0.5,  0.5,  0.5,  1.0, 0.0,
  -0.5,  0.5,  0.5,  0.0, 0.0,
  -0.5,  0.5, -0.5,  0.0, 1.0
  ]

cubePositions :: [V.Vector Float]
cubePositions =
  [ V.fromList [ 0.0,  0.0,  0.0]
  , V.fromList [ 2.0,  5.0, -15.0]
  , V.fromList [-1.5, -2.2, -2.5]
  , V.fromList [-3.8, -2.0, -12.3]
  , V.fromList [ 2.4, -0.4, -3.5]
  , V.fromList [-1.7,  3.0, -7.5]
  , V.fromList [ 1.3, -2.0, -2.5]
  , V.fromList [ 1.5,  2.0, -2.5]
  , V.fromList [ 1.5,  0.2, -1.5]
  , V.fromList [-1.3,  1.0, -1.5]
  ]

-- indices :: [CInt]
-- indices =
--   [ 0, 1, 3 -- first triangle
--   , 1, 2, 3 -- second triangle
--   ]

rotate4 :: (Floating a) => a -> V.Vector a -> M.Matrix a
rotate4 theta r = let
  sint = sin theta
  cost = cos theta
  rx = r V.! 0
  ry = r V.! 1
  rz = r V.! 2
  len = sqrt $ rx * rx + ry * ry + rz * rz
  nx = rx / len
  ny = ry / len
  nz = rz / len
  in M.fromList 4 4
    [ cost + (nx ** 2) * (1 - cost)
    , nx * ny * (1 - cost) - nz * sint
    , nx * nz * (1 - cost) + ny * sint
    , 0
    , ny * nx * (1 - cost) + nz * sint
    , cost + (ny ** 2) * (1 - cost)
    , ny * nz * (1 - cost) - nx * sint
    , 0
    , nz * nx * (1 - cost) - ny * sint
    , nz * ny * (1 - cost) + nx * sint
    , cost + (nz ** 2) * (1 - cost)
    , 0
    , 0
    , 0
    , 0
    , 1
    ]

scale4 :: (Num a) => V.Vector a -> M.Matrix a
scale4 v = M.diagonalList 4 0 (V.toList v ++ [1])

translate4 :: (Floating a) => V.Vector a -> M.Matrix a
translate4 v = M.fromList 4 4
  [ 1, 0, 0, v V.! 0
  , 0, 1, 0, v V.! 1
  , 0, 0, 1, v V.! 2
  , 0, 0, 0, 1
  ]

perspective :: (Floating a) => a -> a -> a -> a -> M.Matrix a
perspective fov aspect near far = let
  top = near * tan (fov / 2)
  bottom = -top
  right = top * aspect
  left = -right

  sx = 2 * near / (right - left)
  sy = 2 * near / (top - bottom)

  c2 = (- (far + near)) / (far - near)
  c1 = 2 * near * far / (near - far)

  tx = (-near) * (left + right) / (right - left)
  ty = (-near) * (bottom + top) / (top - bottom)

  in M.fromList 4 4
    [ sx, 0, 0, tx
    , 0, sy, 0, ty
    , 0, 0, c2, c1
    , 0, 0, -1, 0
    ]

degrees :: (Floating a) => a -> a
degrees d = (d / 180) * pi

playDrums :: SDL.Window -> Track Double (D.Gem ()) -> IO ()
playDrums window trk = flip evalStateT trk $ do
  initTime <- SDL.ticks
  glEnable GL_DEPTH_TEST
  shaderProgram <- liftIO $ do
    bracket (compileShader GL_VERTEX_SHADER vertexShaderSource) glDeleteShader $ \vertexShader -> do
      bracket (compileShader GL_FRAGMENT_SHADER fragmentShaderSource) glDeleteShader $ \fragmentShader -> do
        compileProgram [vertexShader, fragmentShader]
  vao <- liftIO $ alloca $ \p -> glGenVertexArrays 1 p >> peek p
  vbo <- liftIO $ alloca $ \p -> glGenBuffers 1 p >> peek p
  ebo <- liftIO $ alloca $ \p -> glGenBuffers 1 p >> peek p
  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo
  liftIO $ withArrayBytes vertices $ \size p -> do
    glBufferData GL_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW
  -- glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  -- liftIO $ withArrayBytes indices $ \size p -> do
  --   glBufferData GL_ELEMENT_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW
  -- position attribute
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
    (fromIntegral $ 5 * sizeOf (undefined :: CFloat))
    nullPtr
  glEnableVertexAttribArray 0
  -- texture coords attribute
  glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE
    (fromIntegral $ 5 * sizeOf (undefined :: CFloat))
    (intPtrToPtr $ fromIntegral $ 3 * sizeOf (undefined :: CFloat))
  glEnableVertexAttribArray 1
  -- texture
  texture <- liftIO $ alloca $ \p -> glGenTextures 1 p >> peek p
  glBindTexture GL_TEXTURE_2D texture
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  let flippedImage = generateImage
        (\x y -> pixelAt onyxAlbum x $ imageHeight onyxAlbum - y - 1)
        (imageWidth onyxAlbum)
        (imageHeight onyxAlbum)
  liftIO $ VS.unsafeWith (imageData flippedImage) $ \p -> do
    glTexImage2D GL_TEXTURE_2D 0 GL_RGB
      (fromIntegral $ imageWidth flippedImage)
      (fromIntegral $ imageHeight flippedImage)
      0
      GL_RGB
      GL_UNSIGNED_BYTE
      (castPtr p)
  glGenerateMipmap GL_TEXTURE_2D
  -- matrix stuff
  modelLoc <- liftIO $ withCString "model" $ glGetUniformLocation shaderProgram
  viewLoc <- liftIO $ withCString "view" $ glGetUniformLocation shaderProgram
  projectionLoc <- liftIO $ withCString "projection" $ glGetUniformLocation shaderProgram
  let loop = SDL.pollEvents >>= processEvents >>= \b -> when b $ do
        timestamp <- SDL.ticks
        modify $ updateTime $ fromIntegral (timestamp - initTime) / 1000
        draw
        liftIO $ threadDelay 5000
        loop
      processEvents [] = return True
      processEvents (e : es) = case SDL.eventPayload e of
        SDL.QuitEvent -> return False
        SDL.KeyboardEvent SDL.KeyboardEventData
          { SDL.keyboardEventKeyMotion = SDL.Pressed
          , SDL.keyboardEventKeysym = ksym
          , SDL.keyboardEventRepeat = False
          } -> do
            let hit gem = modify $ hitPad t gem
                t = fromIntegral (SDL.eventTimestamp e - initTime) / 1000
            case SDL.keysymScancode ksym of
              SDL.ScancodeV     -> hit D.Red
              SDL.ScancodeB     -> hit $ D.Pro D.Yellow ()
              SDL.ScancodeN     -> hit $ D.Pro D.Blue ()
              SDL.ScancodeM     -> hit $ D.Pro D.Green ()
              SDL.ScancodeSpace -> hit D.Kick
              _                 -> return ()
            processEvents es
        _ -> processEvents es
      draw = do
        _trk' <- get
        SDL.V2 w h <- SDL.glGetDrawableSize window
        glViewport 0 0 (fromIntegral w) (fromIntegral h)
        glClearColor 0.2 0.3 0.3 1.0
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
        glBindTexture GL_TEXTURE_2D texture
        glUseProgram shaderProgram
        let view = translate4 (V.fromList [0, 0, -3])
            projection = perspective (degrees 45) (fromIntegral w / fromIntegral h) 0.1 100
            sendMatrix loc m = liftIO $ withArray (M.toList m)
              $ glUniformMatrix4fv loc 1 GL_TRUE {- <- this means row major order -}
        sendMatrix viewLoc view
        sendMatrix projectionLoc projection
        glBindVertexArray vao
        forM_ [0..9] $ \i -> do
          let angle = degrees 20 * realToFrac i + case rem i 3 of
                0 -> realToFrac $ trackTime _trk'
                _ -> 1
              model = translate4 (cubePositions !! i) * rotate4 angle (V.fromList [1, 0.3, 0.5])
          sendMatrix modelLoc $ model
          glDrawArrays GL_TRIANGLES 0 36
        SDL.glSwapWindow window
  loop
