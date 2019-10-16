{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Assimp where

import           Control.Monad     (mapM)
import qualified Data.Text         as T
import qualified Data.Text.Foreign as T
import           Foreign
import           Foreign.C
import           Linear            (V2 (..), V3 (..), V4 (..))

#include "assimp/cimport.h"
#include "assimp/scene.h"
#include "assimp/postprocess.h"

{#pointer *aiScene     as AiScene     newtype #}
{#pointer *aiAnimation as AiAnimation newtype #}
{#pointer *aiCamera    as AiCamera    newtype #}
{#pointer *aiLight     as AiLight     newtype #}
{#pointer *aiMaterial  as AiMaterial  newtype #}
{#pointer *aiMesh      as AiMesh      newtype #}
{#pointer *aiNode      as AiNode      newtype #}
{#pointer *aiTexture   as AiTexture   newtype #}
{#pointer *aiFace      as AiFace      newtype #}
{#pointer *aiVector3D  as AiVector3D  newtype #}
{#pointer *aiColor3D   as AiColor3D   newtype #}
{#pointer *aiColor4D   as AiColor4D   newtype #}

-- these are just storing the pointer, not the structs
deriving instance Storable AiScene
deriving instance Storable AiAnimation
deriving instance Storable AiCamera
deriving instance Storable AiLight
deriving instance Storable AiMaterial
deriving instance Storable AiMesh
deriving instance Storable AiNode
deriving instance Storable AiTexture
deriving instance Storable AiFace
deriving instance Storable AiVector3D
deriving instance Storable AiColor3D
deriving instance Storable AiColor4D

class IncPtr a where
  incPtr :: a -> a
  isNullPtr :: a -> Bool

instance IncPtr AiFace where
  incPtr (AiFace p) = AiFace $ plusPtr p {#sizeof aiFace#}
  isNullPtr (AiFace p) = p == nullPtr

instance IncPtr AiVector3D where
  incPtr (AiVector3D p) = AiVector3D $ plusPtr p {#sizeof aiVector3D#}
  isNullPtr (AiVector3D p) = p == nullPtr

instance IncPtr AiColor3D where
  incPtr (AiColor3D p) = AiColor3D $ plusPtr p {#sizeof aiColor3D#}
  isNullPtr (AiColor3D p) = p == nullPtr

instance IncPtr AiColor4D where
  incPtr (AiColor4D p) = AiColor4D $ plusPtr p {#sizeof aiColor4D#}
  isNullPtr (AiColor4D p) = p == nullPtr

peekMaybeString :: CString -> IO (Maybe String)
peekMaybeString str = if str == nullPtr
  then return Nothing
  else Just <$> peekCString str

enumOr :: (Enum a, Integral i) => [a] -> i
enumOr xs = fromIntegral $ foldr (.|.) 0 $ map fromEnum xs

{#fun aiImportFile
  { withCString* `FilePath'
  , enumOr `[AiPostProcessSteps]'
  } -> `AiScene'
#}

{#fun aiReleaseImport
  { `AiScene'
  } -> `()'
#}

{#fun aiGetErrorString
  {} -> `Maybe String' peekMaybeString*
#}

{#enum aiPostProcessSteps as AiPostProcessSteps {upcaseFirstLetter}
  deriving (Eq, Show)
#}

getAiArray :: (Storable child) =>
  (parent -> IO CUInt) -> (parent -> IO (Ptr child)) -> parent -> IO [child]
getAiArray getN getPtr obj = do
  len <- getN obj
  ptr <- getPtr obj
  if ptr == nullPtr
    then return []
    else peekArray (fromIntegral len) ptr

sceneAnimations :: AiScene -> IO [AiAnimation]
sceneAnimations = getAiArray {#get aiScene->mNumAnimations#} {#get aiScene->mAnimations#}

sceneCameras :: AiScene -> IO [AiCamera]
sceneCameras = getAiArray {#get aiScene->mNumCameras#} {#get aiScene->mCameras#}

sceneLights :: AiScene -> IO [AiLight]
sceneLights = getAiArray {#get aiScene->mNumLights#} {#get aiScene->mLights#}

sceneMaterials :: AiScene -> IO [AiMaterial]
sceneMaterials = getAiArray {#get aiScene->mNumMaterials#} {#get aiScene->mMaterials#}

sceneMeshes :: AiScene -> IO [AiMesh]
sceneMeshes = getAiArray {#get aiScene->mNumMeshes#} {#get aiScene->mMeshes#}

sceneTextures :: AiScene -> IO [AiTexture]
sceneTextures = getAiArray {#get aiScene->mNumTextures#} {#get aiScene->mTextures#}

rootNode :: AiScene -> IO AiNode
rootNode = {#get aiScene->mRootNode#}

nodeName :: AiNode -> IO T.Text
nodeName = propString {#get aiNode->mName.length#} {#get aiNode->mName.data#}

nodeMeshes :: AiNode -> IO [CUInt]
nodeMeshes = getAiArray {#get aiNode->mNumMeshes#} {#get aiNode->mMeshes#}

nodeChildren :: AiNode -> IO [AiNode]
nodeChildren = getAiArray {#get aiNode->mNumChildren#} {#get aiNode->mChildren#}

nodeParent :: AiNode -> IO (Maybe AiNode)
nodeParent node = fmap
  (\parent@(AiNode p) -> if p == nullPtr then Nothing else Just parent)
  ({#get aiNode->mParent#} node)

getRawArray :: (IncPtr a) => (parent -> IO CUInt) -> (parent -> IO a) -> parent -> IO [a]
getRawArray getN getPtr obj = do
  len <- getN obj
  p <- getPtr obj
  return $ if isNullPtr p
    then []
    else take (fromIntegral len) $ iterate incPtr p

readV3 :: AiVector3D -> IO (V3 Float)
readV3 p = fmap (fmap unCFloat) $ V3
  <$> ({#get aiVector3D->x#} p)
  <*> ({#get aiVector3D->y#} p)
  <*> ({#get aiVector3D->z#} p)

readRGBA :: AiColor4D -> IO (RGBA Float)
readRGBA p = fmap (fmap unCFloat) $ V4
  <$> ({#get aiColor4D->r#} p)
  <*> ({#get aiColor4D->g#} p)
  <*> ({#get aiColor4D->b#} p)
  <*> ({#get aiColor4D->a#} p)

unCFloat :: CFloat -> Float
unCFloat (CFloat f) = f

propV4 :: (a -> IO CFloat) -> (a -> IO CFloat) -> (a -> IO CFloat) -> (a -> IO CFloat) -> a -> IO (V4 Float)
propV4 x y z w o = fmap (fmap unCFloat) $ V4 <$> x o <*> y o <*> z o <*> w o

propV3 :: (a -> IO CFloat) -> (a -> IO CFloat) -> (a -> IO CFloat) -> a -> IO (V3 Float)
propV3 x y z o = fmap (fmap unCFloat) $ V3 <$> x o <*> y o <*> z o

propV2 :: (a -> IO CFloat) -> (a -> IO CFloat) -> a -> IO (V2 Float)
propV2 x y o = fmap (fmap unCFloat) $ V2 <$> x o <*> y o

type RGB = V3
type RGBA = V4

propString :: (Integral i) => (a -> IO i) -> (a -> IO CString) -> a -> IO T.Text
propString len dat v = do
  l <- len v
  d <- dat v
  T.peekCStringLen (d, fromIntegral l)

-- -----------------------------

data Camera = Camera
  { cameraAspect        :: Float
  , cameraClipPlaneFar  :: Float
  , cameraClipPlaneNear :: Float
  , cameraHorizontalFOV :: Float
  , cameraLookAt        :: V3 Float
  , cameraName          :: T.Text
  , cameraPosition      :: V3 Float
  , cameraUp            :: V3 Float
  } deriving (Show)

readCamera :: AiCamera -> IO Camera
readCamera cam = do
  cameraAspect        <- unCFloat <$> {#get aiCamera->mAspect#} cam
  cameraClipPlaneFar  <- unCFloat <$> {#get aiCamera->mClipPlaneFar#} cam
  cameraClipPlaneNear <- unCFloat <$> {#get aiCamera->mClipPlaneNear#} cam
  cameraHorizontalFOV <- unCFloat <$> {#get aiCamera->mHorizontalFOV#} cam
  cameraLookAt        <- propV3 {#get aiCamera->mLookAt.x#} {#get aiCamera->mLookAt.y#} {#get aiCamera->mLookAt.z#} cam
  cameraName          <- propString {#get aiCamera->mName.length#} {#get aiCamera->mName.data#} cam
  cameraPosition      <- propV3 {#get aiCamera->mPosition.x#} {#get aiCamera->mPosition.y#} {#get aiCamera->mPosition.z#} cam
  cameraUp            <- propV3 {#get aiCamera->mUp.x#} {#get aiCamera->mUp.y#} {#get aiCamera->mUp.z#} cam
  return Camera{..}

-- -----------------------------

{#enum aiLightSourceType as AiLightSourceType {upcaseFirstLetter}
  deriving (Eq, Show)
#}

data Light = Light
  { lightAngleInnerCone       :: Float
  , lightAngleOuterCone       :: Float
  , lightAttenuationConstant  :: Float
  , lightAttenuationLinear    :: Float
  , lightAttenuationQuadratic :: Float
  , lightColorAmbient         :: RGB Float
  , lightColorDiffuse         :: RGB Float
  , lightColorSpecular        :: RGB Float
  , lightDirection            :: V3 Float
  , lightName                 :: T.Text
  , lightPosition             :: V3 Float
  , lightSize                 :: V2 Float
  , lightType                 :: AiLightSourceType
  , lightUp                   :: V3 Float
  } deriving (Show)

readLight :: AiLight -> IO Light
readLight light = do
  lightAngleInnerCone       <- unCFloat <$> {#get aiLight->mAngleInnerCone#}       light
  lightAngleOuterCone       <- unCFloat <$> {#get aiLight->mAngleOuterCone#}       light
  lightAttenuationConstant  <- unCFloat <$> {#get aiLight->mAttenuationConstant#}  light
  lightAttenuationLinear    <- unCFloat <$> {#get aiLight->mAttenuationLinear#}    light
  lightAttenuationQuadratic <- unCFloat <$> {#get aiLight->mAttenuationQuadratic#} light
  lightColorAmbient         <- propV3 {#get aiLight->mColorAmbient.r#} {#get aiLight->mColorAmbient.g#} {#get aiLight->mColorAmbient.b#} light
  lightColorDiffuse         <- propV3 {#get aiLight->mColorDiffuse.r#} {#get aiLight->mColorDiffuse.g#} {#get aiLight->mColorDiffuse.b#} light
  lightColorSpecular        <- propV3 {#get aiLight->mColorSpecular.r#} {#get aiLight->mColorSpecular.g#} {#get aiLight->mColorSpecular.b#} light
  lightDirection            <- propV3 {#get aiLight->mDirection.x#} {#get aiLight->mDirection.y#} {#get aiLight->mDirection.z#} light
  lightName                 <- propString {#get aiLight->mName.length#} {#get aiLight->mName.data#} light
  lightPosition             <- propV3 {#get aiLight->mPosition.x#} {#get aiLight->mPosition.y#} {#get aiLight->mPosition.z#} light
  lightSize                 <- propV2 {#get aiLight->mSize.x#} {#get aiLight->mSize.y#} light
  lightType                 <- toEnum . fromIntegral <$> {#get aiLight->mType#} light
  lightUp                   <- propV3 {#get aiLight->mUp.x#} {#get aiLight->mUp.y#} {#get aiLight->mUp.z#} light
  return Light{..}

-- -----------------------------

{#enum aiPrimitiveType as AiPrimitiveType {upcaseFirstLetter}
  omit (_aiPrimitiveType_Force32Bit)
  deriving (Eq, Show, Bounded)
#}

data Mesh = Mesh
  { meshBitangents     :: [V3 Float]
  , meshColors         :: [[RGBA Float]]
  , meshFaces          :: [Face]
  , meshMaterialIndex  :: Int
  , meshMethod         :: Int
  , meshName           :: T.Text
  , meshNormals        :: [V3 Float]
  , meshPrimitiveTypes :: [AiPrimitiveType]
  , meshTangents       :: [V3 Float]
  , meshTextureCoords  :: [[V3 Float]]
  , meshVertices       :: [V3 Float]
  } deriving (Show)
  -- TODO: mAnimMeshes, mBones

readMesh :: AiMesh -> IO Mesh
readMesh mesh = do
  meshBitangents     <- getRawArray {#get aiMesh->mNumVertices#} {#get aiMesh->mBitangents#} mesh >>= mapM readV3
  meshColors         <- do
    pchans <- {#get aiMesh->mColors#} mesh
    chans <- takeWhile (not . isNullPtr) <$> peekArray {#const AI_MAX_NUMBER_OF_COLOR_SETS#} pchans
    len <- fromIntegral <$> {#get aiMesh->mNumVertices#} mesh
    mapM (mapM readRGBA . take len . iterate incPtr) chans
  meshFaces          <- getRawArray {#get aiMesh->mNumFaces#} {#get aiMesh->mFaces#} mesh >>= mapM readFace
  meshMaterialIndex  <- fromIntegral <$> {#get aiMesh->mMaterialIndex#} mesh
  meshMethod         <- fromIntegral <$> {#get aiMesh->mMethod#} mesh
  meshName           <- propString {#get aiMesh->mName.length#} {#get aiMesh->mName.data#} mesh
  meshNormals        <- getRawArray {#get aiMesh->mNumVertices#} {#get aiMesh->mNormals#} mesh >>= mapM readV3
  meshPrimitiveTypes <- do
    i <- fromIntegral <$> {#get aiMesh->mPrimitiveTypes#} mesh
    return $ filter (\typ -> fromEnum typ .&. i /= 0) [minBound .. maxBound]
  meshTangents       <- getRawArray {#get aiMesh->mNumVertices#} {#get aiMesh->mTangents#} mesh >>= mapM readV3
  meshTextureCoords  <- do
    pchans <- {#get aiMesh->mTextureCoords#} mesh
    chans <- takeWhile (not . isNullPtr) <$> peekArray {#const AI_MAX_NUMBER_OF_TEXTURECOORDS#} pchans
    len <- fromIntegral <$> {#get aiMesh->mNumVertices#} mesh
    mapM (mapM readV3 . take len . iterate incPtr) chans
  meshVertices       <- getRawArray {#get aiMesh->mNumVertices#} {#get aiMesh->mVertices#} mesh >>= mapM readV3
  return Mesh{..}

newtype Face = Face
  { faceIndices :: [Int]
  } deriving (Show)

readFace :: AiFace -> IO Face
readFace face = do
  faceIndices <- map fromIntegral <$> getAiArray {#get aiFace->mNumIndices#} {#get aiFace->mIndices#} face
  return Face{..}

-- -----------------------------

tester :: IO AiScene
tester = aiImportFile "/Users/mtolly/Desktop/3d/drums.dae" []
