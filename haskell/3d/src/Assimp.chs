{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Assimp where

import           Control.Monad     (mapM)
import           Data.Maybe        (fromMaybe)
import qualified Data.Text         as T
import qualified Data.Text.Foreign as T
import           Foreign
import           Foreign.C
import           Linear            (V2 (..), V3 (..), V4 (..), M44, lookAt)

#include "assimp/cimport.h"
#include "assimp/scene.h"
#include "assimp/postprocess.h"
#include "assimp/material.h"

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
{#pointer *aiMatrix4x4 as AiMatrix4x4 newtype #}
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
deriving instance Storable AiMatrix4x4
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

importFile :: FilePath -> [AiPostProcessSteps] -> IO (Either String AiScene)
importFile fp pp = do
  scene@(AiScene p) <- aiImportFile fp pp
  if p == nullPtr
    then Left . fromMaybe "Unknown error (aiImportFile returned NULL)" <$> aiGetErrorString
    else return $ Right scene

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

nodeTransformation :: AiNode -> IO (M44 Float)
nodeTransformation node = fmap (fmap unCFloat) <$> let
  getRow v1 v2 v3 v4 = V4 <$> v1 node <*> v2 node <*> v3 node <*> v4 node
  in V4
    <$> getRow
      {#get aiNode->mTransformation.a1#}
      {#get aiNode->mTransformation.a2#}
      {#get aiNode->mTransformation.a3#}
      {#get aiNode->mTransformation.a4#}
    <*> getRow
      {#get aiNode->mTransformation.b1#}
      {#get aiNode->mTransformation.b2#}
      {#get aiNode->mTransformation.b3#}
      {#get aiNode->mTransformation.b4#}
    <*> getRow
      {#get aiNode->mTransformation.c1#}
      {#get aiNode->mTransformation.c2#}
      {#get aiNode->mTransformation.c3#}
      {#get aiNode->mTransformation.c4#}
    <*> getRow
      {#get aiNode->mTransformation.d1#}
      {#get aiNode->mTransformation.d2#}
      {#get aiNode->mTransformation.d3#}
      {#get aiNode->mTransformation.d4#}

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

getCameraMatrix :: Camera -> M44 Float
getCameraMatrix cam = lookAt
  (cameraLookAt   cam)
  (cameraPosition cam)
  (cameraUp       cam)

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

{#enum aiReturn as AiReturn {upcaseFirstLetter}
  deriving (Eq, Show, Bounded)
#}

{#fun aiGetMaterialFloatArray
  { `AiMaterial'
  , `CString'
  , `CUInt'
  , `CUInt'
  , id `Ptr CFloat'
  , id `Ptr CUInt'
  } -> `AiReturn'
#}

materialGetFloat :: AiMaterial -> (String, CUInt, CUInt) -> IO (Either AiReturn Float)
materialGetFloat mat (k, t, i) = alloca $ \p -> do
  res <- withCString k $ \k' -> aiGetMaterialFloatArray mat k' t i p nullPtr
  case res of
    AiReturn_SUCCESS -> Right . unCFloat <$> peek p
    _                -> return $ Left res

{#fun aiGetMaterialIntegerArray
  { `AiMaterial'
  , `CString'
  , `CUInt'
  , `CUInt'
  , id `Ptr CInt'
  , id `Ptr CUInt'
  } -> `AiReturn'
#}

materialGetInteger :: AiMaterial -> (String, CUInt, CUInt) -> IO (Either AiReturn CInt)
materialGetInteger mat (k, t, i) = alloca $ \p -> do
  res <- withCString k $ \k' -> aiGetMaterialIntegerArray mat k' t i p nullPtr
  case res of
    AiReturn_SUCCESS -> Right <$> peek p
    _                -> return $ Left res

{#pointer *aiString as AiString #}

{#fun aiGetMaterialString
  { `AiMaterial'
  , `CString'
  , `CUInt'
  , `CUInt'
  , `AiString'
  } -> `AiReturn'
#}

materialGetString :: AiMaterial -> (String, CUInt, CUInt) -> IO (Either AiReturn T.Text)
materialGetString mat (k, t, i) = allocaBytes {#sizeof aiString#} $ \p -> do
  res <- withCString k $ \k' -> aiGetMaterialString mat k' t i p
  case res of
    AiReturn_SUCCESS -> Right <$> propString {#get aiString->length#} {#get aiString->data#} p
    _                -> return $ Left res

{#fun aiGetMaterialColor
  { `AiMaterial'
  , `CString'
  , `CUInt'
  , `CUInt'
  , `AiColor4D'
  } -> `AiReturn'
#}

materialGetColor :: AiMaterial -> (String, CUInt, CUInt) -> IO (Either AiReturn (RGBA Float))
materialGetColor mat (k, t, i) = allocaBytes {#sizeof aiColor4D#} $ \p -> do
  res <- withCString k $ \k' -> aiGetMaterialColor mat k' t i (AiColor4D p)
  case res of
    AiReturn_SUCCESS -> Right <$> readRGBA (AiColor4D p)
    _                -> return $ Left res

data AI_MATKEY a where
  AI_MATKEY_NAME                    :: AI_MATKEY T.Text
  AI_MATKEY_TWOSIDED                :: AI_MATKEY CInt -- Bool
  AI_MATKEY_SHADING_MODEL           :: AI_MATKEY CInt -- aiShadingMode enum
  AI_MATKEY_ENABLE_WIREFRAME        :: AI_MATKEY CInt -- Bool
  AI_MATKEY_BLEND_FUNC              :: AI_MATKEY CInt -- aiBlendMode enum
  AI_MATKEY_OPACITY                 :: AI_MATKEY Float
  AI_MATKEY_BUMPSCALING             :: AI_MATKEY CInt -- not in docs?
  AI_MATKEY_SHININESS               :: AI_MATKEY Float
  AI_MATKEY_REFLECTIVITY            :: AI_MATKEY Float -- not in docs?
  AI_MATKEY_SHININESS_STRENGTH      :: AI_MATKEY Float
  AI_MATKEY_REFRACTI                :: AI_MATKEY Float
  AI_MATKEY_COLOR_DIFFUSE           :: AI_MATKEY (RGBA Float)
  AI_MATKEY_COLOR_AMBIENT           :: AI_MATKEY (RGBA Float)
  AI_MATKEY_COLOR_SPECULAR          :: AI_MATKEY (RGBA Float)
  AI_MATKEY_COLOR_EMISSIVE          :: AI_MATKEY (RGBA Float)
  AI_MATKEY_COLOR_TRANSPARENT       :: AI_MATKEY (RGBA Float)
  AI_MATKEY_COLOR_REFLECTIVE        :: AI_MATKEY (RGBA Float)
  AI_MATKEY_GLOBAL_BACKGROUND_IMAGE :: AI_MATKEY T.Text -- not in docs?

materialGet :: AiMaterial -> AI_MATKEY a -> IO (Either AiReturn a)
materialGet mat = \case
  -- triples copied from material.h
  AI_MATKEY_NAME                    -> materialGetString  mat ("?mat.name",0,0)
  AI_MATKEY_TWOSIDED                -> materialGetInteger mat ("$mat.twosided",0,0)
  AI_MATKEY_SHADING_MODEL           -> materialGetInteger mat ("$mat.shadingm",0,0)
  AI_MATKEY_ENABLE_WIREFRAME        -> materialGetInteger mat ("$mat.wireframe",0,0)
  AI_MATKEY_BLEND_FUNC              -> materialGetInteger mat ("$mat.blend",0,0)
  AI_MATKEY_OPACITY                 -> materialGetFloat   mat ("$mat.opacity",0,0)
  AI_MATKEY_BUMPSCALING             -> materialGetInteger mat ("$mat.bumpscaling",0,0)
  AI_MATKEY_SHININESS               -> materialGetFloat   mat ("$mat.shininess",0,0)
  AI_MATKEY_REFLECTIVITY            -> materialGetFloat   mat ("$mat.reflectivity",0,0)
  AI_MATKEY_SHININESS_STRENGTH      -> materialGetFloat   mat ("$mat.shinpercent",0,0)
  AI_MATKEY_REFRACTI                -> materialGetFloat   mat ("$mat.refracti",0,0)
  AI_MATKEY_COLOR_DIFFUSE           -> materialGetColor   mat ("$clr.diffuse",0,0)
  AI_MATKEY_COLOR_AMBIENT           -> materialGetColor   mat ("$clr.ambient",0,0)
  AI_MATKEY_COLOR_SPECULAR          -> materialGetColor   mat ("$clr.specular",0,0)
  AI_MATKEY_COLOR_EMISSIVE          -> materialGetColor   mat ("$clr.emissive",0,0)
  AI_MATKEY_COLOR_TRANSPARENT       -> materialGetColor   mat ("$clr.transparent",0,0)
  AI_MATKEY_COLOR_REFLECTIVE        -> materialGetColor   mat ("$clr.reflective",0,0)
  AI_MATKEY_GLOBAL_BACKGROUND_IMAGE -> materialGetString  mat ("?bg.global",0,0)

data Material = Material
  { material_NAME                    :: Either AiReturn T.Text
  , material_TWOSIDED                :: Either AiReturn CInt -- Bool
  , material_SHADING_MODEL           :: Either AiReturn CInt -- aiShadingMode enum
  , material_ENABLE_WIREFRAME        :: Either AiReturn CInt -- Bool
  , material_BLEND_FUNC              :: Either AiReturn CInt -- aiBlendMode enum
  , material_OPACITY                 :: Either AiReturn Float
  , material_BUMPSCALING             :: Either AiReturn CInt -- not in docs?
  , material_SHININESS               :: Either AiReturn Float
  , material_REFLECTIVITY            :: Either AiReturn Float -- not in docs?
  , material_SHININESS_STRENGTH      :: Either AiReturn Float
  , material_REFRACTI                :: Either AiReturn Float
  , material_COLOR_DIFFUSE           :: Either AiReturn (RGBA Float)
  , material_COLOR_AMBIENT           :: Either AiReturn (RGBA Float)
  , material_COLOR_SPECULAR          :: Either AiReturn (RGBA Float)
  , material_COLOR_EMISSIVE          :: Either AiReturn (RGBA Float)
  , material_COLOR_TRANSPARENT       :: Either AiReturn (RGBA Float)
  , material_COLOR_REFLECTIVE        :: Either AiReturn (RGBA Float)
  , material_GLOBAL_BACKGROUND_IMAGE :: Either AiReturn T.Text -- not in docs?
  } deriving (Show)

readMaterial :: AiMaterial -> IO Material
readMaterial mat = Material
  <$> materialGet mat AI_MATKEY_NAME
  <*> materialGet mat AI_MATKEY_TWOSIDED
  <*> materialGet mat AI_MATKEY_SHADING_MODEL
  <*> materialGet mat AI_MATKEY_ENABLE_WIREFRAME
  <*> materialGet mat AI_MATKEY_BLEND_FUNC
  <*> materialGet mat AI_MATKEY_OPACITY
  <*> materialGet mat AI_MATKEY_BUMPSCALING
  <*> materialGet mat AI_MATKEY_SHININESS
  <*> materialGet mat AI_MATKEY_REFLECTIVITY
  <*> materialGet mat AI_MATKEY_SHININESS_STRENGTH
  <*> materialGet mat AI_MATKEY_REFRACTI
  <*> materialGet mat AI_MATKEY_COLOR_DIFFUSE
  <*> materialGet mat AI_MATKEY_COLOR_AMBIENT
  <*> materialGet mat AI_MATKEY_COLOR_SPECULAR
  <*> materialGet mat AI_MATKEY_COLOR_EMISSIVE
  <*> materialGet mat AI_MATKEY_COLOR_TRANSPARENT
  <*> materialGet mat AI_MATKEY_COLOR_REFLECTIVE
  <*> materialGet mat AI_MATKEY_GLOBAL_BACKGROUND_IMAGE

-- -----------------------------

tester :: IO AiScene
tester = aiImportFile "/Users/mtolly/Desktop/3d/drums.dae" []
