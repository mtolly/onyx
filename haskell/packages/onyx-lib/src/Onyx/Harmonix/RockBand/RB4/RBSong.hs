{-
Ported from LibForge
https://github.com/maxton/LibForge
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Harmonix.RockBand.RB4.RBSong where

import           Control.Monad
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy                as BL
import           Data.Int
import           Data.Word
import           Onyx.Harmonix.RockBand.Milo.Lipsync
import           Onyx.MIDI.Track.Lipsync             (VisemeEvent (..))
import           Onyx.Util.Binary                    (runGetM)

data Resource = Resource
  { type_    :: B.ByteString
  , path     :: B.ByteString
  , contents :: ResourceContents
  } deriving (Show)

data ResourceContents
  = RC_PropAnimResource EntityResource
  | RC_RBSongResource EntityResource
  | RC_EntityResource EntityResource
  deriving (Show)

data EntityResource = EntityResource
  { version          :: Int32
  , inlineLayerNames :: [B.ByteString]
  , entity           :: Entity
  } deriving (Show)

data Entity = Entity
  { version   :: Int32
  , unkString :: Maybe B.ByteString
  , layers    :: [(EntityLayer, Maybe [Resource])]
  } deriving (Show)

data EntityLayer = EntityLayer
  { version           :: Int32
  , fileSlotIndex     :: Int32
  , totalObjectLayers :: Int16
  , objects           :: [GameObject]
  } deriving (Show)

data GameObjectID = GameObjectID
  { index :: Int32
  , layer :: Int16
  } deriving (Show)

data GameObject = GameObject
  { objectID   :: GameObjectID
  , rev        :: Int32
  , name       :: B.ByteString
  , components :: [Component]
  } deriving (Show)

data Component = Component
  { name1    :: B.ByteString
  , name2    :: Maybe B.ByteString
  , rev      :: Int32
  , unknown2 :: Int64
  , props    :: [Property]
  } deriving (Show)

data Property = Property
  { name  :: B.ByteString
  , value :: Value
  } deriving (Show)

data Value
  = FloatValue Float
  | IntValue Int32
  | ByteValue Word8
  | UIntValue Word32
  | LongValue Int64
  | BoolValue Bool
  | SymbolValue B.ByteString
  | ResourcePathValue B.ByteString Word8
  | StructValue [Property]
  | GameObjectIdValue Int32 Int32 Int32 Int32 Int32 Int32
  | ColorValue Float Float Float Float Int32 Int32 Int32 Int32
  | DrivenProp Int32 Int32 (Maybe B.ByteString) Int32 Int64 (Maybe B.ByteString)
  | ArrayValue [Value]
  deriving (Show)

getEntityResource :: Get EntityResource
getEntityResource = do
  version <- getInt32le
  when (version < 0xC || version > 0x11) $ do
    fail $ "Can't handle EntityResource version " <> show version
  inlineLayerNames <- getArray getString
  when (version >= 0xF) $ do
    _unk0 <- getInt32le
    when (version >= 0x10) $ do
      _unk1 <- getInt32le
      return ()
  entity <- getEntity
  return EntityResource{..}

getArray :: Get a -> Get [a]
getArray f = do
  len <- getWord32le
  replicateM (fromIntegral len) f

getEntity :: Get Entity
getEntity = do
  version <- getInt32le
  when (version > 0x1E) $ fail $ "Can't handle entity version " <> show version
  when (version <= 1) $ fail "Entity version must be > 1"
  unkString <- if version <= 4
    then Just <$> getString
    else return Nothing
  numLayers <- if version >= 9
    then getInt32le
    else do
      _rootId <- getInt32le
      if version > 6
        then getInt32le
        else do
          _arraySize <- getInt16le
          return 1
  replicateM_ (fromIntegral numLayers) $ do
    if version < 8
      then do
        when (version >= 7) $ void getInt16le -- layer_field_28
        when (numLayers /= 1) $ fail "Num layers should be 1 for version <8"
        propArrayBaseSize <- getInt32le
        when (propArrayBaseSize > 0) $ fail "Version < 8 should load objs here"
      else when (version <= 11) $ void getString -- layer_name
  when (version < 8) $ fail "Version < 8 not implemented"
  layers <- forM [0 .. numLayers - 1] $ \i -> do
    layer <- getEntityLayer i version
    inlineResources <- if version >= 0xC
      then Just <$> loadInlineResources
      else return Nothing
    return (layer, inlineResources)
  return Entity{..}

loadInlineResources :: Get [Resource]
loadInlineResources = do
  versionOrig <- getInt32le
  -- HACK: Allows reading codemonkey_rbn.rbsong
  _version <- if versionOrig == 1
    then do
      void getInt32le
      void getInt32le
      void getInt32le
      getInt32le
    else return versionOrig
  count <- getInt32le
  replicateM (fromIntegral count) $ do
    type_ <- getString
    path <- getString
    contents <- case type_ of
      "PropAnimResource" -> RC_PropAnimResource <$> getEntityResource
      "RBSongResource"   -> RC_RBSongResource <$> getEntityResource
      "EntityResource"   -> RC_EntityResource <$> getEntityResource
      _                  -> fail $ "Unimplemented resource type: " <> show type_
    return Resource{..}

getString :: Get B.ByteString
getString = do
  len <- getInt32le
  getByteString $ fromIntegral len

getEntityLayer :: Int32 -> Int32 -> Get EntityLayer
getEntityLayer _index _entityVersion = do
  version <- getInt32le
  when (version < 8) $ fail "Entity layer version should be > 8"
  when (version >= 0x17) $ void getInt32le -- unknown
  fileSlotIndex <- getInt32le
  -- fileSlotIndex should equal _index, or may be not? commented out error
  totalObjectLayers <- getInt16le
  numObjects <- getInt32le
  let getObjects i = if i >= numObjects
        then return []
        else do
          obj <- getObject version
          if obj.rev >= 3 && B.null obj.name
            then getObjects i -- ghost object
            else (obj :) <$> getObjects (i + 1)
  objects <- getObjects 0
  return EntityLayer{..}

blankObject :: GameObject
blankObject = GameObject
  { objectID = GameObjectID 0 0
  , rev = 0
  , name = ""
  , components = []
  }

getObject :: Int32 -> Get GameObject
getObject layerVersion = do
  intID <- getInt32le
  if intID == (-1)
    then return blankObject
    else do
      let objectID = GameObjectID
            { index = intID .&. 0xFFF
            , layer = fromIntegral $ intID `shiftR` 16
            }
      rev <- getInt32le
      name <- if rev < 0
        then B.singleton <$> getWord8
        else getString
      if rev >= 3 && B.null name
        then do
          -- TODO: Newer GameObject unknown stuff
          void getInt32le
          void getInt32le
          when (rev >= 4) $ void getInt32le
          let components = []
          return GameObject{..}
        else do
          numChildren <- getInt32le
          components <- replicateM (fromIntegral numChildren) $ do
            getComponent rev layerVersion
          return GameObject{..}

getComponent :: Int32 -> Int32 -> Get Component
getComponent objRev layerVersion = do
  name1 <- getString
  name2 <- if objRev >= 2
    then Just <$> getString
    else return Nothing
  rev <- getInt32le
  unknown2 <- getInt64le
  props <- if layerVersion >= 0xE
    then getProperties
    else return []
  return Component{..}

getPropertyType :: Get (Get Value)
getPropertyType = do
  typeID <- getInt32le
  case typeID of
    0 -> return $ FloatValue <$> getFloatle
    3 -> return $ IntValue <$> getInt32le
    5 -> return $ ByteValue <$> getWord8
    7 -> return $ UIntValue <$> getWord32le
    8 -> return $ LongValue <$> getInt64le
    9 -> return $ BoolValue . (/= 0) <$> getWord8
    0xB -> return $ SymbolValue <$> getString
    0xC -> return $ do
      prefix <- getWord8
      str <- getString
      return $ ResourcePathValue str prefix
    0x10 -> return $ do
      unk_driven_prop_1 <- getInt32le
      unk_driven_prop_2 <- getInt32le
      if unk_driven_prop_2 == 0
        then do
          className <- getString
          unknown3 <- getInt32le
          unknown4 <- getInt64le
          propertyName <- getString
          return $ DrivenProp unk_driven_prop_1 unk_driven_prop_2 (Just className) unknown3 unknown4 (Just propertyName)
        else do
          unknown3 <- getInt32le
          unknown4 <- getInt64le
          return $ DrivenProp unk_driven_prop_1 unk_driven_prop_2 Nothing unknown3 unknown4 Nothing
    0xA -> fail "unhandled property type: 0xA (GameObjectId)"
    0xD -> fail "unhandled property type: 0xD (Color)"
    0xF -> do
      _refCount <- getInt64le
      propertyReaders <- getPropertyReaders
      return $ fmap StructValue $ forM propertyReaders $ \(name, reader) -> do
        value <- reader
        return $ Property name value
    _ -> if typeID .&. 0x100 == 0x100
      then do
        elementType <- getPropertyType
        return $ ArrayValue <$> getArray elementType
      else fail $ "Unhandled property type: " <> show typeID

getPropertyReaders :: Get [(B.ByteString, Get Value)]
getPropertyReaders = do
  numProps <- getInt32le
  replicateM (fromIntegral numProps) $ do
    name <- getString
    reader <- getPropertyType
    return (name, reader)

getProperties :: Get [Property]
getProperties = do
  propertyReaders <- getPropertyReaders
  forM propertyReaders $ \(name, reader) -> do
    value <- reader
    return $ Property name value

--------------------------------------------------------------------------------

getRBSongLipsync :: (MonadFail m) => EntityResource -> m [(B.ByteString, Lipsync)]
getRBSongLipsync res = let
  lipsyncList = do
    (layer, _resources) <- res.entity.layers
    obj <- layer.objects
    let lipsyncName = obj.name
    comp <- obj.components
    guard $ comp.name1 == "RBCharLipSync"
    let visemes = do
          prop <- comp.props
          guard $ prop.name == "visemes"
          ArrayValue arr <- return $ prop.value
          SymbolValue sym <- arr
          return sym
        keyframeData = BL.pack $ do
          prop <- comp.props
          guard $ prop.name == "data"
          ArrayValue arr <- return $ prop.value
          ByteValue b <- arr
          return b
    return (lipsyncName, visemes, keyframeData)
  parseKeyframes prev = isEmpty >>= \case
    True -> return $ reverse prev
    False -> do
      eventCount <- getWord8
      keyframeEvents <- replicateM (fromIntegral eventCount) $ do
        visemeKey <- fromIntegral <$> getWord8
        visemeWeight <- getWord8
        let visemeGraph = ()
        return VisemeEvent{..}
      parseKeyframes $ Keyframe{..} : prev
  in forM lipsyncList $ \(lipsyncName, visemes, keyframeData) -> do
    keyframes <- runGetM (parseKeyframes []) keyframeData
    return (lipsyncName, emptyLipsync { lipsyncVisemes = visemes, lipsyncKeyframes = keyframes })
