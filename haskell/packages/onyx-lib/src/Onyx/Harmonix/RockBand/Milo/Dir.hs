{-
Structures here are based on the Rock Band 3 decompilation project,
mainly these files:
- src/system/obj/DirLoader.cpp (outer header, class + entry tables)
- src/system/obj/Dir.cpp (ObjectDir::PreLoad and ObjectDir::PostLoad)
- src/system/obj/Object.cpp (Hmx::Object::LoadType and LoadRest)
- for the dir subclasses, their PreLoad/PostLoad in src/system/rndobj/Dir.cpp,
  ui/PanelDir.cpp, ui/UIListDir.cpp, ui/UILabelDir.cpp, ui/UIFontImporter.cpp,
  world/Dir.cpp, char/Character.cpp, char/CharacterTest.cpp,
  char/CharClipSet.cpp, char/CharBoneDir.cpp, and the superclass Load
  functions in rndobj/Anim.cpp, rndobj/Draw.cpp, rndobj/Trans.cpp
-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Harmonix.RockBand.Milo.Dir where

import           Control.Monad                           (forM, forM_,
                                                          replicateM, unless,
                                                          void, when)
import qualified Data.ByteString                         as B
import qualified Data.ByteString.Lazy                    as BL
import           Data.Int                                (Int32)
import           Data.Maybe                              (catMaybes)
import           Onyx.Codec.Binary
import           Onyx.Harmonix.RockBand.Milo.Compression (magicBarrier)

#if MIN_VERSION_base(4,18,0)
import           Control.Applicative                     ((<|>))
#else
import           Control.Applicative                     (liftA2, (<|>))
#endif

getStringBE :: Get B.ByteString
getStringBE = do
  len <- getWord32be
  getByteString $ fromIntegral len

putStringBE :: B.ByteString -> Put
putStringBE bs = do
  putWord32be $ fromIntegral $ B.length bs
  putByteString bs

getArrayBE :: Get a -> Get [a]
getArrayBE g = do
  len <- getWord32be
  replicateM (fromIntegral len) g

putArrayBE :: (a -> Put) -> [a] -> Put
putArrayBE p xs = do
  putWord32be $ fromIntegral $ length xs
  mapM_ p xs

breakMilo :: B.ByteString -> [B.ByteString]
breakMilo b = case B.breakSubstring (B.pack magicBarrier) b of
  (x, y) -> if B.null y
    then [b]
    else x : breakMilo (B.drop 4 y)

parseFileADDE :: Get BL.ByteString
parseFileADDE = let
  barrier = B.pack magicBarrier
  lookAheadADDE !n = do
    magic <- lookAhead $ getByteString 4
    if magic == barrier
      then return n
      else skip 1 >> lookAheadADDE (n + 1)
  in do
    fileLength <- lookAhead $ lookAheadADDE 0
    bs <- getLazyByteString fileLength
    skip 4 -- skip the ADDE
    return bs

putFileADDE :: BL.ByteString -> Put
putFileADDE bs = do
  putLazyByteString bs
  putByteString $ B.pack magicBarrier

-- | The container structure produced by the game's DirLoader.
-- Contains an object directory (ObjectDir or a subclass) and the data for
-- each object inside it.
data MiloDir = MiloDir
  { miloVersion       :: Word32
  -- ^ DirLoader revision. Observed:
  -- - 25 for all song milos before RB3 (RB1/RB2/Lego/Beatles/GDRB)
  -- - 28 for RB3 (songs and on-disc content)
  , miloType          :: B.ByteString
  -- ^ Class name of the directory: "ObjectDir" for song milos.
  -- On-disc content also uses "RndDir", "PanelDir", "WorldDir", "Character",
  -- "CharClipSet" and others, whose extra dir data we can't parse yet.
  , miloName          :: B.ByteString
  , miloHashReserve   :: Word32
  -- ^ Extra hash table slots to reserve when loading; roughly tracks the
  -- number of objects. (DirLoader::LoadHeader "size1")
  , miloStringReserve :: Word32
  -- ^ Extra string table bytes to reserve when loading; roughly tracks the
  -- total length of object names. (DirLoader::LoadHeader "size2")
  , miloEntryNames    :: [(B.ByteString, B.ByteString)]
  -- ^ (class, name) for each object in the dir, in the same order as miloFiles
  , miloExtResources  :: Maybe [B.ByteString]
  -- ^ External resource paths, only in version <= 16 (untested)
  , miloObjectDir     :: ObjectDirInfo
  -- ^ The directory's own data (ObjectDir::PreLoad + PostLoad),
  -- including any inline subdirectories
  , miloClassInfo     :: DirClassInfo
  -- ^ Extra data for dir classes other than plain ObjectDir: their own
  -- revisions (stored before the ObjectDir data) and extra fields
  -- (stored after it)
  , miloRemainder     :: BL.ByteString
  -- ^ any bytes between the parsed dir data and the AD DE AD DE barrier;
  -- should be empty if we understood the whole structure
  , miloFiles         :: [MiloEntry]
  -- ^ Serialized data for each entry, in the same order as miloEntryNames
  } deriving (Show)

-- | The data for one object inside a dir.
data MiloEntry = MiloEntry
  { entryData      :: BL.ByteString
  -- ^ the object's serialized data, without the AD DE AD DE barrier that
  -- follows it in the file. For an entry that is itself a dir class, this is
  -- the dir's data in proxy form.
  , entryInlineDir :: Maybe MiloDir
  -- ^ for a dir-class entry with an inlined proxy: the embedded dir stream,
  -- which the game reads after the barrier (ObjectDir::PostLoad makes a
  -- nested DirLoader on the same stream). Its own final barrier ends the
  -- entry, so no additional barrier follows.
  } deriving (Show)

-- | Data for the ObjectDir class itself, whose format depends on its own
-- revision (odVersion), separate from the outer DirLoader revision.
-- Field presence is decided by odVersion when parsing; when constructing
-- by hand, fill fields consistently with the version (see comments).
data ObjectDirInfo = ObjectDirInfo
  { odVersion          :: Word32
  -- ^ ObjectDir revision. Observed: 20 (RB1/RB2/Lego/GDRB), 22 (Beatles),
  -- 27 (RB3). We support 18-27.
  , odObjectType       :: Maybe (Word32, B.ByteString)
  -- ^ version >= 22: Hmx::Object::LoadType, the base object revision
  -- (always 2) and the dir's type property symbol
  -- ("song" for song milo roots, "" otherwise)
  , odAlwaysInlined    :: Maybe Word32
  -- ^ version >= 26: mAlwaysInlined (observed 0)
  , odAlwaysInlineHash :: Maybe B.ByteString
  -- ^ version >= 26: mAlwaysInlineHash (observed empty)
  , odViewports        :: [[Float]]
  -- ^ Camera viewports, 12 floats each (3x3 rotation matrix + position).
  -- Always 7 in practice: perspective, left, right, top, bottom, front, back
  , odCurViewport      :: Word32
  -- ^ Index of the current viewport (usually 0 = perspective)
  , odInlineProxy      :: Maybe Word8
  -- ^ version >= 20: mInlineProxy bool (observed 1)
  , odProxyPath        :: B.ByteString
  -- ^ mProxyFile path (observed empty)
  , odExtSubdirPaths   :: [B.ByteString]
  -- ^ Paths of external (not inlined) subdirs, loaded from separate files
  , odRev23Ints        :: Maybe [Word32]
  -- ^ version == 23 only: unknown ints, one per external subdir (untested)
  , odInlineSubdirType :: Maybe Word8
  -- ^ version >= 21: mInlineSubDirType
  -- (0 = never, 1 = cached, 2 = always, 3 = cached shared)
  , odSubdirs          :: [MiloSubdir]
  -- ^ version >= 21: inlined subdirectories
  , odRev18Int         :: Maybe Word32
  -- ^ version 17-18 only: old inline proxy value (untested)
  , odUnknownString    :: B.ByteString
  -- ^ first string in ObjectDir::PostLoad (observed empty)
  , odCurCamera        :: B.ByteString
  -- ^ name of the current camera object (observed empty in songs)
  , odObjectTypePost   :: Maybe (Word32, B.ByteString)
  -- ^ version 17-21: same as odObjectType but stored at the end
  -- (Hmx::Object::Load inside ObjectDir::PostLoad)
  , odTypeProps        :: Maybe MiloDataArray
  -- ^ the dir object's type properties, a DataArray (dtb-style script data)
  , odReserved         :: Maybe BL.ByteString
  -- ^ present when the base object revision above is nonzero:
  -- reserved data, skipped by the game (observed empty)
  } deriving (Show)

-- | Serialized DataArray (dtb-like), from DataArray::Save/Load
data MiloDataArray = MiloDataArray
  { mdaLine       :: Word16
  , mdaDeprecated :: Word16
  , mdaNodes      :: [(Word32, MiloDataValue)] -- ^ (node type, value)
  } deriving (Show)

data MiloDataValue
  = MDInt    Word32          -- ^ types 0, 6, 8, 9 (note: int type id is
                             -- swapped 0 <-> 6 on disk vs in memory)
  | MDFloat  Float           -- ^ type 1
  | MDString B.ByteString    -- ^ types 2 (var), 3 (func), 4 (object),
                             -- 5 (symbol), 7 (ifdef), 32-35, 37
  | MDBlob   Word32 BL.ByteString -- ^ type 18 (string), 20 (glob):
                             -- raw stored size (negative for globs) + bytes
  | MDArray  MiloDataArray   -- ^ types 16 (array), 17 (command), 19 (property)
  deriving (Show)

parseDataArray :: Get MiloDataArray
parseDataArray = do
  size <- getWord16be
  mdaLine <- getWord16be
  mdaDeprecated <- getWord16be
  mdaNodes <- replicateM (fromIntegral size) do
    t <- getWord32be
    val <- if
      | elem t [0, 6, 8, 9]        -> MDInt <$> getWord32be
      | t == 1                     -> MDFloat <$> getFloatbe
      | elem t [2, 3, 4, 5, 7, 32, 33, 34, 35, 37] -> MDString <$> getStringBE
      | elem t [16, 17, 19]        -> MDArray <$> parseDataArray
      | t == 18                    -> do
        v <- getWord32be
        MDBlob v <$> getLazyByteString (fromIntegral v)
      | t == 20                    -> do
        v <- getWord32be
        MDBlob v <$> getLazyByteString
          (fromIntegral $ negate (fromIntegral v :: Int32))
      | otherwise -> fail $ "Unsupported DataArray node type " <> show t
    return (t, val)
  return MiloDataArray{..}

putDataArray :: MiloDataArray -> Put
putDataArray MiloDataArray{..} = do
  putWord16be $ fromIntegral $ length mdaNodes
  putWord16be mdaLine
  putWord16be mdaDeprecated
  forM_ mdaNodes \(t, val) -> do
    putWord32be t
    case val of
      MDInt    x    -> putWord32be x
      MDFloat  x    -> putFloatbe x
      MDString x    -> putStringBE x
      MDBlob   v bs -> putWord32be v >> putLazyByteString bs
      MDArray  x    -> putDataArray x

-- | An inlined subdirectory of an ObjectDir.
data MiloSubdir = MiloSubdir
  { sdName   :: B.ByteString
  , sdInline :: Maybe (Word8, Word8)
  -- ^ dir version >= 25: inline type (1 = cached, 2 = always, 3 = cached
  -- shared) and shared flag. Note, these are (de)serialized grouped across
  -- subdirs (all types, then all flags), not per subdir.
  , sdDir    :: Maybe MiloDir
  -- ^ the subdir contents; Nothing if the shared flag is set, meaning the
  -- contents live in a previously loaded dir
  } deriving (Show)

instance Bin MiloDir where
  bin = Codec

    { codecIn = do
      miloVersion <- getWord32be
      when (miloVersion < 14 || miloVersion > 32) $ fail $
        "Unsupported milo dir version " <> show miloVersion
      miloType <- getStringBE
      miloName <- getStringBE
      miloHashReserve <- getWord32be
      miloStringReserve <- getWord32be
      entryCount <- getWord32be
      miloEntryNames <- replicateM (fromIntegral entryCount)
        $ liftA2 (,) getStringBE getStringBE
      miloExtResources <- if miloVersion <= 16
        then Just <$> getArrayBE getStringBE
        else return Nothing
      (miloObjectDir, miloClassInfo) <- parseDirBody False miloType
      miloRemainder <- parseFileADDE
      miloFiles <- forM miloEntryNames $ parseEntry . fst
      return MiloDir{..}

    , codecOut = fmapArg \MiloDir{..} -> do
      putWord32be miloVersion
      putStringBE miloType
      putStringBE miloName
      putWord32be miloHashReserve
      putWord32be miloStringReserve
      putArrayBE (\(x, y) -> putStringBE x >> putStringBE y) miloEntryNames
      mapM_ (mapM_ putStringBE) miloExtResources
      putDirBody miloObjectDir miloClassInfo
      putFileADDE miloRemainder
      forM_ miloFiles \entry -> do
        putFileADDE entry.entryData
        mapM_ (void . codecOut bin) entry.entryInlineDir

    }

-- ObjectDir::PreLoad then ObjectDir::PostLoad.
parseObjectDir :: Get ObjectDirInfo
parseObjectDir = do
  odVersion <- getWord32be
  when (odVersion < 18 || odVersion > 27) $ fail $
    "Unsupported ObjectDir version " <> show odVersion
  odObjectType <- if odVersion >= 22
    then Just <$> liftA2 (,) getWord32be getStringBE
    else return Nothing
  odAlwaysInlined <- if odVersion >= 26
    then Just <$> getWord32be
    else return Nothing
  odAlwaysInlineHash <- if odVersion >= 26
    then Just <$> getStringBE
    else return Nothing
  odViewports <- getArrayBE $ replicateM 12 getFloatbe
  odCurViewport <- getWord32be
  odInlineProxy <- if odVersion >= 20
    then Just <$> getWord8
    else return Nothing
  odProxyPath <- getStringBE
  odExtSubdirPaths <- getArrayBE getStringBE
  odRev23Ints <- if odVersion == 23
    then Just <$> getArrayBE getWord32be
    else return Nothing
  (odInlineSubdirType, subdirNames) <- if odVersion >= 21
    then liftA2 (,) (Just <$> getWord8) (getArrayBE getStringBE)
    else return (Nothing, [])
  when (odVersion == 24 && not (null subdirNames)) $ fail
    "Version 24 dir with inline subdirs not supported (unknown layout)"
  odSubdirs <- if odVersion >= 25
    then do
      -- all InlineDirType bytes, then all shared flags, then all contents
      types <- replicateM (length subdirNames) getWord8
      shareds <- replicateM (length subdirNames) getWord8
      forM (zip subdirNames $ zip types shareds) \(name, inline@(_, shared)) -> do
        dir <- if shared == 0
          then Just <$> codecIn bin
          else return Nothing
        return $ MiloSubdir name (Just inline) dir
    else forM subdirNames \name -> do
      dir <- codecIn bin
      return $ MiloSubdir name Nothing $ Just dir
  odRev18Int <- if odVersion <= 18
    then Just <$> getWord32be
    else return Nothing
  -- here begins ObjectDir::PostLoad
  odUnknownString <- getStringBE
  odCurCamera <- getStringBE
  odObjectTypePost <- if odVersion <= 21
    then Just <$> liftA2 (,) getWord32be getStringBE
    else return Nothing
  propsFlag <- getWord8
  odTypeProps <- getMaybe (propsFlag /= 0) parseDataArray
  let objectRev = maybe 0 fst $ odObjectType <|> odObjectTypePost
  odReserved <- if objectRev /= 0
    then Just <$> (getWord32be >>= getLazyByteString . fromIntegral)
    else return Nothing
  return ObjectDirInfo{..}

putObjectDir :: ObjectDirInfo -> Put
putObjectDir ObjectDirInfo{..} = do
  putWord32be odVersion
  forM_ odObjectType \(rev, sym) -> putWord32be rev >> putStringBE sym
  mapM_ putWord32be odAlwaysInlined
  mapM_ putStringBE odAlwaysInlineHash
  putArrayBE (mapM_ putFloatbe) odViewports
  putWord32be odCurViewport
  mapM_ putWord8 odInlineProxy
  putStringBE odProxyPath
  putArrayBE putStringBE odExtSubdirPaths
  mapM_ (putArrayBE putWord32be) odRev23Ints
  forM_ odInlineSubdirType \t -> do
    putWord8 t
    putArrayBE putStringBE $ map (.sdName) odSubdirs
  if odVersion >= 25
    then do
      forM_ odSubdirs $ mapM_ (putWord8 . fst) . (.sdInline)
      forM_ odSubdirs $ mapM_ (putWord8 . snd) . (.sdInline)
      forM_ odSubdirs $ mapM_ (void . codecOut bin) . (.sdDir)
    else forM_ odSubdirs $ mapM_ (void . codecOut bin) . (.sdDir)
  mapM_ putWord32be odRev18Int
  putStringBE odUnknownString
  putStringBE odCurCamera
  forM_ odObjectTypePost \(rev, sym) -> putWord32be rev >> putStringBE sym
  case odTypeProps of
    Nothing -> putWord8 0
    Just da -> putWord8 1 >> putDataArray da
  forM_ odReserved \r -> do
    putWord32be $ fromIntegral $ BL.length r
    putLazyByteString r

--------------------------------------------------------------------------------
-- Dir subclasses. Each subclass's revision is stored before its superclass
-- chain's data, and its extra fields after, mirroring the game's
-- PreLoad/PostLoad call order.
--------------------------------------------------------------------------------

data DirClassInfo
  = InfoObjectDir
  | InfoRndDir      RndDirInfo
  | InfoPanelDir    PanelDirInfo
  | InfoWorldDir    WorldDirInfo
  | InfoCharacter   CharacterInfo
  | InfoCharClipSet CharClipSetInfo
  | InfoCharBoneDir CharBoneDirInfo
  | InfoUIListDir   UIListDirInfo
  | InfoUILabelDir  UILabelDirInfo
  deriving (Show)

getMaybe :: Bool -> Get a -> Get (Maybe a)
getMaybe b g = if b then Just <$> g else return Nothing

-- | RndAnimatable::Load: animation frame state
data AnimInfo = AnimInfo
  { animVersion :: Word32 -- ^ 4 in RB3
  , animFrame   :: Maybe Float  -- ^ version >= 2
  , animRate    :: Maybe Word32 -- ^ version >= 4; 0 = 30fps
  } deriving (Show)

parseAnimInfo :: Get AnimInfo
parseAnimInfo = do
  animVersion <- getWord32be
  when (animVersion < 1 || animVersion > 4) $ fail $
    "Unsupported RndAnimatable version " <> show animVersion
  animFrame <- getMaybe (animVersion > 1) getFloatbe
  animRate <- getMaybe (animVersion > 3) getWord32be
  return AnimInfo{..}

putAnimInfo :: AnimInfo -> Put
putAnimInfo AnimInfo{..} = do
  putWord32be animVersion
  mapM_ putFloatbe animFrame
  mapM_ putWord32be animRate

-- | RndDrawable::Load: showing flag, bounding sphere, draw order
data DrawInfo = DrawInfo
  { drawVersion :: Word32 -- ^ 3 in RB3
  , drawShowing :: Word8
  , drawSphere  :: [Float] -- ^ 4 floats: center + radius
  , drawOrder   :: Maybe Float -- ^ version >= 3
  } deriving (Show)

parseDrawInfo :: Get DrawInfo
parseDrawInfo = do
  drawVersion <- getWord32be
  when (drawVersion < 2 || drawVersion > 3) $ fail $
    "Unsupported RndDrawable version " <> show drawVersion
  drawShowing <- getWord8
  drawSphere <- replicateM 4 getFloatbe
  drawOrder <- getMaybe (drawVersion > 2) getFloatbe
  return DrawInfo{..}

putDrawInfo :: DrawInfo -> Put
putDrawInfo DrawInfo{..} = do
  putWord32be drawVersion
  putWord8 drawShowing
  mapM_ putFloatbe drawSphere
  mapM_ putFloatbe drawOrder

-- | RndTransformable::Load: local/world transforms and trans parent
data TransInfo = TransInfo
  { transVersion       :: Word32 -- ^ 9 in RB3 (only 9 supported)
  , transLocal         :: [Float] -- ^ 12 floats
  , transWorld         :: [Float] -- ^ 12 floats
  , transConstraint    :: Word32
  , transTarget        :: B.ByteString
  , transPreserveScale :: Word8
  , transParent        :: B.ByteString
  } deriving (Show)

parseTransInfo :: Get TransInfo
parseTransInfo = do
  transVersion <- getWord32be
  when (transVersion /= 9) $ fail $
    "Unsupported RndTransformable version " <> show transVersion
  transLocal <- replicateM 12 getFloatbe
  transWorld <- replicateM 12 getFloatbe
  transConstraint <- getWord32be
  transTarget <- getStringBE
  transPreserveScale <- getWord8
  transParent <- getStringBE
  return TransInfo{..}

putTransInfo :: TransInfo -> Put
putTransInfo TransInfo{..} = do
  putWord32be transVersion
  mapM_ putFloatbe transLocal
  mapM_ putFloatbe transWorld
  putWord32be transConstraint
  putStringBE transTarget
  putWord8 transPreserveScale
  putStringBE transParent

-- | RndDir extras (RndDir::PostLoad after ObjectDir::PostLoad)
data RndDirInfo = RndDirInfo
  { rndVersion   :: Word32 -- ^ 10 in RB3
  , rndAnim      :: AnimInfo
  , rndDraw      :: DrawInfo
  , rndTrans     :: Maybe TransInfo -- ^ version >= 1
  , rndEnviron   :: Maybe B.ByteString -- ^ version >= 2
  , rndTestEvent :: Maybe B.ByteString -- ^ version >= 3 except 9
  } deriving (Show)

parseRndDirExtras :: Word32 -> Get RndDirInfo
parseRndDirExtras rndVersion = do
  when (rndVersion > 10) $ fail $
    "Unsupported RndDir version " <> show rndVersion
  rndAnim <- parseAnimInfo
  rndDraw <- parseDrawInfo
  rndTrans <- getMaybe (rndVersion /= 0) parseTransInfo
  rndEnviron <- getMaybe (rndVersion > 1) getStringBE
  rndTestEvent <- getMaybe (rndVersion > 2 && rndVersion /= 9) getStringBE
  return RndDirInfo{..}

putRndDirExtras :: RndDirInfo -> Put
putRndDirExtras RndDirInfo{..} = do
  putAnimInfo rndAnim
  putDrawInfo rndDraw
  mapM_ putTransInfo rndTrans
  mapM_ putStringBE rndEnviron
  mapM_ putStringBE rndTestEvent

-- | PanelDir extras (PanelDir::PostLoad after RndDir::PostLoad)
data PanelDirInfo = PanelDirInfo
  { panelVersion            :: Word32 -- ^ 8 in RB3
  , panelRnd                :: RndDirInfo
  , panelCam                :: Maybe B.ByteString -- ^ version >= 1
  , panelRev2Symbol         :: Maybe B.ByteString -- ^ version == 2 only
  , panelCanEndWorld        :: Maybe Word8 -- ^ version >= 4
  , panelBackFilenames      :: Maybe [B.ByteString] -- ^ version >= 5
  , panelFrontFilenames     :: Maybe [B.ByteString] -- ^ version >= 5
  , panelShowEditModePanels :: Maybe Word8 -- ^ version >= 6
  , panelUseSpecifiedCam    :: Maybe Word8 -- ^ version >= 8
  } deriving (Show)

parsePanelExtras :: Bool -> Word32 -> RndDirInfo -> Get PanelDirInfo
parsePanelExtras isProxy panelVersion panelRnd = do
  when (panelVersion > 8) $ fail $
    "Unsupported PanelDir version " <> show panelVersion
  panelCam <- getMaybe (panelVersion /= 0 && not isProxy) getStringBE
  panelRev2Symbol <- getMaybe (panelVersion == 2 && not isProxy) getStringBE
  panelCanEndWorld <- getMaybe (panelVersion > 3) getWord8
  panelBackFilenames <- getMaybe (panelVersion > 4) $ getArrayBE getStringBE
  panelFrontFilenames <- getMaybe (panelVersion > 4) $ getArrayBE getStringBE
  panelShowEditModePanels <- getMaybe (panelVersion > 5) getWord8
  panelUseSpecifiedCam <- getMaybe (panelVersion > 7) getWord8
  return PanelDirInfo{..}

putPanelExtras :: PanelDirInfo -> Put
putPanelExtras PanelDirInfo{..} = do
  mapM_ putStringBE panelCam
  mapM_ putStringBE panelRev2Symbol
  mapM_ putWord8 panelCanEndWorld
  mapM_ (putArrayBE putStringBE) panelBackFilenames
  mapM_ (putArrayBE putStringBE) panelFrontFilenames
  mapM_ putWord8 panelShowEditModePanels
  mapM_ putWord8 panelUseSpecifiedCam

-- | WorldDir: venue/world files. Pre fields go before PanelDir's data,
-- extras after (WorldDir::PreLoad / PostLoad).
data WorldDirInfo = WorldDirInfo
  { worldVersion          :: Word32 -- ^ we support 13-25
  , worldPreInts          :: Maybe (Word32, Word32) -- ^ version <= 20
  , worldFakeHudFilename  :: Maybe B.ByteString -- ^ version >= 10
  , worldPanel            :: PanelDirInfo
  , worldUnknownXfm       :: Maybe [Float] -- ^ version 11-24: 12 floats
  , worldHideOverrides    :: [B.ByteString]
  , worldBitmapOverrides  :: [(B.ByteString, B.ByteString)] -- ^ (original tex, replacement tex)
  , worldMatOverrides     :: Maybe [(B.ByteString, B.ByteString)] -- ^ version >= 14: (mesh, mat)
  , worldPresetOverrides  :: Maybe [(B.ByteString, B.ByteString)] -- ^ version >= 15: (preset, hue)
  , worldCamShotOverrides :: Maybe [B.ByteString] -- ^ version >= 16
  , worldPS3PerPixelHides :: Maybe [B.ByteString] -- ^ version >= 17 except 23
  , worldPS3PerPixelShows :: Maybe [B.ByteString] -- ^ version >= 17 except 23
  , worldMidSymbol        :: Maybe B.ByteString -- ^ version 18-21 only
  , worldTestPreset1      :: Maybe B.ByteString -- ^ version >= 19
  , worldTestPreset2      :: Maybe B.ByteString -- ^ version >= 19
  , worldTestAnimTime     :: Maybe Float -- ^ version >= 19
  , worldHud              :: Maybe B.ByteString -- ^ version >= 20
  } deriving (Show)

parseWorldPre :: Get (Word32, Maybe (Word32, Word32), Maybe B.ByteString)
parseWorldPre = do
  worldVersion <- getWord32be
  when (worldVersion < 13 || worldVersion > 25) $ fail $
    "Unsupported WorldDir version " <> show worldVersion
  worldPreInts <- getMaybe (worldVersion <= 20)
    $ liftA2 (,) getWord32be getWord32be
  worldFakeHudFilename <- getMaybe (worldVersion > 9) getStringBE
  return (worldVersion, worldPreInts, worldFakeHudFilename)

parseWorldExtras
  :: (Word32, Maybe (Word32, Word32), Maybe B.ByteString)
  -> PanelDirInfo -> Get WorldDirInfo
parseWorldExtras (worldVersion, worldPreInts, worldFakeHudFilename) worldPanel = do
  worldUnknownXfm <- getMaybe (worldVersion > 10 && worldVersion < 25)
    $ replicateM 12 getFloatbe
  worldHideOverrides <- getArrayBE getStringBE
  worldBitmapOverrides <- getArrayBE $ liftA2 (,) getStringBE getStringBE
  worldMatOverrides <- getMaybe (worldVersion > 13)
    $ getArrayBE $ liftA2 (,) getStringBE getStringBE
  worldPresetOverrides <- getMaybe (worldVersion > 14)
    $ getArrayBE $ liftA2 (,) getStringBE getStringBE
  worldCamShotOverrides <- getMaybe (worldVersion > 15) $ getArrayBE getStringBE
  worldPS3PerPixelHides <- getMaybe (worldVersion > 16 && worldVersion /= 23)
    $ getArrayBE getStringBE
  worldPS3PerPixelShows <- getMaybe (worldVersion > 16 && worldVersion /= 23)
    $ getArrayBE getStringBE
  worldMidSymbol <- getMaybe (worldVersion >= 18 && worldVersion <= 21) getStringBE
  worldTestPreset1 <- getMaybe (worldVersion > 18) getStringBE
  worldTestPreset2 <- getMaybe (worldVersion > 18) getStringBE
  worldTestAnimTime <- getMaybe (worldVersion > 18) getFloatbe
  worldHud <- getMaybe (worldVersion > 19) getStringBE
  return WorldDirInfo{..}

putWorldPre :: WorldDirInfo -> Put
putWorldPre WorldDirInfo{..} = do
  putWord32be worldVersion
  forM_ worldPreInts \(x, y) -> putWord32be x >> putWord32be y
  mapM_ putStringBE worldFakeHudFilename

putWorldExtras :: WorldDirInfo -> Put
putWorldExtras WorldDirInfo{..} = do
  mapM_ (mapM_ putFloatbe) worldUnknownXfm
  putArrayBE putStringBE worldHideOverrides
  putArrayBE (\(x, y) -> putStringBE x >> putStringBE y) worldBitmapOverrides
  mapM_ (putArrayBE \(x, y) -> putStringBE x >> putStringBE y) worldMatOverrides
  mapM_ (putArrayBE \(x, y) -> putStringBE x >> putStringBE y) worldPresetOverrides
  mapM_ (putArrayBE putStringBE) worldCamShotOverrides
  mapM_ (putArrayBE putStringBE) worldPS3PerPixelHides
  mapM_ (putArrayBE putStringBE) worldPS3PerPixelShows
  mapM_ putStringBE worldMidSymbol
  mapM_ putStringBE worldTestPreset1
  mapM_ putStringBE worldTestPreset2
  mapM_ putFloatbe worldTestAnimTime
  mapM_ putStringBE worldHud

-- | Character: player/crowd character dirs
data CharacterInfo = CharacterInfo
  { charVersion    :: Word32 -- ^ we support 9-17 (17 in RB3)
  , charRnd        :: RndDirInfo
  , charStandalone :: Maybe CharacterExtras
  -- ^ present when the dir is not a proxy
  , charProxyTest  :: Maybe CharacterTestInfo
  -- ^ present when the dir is a proxy and version >= 16
  } deriving (Show)

data CharacterExtras = CharacterExtras
  { charLods       :: [(Float, B.ByteString, Maybe B.ByteString)]
  -- ^ (screen size, group, trans group [version >= 14])
  , charShadow     :: B.ByteString
  , charSelfShadow :: Maybe Word8 -- ^ version >= 3
  , charSphereBase :: Maybe B.ByteString -- ^ version >= 5
  , charBounding   :: Maybe [Float] -- ^ version >= 11: bounding sphere, 4 floats
  , charFrozen     :: Maybe Word8 -- ^ version >= 13
  , charMinLod     :: Maybe Word32 -- ^ version >= 15
  , charTransGroup :: Maybe B.ByteString -- ^ version >= 17
  , charTest       :: Maybe CharacterTestInfo -- ^ version >= 10
  } deriving (Show)

parseCharacterExtras :: Bool -> Word32 -> RndDirInfo -> Get CharacterInfo
parseCharacterExtras isProxy charVersion charRnd = do
  when (charVersion < 9 || charVersion > 17) $ fail $
    "Unsupported Character version " <> show charVersion
  charStandalone <- getMaybe (not isProxy) do
    charLods <- getArrayBE do
      screenSize <- getFloatbe
      grp <- getStringBE
      transGrp <- getMaybe (charVersion > 13) getStringBE
      return (screenSize, grp, transGrp)
    charShadow <- getStringBE
    charSelfShadow <- getMaybe (charVersion > 2) getWord8
    charSphereBase <- getMaybe (charVersion > 4) getStringBE
    charBounding <- getMaybe (charVersion > 10) $ replicateM 4 getFloatbe
    charFrozen <- getMaybe (charVersion > 12) getWord8
    charMinLod <- getMaybe (charVersion > 14) getWord32be
    charTransGroup <- getMaybe (charVersion > 16) getStringBE
    charTest <- getMaybe (charVersion > 9) parseCharacterTest
    return CharacterExtras{..}
  charProxyTest <- getMaybe (isProxy && charVersion > 15) parseCharacterTest
  return CharacterInfo{..}

putCharacterExtras :: CharacterInfo -> Put
putCharacterExtras char = do
  forM_ char.charStandalone \CharacterExtras{..} -> do
    putArrayBE
      (\(ss, grp, transGrp) -> putFloatbe ss >> putStringBE grp >> mapM_ putStringBE transGrp)
      charLods
    putStringBE charShadow
    mapM_ putWord8 charSelfShadow
    mapM_ putStringBE charSphereBase
    mapM_ (mapM_ putFloatbe) charBounding
    mapM_ putWord8 charFrozen
    mapM_ putWord32be charMinLod
    mapM_ putStringBE charTransGroup
    mapM_ putCharacterTest charTest
  mapM_ putCharacterTest char.charProxyTest

-- | CharacterTest: test/preview state saved with characters
-- (CharacterTest::Load)
data CharacterTestInfo = CharacterTestInfo
  { ctVersion         :: Word32 -- ^ we support 11-15 (15 in RB3)
  , ctDriver          :: Maybe B.ByteString -- ^ absent in version 13 only
  , ctClip1           :: B.ByteString
  , ctClip2           :: B.ByteString
  , ctTeleportTo      :: B.ByteString
  , ctWalkPath        :: [B.ByteString]
  , ctShowDistMap     :: B.ByteString
  , ctTransition      :: Word32
  , ctCycleTransition :: Word8
  , ctUnknownInt      :: Word32
  , ctMetronome       :: Word8
  , ctZeroTravel      :: Word8
  , ctShowScreenSize  :: Word8
  , ctRev11Symbol     :: Maybe B.ByteString -- ^ version <= 11
  , ctShowFootExtents :: Word8
  , ctOldBoolInt      :: Maybe (Word8, Word32) -- ^ version <= 14
  } deriving (Show)

parseCharacterTest :: Get CharacterTestInfo
parseCharacterTest = do
  ctVersion <- getWord32be
  when (ctVersion < 11 || ctVersion > 15) $ fail $
    "Unsupported CharacterTest version " <> show ctVersion
  ctDriver <- getMaybe (ctVersion /= 13) getStringBE
  ctClip1 <- getStringBE
  ctClip2 <- getStringBE
  ctTeleportTo <- getStringBE
  ctWalkPath <- getArrayBE getStringBE
  ctShowDistMap <- getStringBE
  ctTransition <- getWord32be
  ctCycleTransition <- getWord8
  ctUnknownInt <- getWord32be
  ctMetronome <- getWord8
  ctZeroTravel <- getWord8
  ctShowScreenSize <- getWord8
  ctRev11Symbol <- getMaybe (ctVersion < 12) getStringBE
  ctShowFootExtents <- getWord8
  ctOldBoolInt <- getMaybe (ctVersion < 15) $ liftA2 (,) getWord8 getWord32be
  return CharacterTestInfo{..}

putCharacterTest :: CharacterTestInfo -> Put
putCharacterTest CharacterTestInfo{..} = do
  putWord32be ctVersion
  mapM_ putStringBE ctDriver
  putStringBE ctClip1
  putStringBE ctClip2
  putStringBE ctTeleportTo
  putArrayBE putStringBE ctWalkPath
  putStringBE ctShowDistMap
  putWord32be ctTransition
  putWord8 ctCycleTransition
  putWord32be ctUnknownInt
  putWord8 ctMetronome
  putWord8 ctZeroTravel
  putWord8 ctShowScreenSize
  mapM_ putStringBE ctRev11Symbol
  putWord8 ctShowFootExtents
  forM_ ctOldBoolInt \(b, i) -> putWord8 b >> putWord32be i

-- | CharBoneDir extras (CharBoneDir::PostLoad)
data CharBoneDirInfo = CharBoneDirInfo
  { cbVersion       :: Word32 -- ^ we support 2-4 (4 in RB3)
  , cbMoveContext   :: Word32
  , cbTargets       :: [B.ByteString]
  , cbAverage       :: [B.ByteString]
  , cbSlide         :: Word8
  , cbBakeOutFacing :: Maybe Word8 -- ^ version >= 4
  } deriving (Show)

parseCharBoneDirExtras :: Word32 -> Get CharBoneDirInfo
parseCharBoneDirExtras cbVersion = do
  when (cbVersion < 2 || cbVersion > 4) $ fail $
    "Unsupported CharBoneDir version " <> show cbVersion
  cbMoveContext <- getWord32be
  cbTargets <- getArrayBE getStringBE
  cbAverage <- getArrayBE getStringBE
  cbSlide <- getWord8
  cbBakeOutFacing <- getMaybe (cbVersion > 3) getWord8
  return CharBoneDirInfo{..}

putCharBoneDirExtras :: CharBoneDirInfo -> Put
putCharBoneDirExtras CharBoneDirInfo{..} = do
  putWord32be cbMoveContext
  putArrayBE putStringBE cbTargets
  putArrayBE putStringBE cbAverage
  putWord8 cbSlide
  mapM_ putWord8 cbBakeOutFacing

-- | UIListDir extras (UIListDir::PostLoad)
data UIListDirInfo = UIListDirInfo
  { ulVersion               :: Word32 -- ^ 1 in RB3
  , ulRnd                   :: RndDirInfo
  , ulOrientation           :: Word32
  , ulFadeOffset            :: Word32
  , ulTestMode              :: Word8
  , ulNumDisplay            :: Word32
  , ulElementSpacing        :: Float
  , ulSpeed                 :: Float
  , ulTestNumData           :: Word32
  , ulComponentState        :: Word32
  , ulTestGapSize           :: Float
  , ulTestDisableElements   :: Word8
  , ulScrollHighlightChange :: Maybe Float -- ^ version >= 1
  } deriving (Show)

parseUIListExtras :: Word32 -> RndDirInfo -> Get UIListDirInfo
parseUIListExtras ulVersion ulRnd = do
  when (ulVersion > 1) $ fail $
    "Unsupported UIListDir version " <> show ulVersion
  ulOrientation <- getWord32be
  ulFadeOffset <- getWord32be
  ulTestMode <- getWord8
  ulNumDisplay <- getWord32be
  ulElementSpacing <- getFloatbe
  ulSpeed <- getFloatbe
  ulTestNumData <- getWord32be
  ulComponentState <- getWord32be
  ulTestGapSize <- getFloatbe
  ulTestDisableElements <- getWord8
  ulScrollHighlightChange <- getMaybe (ulVersion /= 0) getFloatbe
  return UIListDirInfo{..}

putUIListExtras :: UIListDirInfo -> Put
putUIListExtras UIListDirInfo{..} = do
  putWord32be ulOrientation
  putWord32be ulFadeOffset
  putWord8 ulTestMode
  putWord32be ulNumDisplay
  putFloatbe ulElementSpacing
  putFloatbe ulSpeed
  putWord32be ulTestNumData
  putWord32be ulComponentState
  putFloatbe ulTestGapSize
  putWord8 ulTestDisableElements
  mapM_ putFloatbe ulScrollHighlightChange

-- | UILabelDir extras (UILabelDir::PostLoad)
data UILabelDirInfo = UILabelDirInfo
  { uldVersion                  :: Word32 -- ^ 9 in RB3
  , uldRnd                      :: RndDirInfo
  , uldTextObj                  :: B.ByteString
  , uldOldFont                  :: Maybe B.ByteString -- ^ version 3-8
  , uldFocusAnim                :: Maybe B.ByteString -- ^ version >= 1
  , uldPulseAnim                :: Maybe B.ByteString -- ^ version >= 2
  , uldHighlightGroup           :: Maybe B.ByteString -- ^ version >= 4
  , uldTopLeftBone              :: Maybe B.ByteString -- ^ version >= 4
  , uldTopRightBone             :: Maybe B.ByteString -- ^ version >= 4
  , uldBottomLeftBone           :: Maybe B.ByteString -- ^ version >= 5
  , uldBottomRightBone          :: Maybe B.ByteString -- ^ version >= 5
  , uldFocusedBackgroundGroup   :: Maybe B.ByteString -- ^ version >= 6
  , uldUnfocusedBackgroundGroup :: Maybe B.ByteString -- ^ version >= 6
  , uldAllowEditText            :: Maybe Word8 -- ^ version >= 7
  , uldDefaultColor             :: B.ByteString
  , uldColors                   :: [B.ByteString] -- ^ one per UIComponent state (5)
  , uldFontImporter             :: Maybe UIFontImporterInfo -- ^ version >= 8
  } deriving (Show)

parseUILabelExtras :: Word32 -> RndDirInfo -> Get UILabelDirInfo
parseUILabelExtras uldVersion uldRnd = do
  when (uldVersion > 9) $ fail $
    "Unsupported UILabelDir version " <> show uldVersion
  uldTextObj <- getStringBE
  uldOldFont <- getMaybe (uldVersion >= 3 && uldVersion <= 8) getStringBE
  uldFocusAnim <- getMaybe (uldVersion >= 1) getStringBE
  uldPulseAnim <- getMaybe (uldVersion >= 2) getStringBE
  uldHighlightGroup <- getMaybe (uldVersion >= 4) getStringBE
  uldTopLeftBone <- getMaybe (uldVersion >= 4) getStringBE
  uldTopRightBone <- getMaybe (uldVersion >= 4) getStringBE
  uldBottomLeftBone <- getMaybe (uldVersion >= 5) getStringBE
  uldBottomRightBone <- getMaybe (uldVersion >= 5) getStringBE
  uldFocusedBackgroundGroup <- getMaybe (uldVersion >= 6) getStringBE
  uldUnfocusedBackgroundGroup <- getMaybe (uldVersion >= 6) getStringBE
  uldAllowEditText <- getMaybe (uldVersion >= 7) getWord8
  uldDefaultColor <- getStringBE
  uldColors <- replicateM 5 getStringBE
  uldFontImporter <- getMaybe (uldVersion >= 8) parseUIFontImporter
  return UILabelDirInfo{..}

putUILabelExtras :: UILabelDirInfo -> Put
putUILabelExtras UILabelDirInfo{..} = do
  putStringBE uldTextObj
  mapM_ putStringBE uldOldFont
  mapM_ putStringBE uldFocusAnim
  mapM_ putStringBE uldPulseAnim
  mapM_ putStringBE uldHighlightGroup
  mapM_ putStringBE uldTopLeftBone
  mapM_ putStringBE uldTopRightBone
  mapM_ putStringBE uldBottomLeftBone
  mapM_ putStringBE uldBottomRightBone
  mapM_ putStringBE uldFocusedBackgroundGroup
  mapM_ putStringBE uldUnfocusedBackgroundGroup
  mapM_ putWord8 uldAllowEditText
  putStringBE uldDefaultColor
  mapM_ putStringBE uldColors
  mapM_ putUIFontImporter uldFontImporter

-- | UIFontImporter::Load, embedded in UILabelDir
data UIFontImporterInfo = UIFontImporterInfo
  { fiVersion           :: Word32 -- ^ we support 5-9 (9 in RB3)
  , fiIncludeFlags      :: [Word8] -- ^ 6 bools: upper, lower, numbers,
                                  -- punctuation, upper euro, lower euro
  , fiPlusChars         :: B.ByteString
  , fiMinusChars        :: B.ByteString
  , fiFontName          :: B.ByteString
  , fiFontPctSize       :: Float
  , fiFontWeight        :: Word32
  , fiItalics           :: Word8
  , fiPitchAndFamily    :: Word32
  , fiFontQuality       :: Word32
  , fiFontCharset       :: Word32
  , fiFontSupersample   :: Word32
  , fiBitmapSavePath    :: B.ByteString
  , fiBitmapSaveName    :: B.ByteString
  , fiPadding           :: [Word32] -- ^ left, right, top, bottom
  , fiFillWithSafeWhite :: Word8
  , fiFontToImportFrom  :: Maybe B.ByteString -- ^ version <= 7
  , fiGennedFonts       :: [B.ByteString]
  , fiReferenceKerning  :: B.ByteString
  , fiMatVariations     :: [B.ByteString]
  , fiDefaultMat        :: Maybe B.ByteString -- ^ version >= 6
  , fiHandmadeFont      :: Maybe B.ByteString -- ^ version >= 7
  , fiSyncResource      :: Maybe B.ByteString -- ^ version >= 8
  , fiLastGenWasNG      :: Maybe Word8 -- ^ version >= 9
  } deriving (Show)

parseUIFontImporter :: Get UIFontImporterInfo
parseUIFontImporter = do
  fiVersion <- getWord32be
  when (fiVersion < 5 || fiVersion > 9) $ fail $
    "Unsupported UIFontImporter version " <> show fiVersion
  fiIncludeFlags <- replicateM 6 getWord8
  fiPlusChars <- getStringBE
  fiMinusChars <- getStringBE
  fiFontName <- getStringBE
  fiFontPctSize <- getFloatbe
  fiFontWeight <- getWord32be
  fiItalics <- getWord8
  fiPitchAndFamily <- getWord32be
  fiFontQuality <- getWord32be
  fiFontCharset <- getWord32be
  fiFontSupersample <- getWord32be
  fiBitmapSavePath <- getStringBE
  fiBitmapSaveName <- getStringBE
  fiPadding <- replicateM 4 getWord32be
  fiFillWithSafeWhite <- getWord8
  fiFontToImportFrom <- getMaybe (fiVersion < 8) getStringBE
  fiGennedFonts <- getArrayBE getStringBE
  fiReferenceKerning <- getStringBE
  fiMatVariations <- getArrayBE getStringBE
  fiDefaultMat <- getMaybe (fiVersion > 5) getStringBE
  fiHandmadeFont <- getMaybe (fiVersion > 6) getStringBE
  fiSyncResource <- getMaybe (fiVersion > 7) getStringBE
  fiLastGenWasNG <- getMaybe (fiVersion > 8) getWord8
  return UIFontImporterInfo{..}

putUIFontImporter :: UIFontImporterInfo -> Put
putUIFontImporter UIFontImporterInfo{..} = do
  putWord32be fiVersion
  mapM_ putWord8 fiIncludeFlags
  putStringBE fiPlusChars
  putStringBE fiMinusChars
  putStringBE fiFontName
  putFloatbe fiFontPctSize
  putWord32be fiFontWeight
  putWord8 fiItalics
  putWord32be fiPitchAndFamily
  putWord32be fiFontQuality
  putWord32be fiFontCharset
  putWord32be fiFontSupersample
  putStringBE fiBitmapSavePath
  putStringBE fiBitmapSaveName
  mapM_ putWord32be fiPadding
  putWord8 fiFillWithSafeWhite
  mapM_ putStringBE fiFontToImportFrom
  putArrayBE putStringBE fiGennedFonts
  putStringBE fiReferenceKerning
  putArrayBE putStringBE fiMatVariations
  mapM_ putStringBE fiDefaultMat
  mapM_ putStringBE fiHandmadeFont
  mapM_ putStringBE fiSyncResource
  mapM_ putWord8 fiLastGenWasNG

-- | CharClipSet extras (end of CharClipSet::PostLoad).
-- Absent when the dir is a proxy (an entry inside another dir).
data CharClipSetInfo = CharClipSetInfo
  { ccsVersion :: Word32 -- ^ only 24 supported (RB3)
  , ccsExtras  :: Maybe CharClipSetExtras
  } deriving (Show)

data CharClipSetExtras = CharClipSetExtras
  { ccsCharFilePath :: B.ByteString -- ^ character milo to preview clips on
  , ccsPreviewClip  :: B.ByteString
  , ccsFilterFlags  :: Word32
  , ccsBpm          :: Word32
  , ccsPreviewWalk  :: Word8
  , ccsStillClip    :: B.ByteString
  } deriving (Show)

parseCharClipSetExtras :: Bool -> Word32 -> Get CharClipSetInfo
parseCharClipSetExtras isProxy ccsVersion = do
  when (ccsVersion /= 24) $ fail $
    "Unsupported CharClipSet version " <> show ccsVersion
  ccsExtras <- getMaybe (not isProxy) do
    ccsCharFilePath <- getStringBE
    ccsPreviewClip <- getStringBE
    ccsFilterFlags <- getWord32be
    ccsBpm <- getWord32be
    ccsPreviewWalk <- getWord8
    ccsStillClip <- getStringBE
    return CharClipSetExtras{..}
  return CharClipSetInfo{..}

putCharClipSetExtras :: CharClipSetInfo -> Put
putCharClipSetExtras ccs = forM_ ccs.ccsExtras \CharClipSetExtras{..} -> do
  putStringBE ccsCharFilePath
  putStringBE ccsPreviewClip
  putWord32be ccsFilterFlags
  putWord32be ccsBpm
  putWord8 ccsPreviewWalk
  putStringBE ccsStillClip

-- | Dir classes whose object entries are serialized as nested dir data
-- rather than ordinary object data.
dirClasses :: [B.ByteString]
dirClasses =
  [ "ObjectDir", "RndDir", "PanelDir", "WorldDir", "Character"
  , "CharClipSet", "CharBoneDir", "UIListDir", "UILabelDir"
  ]

-- | The full dir body for a given dir class: subclass revisions, the shared
-- ObjectDir data, then subclass extras. The Bool is True when this dir is a
-- proxy (a dir-class object stored as an entry of another dir), which omits
-- some extras.
parseDirBody :: Bool -> B.ByteString -> Get (ObjectDirInfo, DirClassInfo)
parseDirBody isProxy = \case
  "ObjectDir" -> do
    core <- parseObjectDir
    return (core, InfoObjectDir)
  "RndDir" -> do
    rndRev <- getWord32be
    core <- parseObjectDir
    rnd <- parseRndDirExtras rndRev
    return (core, InfoRndDir rnd)
  "PanelDir" -> do
    panelRev <- getWord32be
    rndRev <- getWord32be
    core <- parseObjectDir
    rnd <- parseRndDirExtras rndRev
    panel <- parsePanelExtras isProxy panelRev rnd
    return (core, InfoPanelDir panel)
  "WorldDir" -> do
    worldPre <- parseWorldPre
    panelRev <- getWord32be
    rndRev <- getWord32be
    core <- parseObjectDir
    rnd <- parseRndDirExtras rndRev
    panel <- parsePanelExtras isProxy panelRev rnd
    world <- parseWorldExtras worldPre panel
    return (core, InfoWorldDir world)
  "Character" -> do
    charRev <- getWord32be
    rndRev <- getWord32be
    core <- parseObjectDir
    rnd <- parseRndDirExtras rndRev
    char <- parseCharacterExtras isProxy charRev rnd
    return (core, InfoCharacter char)
  "CharClipSet" -> do
    ccsRev <- getWord32be
    core <- parseObjectDir
    ccs <- parseCharClipSetExtras isProxy ccsRev
    return (core, InfoCharClipSet ccs)
  "CharBoneDir" -> do
    cbRev <- getWord32be
    core <- parseObjectDir
    cb <- parseCharBoneDirExtras cbRev
    return (core, InfoCharBoneDir cb)
  "UIListDir" -> do
    ulRev <- getWord32be
    rndRev <- getWord32be
    core <- parseObjectDir
    rnd <- parseRndDirExtras rndRev
    ul <- parseUIListExtras ulRev rnd
    return (core, InfoUIListDir ul)
  "UILabelDir" -> do
    uldRev <- getWord32be
    rndRev <- getWord32be
    core <- parseObjectDir
    rnd <- parseRndDirExtras rndRev
    uld <- parseUILabelExtras uldRev rnd
    return (core, InfoUILabelDir uld)
  cls -> fail $ "Can't parse dir data for class " <> show cls <> " yet"

putDirBody :: ObjectDirInfo -> DirClassInfo -> Put
putDirBody core = \case
  InfoObjectDir -> putObjectDir core
  InfoRndDir rnd -> do
    putWord32be rnd.rndVersion
    putObjectDir core
    putRndDirExtras rnd
  InfoPanelDir panel -> do
    putWord32be panel.panelVersion
    putWord32be panel.panelRnd.rndVersion
    putObjectDir core
    putRndDirExtras panel.panelRnd
    putPanelExtras panel
  InfoWorldDir world -> do
    putWorldPre world
    putWord32be world.worldPanel.panelVersion
    putWord32be world.worldPanel.panelRnd.rndVersion
    putObjectDir core
    putRndDirExtras world.worldPanel.panelRnd
    putPanelExtras world.worldPanel
    putWorldExtras world
  InfoCharacter char -> do
    putWord32be char.charVersion
    putWord32be char.charRnd.rndVersion
    putObjectDir core
    putRndDirExtras char.charRnd
    putCharacterExtras char
  InfoCharClipSet ccs -> do
    putWord32be ccs.ccsVersion
    putObjectDir core
    putCharClipSetExtras ccs
  InfoCharBoneDir cb -> do
    putWord32be cb.cbVersion
    putObjectDir core
    putCharBoneDirExtras cb
  InfoUIListDir ul -> do
    putWord32be ul.ulVersion
    putWord32be ul.ulRnd.rndVersion
    putObjectDir core
    putRndDirExtras ul.ulRnd
    putUIListExtras ul
  InfoUILabelDir uld -> do
    putWord32be uld.uldVersion
    putWord32be uld.uldRnd.rndVersion
    putObjectDir core
    putRndDirExtras uld.uldRnd
    putUILabelExtras uld

-- | Parse one entry's data. Entries of dir classes are measured
-- structurally, since their data can contain ADDE barriers of its own;
-- everything else (and any dir entry we fail to understand) is simply
-- split on the next barrier.
parseEntry :: B.ByteString -> Get MiloEntry
parseEntry cls = if elem cls dirClasses
  then parseDirEntry cls <|> plainEntry
  else plainEntry
  where plainEntry = flip MiloEntry Nothing <$> parseFileADDE

-- | A dir-class object stored as an entry of another dir: its dir data in
-- proxy form, the barrier, then (when it has an inlined proxy) a full
-- embedded dir stream.
parseDirEntry :: B.ByteString -> Get MiloEntry
parseDirEntry cls = do
  (bodyLen, hasInline) <- lookAhead do
    before <- bytesRead
    (core, _) <- parseDirBody True cls
    after <- bytesRead
    -- ObjectDir::PostLoad loads an inlined proxy when the dir has a proxy
    -- file and allows inlining (InlineProxy: mInlineProxy && bs.Cached())
    return
      ( after - before
      , not (B.null core.odProxyPath) && core.odInlineProxy /= Just 0
      )
  body <- getLazyByteString bodyLen
  magic <- getByteString 4
  unless (magic == B.pack magicBarrier) $ fail
    "Dir-class entry data not followed by ADDE barrier"
  inlineDir <- getMaybe hasInline $ codecIn bin
  return $ MiloEntry body inlineDir

-- | All inlined subdir contents, in order.
miloSubdirs :: MiloDir -> [MiloDir]
miloSubdirs dir = catMaybes $ map (.sdDir) dir.miloObjectDir.odSubdirs

-- | The embedded dir streams of any dir-class entries, in order.
miloInlineDirs :: MiloDir -> [MiloDir]
miloInlineDirs dir = catMaybes $ map (.entryInlineDir) dir.miloFiles

parseMiloFile :: Get MiloDir
parseMiloFile = do
  dir <- codecIn bin
  eof <- isEmpty
  if eof
    then return dir
    else fail "Didn't parse entire .milo file"

makeMiloFile :: MiloDir -> BL.ByteString
makeMiloFile = runPut . void . codecOut bin
