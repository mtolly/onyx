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
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Onyx.Harmonix.RockBand.Milo.Dir where

import           Control.Monad                           (forM, forM_,
                                                          replicateM, unless,
                                                          void, when)
import qualified Data.ByteString                         as B
import qualified Data.ByteString.Lazy                    as BL
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
  { version       :: Word32
  -- ^ DirLoader revision. Observed:
  -- - 25 for all song milos before RB3 (RB1/RB2/Lego/Beatles/GDRB)
  -- - 28 for RB3 (songs and on-disc content)
  , type_         :: B.ByteString
  -- ^ Class name of the directory: "ObjectDir" for song milos.
  -- On-disc content also uses "RndDir", "PanelDir", "WorldDir", "Character",
  -- "CharClipSet" and others, whose extra dir data we can't parse yet.
  , name          :: B.ByteString
  , hashReserve   :: Word32
  -- ^ Extra hash table slots to reserve when loading; roughly tracks the
  -- number of objects. (DirLoader::LoadHeader "size1")
  , stringReserve :: Word32
  -- ^ Extra string table bytes to reserve when loading; roughly tracks the
  -- total length of object names. (DirLoader::LoadHeader "size2")
  , entryNames    :: [(B.ByteString, B.ByteString)]
  -- ^ (class, name) for each object in the dir, in the same order as files
  , extResources  :: Maybe [B.ByteString]
  -- ^ External resource paths, only in version <= 16 (untested)
  , objectDir     :: ObjectDirInfo
  -- ^ The directory's own data (ObjectDir::PreLoad + PostLoad),
  -- including any inline subdirectories
  , classInfo     :: DirClassInfo
  -- ^ Extra data for dir classes other than plain ObjectDir: their own
  -- revisions (stored before the ObjectDir data) and extra fields
  -- (stored after it)
  , remainder     :: BL.ByteString
  -- ^ any bytes between the parsed dir data and the AD DE AD DE barrier;
  -- should be empty if we understood the whole structure
  , files         :: [MiloEntry]
  -- ^ Serialized data for each entry, in the same order as entryNames
  } deriving (Show)

-- | The data for one object inside a dir.
data MiloEntry = MiloEntry
  { data_     :: BL.ByteString
  -- ^ the object's serialized data, without the AD DE AD DE barrier that
  -- follows it in the file. For an entry that is itself a dir class, this is
  -- the dir's data in proxy form.
  , inlineDir :: Maybe MiloDir
  -- ^ for a dir-class entry with an inlined proxy: the embedded dir stream,
  -- which the game reads after the barrier (ObjectDir::PostLoad makes a
  -- nested DirLoader on the same stream). Its own final barrier ends the
  -- entry, so no additional barrier follows.
  } deriving (Show)

-- | Data for the ObjectDir class itself, whose format depends on its own
-- revision, separate from the outer DirLoader revision in 'MiloDir'.
-- Field presence is decided by that revision when parsing; when constructing
-- by hand, fill fields consistently with it (see comments).
data ObjectDirInfo = ObjectDirInfo
  { version          :: Word32
  -- ^ ObjectDir revision. Observed: 20 (RB1/RB2/Lego/GDRB), 22 (Beatles),
  -- 27 (RB3). We support 18-27.
  , objectType       :: Maybe (Word32, B.ByteString)
  -- ^ version >= 22: Hmx::Object::LoadType, the base object revision
  -- (always 2) and the dir's type property symbol
  -- ("song" for song milo roots, "" otherwise)
  , alwaysInlined    :: Maybe Word32
  -- ^ version >= 26: mAlwaysInlined (observed 0)
  , alwaysInlineHash :: Maybe B.ByteString
  -- ^ version >= 26: mAlwaysInlineHash (observed empty)
  , viewports        :: [[Float]]
  -- ^ Camera viewports, 12 floats each (3x3 rotation matrix + position).
  -- Always 7 in practice: perspective, left, right, top, bottom, front, back
  , curViewport      :: Word32
  -- ^ Index of the current viewport (usually 0 = perspective)
  , inlineProxy      :: Maybe Word8
  -- ^ version >= 20: mInlineProxy bool (observed 1)
  , proxyPath        :: B.ByteString
  -- ^ mProxyFile path (observed empty)
  , extSubdirPaths   :: [B.ByteString]
  -- ^ Paths of external (not inlined) subdirs, loaded from separate files
  , rev23Ints        :: Maybe [Word32]
  -- ^ version == 23 only: unknown ints, one per external subdir (untested)
  , inlineSubdirType :: Maybe Word8
  -- ^ version >= 21: mInlineSubDirType
  -- (0 = never, 1 = cached, 2 = always, 3 = cached shared)
  , subdirs          :: [MiloSubdir]
  -- ^ version >= 21: inlined subdirectories
  , rev18Int         :: Maybe Word32
  -- ^ version 17-18 only: old inline proxy value (untested)
  , unknownString    :: B.ByteString
  -- ^ first string in ObjectDir::PostLoad (observed empty)
  , curCamera        :: B.ByteString
  -- ^ name of the current camera object (observed empty in songs)
  , objectTypePost   :: Maybe (Word32, B.ByteString)
  -- ^ version 17-21: same as objectType but stored at the end
  -- (Hmx::Object::Load inside ObjectDir::PostLoad)
  , typeProps        :: Maybe MiloDataArray
  -- ^ the dir object's type properties, a DataArray (dtb-style script data)
  , reserved         :: Maybe BL.ByteString
  -- ^ present when the base object revision above is nonzero:
  -- reserved data, skipped by the game (observed empty)
  } deriving (Show)

-- | Serialized DataArray (dtb-like), from DataArray::Save/Load
data MiloDataArray = MiloDataArray
  { line       :: Word16
  , deprecated :: Word16
  , nodes      :: [(Word32, MiloDataValue)] -- ^ (node type, value)
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
  line <- getWord16be
  deprecated <- getWord16be
  nodes <- replicateM (fromIntegral size) do
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
putDataArray da = do
  putWord16be $ fromIntegral $ length da.nodes
  putWord16be da.line
  putWord16be da.deprecated
  forM_ da.nodes \(t, val) -> do
    putWord32be t
    case val of
      MDInt    x    -> putWord32be x
      MDFloat  x    -> putFloatbe x
      MDString x    -> putStringBE x
      MDBlob   v bs -> putWord32be v >> putLazyByteString bs
      MDArray  x    -> putDataArray x

-- | An inlined subdirectory of an ObjectDir.
data MiloSubdir = MiloSubdir
  { name   :: B.ByteString
  , inline :: Maybe (Word8, Word8)
  -- ^ dir version >= 25: inline type (1 = cached, 2 = always, 3 = cached
  -- shared) and shared flag. Note, these are (de)serialized grouped across
  -- subdirs (all types, then all flags), not per subdir.
  , dir    :: Maybe MiloDir
  -- ^ the subdir contents; Nothing if the shared flag is set, meaning the
  -- contents live in a previously loaded dir
  } deriving (Show)

instance Bin MiloDir where
  bin = Codec

    { codecIn = do
      version <- getWord32be
      when (version < 14 || version > 32) $ fail $
        "Unsupported milo dir version " <> show version
      type_ <- getStringBE
      name <- getStringBE
      hashReserve <- getWord32be
      stringReserve <- getWord32be
      entryCount <- getWord32be
      entryNames <- replicateM (fromIntegral entryCount)
        $ liftA2 (,) getStringBE getStringBE
      extResources <- if version <= 16
        then Just <$> getArrayBE getStringBE
        else return Nothing
      (objectDir, classInfo) <- parseDirBody False type_
      remainder <- parseFileADDE
      files <- forM entryNames $ parseEntry . fst
      return MiloDir{..}

    , codecOut = fmapArg \d -> do
      putWord32be d.version
      putStringBE d.type_
      putStringBE d.name
      putWord32be d.hashReserve
      putWord32be d.stringReserve
      putArrayBE (\(x, y) -> putStringBE x >> putStringBE y) d.entryNames
      mapM_ (mapM_ putStringBE) d.extResources
      putDirBody d.objectDir d.classInfo
      putFileADDE d.remainder
      forM_ d.files \entry -> do
        putFileADDE entry.data_
        mapM_ (void . codecOut bin) entry.inlineDir

    }

-- ObjectDir::PreLoad then ObjectDir::PostLoad.
parseObjectDir :: Get ObjectDirInfo
parseObjectDir = do
  version <- getWord32be
  when (version < 18 || version > 27) $ fail $
    "Unsupported ObjectDir version " <> show version
  objectType <- if version >= 22
    then Just <$> liftA2 (,) getWord32be getStringBE
    else return Nothing
  alwaysInlined <- if version >= 26
    then Just <$> getWord32be
    else return Nothing
  alwaysInlineHash <- if version >= 26
    then Just <$> getStringBE
    else return Nothing
  viewports <- getArrayBE $ replicateM 12 getFloatbe
  curViewport <- getWord32be
  inlineProxy <- if version >= 20
    then Just <$> getWord8
    else return Nothing
  proxyPath <- getStringBE
  extSubdirPaths <- getArrayBE getStringBE
  rev23Ints <- if version == 23
    then Just <$> getArrayBE getWord32be
    else return Nothing
  (inlineSubdirType, subdirNames) <- if version >= 21
    then liftA2 (,) (Just <$> getWord8) (getArrayBE getStringBE)
    else return (Nothing, [])
  when (version == 24 && not (null subdirNames)) $ fail
    "Version 24 dir with inline subdirs not supported (unknown layout)"
  subdirs <- if version >= 25
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
  rev18Int <- if version <= 18
    then Just <$> getWord32be
    else return Nothing
  -- here begins ObjectDir::PostLoad
  unknownString <- getStringBE
  curCamera <- getStringBE
  objectTypePost <- if version <= 21
    then Just <$> liftA2 (,) getWord32be getStringBE
    else return Nothing
  propsFlag <- getWord8
  typeProps <- getMaybe (propsFlag /= 0) parseDataArray
  let objectRev = maybe 0 fst $ objectType <|> objectTypePost
  reserved <- if objectRev /= 0
    then Just <$> (getWord32be >>= getLazyByteString . fromIntegral)
    else return Nothing
  return ObjectDirInfo{..}

putObjectDir :: ObjectDirInfo -> Put
putObjectDir di = do
  putWord32be di.version
  forM_ di.objectType \(rev, sym) -> putWord32be rev >> putStringBE sym
  mapM_ putWord32be di.alwaysInlined
  mapM_ putStringBE di.alwaysInlineHash
  putArrayBE (mapM_ putFloatbe) di.viewports
  putWord32be di.curViewport
  mapM_ putWord8 di.inlineProxy
  putStringBE di.proxyPath
  putArrayBE putStringBE di.extSubdirPaths
  mapM_ (putArrayBE putWord32be) di.rev23Ints
  forM_ di.inlineSubdirType \t -> do
    putWord8 t
    putArrayBE putStringBE $ map (.name) di.subdirs
  if di.version >= 25
    then do
      forM_ di.subdirs $ mapM_ (putWord8 . fst) . (.inline)
      forM_ di.subdirs $ mapM_ (putWord8 . snd) . (.inline)
      forM_ di.subdirs $ mapM_ (void . codecOut bin) . (.dir)
    else forM_ di.subdirs $ mapM_ (void . codecOut bin) . (.dir)
  mapM_ putWord32be di.rev18Int
  putStringBE di.unknownString
  putStringBE di.curCamera
  forM_ di.objectTypePost \(rev, sym) -> putWord32be rev >> putStringBE sym
  case di.typeProps of
    Nothing -> putWord8 0
    Just da -> putWord8 1 >> putDataArray da
  forM_ di.reserved \r -> do
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

-- | A dir's data always ends at an ADDE barrier (its remainder is empty in
-- every file seen so far), which lets us check that we read the right amount.
endsAtBarrier :: Get ()
endsAtBarrier = do
  magic <- lookAhead $ getByteString 4
  unless (magic == B.pack magicBarrier) $ fail
    "dir data doesn't end at an ADDE barrier"

-- | Choose between two possible layouts for a dir class's data, preferring
-- the first, by seeing which one ends where the dir's data should end.
-- Falls back to the preferred one, so an unrecognized layout still reports
-- its own parse error rather than this one.
chooseVariant :: (Get a, Get a) -> Get a
chooseVariant (preferred, other)
  =   (preferred <* endsAtBarrier)
  <|> (other     <* endsAtBarrier)
  <|> preferred

-- | RndAnimatable::Load: animation frame state
data AnimInfo = AnimInfo
  { version :: Word32 -- ^ 4 in RB3
  , frame   :: Maybe Float  -- ^ version >= 2
  , rate    :: Maybe Word32 -- ^ version >= 4; 0 = 30fps
  } deriving (Show)

parseAnimInfo :: Get AnimInfo
parseAnimInfo = do
  version <- getWord32be
  when (version < 1 || version > 4) $ fail $
    "Unsupported RndAnimatable version " <> show version
  frame <- getMaybe (version > 1) getFloatbe
  rate <- getMaybe (version > 3) getWord32be
  return AnimInfo{..}

putAnimInfo :: AnimInfo -> Put
putAnimInfo x = do
  putWord32be x.version
  mapM_ putFloatbe x.frame
  mapM_ putWord32be x.rate

-- | RndDrawable::Load: showing flag, bounding sphere, draw order
data DrawInfo = DrawInfo
  { version :: Word32 -- ^ 3 in RB3
  , showing :: Word8
  , sphere  :: [Float] -- ^ 4 floats: center + radius
  , order   :: Maybe Float -- ^ version >= 3
  } deriving (Show)

parseDrawInfo :: Get DrawInfo
parseDrawInfo = do
  version <- getWord32be
  when (version < 2 || version > 3) $ fail $
    "Unsupported RndDrawable version " <> show version
  showing <- getWord8
  sphere <- replicateM 4 getFloatbe
  order <- getMaybe (version > 2) getFloatbe
  return DrawInfo{..}

putDrawInfo :: DrawInfo -> Put
putDrawInfo x = do
  putWord32be x.version
  putWord8 x.showing
  mapM_ putFloatbe x.sphere
  mapM_ putFloatbe x.order

-- | RndTransformable::Load: local/world transforms and trans parent
data TransInfo = TransInfo
  { version       :: Word32 -- ^ 9 in RB3 (only 9 supported)
  , local         :: [Float] -- ^ 12 floats
  , world         :: [Float] -- ^ 12 floats
  , constraint    :: Word32
  , target        :: B.ByteString
  , preserveScale :: Word8
  , parent        :: B.ByteString
  } deriving (Show)

parseTransInfo :: Get TransInfo
parseTransInfo = do
  version <- getWord32be
  when (version /= 9) $ fail $
    "Unsupported RndTransformable version " <> show version
  local <- replicateM 12 getFloatbe
  world <- replicateM 12 getFloatbe
  constraint <- getWord32be
  target <- getStringBE
  preserveScale <- getWord8
  parent <- getStringBE
  return TransInfo{..}

putTransInfo :: TransInfo -> Put
putTransInfo x = do
  putWord32be x.version
  mapM_ putFloatbe x.local
  mapM_ putFloatbe x.world
  putWord32be x.constraint
  putStringBE x.target
  putWord8 x.preserveScale
  putStringBE x.parent

-- | RndDir extras (RndDir::PostLoad after ObjectDir::PostLoad)
data RndDirInfo = RndDirInfo
  { version   :: Word32 -- ^ 10 in RB3
  , anim      :: AnimInfo
  , draw      :: DrawInfo
  , trans     :: Maybe TransInfo -- ^ version >= 1
  , environ   :: Maybe B.ByteString -- ^ version >= 2
  , testEvent :: Maybe B.ByteString -- ^ version >= 3 except 9
  } deriving (Show)

parseRndDirExtras :: Word32 -> Get RndDirInfo
parseRndDirExtras version = do
  when (version > 10) $ fail $
    "Unsupported RndDir version " <> show version
  anim <- parseAnimInfo
  draw <- parseDrawInfo
  trans <- getMaybe (version /= 0) parseTransInfo
  environ <- getMaybe (version > 1) getStringBE
  testEvent <- getMaybe (version > 2 && version /= 9) getStringBE
  return RndDirInfo{..}

putRndDirExtras :: RndDirInfo -> Put
putRndDirExtras x = do
  putAnimInfo x.anim
  putDrawInfo x.draw
  mapM_ putTransInfo x.trans
  mapM_ putStringBE x.environ
  mapM_ putStringBE x.testEvent

-- | PanelDir extras (PanelDir::PostLoad after RndDir::PostLoad)
data PanelDirInfo = PanelDirInfo
  { version            :: Word32 -- ^ 8 in RB3
  , rnd                :: RndDirInfo
  , cam                :: Maybe B.ByteString -- ^ version >= 1
  , rev2Symbol         :: Maybe B.ByteString -- ^ version == 2 only
  , canEndWorld        :: Maybe Word8 -- ^ version >= 4
  , backFilenames      :: Maybe [B.ByteString] -- ^ version >= 5
  , frontFilenames     :: Maybe [B.ByteString] -- ^ version >= 5
  , showEditModePanels :: Maybe Word8 -- ^ version >= 6
  , useSpecifiedCam    :: Maybe Word8 -- ^ version >= 8
  } deriving (Show)

parsePanelExtras :: Bool -> Word32 -> RndDirInfo -> Get PanelDirInfo
parsePanelExtras isProxy version rnd = do
  when (version > 8) $ fail $
    "Unsupported PanelDir version " <> show version
  cam <- getMaybe (version /= 0 && not isProxy) getStringBE
  rev2Symbol <- getMaybe (version == 2 && not isProxy) getStringBE
  canEndWorld <- getMaybe (version > 3) getWord8
  backFilenames <- getMaybe (version > 4) $ getArrayBE getStringBE
  frontFilenames <- getMaybe (version > 4) $ getArrayBE getStringBE
  showEditModePanels <- getMaybe (version > 5) getWord8
  useSpecifiedCam <- getMaybe (version > 7) getWord8
  return PanelDirInfo{..}

putPanelExtras :: PanelDirInfo -> Put
putPanelExtras x = do
  mapM_ putStringBE x.cam
  mapM_ putStringBE x.rev2Symbol
  mapM_ putWord8 x.canEndWorld
  mapM_ (putArrayBE putStringBE) x.backFilenames
  mapM_ (putArrayBE putStringBE) x.frontFilenames
  mapM_ putWord8 x.showEditModePanels
  mapM_ putWord8 x.useSpecifiedCam

-- | WorldDir: venue/world files. Pre fields go before PanelDir's data,
-- extras after (WorldDir::PreLoad / PostLoad).
data WorldDirInfo = WorldDirInfo
  { version          :: Word32 -- ^ we support 13-25
  , preInts          :: Maybe (Word32, Word32) -- ^ version <= 20
  , fakeHudFilename  :: Maybe B.ByteString -- ^ version >= 10
  , panel            :: PanelDirInfo
  , unknownXfm       :: Maybe [Float] -- ^ version 11-24: 12 floats
  , hideOverrides    :: [B.ByteString]
  , bitmapOverrides  :: [(B.ByteString, B.ByteString)] -- ^ (original tex, replacement tex)
  , matOverrides     :: Maybe [(B.ByteString, B.ByteString)] -- ^ version >= 14: (mesh, mat)
  , presetOverrides  :: Maybe [(B.ByteString, B.ByteString)] -- ^ version >= 15: (preset, hue)
  , camShotOverrides :: Maybe [B.ByteString] -- ^ version >= 16
  , ps3PerPixelHides :: Maybe [B.ByteString] -- ^ version >= 17 except 23
  , ps3PerPixelShows :: Maybe [B.ByteString] -- ^ version >= 17 except 23
  , midSymbol        :: Maybe B.ByteString -- ^ version 18-21 only
  , testPreset1      :: Maybe B.ByteString -- ^ version >= 19
  , testPreset2      :: Maybe B.ByteString -- ^ version >= 19
  , testAnimTime     :: Maybe Float -- ^ version >= 19
  , hud              :: Maybe B.ByteString -- ^ version >= 20
  } deriving (Show)

parseWorldPre :: Get (Word32, Maybe (Word32, Word32), Maybe B.ByteString)
parseWorldPre = do
  version <- getWord32be
  when (version < 13 || version > 25) $ fail $
    "Unsupported WorldDir version " <> show version
  preInts <- getMaybe (version <= 20)
    $ liftA2 (,) getWord32be getWord32be
  fakeHudFilename <- getMaybe (version > 9) getStringBE
  return (version, preInts, fakeHudFilename)

parseWorldExtras
  :: (Word32, Maybe (Word32, Word32), Maybe B.ByteString)
  -> PanelDirInfo -> Get WorldDirInfo
parseWorldExtras (version, preInts, fakeHudFilename) panel = do
  unknownXfm <- getMaybe (version > 10 && version < 25)
    $ replicateM 12 getFloatbe
  hideOverrides <- getArrayBE getStringBE
  bitmapOverrides <- getArrayBE $ liftA2 (,) getStringBE getStringBE
  matOverrides <- getMaybe (version > 13)
    $ getArrayBE $ liftA2 (,) getStringBE getStringBE
  presetOverrides <- getMaybe (version > 14)
    $ getArrayBE $ liftA2 (,) getStringBE getStringBE
  camShotOverrides <- getMaybe (version > 15) $ getArrayBE getStringBE
  ps3PerPixelHides <- getMaybe (version > 16 && version /= 23)
    $ getArrayBE getStringBE
  ps3PerPixelShows <- getMaybe (version > 16 && version /= 23)
    $ getArrayBE getStringBE
  midSymbol <- getMaybe (version >= 18 && version <= 21) getStringBE
  testPreset1 <- getMaybe (version > 18) getStringBE
  testPreset2 <- getMaybe (version > 18) getStringBE
  testAnimTime <- getMaybe (version > 18) getFloatbe
  hud <- getMaybe (version > 19) getStringBE
  return WorldDirInfo{..}

putWorldPre :: WorldDirInfo -> Put
putWorldPre di = do
  putWord32be di.version
  forM_ di.preInts \(x, y) -> putWord32be x >> putWord32be y
  mapM_ putStringBE di.fakeHudFilename

putWorldExtras :: WorldDirInfo -> Put
putWorldExtras di = do
  mapM_ (mapM_ putFloatbe) di.unknownXfm
  putArrayBE putStringBE di.hideOverrides
  putArrayBE (\(x, y) -> putStringBE x >> putStringBE y) di.bitmapOverrides
  mapM_ (putArrayBE \(x, y) -> putStringBE x >> putStringBE y) di.matOverrides
  mapM_ (putArrayBE \(x, y) -> putStringBE x >> putStringBE y) di.presetOverrides
  mapM_ (putArrayBE putStringBE) di.camShotOverrides
  mapM_ (putArrayBE putStringBE) di.ps3PerPixelHides
  mapM_ (putArrayBE putStringBE) di.ps3PerPixelShows
  mapM_ putStringBE di.midSymbol
  mapM_ putStringBE di.testPreset1
  mapM_ putStringBE di.testPreset2
  mapM_ putFloatbe di.testAnimTime
  mapM_ putStringBE di.hud

-- | Character: player/crowd character dirs
data CharacterInfo = CharacterInfo
  { version    :: Word32 -- ^ we support 9-17 (17 in RB3)
  , rnd        :: RndDirInfo
  , standalone :: Maybe CharacterExtras
  -- ^ present when the dir is not a proxy
  , proxyTest  :: Maybe CharacterTestInfo
  -- ^ present when the dir is a proxy and version >= 16
  } deriving (Show)

data CharacterExtras = CharacterExtras
  { lods       :: [(Float, B.ByteString, Maybe B.ByteString)]
  -- ^ (screen size, group, trans group [version >= 14])
  , shadow     :: B.ByteString
  , selfShadow :: Maybe Word8 -- ^ version >= 3
  , sphereBase :: Maybe B.ByteString -- ^ version >= 5
  , bounding   :: Maybe [Float] -- ^ version >= 11: bounding sphere, 4 floats
  , frozen     :: Maybe Word8 -- ^ version >= 13
  , minLod     :: Maybe Word32 -- ^ version >= 15
  , transGroup :: Maybe B.ByteString -- ^ version >= 17
  , test       :: Maybe CharacterTestInfo -- ^ version >= 10
  } deriving (Show)

parseCharacterExtras :: Bool -> Word32 -> RndDirInfo -> Get CharacterInfo
parseCharacterExtras isProxy version rnd = do
  when (version < 9 || version > 17) $ fail $
    "Unsupported Character version " <> show version
  let standaloneForm = do
        standalone <- fmap Just do
          lods <- getArrayBE do
            screenSize <- getFloatbe
            grp <- getStringBE
            transGrp <- getMaybe (version > 13) getStringBE
            return (screenSize, grp, transGrp)
          shadow <- getStringBE
          selfShadow <- getMaybe (version > 2) getWord8
          sphereBase <- getMaybe (version > 4) getStringBE
          bounding <- getMaybe (version > 10) $ replicateM 4 getFloatbe
          frozen <- getMaybe (version > 12) getWord8
          minLod <- getMaybe (version > 14) getWord32be
          transGroup <- getMaybe (version > 16) getStringBE
          test <- getMaybe (version > 9) parseCharacterTest
          return CharacterExtras{..}
        return CharacterInfo{ proxyTest = Nothing, .. }
      proxyForm = do
        proxyTest <- getMaybe (version > 15) parseCharacterTest
        return CharacterInfo{ standalone = Nothing, .. }
  -- Character::PostLoad only saves the lods and the rest when the dir isn't
  -- a proxy, but which form was used isn't recorded in the file, and a dir
  -- can be saved in proxy form even at the top level of a milo (e.g.
  -- char/main/gen/skeleton.milo_xbox). Try the likely form for where this dir
  -- came from, and check that the dir data ends where it should.
  chooseVariant $ if isProxy
    then (proxyForm, standaloneForm)
    else (standaloneForm, proxyForm)

putCharacterExtras :: CharacterInfo -> Put
putCharacterExtras char = do
  forM_ char.standalone \x -> do
    putArrayBE
      (\(ss, grp, transGrp) -> putFloatbe ss >> putStringBE grp >> mapM_ putStringBE transGrp)
      x.lods
    putStringBE x.shadow
    mapM_ putWord8 x.selfShadow
    mapM_ putStringBE x.sphereBase
    mapM_ (mapM_ putFloatbe) x.bounding
    mapM_ putWord8 x.frozen
    mapM_ putWord32be x.minLod
    mapM_ putStringBE x.transGroup
    mapM_ putCharacterTest x.test
  mapM_ putCharacterTest char.proxyTest

-- | CharacterTest: test/preview state saved with characters
-- (CharacterTest::Load)
data CharacterTestInfo = CharacterTestInfo
  { version         :: Word32 -- ^ we support 11-15 (15 in RB3)
  , driver          :: Maybe B.ByteString -- ^ absent in version 13 only
  , clip1           :: B.ByteString
  , clip2           :: B.ByteString
  , teleportTo      :: B.ByteString
  , walkPath        :: [B.ByteString]
  , showDistMap     :: B.ByteString
  , transition      :: Word32
  , cycleTransition :: Word8
  , unknownInt      :: Word32
  , metronome       :: Word8
  , zeroTravel      :: Word8
  , showScreenSize  :: Word8
  , rev11Symbol     :: Maybe B.ByteString -- ^ version <= 11
  , showFootExtents :: Word8
  , oldBoolInt      :: Maybe (Word8, Word32) -- ^ version <= 14
  } deriving (Show)

parseCharacterTest :: Get CharacterTestInfo
parseCharacterTest = do
  version <- getWord32be
  when (version < 11 || version > 15) $ fail $
    "Unsupported CharacterTest version " <> show version
  driver <- getMaybe (version /= 13) getStringBE
  clip1 <- getStringBE
  clip2 <- getStringBE
  teleportTo <- getStringBE
  walkPath <- getArrayBE getStringBE
  showDistMap <- getStringBE
  transition <- getWord32be
  cycleTransition <- getWord8
  unknownInt <- getWord32be
  metronome <- getWord8
  zeroTravel <- getWord8
  showScreenSize <- getWord8
  rev11Symbol <- getMaybe (version < 12) getStringBE
  showFootExtents <- getWord8
  oldBoolInt <- getMaybe (version < 15) $ liftA2 (,) getWord8 getWord32be
  return CharacterTestInfo{..}

putCharacterTest :: CharacterTestInfo -> Put
putCharacterTest x = do
  putWord32be x.version
  mapM_ putStringBE x.driver
  putStringBE x.clip1
  putStringBE x.clip2
  putStringBE x.teleportTo
  putArrayBE putStringBE x.walkPath
  putStringBE x.showDistMap
  putWord32be x.transition
  putWord8 x.cycleTransition
  putWord32be x.unknownInt
  putWord8 x.metronome
  putWord8 x.zeroTravel
  putWord8 x.showScreenSize
  mapM_ putStringBE x.rev11Symbol
  putWord8 x.showFootExtents
  forM_ x.oldBoolInt \(b, i) -> putWord8 b >> putWord32be i

-- | CharBoneDir extras (CharBoneDir::PostLoad)
data CharBoneDirInfo = CharBoneDirInfo
  { version       :: Word32 -- ^ we support 2-4 (4 in RB3)
  , moveContext   :: Word32
  , targets       :: [B.ByteString]
  , average       :: [B.ByteString]
  , slide         :: Word8
  , bakeOutFacing :: Maybe Word8 -- ^ version >= 4
  } deriving (Show)

parseCharBoneDirExtras :: Word32 -> Get CharBoneDirInfo
parseCharBoneDirExtras version = do
  when (version < 2 || version > 4) $ fail $
    "Unsupported CharBoneDir version " <> show version
  moveContext <- getWord32be
  targets <- getArrayBE getStringBE
  average <- getArrayBE getStringBE
  slide <- getWord8
  bakeOutFacing <- getMaybe (version > 3) getWord8
  return CharBoneDirInfo{..}

putCharBoneDirExtras :: CharBoneDirInfo -> Put
putCharBoneDirExtras x = do
  putWord32be x.moveContext
  putArrayBE putStringBE x.targets
  putArrayBE putStringBE x.average
  putWord8 x.slide
  mapM_ putWord8 x.bakeOutFacing

-- | UIListDir extras (UIListDir::PostLoad)
data UIListDirInfo = UIListDirInfo
  { version               :: Word32 -- ^ 1 in RB3
  , rnd                   :: RndDirInfo
  , orientation           :: Word32
  , fadeOffset            :: Word32
  , testMode              :: Word8
  , numDisplay            :: Word32
  , elementSpacing        :: Float
  , speed                 :: Float
  , testNumData           :: Word32
  , componentState        :: Word32
  , testGapSize           :: Float
  , testDisableElements   :: Word8
  , scrollHighlightChange :: Maybe Float -- ^ version >= 1
  } deriving (Show)

parseUIListExtras :: Word32 -> RndDirInfo -> Get UIListDirInfo
parseUIListExtras version rnd = do
  when (version > 1) $ fail $
    "Unsupported UIListDir version " <> show version
  orientation <- getWord32be
  fadeOffset <- getWord32be
  testMode <- getWord8
  numDisplay <- getWord32be
  elementSpacing <- getFloatbe
  speed <- getFloatbe
  testNumData <- getWord32be
  componentState <- getWord32be
  testGapSize <- getFloatbe
  testDisableElements <- getWord8
  scrollHighlightChange <- getMaybe (version /= 0) getFloatbe
  return UIListDirInfo{..}

putUIListExtras :: UIListDirInfo -> Put
putUIListExtras x = do
  putWord32be x.orientation
  putWord32be x.fadeOffset
  putWord8 x.testMode
  putWord32be x.numDisplay
  putFloatbe x.elementSpacing
  putFloatbe x.speed
  putWord32be x.testNumData
  putWord32be x.componentState
  putFloatbe x.testGapSize
  putWord8 x.testDisableElements
  mapM_ putFloatbe x.scrollHighlightChange

-- | UILabelDir extras (UILabelDir::PostLoad)
data UILabelDirInfo = UILabelDirInfo
  { version                  :: Word32 -- ^ 9 in RB3
  , rnd                      :: RndDirInfo
  , textObj                  :: B.ByteString
  , oldFont                  :: Maybe B.ByteString -- ^ version 3-8
  , focusAnim                :: Maybe B.ByteString -- ^ version >= 1
  , pulseAnim                :: Maybe B.ByteString -- ^ version >= 2
  , highlightGroup           :: Maybe B.ByteString -- ^ version >= 4
  , topLeftBone              :: Maybe B.ByteString -- ^ version >= 4
  , topRightBone             :: Maybe B.ByteString -- ^ version >= 4
  , bottomLeftBone           :: Maybe B.ByteString -- ^ version >= 5
  , bottomRightBone          :: Maybe B.ByteString -- ^ version >= 5
  , focusedBackgroundGroup   :: Maybe B.ByteString -- ^ version >= 6
  , unfocusedBackgroundGroup :: Maybe B.ByteString -- ^ version >= 6
  , allowEditText            :: Maybe Word8 -- ^ version >= 7
  , defaultColor             :: B.ByteString
  , colors                   :: [B.ByteString] -- ^ one per UIComponent state (5)
  , fontImporter             :: Maybe UIFontImporterInfo -- ^ version >= 8
  } deriving (Show)

parseUILabelExtras :: Word32 -> RndDirInfo -> Get UILabelDirInfo
parseUILabelExtras version rnd = do
  when (version > 9) $ fail $
    "Unsupported UILabelDir version " <> show version
  textObj <- getStringBE
  oldFont <- getMaybe (version >= 3 && version <= 8) getStringBE
  focusAnim <- getMaybe (version >= 1) getStringBE
  pulseAnim <- getMaybe (version >= 2) getStringBE
  highlightGroup <- getMaybe (version >= 4) getStringBE
  topLeftBone <- getMaybe (version >= 4) getStringBE
  topRightBone <- getMaybe (version >= 4) getStringBE
  bottomLeftBone <- getMaybe (version >= 5) getStringBE
  bottomRightBone <- getMaybe (version >= 5) getStringBE
  focusedBackgroundGroup <- getMaybe (version >= 6) getStringBE
  unfocusedBackgroundGroup <- getMaybe (version >= 6) getStringBE
  allowEditText <- getMaybe (version >= 7) getWord8
  defaultColor <- getStringBE
  colors <- replicateM 5 getStringBE
  fontImporter <- getMaybe (version >= 8) parseUIFontImporter
  return UILabelDirInfo{..}

putUILabelExtras :: UILabelDirInfo -> Put
putUILabelExtras x = do
  putStringBE x.textObj
  mapM_ putStringBE x.oldFont
  mapM_ putStringBE x.focusAnim
  mapM_ putStringBE x.pulseAnim
  mapM_ putStringBE x.highlightGroup
  mapM_ putStringBE x.topLeftBone
  mapM_ putStringBE x.topRightBone
  mapM_ putStringBE x.bottomLeftBone
  mapM_ putStringBE x.bottomRightBone
  mapM_ putStringBE x.focusedBackgroundGroup
  mapM_ putStringBE x.unfocusedBackgroundGroup
  mapM_ putWord8 x.allowEditText
  putStringBE x.defaultColor
  mapM_ putStringBE x.colors
  mapM_ putUIFontImporter x.fontImporter

-- | UIFontImporter::Load, embedded in UILabelDir
data UIFontImporterInfo = UIFontImporterInfo
  { version           :: Word32 -- ^ we support 5-9 (9 in RB3)
  , includeFlags      :: [Word8] -- ^ 6 bools: upper, lower, numbers,
                                  -- punctuation, upper euro, lower euro
  , plusChars         :: B.ByteString
  , minusChars        :: B.ByteString
  , fontName          :: B.ByteString
  , fontPctSize       :: Float
  , fontWeight        :: Word32
  , italics           :: Word8
  , pitchAndFamily    :: Word32
  , fontQuality       :: Word32
  , fontCharset       :: Word32
  , fontSupersample   :: Word32
  , bitmapSavePath    :: B.ByteString
  , bitmapSaveName    :: B.ByteString
  , padding           :: [Word32] -- ^ left, right, top, bottom
  , fillWithSafeWhite :: Word8
  , fontToImportFrom  :: Maybe B.ByteString -- ^ version <= 7
  , gennedFonts       :: [B.ByteString]
  , referenceKerning  :: B.ByteString
  , matVariations     :: [B.ByteString]
  , defaultMat        :: Maybe B.ByteString -- ^ version >= 6
  , handmadeFont      :: Maybe B.ByteString -- ^ version >= 7
  , syncResource      :: Maybe B.ByteString -- ^ version >= 8
  , lastGenWasNG      :: Maybe Word8 -- ^ version >= 9
  } deriving (Show)

parseUIFontImporter :: Get UIFontImporterInfo
parseUIFontImporter = do
  version <- getWord32be
  when (version < 5 || version > 9) $ fail $
    "Unsupported UIFontImporter version " <> show version
  includeFlags <- replicateM 6 getWord8
  plusChars <- getStringBE
  minusChars <- getStringBE
  fontName <- getStringBE
  fontPctSize <- getFloatbe
  fontWeight <- getWord32be
  italics <- getWord8
  pitchAndFamily <- getWord32be
  fontQuality <- getWord32be
  fontCharset <- getWord32be
  fontSupersample <- getWord32be
  bitmapSavePath <- getStringBE
  bitmapSaveName <- getStringBE
  padding <- replicateM 4 getWord32be
  fillWithSafeWhite <- getWord8
  fontToImportFrom <- getMaybe (version < 8) getStringBE
  gennedFonts <- getArrayBE getStringBE
  referenceKerning <- getStringBE
  matVariations <- getArrayBE getStringBE
  defaultMat <- getMaybe (version > 5) getStringBE
  handmadeFont <- getMaybe (version > 6) getStringBE
  syncResource <- getMaybe (version > 7) getStringBE
  lastGenWasNG <- getMaybe (version > 8) getWord8
  return UIFontImporterInfo{..}

putUIFontImporter :: UIFontImporterInfo -> Put
putUIFontImporter x = do
  putWord32be x.version
  mapM_ putWord8 x.includeFlags
  putStringBE x.plusChars
  putStringBE x.minusChars
  putStringBE x.fontName
  putFloatbe x.fontPctSize
  putWord32be x.fontWeight
  putWord8 x.italics
  putWord32be x.pitchAndFamily
  putWord32be x.fontQuality
  putWord32be x.fontCharset
  putWord32be x.fontSupersample
  putStringBE x.bitmapSavePath
  putStringBE x.bitmapSaveName
  mapM_ putWord32be x.padding
  putWord8 x.fillWithSafeWhite
  mapM_ putStringBE x.fontToImportFrom
  putArrayBE putStringBE x.gennedFonts
  putStringBE x.referenceKerning
  putArrayBE putStringBE x.matVariations
  mapM_ putStringBE x.defaultMat
  mapM_ putStringBE x.handmadeFont
  mapM_ putStringBE x.syncResource
  mapM_ putWord8 x.lastGenWasNG

-- | CharClipSet extras (end of CharClipSet::PostLoad).
-- Absent when the dir is a proxy (an entry inside another dir).
data CharClipSetInfo = CharClipSetInfo
  { version :: Word32 -- ^ only 24 supported (RB3)
  , extras  :: Maybe CharClipSetExtras
  } deriving (Show)

data CharClipSetExtras = CharClipSetExtras
  { charFilePath :: B.ByteString -- ^ character milo to preview clips on
  , previewClip  :: B.ByteString
  , filterFlags  :: Word32
  , bpm          :: Word32
  , previewWalk  :: Word8
  , stillClip    :: B.ByteString
  } deriving (Show)

parseCharClipSetExtras :: Bool -> Word32 -> Get CharClipSetInfo
parseCharClipSetExtras isProxy version = do
  when (version /= 24) $ fail $
    "Unsupported CharClipSet version " <> show version
  extras <- getMaybe (not isProxy) do
    charFilePath <- getStringBE
    previewClip <- getStringBE
    filterFlags <- getWord32be
    bpm <- getWord32be
    previewWalk <- getWord8
    stillClip <- getStringBE
    return CharClipSetExtras{..}
  return CharClipSetInfo{..}

putCharClipSetExtras :: CharClipSetInfo -> Put
putCharClipSetExtras ccs = forM_ ccs.extras \x -> do
  putStringBE x.charFilePath
  putStringBE x.previewClip
  putWord32be x.filterFlags
  putWord32be x.bpm
  putWord8 x.previewWalk
  putStringBE x.stillClip

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
    putWord32be rnd.version
    putObjectDir core
    putRndDirExtras rnd
  InfoPanelDir panel -> do
    putWord32be panel.version
    putWord32be panel.rnd.version
    putObjectDir core
    putRndDirExtras panel.rnd
    putPanelExtras panel
  InfoWorldDir world -> do
    putWorldPre world
    putWord32be world.panel.version
    putWord32be world.panel.rnd.version
    putObjectDir core
    putRndDirExtras world.panel.rnd
    putPanelExtras world.panel
    putWorldExtras world
  InfoCharacter char -> do
    putWord32be char.version
    putWord32be char.rnd.version
    putObjectDir core
    putRndDirExtras char.rnd
    putCharacterExtras char
  InfoCharClipSet ccs -> do
    putWord32be ccs.version
    putObjectDir core
    putCharClipSetExtras ccs
  InfoCharBoneDir cb -> do
    putWord32be cb.version
    putObjectDir core
    putCharBoneDirExtras cb
  InfoUIListDir ul -> do
    putWord32be ul.version
    putWord32be ul.rnd.version
    putObjectDir core
    putRndDirExtras ul.rnd
    putUIListExtras ul
  InfoUILabelDir uld -> do
    putWord32be uld.version
    putWord32be uld.rnd.version
    putObjectDir core
    putRndDirExtras uld.rnd
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
      , not (B.null core.proxyPath) && core.inlineProxy /= Just 0
      )
  body <- getLazyByteString bodyLen
  magic <- getByteString 4
  unless (magic == B.pack magicBarrier) $ fail
    "Dir-class entry data not followed by ADDE barrier"
  inlineDir <- getMaybe hasInline $ codecIn bin
  return $ MiloEntry body inlineDir

-- | All inlined subdir contents, in order.
miloSubdirs :: MiloDir -> [MiloDir]
miloSubdirs dir = catMaybes $ map (.dir) dir.objectDir.subdirs

-- | The embedded dir streams of any dir-class entries, in order.
miloInlineDirs :: MiloDir -> [MiloDir]
miloInlineDirs dir = catMaybes $ map (.inlineDir) dir.files

parseMiloFile :: Get MiloDir
parseMiloFile = do
  dir <- codecIn bin
  eof <- isEmpty
  if eof
    then return dir
    else fail "Didn't parse entire .milo file"

makeMiloFile :: MiloDir -> BL.ByteString
makeMiloFile = runPut . void . codecOut bin
