{-
Structures here are based on the Rock Band 3 decompilation project,
mainly these files:
- src/system/obj/DirLoader.cpp (outer header, class + entry tables)
- src/system/obj/Dir.cpp (ObjectDir::PreLoad and ObjectDir::PostLoad)
- src/system/obj/Object.cpp (Hmx::Object::LoadType and LoadRest)
-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
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
  , miloFiles         :: [BL.ByteString]
  -- ^ Serialized object data, one per entry, each terminated in the file
  -- by the AD DE AD DE barrier
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
  , odReserved         :: Maybe BL.ByteString
  -- ^ present when the base object revision above is nonzero:
  -- reserved data, skipped by the game (observed empty)
  , odRemainder        :: BL.ByteString
  -- ^ any bytes between the parsed dir data and the AD DE AD DE barrier;
  -- should be empty if we understood the whole structure
  } deriving (Show)

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
      unless (miloType == "ObjectDir") $ fail $
        "Can't parse dir data for class " <> show miloType <> " yet, only ObjectDir"
      miloObjectDir <- parseObjectDir
      miloFiles <- replicateM (fromIntegral entryCount) parseFileADDE
      return MiloDir{..}

    , codecOut = fmapArg \MiloDir{..} -> do
      putWord32be miloVersion
      putStringBE miloType
      putStringBE miloName
      putWord32be miloHashReserve
      putWord32be miloStringReserve
      putArrayBE (\(x, y) -> putStringBE x >> putStringBE y) miloEntryNames
      mapM_ (mapM_ putStringBE) miloExtResources
      putObjectDir miloObjectDir
      mapM_ putFileADDE miloFiles

    }

-- ObjectDir::PreLoad then ObjectDir::PostLoad, ending with the ADDE barrier.
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
  when (propsFlag /= 0) $ fail
    "Dir has type properties (a DataArray); not supported yet"
  let objectRev = maybe 0 fst $ odObjectType <|> odObjectTypePost
  odReserved <- if objectRev /= 0
    then Just <$> (getWord32be >>= getLazyByteString . fromIntegral)
    else return Nothing
  odRemainder <- parseFileADDE
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
  putWord8 0 -- no type properties DataArray
  forM_ odReserved \r -> do
    putWord32be $ fromIntegral $ BL.length r
    putLazyByteString r
  putFileADDE odRemainder

-- | All inlined subdir contents, in order.
miloSubdirs :: MiloDir -> [MiloDir]
miloSubdirs dir = catMaybes $ map (.sdDir) dir.miloObjectDir.odSubdirs

parseMiloFile :: Get MiloDir
parseMiloFile = do
  dir <- codecIn bin
  eof <- isEmpty
  if eof
    then return dir
    else fail "Didn't parse entire .milo file"

makeMiloFile :: MiloDir -> BL.ByteString
makeMiloFile = runPut . void . codecOut bin
