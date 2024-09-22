{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Harmonix.RockBand.Milo.Dir where

import           Control.Monad                           (replicateM, void)
import qualified Data.ByteString                         as B
import qualified Data.ByteString.Lazy                    as BL
import           Onyx.Codec.Binary
import           Onyx.Harmonix.RockBand.Milo.Compression (magicBarrier)

#if MIN_VERSION_base(4,18,0)
-- liftA2 from Prelude
#else
import           Control.Applicative                     (liftA2)
#endif

getStringBE :: Get B.ByteString
getStringBE = do
  len <- getWord32be
  getByteString $ fromIntegral len

putStringBE :: B.ByteString -> Put
putStringBE bs = do
  putWord32be $ fromIntegral $ B.length bs
  putByteString bs

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

data MiloDir = MiloDir
  { miloVersion      :: Word32
  -- ^ Observed versions:
  -- - 25 for all song milos before RB3
  -- - 28 for all song milos after RB3
  , miloType         :: B.ByteString
  , miloName         :: B.ByteString
  , miloU1           :: Word32
  -- ^ Pikmin says: Count of Strings (All Strings in this part)
  -- Seen: 4, 6, 8, 10, 12
  , miloU2           :: Word32
  -- ^ Pikmin says: Count of Names + Total Length
  -- Seen: lots of numbers ranging from 14 to 69
  , miloEntryNames   :: [(B.ByteString, B.ByteString)]
  , miloU3           :: Word32
  -- ^ Seen: 20, 22, 27
  , miloU4           :: Maybe Word32
  -- ^ Nothing for version 25, Just 2 for version 28
  -- EXCEPT Beatles which is version 25 with Just 2
  , miloSubname      :: Maybe B.ByteString
  -- ^ Same as miloU4, value is Just "song" for top, Just "" for subdir
  , miloU5           :: Maybe Word32
  -- ^ Nothing for version 25 (incl. Beatles), Just 0 for version 28
  , miloU6           :: Maybe Word32
  -- ^ Same as miloU5
  , miloMatrices     :: [[Float]]
  , miloU7           :: Word32
  -- ^ always 0, except for herecomesthesun where it's 6
  , miloU8           :: Word8
  -- ^ always 1
  , miloU9           :: Word32
  -- ^ always 0
  , miloParents      :: [B.ByteString]
  , miloU10          :: Word8
  -- ^ appears to be 0 in root, 1 in some subdirectories but not all
  , miloChildren     :: [B.ByteString]
  , miloU11          :: Maybe Word16
  -- ^ in v25, always Nothing. in v28, Just 256 in root, Nothing in subdir
  , miloSubdirs      :: [MiloDir]
  , miloUnknownBytes :: BL.ByteString
  -- ^ in version 28 and beatles (25), this is [0,0,0,0,0,0,0,0,0,0,0,0,0]
  -- in version 25 other than beatles, it's [0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0]
  , miloFiles        :: [BL.ByteString]
  } deriving (Show)

{-
Currently this can parse:
* all RB 1/2/3/Lego/Beatles/GD songs,
  except fantasma2 which is very weird (has an ObjectDir in the files list)
But fails on e.g. most on-disc RB3 game content milos.
-}
instance Bin MiloDir where
  bin = Codec

    { codecIn = do
      miloVersion <- getWord32be
      miloType <- getStringBE
      miloName <- getStringBE
      miloU1 <- getWord32be
      miloU2 <- getWord32be
      entryCount <- getWord32be
      miloEntryNames <- replicateM (fromIntegral entryCount)
        $ liftA2 (,) getStringBE getStringBE
      miloU3 <- getWord32be
      let expectedMatrixCount = 7
      flag1 <- (/= expectedMatrixCount) <$> lookAhead getWord32be
      -- flag1 is false on 2minutestomidnight, but true on TBRB, magma2, and 2112
      miloU4 <- if flag1 then Just <$> getWord32be else return Nothing
      miloSubname <- if flag1 then Just <$> getStringBE else return Nothing
      flag2 <- (/= expectedMatrixCount) <$> lookAhead getWord32be
      -- flag2 is false on TBRB, but true on magma2 and onthebacksofangels
      miloU5 <- if flag2 then Just <$> getWord32be else return Nothing
      miloU6 <- if flag2 then Just <$> getWord32be else return Nothing
      matrixCount <- getWord32be
      miloMatrices <- replicateM (fromIntegral matrixCount)
        $ replicateM 12 getFloatbe
      miloU7 <- getWord32be
      miloU8 <- getWord8
      miloU9 <- getWord32be
      parentMiloCount <- getWord32be
      miloParents <- replicateM (fromIntegral parentMiloCount) getStringBE
      miloU10 <- getWord8
      subMiloCount <- getWord32be
      miloChildren <- replicateM (fromIntegral subMiloCount) getStringBE
      miloU11 <- lookAhead getWord8 >>= \case
        1 -> Just <$> getWord16be
        _ -> return Nothing
      miloSubdirs <- replicateM (fromIntegral subMiloCount) $ codecIn bin
      miloUnknownBytes <- parseFileADDE
      miloFiles <- replicateM (fromIntegral entryCount) parseFileADDE
      return MiloDir{..}

    , codecOut = fmapArg $ \MiloDir{..} -> do
      putWord32be miloVersion
      putStringBE miloType
      putStringBE miloName
      putWord32be miloU1
      putWord32be miloU2
      let putArray xs p = do
            putWord32be $ fromIntegral $ length xs
            mapM_ p xs
      putArray miloEntryNames $ \(x, y) -> putStringBE x >> putStringBE y
      putWord32be miloU3
      mapM_ putWord32be miloU4
      mapM_ putStringBE miloSubname
      mapM_ putWord32be miloU5
      mapM_ putWord32be miloU6
      putArray miloMatrices $ mapM_ putFloatbe
      putWord32be miloU7
      putWord8 miloU8
      putWord32be miloU9
      putArray miloParents putStringBE
      putWord8 miloU10
      putWord32be $ fromIntegral $ length miloChildren
      mapM_ putStringBE miloChildren
      maybe (return ()) putWord16be miloU11
      mapM_ (codecOut bin) miloSubdirs
      let putADDE bs = do
            putLazyByteString bs
            putByteString $ B.pack magicBarrier
      putADDE miloUnknownBytes
      mapM_ putADDE miloFiles

    }

parseMiloFile :: Get MiloDir
parseMiloFile = do
  dir <- codecIn bin
  eof <- isEmpty
  if eof
    then return dir
    else fail "Didn't parse entire .milo file"

makeMiloFile :: MiloDir -> BL.ByteString
makeMiloFile = runPut . void . codecOut bin
