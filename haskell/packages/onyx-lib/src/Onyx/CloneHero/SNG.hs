{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.CloneHero.SNG where

import           Control.Applicative  (liftA2)
import           Control.Monad
import           Data.Bifunctor       (bimap)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits            (xor, (.&.))
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        (toList)
import           Data.Hashable        (hash)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Data.Word
import qualified GHC.IO.Handle        as H
import           Onyx.Util.Binary     (runGetM)
import           Onyx.Util.Handle
import           System.IO            (SeekMode (..), hClose, hFileSize, hSeek)

data SNGHeader = SNGHeader
  { version  :: Word32
  , seed     :: B.ByteString -- 16 bytes
  , metadata :: [(T.Text, T.Text)]
  , files    :: [SNGFileInfo]
  } deriving (Show)

data SNGFileInfo = SNGFileInfo
  { name   :: T.Text
  , size   :: Word64
  , offset :: Word64
  } deriving (Show)

readSNGHeader :: Readable -> IO SNGHeader
readSNGHeader r = useHandle r $ \h -> do
  let load len getter = BL.hGet h len >>= runGetM getter
  magic   <- B.hGet h 6
  unless (magic == "SNGPKG") $ fail $ "Unrecognized .sng identifier: " <> show magic
  version <- load 4 getWord32le
  seed    <- B.hGet h 16
  let getString32 = do
        len <- fromIntegral <$> getInt32le
        TE.decodeUtf8 <$> getByteString len
      getString8 = do
        len <- fromIntegral <$> getWord8
        TE.decodeUtf8 <$> getByteString len
  metaLength <- fromIntegral <$> load 8 getWord64le
  metadata <- load metaLength $ do
    count <- fromIntegral <$> getWord64le
    replicateM count $ liftA2 (,) getString32 getString32
  fileMetaLength <- fromIntegral <$> load 8 getWord64le
  files <- load fileMetaLength $ do
    count <- fromIntegral <$> getWord64le
    replicateM count $ do
      name   <- getString8
      size   <- getWord64le
      offset <- getWord64le
      return SNGFileInfo{..}
  return SNGHeader{..}

showSNGHeader :: SNGHeader -> BL.ByteString
showSNGHeader hdr = runPut $ do
  putByteString "SNGPKG"
  putWord32le hdr.version
  putByteString hdr.seed
  let metaBytes = map (bimap TE.encodeUtf8 TE.encodeUtf8) hdr.metadata
      metaLength = 8 + sum (map (\(x, y) -> 4 + B.length x + 4 + B.length y) metaBytes)
  putWord64le $ fromIntegral metaLength
  putWord64le $ fromIntegral $ length metaBytes
  forM_ metaBytes $ \(x, y) -> do
    putInt32le $ fromIntegral $ B.length x
    putByteString x
    putInt32le $ fromIntegral $ B.length y
    putByteString y
  let fileBytes = do
        f <- hdr.files
        return (TE.encodeUtf8 f.name, f.size, f.offset)
      fileLength = 8 + sum (map (\(name, _, _) -> 1 + B.length name + 8 + 8) fileBytes)
  putWord64le $ fromIntegral fileLength
  putWord64le $ fromIntegral $ length fileBytes
  forM_ fileBytes $ \(name, size, offset) -> do
    putWord8 $ fromIntegral $ B.length name
    putByteString name
    putWord64le size
    putWord64le offset

-- Should be given the 256-byte repeating pattern (seed xor'd with byte indices)
cryptFile :: BL.ByteString -> Readable -> Readable
cryptFile seedPattern orig = Readable
  { rFilePath = Nothing
  , rOpen = do
    h <- rOpen orig
    -- TODO probably want to close h if there's an exception before we're done
    origSize <- hFileSize h
    let seedStream = BL.cycle seedPattern
    openSimpleHandle (handleLabel h) SimpleHandle
      { shSize  = origSize
      , shSeek  = hSeek h AbsoluteSeek
      , shTell  = (\(H.HandlePosn _ n) -> n) <$> H.hGetPosn h
      , shClose = hClose h
      , shRead  = \len -> do
        -- does hGetPosn have much overhead? if so, could store position ourselves
        pos <- (\(H.HandlePosn _ n) -> fromIntegral n) <$> H.hGetPosn h
        enc <- B.hGet h $ fromIntegral len
        return
          $ B.packZipWith xor enc
          $ BL.toStrict
          $ BL.take (fromIntegral len)
          $ BL.drop (pos .&. 0xFF) seedStream
      }
  }

seedToPattern :: B.ByteString -> BL.ByteString
seedToPattern seed = BL.pack $ zipWith xor (cycle $ B.unpack seed) [0 .. 255]

getSNGFolder :: Bool -> SNGHeader -> Readable -> Folder T.Text Readable
getSNGFolder addINI hdr r = fromFiles $ let
  seedPattern = seedToPattern hdr.seed
  normalFiles = do
    file <- hdr.files
    path <- toList $ splitPath file.name
    let subFile = subHandle addLabel (fromIntegral file.offset) (Just $ fromIntegral file.size) r
        addLabel orig = orig <> " | " <> T.unpack file.name
    return (path, cryptFile seedPattern subFile)
  ini = do
    guard addINI
    return
      ( pure "song.ini"
      , makeHandle "song.ini" $ byteStringSimpleHandle
        $ BL.fromStrict $ TE.encodeUtf8 $ makeSongINI hdr
      )
  in ini <> normalFiles

makeSongINI :: SNGHeader -> T.Text
makeSongINI hdr = T.unlines $
  "[Song]" : [ k <> " = " <> v | (k, v) <- hdr.metadata ]

makeSNG :: [(T.Text, T.Text)] -> [(T.Text, Readable)] -> IO [Readable]
makeSNG meta files = do
  filesWithSize <- forM files $ \(name, r) -> do
    size <- fromIntegral <$> useHandle r hFileSize
    return (name, r, size)
  let totalFileSize = sum [ size | (_, _, size) <- filesWithSize ]
      seed = BL.toStrict $ runPut $ do
        -- look random but actually deterministic for simplicity
        putWord64be $ fromIntegral $ hash meta
        putWord64be $ fromIntegral $ hash $ map snd meta
      seedPattern = seedToPattern seed
      headerForFiles fs = let
        headerBytes = showSNGHeader SNGHeader
          { version = 1
          , seed = seed
          , metadata = meta
          , files = fs
          }
        fileSectionSize = runPut $ putWord64le $ fromIntegral totalFileSize
        in headerBytes <> fileSectionSize
      headerSize = fromIntegral $ BL.length $ headerForFiles $ do
        (name, _) <- files
        return $ SNGFileInfo name 0 0
      encodeFiles _       []                     = []
      encodeFiles !offset ((name, r, size) : fs) = let
        file = SNGFileInfo { name = name, size = size, offset = offset }
        in (file, r) : encodeFiles (offset + size) fs
      encoded = encodeFiles headerSize filesWithSize
      finalHeader = headerForFiles $ map fst encoded
  return $ makeHandle ".sng header" (byteStringSimpleHandle finalHeader)
    : map (cryptFile seedPattern . snd) encoded
