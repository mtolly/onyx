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
import           Data.Binary.Get
import           Data.Bits            (xor, (.&.))
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        (toList)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Data.Word
import qualified GHC.IO.Handle        as H
import           Onyx.Util.Handle
import           Onyx.Xbox.STFS       (runGetM)
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

-- Should be given the 256-byte repeating pattern (seed xor'd with byte indices)
cryptFile :: BL.ByteString -> Readable -> Readable
cryptFile seedPattern orig = Readable
  { rFilePath = Nothing
  , rOpen = do
    h <- rOpen orig
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

getSNGFolder :: Bool -> SNGHeader -> Readable -> Folder T.Text Readable
getSNGFolder addINI hdr r = fromFiles $ let
  normalFiles = do
    file <- hdr.files
    path <- toList $ splitPath file.name
    let subFile = subHandle addLabel (fromIntegral file.offset) (Just $ fromIntegral file.size) r
        addLabel orig = orig <> " | " <> T.unpack file.name
        seedPattern = BL.pack $ zipWith xor (cycle $ B.unpack hdr.seed) [0 .. 255]
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
