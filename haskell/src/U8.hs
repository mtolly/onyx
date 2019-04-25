{- |
Written with reference to
- http://wiibrew.org/wiki/U8_archive
- u8it by icefire: https://github.com/Plombo/romextract/blob/master/src/u8it.c
-}
{-# LANGUAGE BangPatterns #-}
module U8 (packU8) where

import           Data.Binary.Put        (Put, putByteString, putWord16be,
                                         putWord32be, putWord8, runPut)
import           Data.Bits              (shiftR)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Lazy   as BL
import           Data.Maybe             (fromMaybe)
import           Data.Word              (Word32)
import           System.Directory.Extra (getFileSize, listDirectories,
                                         listFiles)
import           System.FilePath        (dropTrailingPathSeparator,
                                         takeFileName)
import           System.IO              (IOMode (..), SeekMode (..), hSeek,
                                         withBinaryFile)

data Folder = Folder
  { folderName    :: B8.ByteString
  , folderLevel   :: Int -- 0 for root and root's children, 1 for root's grandchildren, etc.
  , folderFiles   :: [File]
  , folderFolders :: [Folder]
  }

data File = File
  { fileSize :: Int
  , filePath :: FilePath -- on pc filesystem, not u8
  , fileName :: B8.ByteString
  }

getRoot :: FilePath -> IO Folder
getRoot = getFolder Nothing where
  getFolder lvl fp = do
    dirs <- listDirectories fp
    files <- filter ((/= ".") . take 1 . takeFileName) <$> listFiles fp
    let name = case lvl of
          Nothing -> ""
          Just _  -> takeFileName $ dropTrailingPathSeparator fp
        lvl' = Just $ maybe 0 (+ 1) lvl
    dirs' <- mapM (getFolder lvl') dirs
    files' <- mapM getFile files
    return Folder
      { folderName = B8.pack name
      , folderLevel = fromMaybe 0 lvl
      , folderFiles = files'
      , folderFolders = dirs'
      }
  getFile fp = do
    size <- fromIntegral <$> getFileSize fp
    return File
      { fileSize = size
      , filePath = fp
      , fileName = B8.pack $ takeFileName fp
      }

writeU8 :: FilePath -> Folder -> IO ()
writeU8 out root = withBinaryFile out WriteMode $ \hout -> do

  let align a n = case quotRem n a of
        (_, 0) -> n
        (q, _) -> a * (q + 1)
      lookup' x alist = case lookup x alist of
        Nothing -> error $ "writeU8: panic! value not found in lookup table: " ++ show x
        Just y  -> y

  -- string table
  let folderNames d = folderName d :
        (map fileName (folderFiles d) ++ concatMap folderNames (folderFolders d))
      nameOffsets _    []       = []
      nameOffsets !off (n : ns) = (n, off) : nameOffsets (off + B.length n + 1) ns
      names = folderNames root
      nameLookup = nameOffsets 0 names
      nameTable = BL.fromChunks $ names >>= \n -> [n, B.singleton 0]

  -- data table
  let pathSizes d
        =  map (\f -> (filePath f, fileSize f, align 0x20 $ fileSize f)) (folderFiles d)
        ++ concatMap pathSizes (folderFolders d)
      dataOffsets _    []                  = []
      dataOffsets !off ((fp, _, aligned) : rest) = (fp, off) : dataOffsets (off + aligned) rest
      datas = pathSizes root
      dataLookup = dataOffsets 0 datas
      writeData (fp, size, aligned) = do
        withBinaryFile fp ReadMode $ \hin -> BL.hGetContents hin >>= BL.hPut hout
        B.hPut hout $ B.replicate (aligned - size) 0

  -- dir/file nodes
  let nodes = dirToNodes 1 root
      putWord24be :: Word32 -> Put
      putWord24be n = do
        putWord8 $ fromIntegral $ n `shiftR` 16
        putWord16be $ fromIntegral n
      fileToNode f = runPut $ do
        putWord8 0
        putWord24be $ fromIntegral $ lookup' (fileName f) nameLookup
        putWord32be $ fromIntegral  (lookup' (filePath f) dataLookup) + alignedDataOffset
        putWord32be $ fromIntegral $ fileSize f
      subdirNodes _  []       = []
      subdirNodes !n (d : ds) = let
        dnodes = dirToNodes n d
        in dnodes ++ subdirNodes (n + length dnodes) ds
      dirToNodes n dir = let
        contents = subdirNodes (n + 1) (folderFolders dir) ++ map fileToNode (folderFiles dir)
        top = runPut $ do
          putWord8 1
          putWord24be $ fromIntegral $ lookup' (folderName dir) nameLookup
          putWord32be $ fromIntegral $ folderLevel dir
          putWord32be $ fromIntegral $ n + length contents
        in top : contents

      headerSize = fromIntegral (length nodes * 12) + fromIntegral (BL.length nameTable) -- nodes size + string table size
      alignedDataOffset = align 0x40 $ 0x20 + headerSize

  BL.hPut hout $ runPut $ do
    putWord32be 0x55AA382D -- tag, "U.8-"
    putWord32be 0x20 -- offset to root node
    putWord32be headerSize
    putWord32be alignedDataOffset
    putByteString $ B.replicate 16 0
  mapM_ (BL.hPut hout) nodes
  BL.hPut hout nameTable
  hSeek hout AbsoluteSeek $ fromIntegral alignedDataOffset
  mapM_ writeData datas

packU8 :: FilePath -> FilePath -> IO ()
packU8 dir fout = getRoot dir >>= writeU8 fout
