{- |
This code was messily ported from the much more well-written py360:
https://github.com/arkem/py360
-}
module STFS.Extract (extractSTFS, withSTFS, STFSContents(..)) where

import           Control.Monad
import           Control.Monad.Loops        (whileM_)
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Int
import           Data.IORef
import qualified Data.Map                   as Map
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Data.Word
import           System.Directory
import           System.FilePath
import           System.IO

table_spacing :: [(Integer, Integer, Integer)]
table_spacing = [(0xAB, 0x718F, 0xFE7DA), (0xAC, 0x723A, 0xFD00B)]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

data STFSContents = STFSContents
  { stfsDirectories :: [FilePath]
  , stfsFiles       :: [(FilePath, IO BL.ByteString)]
  }

extractSTFS :: FilePath -> FilePath -> IO ()
extractSTFS stfs dir = withSTFS stfs $ \contents -> do
  forM_ (stfsDirectories contents) $ createDirectoryIfMissing True . (dir </>)
  forM_ (stfsFiles contents) $ \(path, getFile) -> do
    getFile >>= BL.writeFile (dir </> path)

withSTFS :: FilePath -> (STFSContents -> IO a) -> IO a
withSTFS stfs fn = withBinaryFile stfs ReadMode $ \fd -> do
  header <- BL.hGet fd 0x971A
  let -- volume_descriptor_size = BL.index header 0x379
      -- block_seperation = BL.index header 0x37B
      filetable_blockcount = runGet getWord16le $ BL.take 2 $ BL.drop (0x37A + 2) header -- Little Endian. Why?
      filetable_blocknumber = runGet getWord32le $ BL.snoc (BL.take 3 $ BL.drop (0x37A + 4) header) 0 -- Why?!?
      -- tophashtable_hash = BL.take 0x14 $ BL.drop (0x37A + 7) header
      allocated_count = runGet getWord32be $ BL.take 4 $ BL.drop (0x37A + 0x1B) header
      -- unallocated_count = runGet getWord32be $ BL.take 4 $ BL.drop (0x37A + 0x1F) header
      -- datafile_count = runGet getWord32be $ BL.take 4 $ BL.drop 0x39D header
      -- datafile_size = runGet getWord64be $ BL.take 8 $ BL.drop 0x3A1 header

      entry_id = runGet getWord32be $ BL.take 4 $ BL.drop 0x340 header
      table_size_shift = if ((entry_id + 0xFFF) .&. 0xF000) `shiftR` 0xC == 0xB
        then 0
        else 1

      fix_blocknum block_num = let
        block_adjust = sum
          [ if block_num >= 0xAA then ((block_num `div` 0xAA) + 1) `shiftL` table_size_shift else 0
          , if block_num > 0x70E4 then ((block_num `div` 0x70E4) + 1) `shiftL` table_size_shift else 0
          ]
        in block_num + block_adjust

      read_block blocknum len = do
        -- Read a block given its block number
        -- If reading data blocks call fix_blocknum first
        hSeek fd AbsoluteSeek $ 0xc000 + blocknum * 0x1000
        BL.hGet fd len

      get_blockhash blocknum table_offset = do
        -- Given a block number return the hash object that goes with it
        let record = toInteger $ blocknum `mod` 0xAA
        -- Num tables * space blocks between each (0xAB or 0xAC for [0])
        let tablenum = sum
              [ (blocknum `div` 0xAA) * fst3 (table_spacing !! table_size_shift)
              , if blocknum >= 0xAA
                then (blocknum `div` 0x70E4 + 1) `shiftL` table_size_shift -- skip level 1 tables
                else 0
              , if blocknum >= 0x70E4
                then 1 `shiftL` table_size_shift -- If we're into level 2 add the level 2 table
                else 0
              -- Read the table block, get the correct record and pass it to BlockHashRecord
              -- Fix to point at the first table (these numbers are offset from data block numbers)
              , table_offset - (1 `shiftL` table_size_shift)
              ]
        hashdata <- read_block tablenum 0x1000
        return $ newBlockHashRecord blocknum
          (BL.take 0x18 $ BL.drop (fromIntegral record * 0x18) hashdata)
          tablenum record

  filelistings <- newIORef []
  allfiles <- newIORef Map.empty
  data_ <- do -- read_filetable
    let firstblock = filetable_blocknumber
        numblocks = filetable_blockcount
    buf <- newIORef BL.empty
    info <- newIORef 0x80
    block <- newIORef firstblock
    forM_ [0 .. numblocks - 1] $ \_ -> do
      b <- readIORef block
      read_block_result <- read_block (fix_blocknum $ toInteger b) 0x1000
      modifyIORef buf (`BL.append` read_block_result)
      blockhash <- get_blockhash (toInteger b) 0
      blockhash' <- if table_size_shift > 0 && blockhash_info blockhash < 0x80
        then get_blockhash (toInteger b) 1
        else return blockhash
      writeIORef block $ blockhash_nextblock blockhash'
      writeIORef info $ blockhash_info blockhash'
    readIORef buf

  forM_ [0, 0x40 .. BL.length data_ - 1] $ \x -> do
    case newFileListing $ BL.take 0x40 $ BL.drop x data_ of
      Left _   -> return ()
      Right fl -> modifyIORef filelistings (++ [fl])
  fls <- readIORef filelistings
  forM_ fls $ \fl -> do
    path_components <- newIORef [fl_filename fl]
    a <- newIORef fl
    whileM_ (readIORef a >>= \av -> return $ fl_pathindex av /= (-1) && fl_pathindex av < fromIntegral (length fls)) $ do
      av <- readIORef a
      writeIORef a $ fls !! fromIntegral (fl_pathindex av)
      av' <- readIORef a
      modifyIORef path_components (++ [fl_filename av'])
    modifyIORef path_components (++ [BL.empty])
    modifyIORef path_components reverse
    pcomps <- readIORef path_components
    modifyIORef allfiles $ Map.insert (BL8.intercalate (BL8.pack "/") pcomps) fl

  let read_file filelisting = do
        buf <- newIORef BL.empty
        size <- newIORef $ fl_size filelisting
        block <- newIORef $ fl_firstblock filelisting
        info <- newIORef 0x80
        let check = do
              s <- readIORef size
              b <- readIORef block
              i <- readIORef info
              return $ s > 0 && b > 0 && b < allocated_count && i >= 0x80
        whileM_ check $ do
          readlen <- min 0x1000 <$> readIORef size
          b <- readIORef block
          rb <- read_block (fix_blocknum $ toInteger b) $ fromIntegral readlen
          modifyIORef buf (`BL.append` rb)
          modifyIORef size $ subtract readlen
          blockhash <- get_blockhash (toInteger b) 0
          blockhash' <- if table_size_shift > 0 && blockhash_info blockhash < 0x80
            then get_blockhash (toInteger b) 1
            else return blockhash
          writeIORef block $ blockhash_nextblock blockhash'
          writeIORef info $ blockhash_info blockhash'
        readIORef buf

  files <- Map.toAscList <$> readIORef allfiles
  let readFilename bs = dropWhile (== '/') (TL.unpack $ TLE.decodeUtf8 bs) -- not sure if actually utf-8
  fn $ STFSContents
    { stfsDirectories = do
      (filename, filelisting) <- files
      guard $ fl_isdirectory filelisting
      return $ readFilename filename
    , stfsFiles = do
      (filename, filelisting) <- files
      guard $ not $ fl_isdirectory filelisting
      return (readFilename filename, read_file filelisting)
    }

data BlockHashRecord = BlockHashRecord
  { blockhash_record    :: Integer
  , blockhash_table     :: Integer
  , blockhash_blocknum  :: Integer
  , blockhash_hash      :: BL.ByteString
  , blockhash_info      :: Word8
  , blockhash_nextblock :: Word32
  } deriving (Show)

instance Eq BlockHashRecord where
  bhr1 == bhr2 = blockhash_hash bhr1 == blockhash_hash bhr2

newBlockHashRecord :: Integer -> BL.ByteString -> Integer -> Integer -> BlockHashRecord
newBlockHashRecord blocknum data_ table record = BlockHashRecord
  { blockhash_record = record
  , blockhash_table = table
  , blockhash_blocknum = blocknum
  , blockhash_hash = BL.take 0x14 data_
  , blockhash_info = BL.index data_ 0x14
  , blockhash_nextblock = runGet getWord32be $ BL.cons 0 $ BL.take 3 $ BL.drop 0x15 data_
  }

data FileListing = FileListing
  { fl_filename    :: BL.ByteString
  , fl_isdirectory :: Bool
  , fl_numblocks   :: Word32
  , fl_firstblock  :: Word32
  , fl_pathindex   :: Int16
  , fl_size        :: Word32
  , fl_udate       :: Word16
  , fl_utime       :: Word16
  , fl_adate       :: Word16
  , fl_atime       :: Word16
  } deriving (Show)

newFileListing :: BL.ByteString -> Either String FileListing
newFileListing data_ = let
  stripNull = BL.reverse . BL.dropWhile (== 0) . BL.reverse . BL.dropWhile (== 0)
  filename = stripNull $ BL.take 0x28 data_
  in if BL.null filename
    then Left "FileListing has empty filename"
    else Right FileListing
      { fl_filename = filename
      , fl_isdirectory = 0x80 .&. BL.index data_ 0x28 == 0x80
      , fl_numblocks = runGet getWord32le $ BL.snoc (BL.take 3 $ BL.drop 0x29 data_) 0 -- struct.unpack("<I", "%s\x00" % data[0x29:0x29+3])[0] # More little endian madness!
      , fl_firstblock = runGet getWord32le $ BL.snoc (BL.take 3 $ BL.drop 0x2F data_) 0 -- struct.unpack("<I", "%s\x00" % data[0x2F:0x2F+3])[0]# And again!
      , fl_pathindex = runGet getInt16be $ BL.take 2 $ BL.drop 0x32 data_ -- struct.unpack(">h", data[0x32:0x34])[0] # Signedness is important here
      , fl_size = runGet getWord32be $ BL.take 4 $ BL.drop 0x34 data_ -- struct.unpack(">I", data[0x34:0x38])[0]
      , fl_udate = runGet getWord16be $ BL.take 2 $ BL.drop 0x38 data_ -- struct.unpack(">H", data[0x38:0x3A])[0]
      , fl_utime = runGet getWord16be $ BL.take 2 $ BL.drop 0x3A data_ -- struct.unpack(">H", data[0x3A:0x3C])[0]
      , fl_adate = runGet getWord16be $ BL.take 2 $ BL.drop 0x3C data_ -- struct.unpack(">H", data[0x3C:0x3E])[0]
      , fl_atime = runGet getWord16be $ BL.take 2 $ BL.drop 0x3E data_ -- struct.unpack(">H", data[0x3E:0x40])[0]
      }
