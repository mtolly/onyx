{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Harmonix.Ark where

import           Control.Monad            (forM, replicateM, when)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Bifunctor           (first)
import           Data.Binary.Get          (getByteString, getInt32le,
                                           getWord32le, getWord64le, runGet)
import           Data.Bits
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as B8
import qualified Data.ByteString.Lazy     as BL
import           Data.Int
import qualified Data.List.NonEmpty       as NE
import           Data.SimpleHandle
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           Data.Word                (Word32)
import           PlayStation.PKG.Backport (lazyPackZipWith)
import           STFS.Package             (runGetM)
import           System.FilePath          (dropExtension, (-<.>))

data FileEntry a = FileEntry
  { fe_offset  :: Integer
  , fe_name    :: a
  , fe_folder  :: Maybe a
  , fe_size    :: Word32
  , fe_inflate :: Word32 -- inflated size, or 0 if not gzipped
  } deriving (Eq, Show)

data Hdr = Hdr
  { hdr_Arks  :: Maybe [Integer] -- sizes of each ark
  , hdr_Files :: [FileEntry B.ByteString]
  } deriving (Eq, Show)

-- Read a .hdr, or an .ark for old games with no .hdr
readHdr :: (MonadFail m) => BL.ByteString -> m Hdr
readHdr bs = do
  testType <- runGetM getWord32le bs
  let dec = if testType <= 10 then bs else decryptHdr bs
      parseEntry parseOffsetIndex = do
        fe_offset <- fromIntegral <$> parseOffsetIndex
        fe_name <- getInt32le
        fe_folder <- (\i -> if i == -1 then Nothing else Just i) <$> getInt32le
        fe_size <- getWord32le
        fe_inflate <- getWord32le
        return FileEntry{..}
      findString offsets bytes i = case offsets V.!? fromIntegral i of
        Nothing     -> fail $ "Couldn't find ARK string index " <> show i
        Just offset -> return $ B.takeWhile (/= 0) $ B.drop (fromIntegral offset) bytes
      parseEntries parseOffsetIndex = do
        entryCount <- getWord32le
        replicateM (fromIntegral entryCount) $ parseEntry parseOffsetIndex
      parseStrings = do
        stringSize <- getWord32le
        getByteString $ fromIntegral stringSize
      parseOffsets = do
        offsetCount <- getWord32le
        V.replicateM (fromIntegral offsetCount) getWord32le
  flip runGetM dec $ do
    arkVersion <- getWord32le
    case arkVersion of
      -- ARK v2: Amplitude (PS2)
      2 -> do
        entries <- parseEntries getWord32le
        stringBytes <- parseStrings
        offsets <- parseOffsets
        entries' <- forM entries $ \entry -> do
          name <- findString offsets stringBytes $ fe_name entry
          folder <- mapM (findString offsets stringBytes) $ fe_folder entry
          return entry { fe_name = name, fe_folder = folder }
        return Hdr { hdr_Arks = Nothing, hdr_Files = entries' }
      -- ARK v3: Guitar Hero 1/2/80s
      3 -> do
        arkCount <- getWord32le
        arkCount2 <- getWord32le
        when (arkCount /= arkCount2) $ fail $ "ARK version 3: ark counts don't match (" <> show arkCount <> " and " <> show arkCount2 <> ")"
        arkSizes <- replicateM (fromIntegral arkCount) getWord32le
        stringBytes <- parseStrings
        offsets <- parseOffsets
        entries <- parseEntries getWord32le
        entries' <- forM entries $ \entry -> do
          name <- findString offsets stringBytes $ fe_name entry
          folder <- mapM (findString offsets stringBytes) $ fe_folder entry
          return entry { fe_name = name, fe_folder = folder }
        return Hdr { hdr_Arks = Just $ map fromIntegral arkSizes, hdr_Files = entries' }
      -- ARK v4: Rock Band
      4 -> do
        arkCount <- getWord32le
        arkCount2 <- getWord32le
        when (arkCount /= arkCount2) $ fail $ "ARK version 4: ark counts don't match (" <> show arkCount <> " and " <> show arkCount2 <> ")"
        arkSizes <- replicateM (fromIntegral arkCount) getWord64le -- new in v4: each ark size is 8 bytes
        stringBytes <- parseStrings
        offsets <- parseOffsets
        entries <- parseEntries getWord64le -- new in v4: each entry's offset index is 8 bytes
        entries' <- forM entries $ \entry -> do
          name <- findString offsets stringBytes $ fe_name entry
          folder <- mapM (findString offsets stringBytes) $ fe_folder entry
          return entry { fe_name = name, fe_folder = folder }
        return Hdr { hdr_Arks = Just $ map fromIntegral arkSizes, hdr_Files = entries' }
      _ -> fail $ "Unsupported ARK version " <> show arkVersion

-- Decrypts .hdr for ARK version 4 and later
decryptHdr :: BL.ByteString -> BL.ByteString
decryptHdr enc = let
  cryptRound :: Int32 -> Int32
  cryptRound key = let
    ret = (key - ((key `quot` 0x1F31D) * 0x1F31D)) * 0x41A7 - (key `quot` 0x1F31D) * 0xB14
    in if ret <= 0
      then ret + 0x7FFFFFFF
      else ret
  initKey = cryptRound $ runGet getInt32le enc
  initCryptStream = BL.pack $ map fromIntegral $ iterate cryptRound initKey
  testArkVersion = runGet getInt32le $ lazyPackZipWith xor (BL.take 4 $ BL.drop 4 enc) initCryptStream
  cryptStream = if testArkVersion < 0
    then BL.map complement initCryptStream
    else initCryptStream
  in lazyPackZipWith xor (BL.drop 4 enc) cryptStream

entryFolder :: Hdr -> Folder B.ByteString (FileEntry B.ByteString)
entryFolder entries = fromFiles $ flip map (hdr_Files entries) $ \entry -> let
  path = case fe_folder entry >>= NE.nonEmpty . B8.split '/' of
    Just dir -> dir <> return (fe_name entry)
    Nothing  -> return (fe_name entry)
  in (path, entry)

selectArk :: (MonadFail m) => Hdr -> Integer -> m (Int, Integer)
selectArk hdr offset = case hdr_Arks hdr of
  Nothing    -> return (0, offset)
  Just sizes -> let
    go i remaining curOffset = case remaining of
      [] -> fail $ "Offset extends past all .ARK files: " <> show curOffset
      size : rest -> if curOffset < size
        then return (i, curOffset)
        else go (i + 1) rest $ curOffset - size
    in go (0 :: Int) sizes offset

readFileEntry :: (MonadFail m) => Hdr -> [Readable] -> FileEntry B.ByteString -> m Readable
readFileEntry hdr arks entry = do
  let path = maybe id (\dir f -> dir <> "/" <> f) (fe_folder entry) (fe_name entry)
  (arkIndex, offset) <- selectArk hdr $ fromIntegral $ fe_offset entry
  ark <- case drop arkIndex arks of
    []      -> fail $ "Couldn't find .ARK index " <> show arkIndex
    ark : _ -> return ark
  return $ subHandle
    (<> (" | " <> B8.unpack path))
    offset
    (Just $ fromIntegral $ fe_size entry)
    ark

getFileArks :: Hdr -> FilePath -> [FilePath]
getFileArks hdr hdrPath = case hdr_Arks hdr of
  Nothing -> [hdrPath -<.> "ARK"]
  Just sizes -> zipWith
    (\i _size -> dropExtension hdrPath <> "_" <> show (i :: Int) <> ".ARK")
    [0..]
    sizes

extractArk :: (MonadIO m, MonadFail m) => Hdr -> [Readable] -> FilePath -> m ()
extractArk hdr arks dout = do
  let legalPaths = first
        -- handles guitar hero arks where weird paths start with "../../"
        (\case ".." -> "dotdot"; p -> T.pack $ B8.unpack p)
        (entryFolder hdr)
  readables <- mapM (readFileEntry hdr arks) legalPaths
  liftIO $ saveHandleFolder readables dout
