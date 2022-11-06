{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Harmonix.Ark where

import           Control.Monad                (forM, replicateM, when)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.Bifunctor               (first)
import           Data.Binary.Get              (getByteString, getInt32le,
                                               getWord32le, getWord64le, runGet,
                                               skip)
import           Data.Bits
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import           Data.Int
import           Data.List.HT                 (partitionMaybe)
import qualified Data.List.NonEmpty           as NE
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import           Data.Word                    (Word32)
import           Onyx.Util.ByteStringBackport (lazyPackZipWith)
import           Onyx.Util.Handle
import           Onyx.Xbox.STFS               (runGetM)
import           System.FilePath              (dropExtension, takeFileName,
                                               (-<.>))

data FileEntry a = FileEntry
  { fe_offset  :: Integer
  , fe_name    :: a
  , fe_folder  :: Maybe a
  , fe_size    :: Word32
  , fe_inflate :: Word32 -- inflated size, or 0 if not gzipped
  } deriving (Eq, Show)

data ArkReference = ArkReference
  { ark_path :: Maybe B.ByteString -- in v5 and up
  , ark_size :: Integer
  } deriving (Eq, Show)

data Hdr = Hdr
  { hdr_Arks  :: Maybe [ArkReference] -- sizes (+ maybe paths) of each ark (or Nothing if no separate hdr/ark)
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
        -- new in v3: separate hdr/ark, support split ark, move file entries to end
        arkCount <- getWord32le
        arkCount2 <- getWord32le
        when (arkCount /= arkCount2) $ fail $ "ARK version 3: ark counts don't match (" <> show arkCount <> " and " <> show arkCount2 <> ")"
        arkSizes <- replicateM (fromIntegral arkCount) $ do
          size <- getWord32le
          return ArkReference { ark_path = Nothing, ark_size = fromIntegral size }
        stringBytes <- parseStrings
        offsets <- parseOffsets
        entries <- parseEntries getWord32le
        entries' <- forM entries $ \entry -> do
          name <- findString offsets stringBytes $ fe_name entry
          folder <- mapM (findString offsets stringBytes) $ fe_folder entry
          return entry { fe_name = name, fe_folder = folder }
        return Hdr { hdr_Arks = Just arkSizes, hdr_Files = entries' }
      -- ARK v4: Rock Band, Rock Band 2 (PS2)
      4 -> do
        arkCount <- getWord32le
        arkCount2 <- getWord32le
        when (arkCount /= arkCount2) $ fail $ "ARK version 4: ark counts don't match (" <> show arkCount <> " and " <> show arkCount2 <> ")"
        arkSizes <- replicateM (fromIntegral arkCount) $ do
          size <- getWord64le -- only in v4: each ark size is 8 bytes
          return ArkReference { ark_path = Nothing, ark_size = fromIntegral size }
        stringBytes <- parseStrings
        offsets <- parseOffsets
        entries <- parseEntries getWord64le -- new in v4: each entry's offset index is 8 bytes
        entries' <- forM entries $ \entry -> do
          name <- findString offsets stringBytes $ fe_name entry
          folder <- mapM (findString offsets stringBytes) $ fe_folder entry
          return entry { fe_name = name, fe_folder = folder }
        return Hdr { hdr_Arks = Just arkSizes, hdr_Files = entries' }
      -- ARK v5: Rock Band 2 (360/PS3)
      5 -> do
        arkCount <- getWord32le
        arkCount2 <- getWord32le
        when (arkCount /= arkCount2) $ fail $ "ARK version 5: ark counts don't match (" <> show arkCount <> " and " <> show arkCount2 <> ")"
        arkSizes <- replicateM (fromIntegral arkCount) getWord32le
        -- new in v5: paths to ark files instead of being implicit
        arkCount3 <- getWord32le
        when (arkCount /= arkCount3) $ fail $ "ARK version 5: ark count doesn't match ark path count (" <> show arkCount <> " and " <> show arkCount3 <> ")"
        arkPaths <- replicateM (fromIntegral arkCount3) $ getWord32le >>= getByteString . fromIntegral
        let arks = zipWith ArkReference (map Just arkPaths) (map fromIntegral arkSizes)
        stringBytes <- parseStrings
        offsets <- parseOffsets
        entries <- parseEntries getWord64le
        entries' <- forM entries $ \entry -> do
          name <- findString offsets stringBytes $ fe_name entry
          folder <- mapM (findString offsets stringBytes) $ fe_folder entry
          return entry { fe_name = name, fe_folder = folder }
        return Hdr { hdr_Arks = Just arks, hdr_Files = entries' }
      -- ARK v6: Rock Band 3
      6 -> do
        skip 20 -- new: "Versions 6,7 have some sort of hash/key at the beginning?"
        arkCount <- getWord32le
        arkCount2 <- getWord32le
        when (arkCount /= arkCount2) $ fail $ "ARK version 6: ark counts don't match (" <> show arkCount <> " and " <> show arkCount2 <> ")"
        arkSizes <- replicateM (fromIntegral arkCount) getWord32le
        arkCount3 <- getWord32le
        when (arkCount /= arkCount3) $ fail $ "ARK version 6: ark count doesn't match ark path count (" <> show arkCount <> " and " <> show arkCount3 <> ")"
        arkPaths <- replicateM (fromIntegral arkCount3) $ getWord32le >>= getByteString . fromIntegral
        let arks = zipWith ArkReference (map Just arkPaths) (map fromIntegral arkSizes)
        -- new: "Versions 6,7,9: appear to have checksums or something for each content file?"
        arkCount4 <- getWord32le
        when (arkCount /= arkCount4) $ fail $ "ARK version 6: ark count doesn't match unknown checksum count (" <> show arkCount <> " and " <> show arkCount4 <> ")"
        skip $ 4 * fromIntegral arkCount4
        stringBytes <- parseStrings
        offsets <- parseOffsets
        entries <- parseEntries getWord64le
        entries' <- forM entries $ \entry -> do
          name <- findString offsets stringBytes $ fe_name entry
          folder <- mapM (findString offsets stringBytes) $ fe_folder entry
          return entry { fe_name = name, fe_folder = folder }
        return Hdr { hdr_Arks = Just arks, hdr_Files = entries' }
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
      ark : rest -> if curOffset < ark_size ark
        then return (i, curOffset)
        else go (i + 1) rest $ curOffset - ark_size ark
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

getFileArks :: Hdr -> T.Text -> [T.Text]
getFileArks hdr hdrName = case hdr_Arks hdr of
  Nothing -> [T.pack $ T.unpack hdrName -<.> "ARK"]
  Just arks -> zipWith
    (\i ark -> T.pack $ case ark_path ark of
      -- paths are e.g. "gen/main_ps3_0.ark"
      Just p  -> takeFileName $ B8.unpack p
      Nothing -> dropExtension (T.unpack hdrName) <> "_" <> show (i :: Int) <> ".ARK"
    )
    [0..]
    arks

extractArk :: (MonadIO m, MonadFail m) => Hdr -> [Readable] -> FilePath -> m ()
extractArk hdr arks dout = do
  let legalPaths = first
        -- handles guitar hero arks where weird paths start with "../../"
        (\case ".." -> "dotdot"; p -> T.pack $ B8.unpack p)
        (entryFolder hdr)
  readables <- mapM (readFileEntry hdr arks) legalPaths
  liftIO $ saveHandleFolder readables dout

searchGEN :: Folder T.Text Readable -> IO (Maybe (Hdr, [Readable]))
searchGEN gen = do
  let findHDR []             = Nothing
      findHDR (name : names) = case findFileCI (pure name) gen of
        Nothing -> findHDR names
        Just r  -> Just (name, r)
  case findHDR ["main.hdr", "main_xbox.hdr", "main_ps3.hdr"] of
    Nothing        -> return Nothing
    Just (name, r) -> do
      hdr <- useHandle r handleToByteString >>= readHdr
      let arkPaths = getFileArks hdr name
          (found, notFound) = partitionMaybe (\ark -> findFileCI (pure ark) gen) arkPaths
      case notFound of
        [] -> return $ Just (hdr, found)
        _  -> fail $ ".ARK files couldn't be found: " <> show notFound

loadGEN :: Folder T.Text Readable -> IO (Hdr, [Readable])
loadGEN gen = searchGEN gen >>= maybe (fail "Couldn't locate .hdr in GEN folder") return

loadArkFolder :: (MonadFail m) => Hdr -> [Readable] -> m (Folder B.ByteString Readable)
loadArkFolder hdr arks = mapM (readFileEntry hdr arks) $ entryFolder hdr
