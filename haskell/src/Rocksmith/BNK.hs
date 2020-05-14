{-

Done with the help of
https://wiki.xentax.com/index.php/Wwise_SoundBank_(*.bnk)

-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rocksmith.BNK where

import           Control.Applicative  (liftA2)
import           Control.Monad        (replicateM)
import           Data.Binary.Get
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           Resources
import           Sound.WW2Ogg
import           System.FilePath      (takeDirectory, (<.>), (</>))

extractRSOgg :: FilePath -> FilePath -> IO ()
extractRSOgg fbnk fout = do
  codebook <- getResourcesPath "packed_codebooks_aoTuV_603.bin"
  chunks <- parseBNK . BL.fromStrict <$> B.readFile fbnk
  let hirc = chunks >>= \case
        ChunkHIRC h -> hirc_objects h
        _           -> []
  actionIDs <- case [ e | HIRCEvent e <- hirc ] of
    [x] -> return $ evt_actions x
    _   -> fail "Not exactly 1 event in .bnk HIRC"
  let actions = [ act | HIRCEventAction act <- hirc, elem (eact_id act) actionIDs ]
  soundID <- case [ act | act <- actions, eact_type act == 4 ] of
    [act] -> return $ eact_objectID act
    _     -> fail "Not exactly 1 play action in .bnk HIRC"
  sourceID <- case [ s | HIRCSound s <- hirc, snd_id s == soundID ] of
    [s] -> return $ snd_sourceID s
    _   -> fail "Sound referenced by play action not found in .bnk HIRC"
  res <- ww2ogg (WW2OggConfig { wwCodebook = codebook })
    (takeDirectory fbnk </> show sourceID <.> "wem")
    fout
  if res then return () else fail "ww2ogg failed"

parseBNK :: BL.ByteString -> [Chunk]
parseBNK bs = flip map (splitChunks bs) $ \(magic, chunk) -> case magic of
  "BKHD" -> ChunkBKHD $ runGet getBKHD chunk
  "DIDX" -> ChunkDIDX $ runGet getDIDX chunk
  "DATA" -> ChunkDATA $ runGet getDATA chunk
  "HIRC" -> ChunkHIRC $ runGet getHIRC chunk
  "STID" -> ChunkSTID $ runGet getSTID chunk
  _      -> ChunkOther magic chunk

splitChunks :: BL.ByteString -> [(B.ByteString, BL.ByteString)]
splitChunks = runGet $ let
  go = isEmpty >>= \case
    True -> return []
    False -> do
      magic <- getByteString 4
      size <- getWord32le
      chunk <- getLazyByteString $ fromIntegral size
      ((magic, chunk) :) <$> go
  in go

data Chunk
  = ChunkBKHD BKHD
  | ChunkDIDX DIDX
  | ChunkDATA DATA
  | ChunkHIRC HIRC
  | ChunkSTID STID
  | ChunkOther B.ByteString BL.ByteString
  deriving (Show)

data BKHD = BKHD
  { bkhd_version :: Word32
  , bkhd_id      :: Word32
  -- rest should be zeroes, wiki says 8 zeroes, but sample file has 20
  } deriving (Show)

getBKHD :: Get BKHD
getBKHD = do
  bkhd_version <- getWord32le
  bkhd_id <- getWord32le
  return BKHD{..}

data DIDX = DIDX
  { didx_files :: [DIDXFile]
  } deriving (Show)

getDIDX :: Get DIDX
getDIDX = let
  go = isEmpty >>= \case
    True -> return []
    False -> do
      file <- getDIDXFile
      (file :) <$> go
  in DIDX <$> go

data DIDXFile = DIDXFile
  { df_id     :: Word32
  , df_offset :: Word32
  , df_length :: Word32
  } deriving (Show)

getDIDXFile :: Get DIDXFile
getDIDXFile = do
  df_id <- getWord32le
  df_offset <- getWord32le
  df_length <- getWord32le
  return DIDXFile{..}

data DATA = DATA
  { data_bytes :: BL.ByteString
  } deriving (Show)

getDATA :: Get DATA
getDATA = DATA <$> getRemainingLazyByteString

data HIRC = HIRC
  { hirc_objects :: [HIRCObject]
  } deriving (Show)

getHIRC :: Get HIRC
getHIRC = do
  n <- getWord32le
  HIRC <$> replicateM (fromIntegral n) getHIRCObject

data HIRCObject
  = HIRCSound Sound -- type 2
  | HIRCEventAction EventAction -- type 3
  | HIRCEvent Event -- type 4
  -- type 7 (actor-mixer) also seen
  | HIRCUnknown Word32 B.ByteString
  deriving (Show)

getHIRCObject :: Get HIRCObject
getHIRCObject = do
  objType <- getWord8
  case objType of
    2 -> do
      len <- getWord32le
      start <- bytesRead
      snd_id               <- getWord32le
      snd_unknown          <- getWord32le
      snd_location         <- getWord32le
      snd_audioFileID      <- getWord32le
      snd_sourceID         <- getWord32le
      snd_bankOffsetLength <- case snd_location of
        0 -> Just <$> liftA2 (,) getWord32le getWord32le
        _ -> return Nothing
      snd_type             <- getWord8
      end <- bytesRead
      skip $ fromIntegral len - fromIntegral (end - start)
      return $ HIRCSound Sound{..}
    3 -> do
      len <- getWord32le
      start <- bytesRead
      eact_id       <- getWord32le
      eact_scope    <- getWord8
      eact_type     <- getWord8
      eact_objectID <- getWord32le
      eact_zero1    <- getWord8
      paramCount <- getWord8
      paramTypes <- replicateM (fromIntegral paramCount) getWord8
      paramValues <- replicateM (fromIntegral paramCount) $ getByteString 4
      let eact_params = zip paramTypes paramValues
      eact_zero2    <- getWord8
      end <- bytesRead
      skip $ fromIntegral len - fromIntegral (end - start)
      return $ HIRCEventAction EventAction{..}
    4 -> do
      _len <- getWord32le
      evt_id <- getWord32le
      actCount <- getWord32le
      evt_actions <- replicateM (fromIntegral actCount) getWord32le
      return $ HIRCEvent Event{..}
    _ -> do
      len <- getWord32le
      unkID <- getWord32le
      bytes <- getByteString $ fromIntegral len - 4
      return $ HIRCUnknown unkID bytes

data Sound = Sound
  { snd_id               :: Word32
  , snd_unknown          :: Word32
  , snd_location         :: Word32 -- 0, 1, 2
  , snd_audioFileID      :: Word32
  , snd_sourceID         :: Word32
  , snd_bankOffsetLength :: Maybe (Word32, Word32)
  , snd_type             :: Word8
  -- more stuff we don't care about
  } deriving (Show)

data Event = Event
  { evt_id      :: Word32
  , evt_actions :: [Word32]
  } deriving (Show)

data EventAction = EventAction
  { eact_id       :: Word32
  , eact_scope    :: Word8
  , eact_type     :: Word8
  , eact_objectID :: Word32
  , eact_zero1    :: Word8
  , eact_params   :: [(Word8, B.ByteString)]
  , eact_zero2    :: Word8
  -- more stuff we don't care about
  } deriving (Show)

data STID = STID
  { stid_unknown :: Word32
  , stid_banks   :: [(Word32, B.ByteString)]
  } deriving (Show)

getSTID :: Get STID
getSTID = do
  stid_unknown <- getWord32le
  bankCount <- getWord32le
  stid_banks <- replicateM (fromIntegral bankCount) $ do
    bankID <- getWord32le
    len <- getWord8
    name <- getByteString $ fromIntegral len
    return (bankID, name)
  return STID{..}
