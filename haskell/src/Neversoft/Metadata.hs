{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Neversoft.Metadata where

import           Data.Binary.Get
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import           Data.Word
import           Neversoft.Checksum
import           Neversoft.Pak
import           Neversoft.QB

readTextPakXen :: BL.ByteString -> Either String [QBSection QSResult Word32]
readTextPakXen bs = do
  let nodes = splitPakNodes bs
  (_, qbFile) <- case filter (\(n, _) -> nodeFileType n == qbKeyCRC ".qb") nodes of
    [qb] -> return qb
    ns   -> Left $ show (length ns) <> " .qb files found"
  let mappingQS = qsBank nodes
      qb = runGet parseQB qbFile
  return $ map (lookupQS mappingQS) qb

data SongInfo = SongInfo
  { songName       :: B.ByteString -- this is an id like "dlc747"
  , songTitle      :: (Word32, T.Text)
  , songArtist     :: (Word32, T.Text)
  , songYear       :: Int
  , songAlbumTitle :: (Word32, T.Text)
  , songDoubleKick :: Bool
  , songTierGuitar :: Int
  , songTierBass   :: Int
  , songTierVocals :: Int
  , songTierDrums  :: Int
  } deriving (Show)

parseSongInfo :: QBSection QSResult Word32 -> Either String [SongInfo]
parseSongInfo = \case
  QBSectionStruct _ _ items -> let
    removeL s = fromMaybe s $ T.stripPrefix "\\L" s
    itemToInfos = \case
      QBStructItemStruct _ songEntries -> do
        songName <- case [ s | QBStructItemString k s <- songEntries, k == qbKeyCRC "name" ] of
          s : _ -> Right s
          []    -> Left "parseSongInfo: couldn't get song internal name"
        songTitle <- case [ (w, removeL s) | QBStructItemQbKeyStringQs k (KnownQS w s) <- songEntries, k == qbKeyCRC "title" ] of
          p : _ -> Right p
          []    -> Left "parseSongInfo: couldn't get song title"
        songArtist <- case [ (w, removeL s) | QBStructItemQbKeyStringQs k (KnownQS w s) <- songEntries, k == qbKeyCRC "artist" ] of
          p : _ -> Right p
          []    -> Left "parseSongInfo: couldn't get song artist"
        songYear <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "year" ] of
          n : _ -> Right $ fromIntegral n
          []    -> Left "parseSongInfo: couldn't get song year"
        songAlbumTitle <- case [ (w, s) | QBStructItemQbKeyStringQs k (KnownQS w s) <- songEntries, k == qbKeyCRC "album_title" ] of
          p : _ -> Right p
          []    -> Left "parseSongInfo: couldn't get song album_title"
        songDoubleKick <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "double_kick" ] of
          0 : _ -> Right False
          1 : _ -> Right True
          [] -> Right False
          _ -> Left "parseSongInfo: couldn't understand double_kick field"
        songTierGuitar <- case [ n | QBStructItemInteger 437674840 n <- songEntries ] of
          n : _ -> Right $ fromIntegral n
          []    -> Left "parseSongInfo: couldn't get guitar tier"
        songTierBass <- case [ n | QBStructItemInteger 3733500155 n <- songEntries ] of
          n : _ -> Right $ fromIntegral n
          []    -> Left "parseSongInfo: couldn't get bass tier"
        songTierVocals <- case [ n | QBStructItemInteger 945984381 n <- songEntries ] of
          n : _ -> Right $ fromIntegral n
          []    -> Left "parseSongInfo: couldn't get vocals tier"
        songTierDrums <- case [ n | QBStructItemInteger 178662704 n <- songEntries ] of
          n : _ -> Right $ fromIntegral n
          []    -> Left "parseSongInfo: couldn't get drums tier"
        Right [SongInfo{..}]
      QBStructHeader -> Right []
      _ -> Left "parseSongInfo: entry in song list that isn't a struct or header"
    in concat <$> mapM itemToInfos items
  _ -> return []
