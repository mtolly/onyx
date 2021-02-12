{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Neversoft.Metadata where

import           Data.Bifunctor
import           Data.Binary.Get
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import qualified Data.Text            as T
import           Data.Word
import           Neversoft.Checksum
import           Neversoft.Pak
import           Neversoft.QB

type QsResult = Either Word32 T.Text

readTextPakXen :: BL.ByteString -> Either String [QBSection QsResult Word32]
readTextPakXen bs = do
  let nodes = splitPakNodes bs
  (_, qbFile) <- case filter (\(n, _) -> nodeFileType n == qbKeyCRC ".qb") nodes of
    [qb] -> return qb
    ns   -> Left $ show (length ns) <> " .qb files found"
  let qs = qsBank nodes
      qb = runGet parseQB qbFile
      lookupN mapping n = case Map.lookup n mapping of
        Nothing -> Left n
        Just s  -> Right s
  return $ map (first $ lookupN qs) qb

data SongInfo = SongInfo
  { songName       :: B.ByteString -- this is an id like "dlc747"
  , songTitle      :: T.Text
  , songArtist     :: T.Text
  , songYear       :: Int
  , songAlbumTitle :: T.Text
  , songDoubleKick :: Bool
  } deriving (Show)

parseSongInfo :: QBSection QsResult Word32 -> Either String [SongInfo]
parseSongInfo = \case
  QBSectionStruct _ _ items -> let
    itemToInfos = \case
      QBStructItemStruct _ songEntries -> do
        songName <- case [ s | QBStructItemString k s <- songEntries, k == qbKeyCRC "name" ] of
          s : _ -> Right s
          []    -> Left "parseSongInfo: couldn't get song internal name"
        songTitle <- case [ s | QBStructItemQbKeyStringQs k (Right s) <- songEntries, k == qbKeyCRC "title" ] of
          s : _ -> Right s
          []    -> Left "parseSongInfo: couldn't get song title"
        songArtist <- case [ s | QBStructItemQbKeyStringQs k (Right s) <- songEntries, k == qbKeyCRC "artist" ] of
          s : _ -> Right s
          []    -> Left "parseSongInfo: couldn't get song artist"
        songYear <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "year" ] of
          n : _ -> Right $ fromIntegral n
          []    -> Left "parseSongInfo: couldn't get song year"
        songAlbumTitle <- case [ s | QBStructItemQbKeyStringQs k (Right s) <- songEntries, k == qbKeyCRC "album_title" ] of
          s : _ -> Right s
          []    -> Left "parseSongInfo: couldn't get song album_title"
        songDoubleKick <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "double_kick" ] of
          0 : _ -> Right False
          1 : _ -> Right True
          [] -> Right False
          _ -> Left "parseSongInfo: couldn't understand double_kick field"
        Right [SongInfo{..}]
      QBStructHeader -> Right []
      _ -> Left "parseSongInfo: entry in song list that isn't a struct or header"
    in concat <$> mapM itemToInfos items
  _ -> return []
