{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Neversoft.Metadata where

import           Control.Monad        (forM)
import           Data.Binary.Get
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.List.Extra      (nubOrdOn)
import           Data.Maybe           (fromMaybe, listToMaybe)
import qualified Data.Text            as T
import           Data.Word
import           Genre
import           Neversoft.Checksum
import           Neversoft.Pak
import           Neversoft.QB

data TextPakQB = TextPakQB
  { textPakFileKey     :: Word32
  , textPakSongStructs :: [(Word32, [QBStructItem QSResult Word32])]
  } deriving (Show)

readTextPakQB :: BL.ByteString -> Either String TextPakQB
readTextPakQB bs = do
  let nodes = splitPakNodes bs
  (_, qbFile) <- case filter (\(n, _) -> nodeFileType n == qbKeyCRC ".qb" && nodeFilenameCRC n == 1379803300) nodes of
    [qb] -> return qb
    ns   -> Left $ show (length ns) <> " .qb files found"
  qsPair <- case filter (\(n, _) -> nodeFileType n == qbKeyCRC ".qs.en" && nodeFilenameCRC n == 1379803300) nodes of
    [qs] -> return qs
    ns   -> Left $ show (length ns) <> " .qs.en files found"
  let mappingQS = qsBank [qsPair]
      qb = map (lookupQS mappingQS) $ runGet parseQB qbFile
  case qb of
    [   QBSectionArray  3796209450 n1 (QBArrayOfQbKey keys)
      , QBSectionStruct 4087958085 n2 (QBStructHeader : structs)
      ] -> if n1 /= n2
        then Left "File keys don't match in the sections of _text.pak"
        else do
          structs' <- forM structs $ \case
            QBStructItemStruct k struct -> Right (k, struct)
            item -> Left $ "Unexpected item in _text.pak instead of song struct: " <> show item
          if keys /= map fst structs'
            then Left "Initial keys list does not match keys in the struct list in _text.pak"
            else Right $ TextPakQB n1 structs'
    _ -> Left "Couldn't match QB node pattern in _text.pak"

combineTextPakQBs :: [TextPakQB] -> [(Word32, [QBStructItem QSResult Word32])]
combineTextPakQBs = nubOrdOn fst . concatMap textPakSongStructs

showTextPakQBQS :: TextPakQB -> (BL.ByteString, BL.ByteString)
showTextPakQBQS contents = let
  qb =
    [ QBSectionArray 3796209450 (textPakFileKey contents)
      $ QBArrayOfQbKey $ map fst $ textPakSongStructs contents
    , QBSectionStruct 4087958085 (textPakFileKey contents)
      $ QBStructHeader : map (uncurry QBStructItemStruct) (textPakSongStructs contents)
    ]
  qs = nubOrdOn fst $ do
    (_, items) <- textPakSongStructs contents
    item <- items
    KnownQS k txt <- allQS item
    return (k, txt)
  in (putQB $ discardQS qb, makeQS qs)

updateTextPakQB :: [(Word32, [QBStructItem QSResult Word32])] -> BL.ByteString -> BL.ByteString
updateTextPakQB library bs = let
  nodes = splitPakNodes bs
  nodes' = flip map nodes $ \pair@(n, _) -> if
    | nodeFileType n == qbKeyCRC ".qb" && nodeFilenameCRC n == 1379803300 -> let
      (qb, _) = showTextPakQBQS $ TextPakQB (nodeFilenameKey n) library
      in (n, qb)
    | elem (nodeFileType n) (map qbKeyCRC [".qs.en", ".qs.es", ".qs.it", ".qs.de", ".qs.fr"]) && nodeFilenameCRC n == 1379803300 -> let
      (_, qs) = showTextPakQBQS $ TextPakQB (nodeFilenameKey n) library
      in (n, qs)
    | otherwise -> pair
  in buildPak nodes'

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
  , songGenre      :: Maybe GenreWoR
  } deriving (Show)

parseSongInfoStruct :: [QBStructItem QSResult Word32] -> Either String SongInfo
parseSongInfoStruct songEntries = do
  let removeL s = fromMaybe s $ T.stripPrefix "\\L" s
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
    []    -> Right False
    _     -> Left "parseSongInfo: couldn't understand double_kick field"
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
  let songGenre = case [ n | QBStructItemQbKey k n <- songEntries, k == qbKeyCRC "genre" ] of
        [] -> Nothing
        n : _ -> listToMaybe $ filter (\wor -> qbWoRGenre wor == n) [minBound .. maxBound]
  Right SongInfo{..}
