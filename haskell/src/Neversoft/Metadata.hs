{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Neversoft.Metadata where

import           Control.Monad        (forM, guard, replicateM)
import           Data.Binary.Get
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.List.Extra      (nubOrdOn)
import qualified Data.List.NonEmpty   as NE
import           Data.Maybe           (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Data.Word
import           Genre
import           GHC.ByteOrder
import           Neversoft.Checksum
import           Neversoft.Pak
import           Neversoft.QB
import           STFS.Package         (runGetM)

-- Metadata in _text.pak.qb for GH5 and WoR

data TextPakQB = TextPakQB
  { textPakFileKey     :: Word32
  , textPakSongStructs :: [(Word32, [QBStructItem QSResult Word32])]
  } deriving (Show)

readTextPakQB :: (MonadFail m) => BL.ByteString -> m TextPakQB
readTextPakQB bs = do
  nodes <- splitPakNodes BigEndian bs Nothing
  let _qbFilenameCRC isWoR = if isWoR then 1379803300 else 3130519416 -- actually GH5 apparently has different ones per package
      qbFiles _isWoR = filter (\(n, _) -> nodeFileType n == qbKeyCRC ".qb") nodes
  (qbFile, _isWoR) <- case (qbFiles True, qbFiles False) of
    ([qb], _) -> return (snd qb, True)
    (_, [qb]) -> return (snd qb, False)
    _         -> fail "Couldn't locate metadata .qb"
  let mappingQS = qsBank nodes -- could also filter by matching nodeFilenameCRC
      qb = let
        ?endian = BigEndian
        in map (lookupQS mappingQS) $ runGet parseQB qbFile
      arrayStructIDPairs =
        [ (qbKeyCRC "gh6_dlc_songlist", 4087958085) -- WoR DLC
        -- rest are seen in GH5 only
        , (qbKeyCRC "gh4_dlc_songlist", 963067081) -- ghwt dlc, starts with dlc1, Guitar Duel With Ted Nugent (Co-Op)
        , (qbKeyCRC "gh4_1_songlist", 1268271471) -- gh metallica, starts with dlc351, Ace Of Spades (Motorhead)
        , (qbKeyCRC "gh5_0_songlist", 178417183) -- gh5 disc, starts with dlc502, All The Pretty Faces (The Killers)
        , (qbKeyCRC "gh5_1_songlist", 2764767118) -- band hero, starts with dlc601, ABC (Jackson 5)
        , (qbKeyCRC "gh4_2_songlist", 1649474973) -- smash hits, starts with dlc406, Caught In A Mosh (Anthrax)
        , (qbKeyCRC "gh4_songlist", 2460628824) -- ghwt disc, starts with dlc251, About A Girl (Unplugged) (Nirvana)
        , (qbKeyCRC "gh5_dlc_songlist", 1543505807) -- gh5 dlc, starts with DLC1001, (I Can't Get No) Satisfaction (Live) (Rolling Stones)
        ]
      _arrays = do
        QBSectionArray arrayID fileID (QBArrayOfQbKey keys) <- qb
        guard $ elem arrayID $ map fst arrayStructIDPairs
        return (fileID, keys)
      structs = do
        QBSectionStruct structID fileID (QBStructHeader : songs) <- qb
        guard $ elem structID $ map snd arrayStructIDPairs
        return (fileID, songs)
  case structs of
    [] -> fail "Couldn't find any song structs"
    (fileID, _) : _ -> do
      structs' <- forM (concat $ map snd structs) $ \case
        QBStructItemStruct k struct -> return (k, struct)
        item -> fail $ "Unexpected item in _text.pak instead of song struct: " <> show item
      return $ TextPakQB fileID structs'

combineTextPakQBs :: [TextPakQB] -> [(Word32, [QBStructItem QSResult Word32])]
combineTextPakQBs = nubOrdOn fst . concatMap textPakSongStructs

showTextPakQBQS :: TextPakQB -> (BL.ByteString, BL.ByteString)
showTextPakQBQS contents = let
  qb =
    [ QBSectionArray (qbKeyCRC "gh6_dlc_songlist") (textPakFileKey contents)
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

updateTextPakQB :: (MonadFail m) => [(Word32, [QBStructItem QSResult Word32])] -> BL.ByteString -> m BL.ByteString
updateTextPakQB library bs = do
  nodes <- splitPakNodes BigEndian bs Nothing
  let nodes' = flip map nodes $ \pair@(n, _) -> if
        | nodeFileType n == qbKeyCRC ".qb" && nodeFilenameCRC n == 1379803300 -> let
          (qb, _) = showTextPakQBQS $ TextPakQB (nodeFilenameKey n) library
          in (n, qb)
        | elem (nodeFileType n) (map qbKeyCRC [".qs.en", ".qs.es", ".qs.it", ".qs.de", ".qs.fr"]) && nodeFilenameCRC n == 1379803300 -> let
          (_, qs) = showTextPakQBQS $ TextPakQB (nodeFilenameKey n) library
          in (n, qs)
        | otherwise -> pair
  return $ buildPak nodes'

data SongInfo = SongInfo
  { songName       :: B.ByteString -- this is an id like "dlc747"
  , songTitle      :: (Word32, T.Text)
  , songArtist     :: (Word32, T.Text)
  , songYear       :: Int
  , songAlbumTitle :: Maybe (Word32, T.Text) -- not all songs have one
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
  songAlbumTitle <- case [ (w, removeL s) | QBStructItemQbKeyStringQs k (KnownQS w s) <- songEntries, k == qbKeyCRC "album_title" ] of
    p : _ -> Right $ Just p
    []    -> Right Nothing
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

-- Metadata in _text.pak.qb for GH3

readGH3TextPakQBDisc :: (MonadFail m, ?endian :: ByteOrder) => BL.ByteString -> BL.ByteString -> m TextPakQB
readGH3TextPakQBDisc pak pab = do
  nodes <- splitPakNodes ?endian pak $ Just pab
  qbFile <- case filter (\(n, _) -> nodeFileType n == qbKeyCRC ".qb" && nodeFilenameCRC n == qbKeyCRC "songlist") nodes of
    []         -> fail "Couldn't locate metadata .qb"
    (_, r) : _ -> return r
  readGH3TextPakQB nodes qbFile

readGH3TextPakQBDLC :: (MonadFail m) => BL.ByteString -> m TextPakQB
readGH3TextPakQBDLC pak = do
  let ?endian = BigEndian
  nodes <- splitPakNodes ?endian pak Nothing
  -- TODO figure out actual filename instead of just getting the last qb
  qbFile <- case NE.nonEmpty $ filter (\(n, _) -> nodeFileType n == qbKeyCRC ".qb") nodes of
    Nothing -> fail "Couldn't locate metadata .qb"
    Just ne -> return $ snd $ NE.last ne
  readGH3TextPakQB nodes qbFile

readGH3TextPakQB :: (MonadFail m, ?endian :: ByteOrder) => [(Node, BL.ByteString)] -> BL.ByteString -> m TextPakQB
readGH3TextPakQB nodes qbFile = do
  let mappingQS = qsBank nodes -- could also filter by matching nodeFilenameCRC
  qb <- map (lookupQS mappingQS) <$> runGetM parseQB qbFile
  let structs = do
        QBSectionStruct structID fileID (QBStructHeader : songs) <- qb
        guard $ elem structID
          [ qbKeyCRC "permanent_songlist_props" -- disc
          , qbKeyCRC "download_songlist_props" -- dlc
          ]
        return (fileID, songs)
  case structs of
    [] -> fail "Couldn't find any song structs"
    (fileID, _) : _ -> do
      structs' <- forM (concat $ map snd structs) $ \case
        QBStructItemStruct8A0000 k struct -> return (k, struct)
        item -> fail $ "Unexpected item in _text.pak instead of song struct: " <> show item
      return $ TextPakQB fileID structs'

data GH3Singer
  = GH3SingerFemale
  | GH3SingerMale
  | GH3SingerBret
  deriving (Show)

data SongInfoGH3 = SongInfoGH3
  { gh3Name                 :: B.ByteString -- this is an id like "dlc3"
  , gh3Title                :: T.Text
  , gh3Artist               :: T.Text
  , gh3Year                 :: Maybe T.Text
  , gh3Singer               :: Maybe GH3Singer
  , gh3Keyboard             :: Bool
  , gh3BandPlaybackVolume   :: Float
  , gh3GuitarPlaybackVolume :: Float
  , gh3RhythmTrack          :: Bool
  , gh3UseCoopNotetracks    :: Bool
  , gh3HammerOnMeasureScale :: Maybe Float
  -- lots of other fields ignored
  } deriving (Show)

parseSongInfoGH3 :: [QBStructItem QSResult Word32] -> Either String SongInfoGH3
parseSongInfoGH3 songEntries = do
  gh3Name <- case [ s | QBStructItemString830000 k s <- songEntries, k == qbKeyCRC "name" ] of
    s : _ -> Right s
    []    -> Left "parseSongInfoGH3: couldn't get song internal name"
  let getString key = let
        crc = qbKeyCRC key
        in listToMaybe $ songEntries >>= \case
          QBStructItemStringW       k s | k == crc -> [s]                 -- 360
          QBStructItemString830000  k s | k == crc -> [TE.decodeLatin1 s] -- PS2
          _                                        -> []
  gh3Title <- maybe (Left $ "parseSongInfoGH3: couldn't get song title") Right $ getString "title"
  gh3Artist <- maybe (Left $ "parseSongInfoGH3: couldn't get song artist") Right $ getString "artist"
  -- tutorial songs don't have a year
  let gh3Year = getString "year"
  gh3Singer <- case [ s | QBStructItemQbKey8D0000 k s <- songEntries, k == qbKeyCRC "singer" ] of
    s : _
      | s == qbKeyCRC "female" -> Right $ Just GH3SingerFemale
      | s == qbKeyCRC "male"   -> Right $ Just GH3SingerMale
      | s == qbKeyCRC "bret"   -> Right $ Just GH3SingerBret
      | s == qbKeyCRC "none"   -> Right Nothing
      | otherwise              -> Left $ "parseSongInfoGH3: unrecognized key for singer (song " <> show gh3Name <> ", key " <> show s <> ")" -- should probably just be warning
    []    -> Right Nothing
  gh3Keyboard <- case [ s | QBStructItemQbKey8D0000 k s <- songEntries, k == qbKeyCRC "keyboard" ] of
    s : _
      | s == qbKeyCRC "true"  -> Right True
      | s == qbKeyCRC "false" -> Right False
      | otherwise             -> Left $ "parseSongInfoGH3: unrecognized key for keyboard (song " <> show gh3Name <> ", key " <> show s <> ")" -- should probably just be warning
    []    -> Right False
  let findFloat key = let
        crc = qbKeyCRC key
        in listToMaybe $ flip mapMaybe songEntries $ \case
          QBStructItemInteger810000 k x | k == crc -> Just $ fromIntegral x
          QBStructItemFloat820000   k x | k == crc -> Just x
          _                                        -> Nothing
      gh3BandPlaybackVolume   = fromMaybe 0 $ findFloat "band_playback_volume"
      gh3GuitarPlaybackVolume = fromMaybe 0 $ findFloat "guitar_playback_volume"
      gh3UseCoopNotetracks    = not $ null
        [ () | QBStructItemQbKey8D0000 0 k <- songEntries, k == qbKeyCRC "use_coop_notetracks" ]
      gh3HammerOnMeasureScale = listToMaybe
        [ n | QBStructItemFloat820000 k n <- songEntries, k == qbKeyCRC "hammer_on_measure_scale" ]
  gh3RhythmTrack <- case [ n | QBStructItemInteger810000 k n <- songEntries, k == qbKeyCRC "rhythm_track" ] of
    0 : _ -> Right False
    1 : _ -> Right True
    _ : _ -> Left "parseSongInfoGH3: unrecognized value for coop-is-rhythm key"
    []    -> Right False
  Right SongInfoGH3{..}

data GH3Dat = GH3Dat
  { gh3DatLength :: Word32 -- length in bytes of .fsb.xen
  , gh3DatAudio  :: [(Word32, Word32)] -- (qb key of e.g. "songname_guitar", fsb stream index)
  }

getGH3Dat :: Get GH3Dat
getGH3Dat = do
  count <- getWord32be
  gh3DatLength <- getWord32be
  gh3DatAudio <- replicateM (fromIntegral count) $ do
    nameKey <- getWord32be
    streamIndex <- getWord32be
    -- are these always zero?
    0 <- getWord32be
    0 <- getWord32be
    0 <- getWord32be
    return (nameKey, streamIndex)
  return GH3Dat{..}

{-
wii notes:
*.wad.ngc are several single track FSB3 glued together into one file. dunno codec yet
*.dat.ngc are different from 360: each track is
  qb key (like 360)
  start bytes in wad
  end bytes in wad (or length? didn't check)
  8 bytes 0
-}
