{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Neversoft.GH5.Metadata where

import           Control.Monad.Extra         (guard, mapMaybeM)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import           Data.List.Extra             (nubOrd, nubOrdOn)
import           Data.Maybe                  (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text                   as T
import           Data.Word
import           GHC.ByteOrder
import           Onyx.Genre
import           Onyx.Neversoft.CRC
import           Onyx.Neversoft.GH4.Metadata (getOverallSongVolume,
                                              getVocalsCents)
import           Onyx.Neversoft.Pak
import           Onyx.Neversoft.QB
import           Onyx.StackTrace
import           Onyx.Util.Binary            (runGetM)

-- Metadata in _text.pak.qb for GH5 and WoR

data TextPakQB = TextPakQB
  { textPakFileKey     :: Word32
  , textPakSongStructs :: [TextPakSongStruct]
  } deriving (Show)

data TextPakSongStruct = TextPakSongStruct
  { songArrayID  :: Word32 -- like "gh6_dlc_songlist"
  , songStructID :: Word32 -- like "gh6_dlc_songlist_props"
  , songDLCID    :: Word32 -- like "dlc123"
  , songData     :: [QBStructItem QSResult Word32]
  } deriving (Show)

readTextPakQB :: (SendMessage m) => BL.ByteString -> Maybe BL.ByteString -> Maybe BL.ByteString -> StackTraceT m TextPakQB
readTextPakQB bs pab mqs = do
  nodes <- splitPakNodes pakFormatWoR bs pab
  let _qbFilenameCRC isWoR = if isWoR then 1379803300 else 3130519416 -- actually GH5 apparently has different ones per package
      qbFiles _isWoR = filter (\(n, _) -> nodeFileType n == qbKeyCRC ".qb") nodes
      qbWoRDisc = filter (\(n, _) -> nodeFilenameCRC n == 3114035354) nodes
  (qbFile, _isWoR) <- case (qbWoRDisc, qbFiles True, qbFiles False) of
    ([qb], _, _) -> return (snd qb, True)
    (_, [qb], _) -> return (snd qb, True)
    (_, _, [qb]) -> return (snd qb, False)
    _            -> fail "Couldn't locate metadata .qb"
  mappingQS <- case mqs of
    Nothing -> return $ qsBank nodes -- could also filter by matching nodeFilenameCRC
    Just qs -> qsBank <$> splitPakNodes pakFormatWoR qs Nothing
  qb <- let
    ?endian = BigEndian
    in map (lookupQS mappingQS) <$> runGetM parseQB qbFile
  let arrayStructIDPairs =
        [ (qbKeyCRC "gh6_songlist"       , qbKeyCRC "gh6_songlist_props"       ) -- WoR Disc
        , (qbKeyCRC "gh6_dlc_songlist"   , qbKeyCRC "gh6_dlc_songlist_props"   ) -- WoR DLC
        , (qbKeyCRC "gh4_dlc_songlist"   , qbKeyCRC "gh4_dlc_songlist_props"   ) -- ghwt dlc, starts with dlc1, Guitar Duel With Ted Nugent (Co-Op)
        , (qbKeyCRC "gh4_1_songlist"     , qbKeyCRC "gh4_1_songlist_props"     ) -- gh metallica, starts with dlc351, Ace Of Spades (Motorhead)
        , (qbKeyCRC "gh5_songlist"       , qbKeyCRC "gh5_songlist_props"       ) -- gh5 disc (on the actual disc)
        , (qbKeyCRC "gh5_0_songlist"     , qbKeyCRC "gh5_0_songlist_props"     ) -- gh5 disc (export), starts with dlc502, All The Pretty Faces (The Killers)
        , (qbKeyCRC "gh5_1_disc_songlist", qbKeyCRC "gh5_1_disc_songlist_props") -- band hero (on the actual disc)
        , (qbKeyCRC "gh5_1_songlist"     , qbKeyCRC "gh5_1_songlist_props"     ) -- band hero (export), starts with dlc601, ABC (Jackson 5)
        , (qbKeyCRC "gh4_2_songlist"     , qbKeyCRC "gh4_2_songlist_props"     ) -- smash hits, starts with dlc406, Caught In A Mosh (Anthrax)
        , (qbKeyCRC "gh4_songlist"       , qbKeyCRC "gh4_songlist_props"       ) -- ghwt disc, starts with dlc251, About A Girl (Unplugged) (Nirvana)
        , (qbKeyCRC "gh5_dlc_songlist"   , qbKeyCRC "gh5_dlc_songlist_props"   ) -- gh5 dlc, starts with DLC1001, (I Can't Get No) Satisfaction (Live) (Rolling Stones)
        , (qbKeyCRC "gh4_3_songlist"     , qbKeyCRC "gh4_3_songlist_props"     ) -- van halen (not actually exported, but WoR expects this and Addy uses it in his export)
        ]
      structs = do
        QBSectionStruct structID fileID (QBStructHeader : songs) <- qb
        (listID, propsID) <- filter (\(_, propsID) -> propsID == structID) arrayStructIDPairs
        song <- songs
        return (fileID, (listID, propsID, song))
  case structs of
    [] -> fail "Couldn't find any song structs"
    (fileID, _) : _ -> do
      structs' <- flip mapMaybeM structs $ \(_, (listID, propsID, song)) -> case song of
        QBStructItemStruct k struct -> return $ Just $ TextPakSongStruct listID propsID k struct
        item -> do
          warn $ "Unexpected item in _text.pak instead of song struct: " <> show item
          return Nothing
      return $ TextPakQB fileID structs'

combineTextPakQBs :: [TextPakQB] -> [TextPakSongStruct]
combineTextPakQBs = nubOrdOn songDLCID . concatMap textPakSongStructs

showTextPakQBQS :: TextPakQB -> (BL.ByteString, BL.ByteString)
showTextPakQBQS contents = let
  qbLists = do
    songlistID <- nubOrd $ map songArrayID $ textPakSongStructs contents
    guard $ songlistID /= qbKeyCRC "gh4_2_songlist" && songlistID /= qbKeyCRC "gh4_3_songlist" -- We only want the metadata updates, not overwrite the setlists
    return $ QBSectionArray songlistID (textPakFileKey contents) $ QBArrayOfQbKey $ do
      struct <- textPakSongStructs contents
      guard $ songArrayID struct == songlistID
      return $ songDLCID struct
  qbProps = do
    propsID <- nubOrd $ map songStructID $ textPakSongStructs contents
    return $ QBSectionStruct propsID (textPakFileKey contents) $ QBStructHeader : do
      struct <- textPakSongStructs contents
      guard $ songStructID struct == propsID
      return $ QBStructItemStruct (songDLCID struct) (songData struct)
  qb = qbLists <> qbProps
  qs = nubOrdOn fst $ do
    item <- textPakSongStructs contents >>= songData
    KnownQS k txt <- allQS item
    return (k, txt)
  in (putQB $ discardQS qb, makeQS qs)

data SongInfo = SongInfo
  { songName                  :: B.ByteString -- this is an id like "dlc747"
  , songTitle                 :: (Word32, T.Text)
  , songArtist                :: (Word32, T.Text)
  , songYear                  :: Int
  , songAlbumTitle            :: Maybe (Word32, T.Text) -- not all songs have one
  , songDoubleKick            :: Bool
  , songTierGuitar            :: Int
  , songTierBass              :: Int
  , songTierVocals            :: Int
  , songTierDrums             :: Int
  , songGenre                 :: Maybe GenreWoR
  , songVocalsPitchScoreShift :: Int
  , songOverallSongVolume     :: Float -- decibels
  } deriving (Show)

parseSongInfoStruct :: [QBStructItem QSResult Word32] -> Either String SongInfo
parseSongInfoStruct songEntries = do
  songName <- case [ s | QBStructItemString k s <- songEntries, k == qbKeyCRC "name" ] of
    s : _ -> Right s
    []    -> Left "parseSongInfo: couldn't get song internal name"
  songTitle <- case [ (w, stripBackL s) | QBStructItemQbKeyStringQs k (KnownQS w s) <- songEntries, k == qbKeyCRC "title" ] of
    p : _ -> Right p
    []    -> Left "parseSongInfo: couldn't get song title"
  songArtist <- case [ (w, stripBackL s) | QBStructItemQbKeyStringQs k (KnownQS w s) <- songEntries, k == qbKeyCRC "artist" ] of
    p : _ -> Right p
    []    -> Left "parseSongInfo: couldn't get song artist"
  songYear <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "year" ] of
    n : _ -> Right $ fromIntegral n
    []    -> Left "parseSongInfo: couldn't get song year"
  songAlbumTitle <- case [ (w, stripBackL s) | QBStructItemQbKeyStringQs k (KnownQS w s) <- songEntries, k == qbKeyCRC "album_title" ] of
    p : _ -> Right $ Just p
    []    -> Right Nothing
  songDoubleKick <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "double_kick" ] of
    0 : _ -> Right False
    1 : _ -> Right True
    []    -> Right False
    _     -> Left "parseSongInfo: couldn't understand double_kick field"
  songTierGuitar <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "guitar_difficulty_rating" ] of
    n : _ -> Right $ fromIntegral n
    []    -> Left "parseSongInfo: couldn't get guitar tier"
  songTierBass <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "bass_difficulty_rating" ] of
    n : _ -> Right $ fromIntegral n
    []    -> Left "parseSongInfo: couldn't get bass tier"
  songTierVocals <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "vocals_difficulty_rating" ] of
    n : _ -> Right $ fromIntegral n
    []    -> Left "parseSongInfo: couldn't get vocals tier"
  songTierDrums <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "drums_difficulty_rating" ] of
    n : _ -> Right $ fromIntegral n
    []    -> Left "parseSongInfo: couldn't get drums tier"
  let songGenre = case [ n | QBStructItemQbKey k n <- songEntries, k == qbKeyCRC "genre" ] of
        [] -> Nothing
        n : _ -> listToMaybe $ filter (\wor -> qbWoRGenre wor == n) [minBound .. maxBound]
      songVocalsPitchScoreShift = fromMaybe 0 $ listToMaybe $ mapMaybe getVocalsCents songEntries
      songOverallSongVolume = fromMaybe 0 $ listToMaybe $ mapMaybe getOverallSongVolume songEntries
  Right SongInfo{..}
