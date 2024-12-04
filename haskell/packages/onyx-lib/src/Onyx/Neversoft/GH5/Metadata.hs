{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Neversoft.GH5.Metadata where

import           Control.Monad.Extra               (guard, mapMaybeM)
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Lazy              as BL
import           Data.List.Extra                   (nubOrd, nubOrdOn)
import           Data.Maybe                        (fromMaybe, listToMaybe,
                                                    mapMaybe)
import qualified Data.Text                         as T
import           Data.Word
import           GHC.ByteOrder
import           Onyx.Genre
import           Onyx.Harmonix.DTA.Serialize.Magma (Gender (..))
import           Onyx.Neversoft.CRC
import           Onyx.Neversoft.GH4.Metadata       (getOverallSongVolume,
                                                    getVocalsCents)
import           Onyx.Neversoft.Pak
import           Onyx.Neversoft.QB
import           Onyx.StackTrace
import           Onyx.Util.Binary                  (runGetM)

-- Metadata in _text.pak.qb for GH5 and WoR

data TextPakQB = TextPakQB
  { textPakFileKey     :: QBKey
  , textPakSongStructs :: [TextPakSongStruct]
  } deriving (Show)

data TextPakSongStruct = TextPakSongStruct
  { songArrayID  :: QBKey -- like "gh6_dlc_songlist"
  , songStructID :: QBKey -- like "gh6_dlc_songlist_props"
  , songDLCID    :: QBKey -- like "dlc123"
  , songData     :: [QBStructItem QSResult QBKey]
  } deriving (Show)

readTextPakQB :: (SendMessage m) => BL.ByteString -> Maybe BL.ByteString -> Maybe BL.ByteString -> StackTraceT m [TextPakQB]
readTextPakQB bs pab mqs = do
  nodes <- splitPakNodes pakFormatWoR bs pab
  let qbFiles = flip mapMaybe nodes $ \(n, node) -> do
        guard $ n.nodeFileType == ".qb"
        return node
  mappingQS <- case mqs of
    Nothing -> return $ qsBank nodes -- could also filter by matching nodeFilenameCRC
    Just qs -> qsBank <$> splitPakNodes pakFormatWoR qs Nothing
  flip mapMaybeM qbFiles $ \qbFile -> do
    qb <- let
      ?endian = BigEndian
      in map (lookupQS mappingQS) <$> runGetM parseQB qbFile
    let arrayStructIDPairs =
          [ ("gh6_songlist"        , "gh6_songlist_props"        ) -- WoR Disc
          , ("gh6_dlc_songlist"    , "gh6_dlc_songlist_props"    ) -- WoR DLC
          , ("gh4_dlc_songlist"    , "gh4_dlc_songlist_props"    ) -- ghwt dlc, starts with dlc1, Guitar Duel With Ted Nugent (Co-Op)
          , ("gh4_1_songlist"      , "gh4_1_songlist_props"      ) -- gh metallica, starts with dlc351, Ace Of Spades (Motorhead)
          , ("gh5_songlist"        , "gh5_songlist_props"        ) -- gh5 disc (on the actual disc)
          , ("gh5_0_songlist"      , "gh5_0_songlist_props"      ) -- gh5 disc (export), starts with dlc502, All The Pretty Faces (The Killers)
          , ("gh5_1_disc_songlist" , "gh5_1_disc_songlist_props" ) -- band hero (on the actual disc)
          , ("gh5_1_songlist"      , "gh5_1_songlist_props"      ) -- band hero (export), starts with dlc601, ABC (Jackson 5)
          , ("gh4_2_songlist"      , "gh4_2_songlist_props"      ) -- smash hits, starts with dlc406, Caught In A Mosh (Anthrax)
          , ("gh4_songlist"        , "gh4_songlist_props"        ) -- ghwt disc, starts with dlc251, About A Girl (Unplugged) (Nirvana)
          , ("gh5_dlc_songlist"    , "gh5_dlc_songlist_props"    ) -- gh5 dlc, starts with DLC1001, (I Can't Get No) Satisfaction (Live) (Rolling Stones)
          , ("gh4_3_songlist"      , "gh4_3_songlist_props"      ) -- van halen (not actually exported, but WoR expects this and Addy uses it in his export)
          -- WoR "Guitar Hero 6" prototype
          , ("old_ondisc_songlist" , "old_ondisc_songlist_props" ) -- some GH5 songs
          , ("gh6_1_songlist"      , "gh6_1_songlist_props"      ) -- band hero 2
          , ("MT_songlist"         , "MT_songlist_props"         ) -- Music Test
          , ("jammode_songlist"    , "jammode_songlist_props"    ) -- empty
          , ("example_jam_songlist", "example_jam_songlist_props") -- empty
          , ("jamsession_songlist" , "jamsession_songlist_props" )
          , ("debug_songlist"      , "debug_songlist_props"      )
          , ("tutorial_songlist"   , "tutorial_songlist_props"   )
          ]
        structs = do
          QBSectionStruct structID fileID (QBStructHeader : songs) <- qb
          (listID, propsID) <- filter (\(_, propsID) -> propsID == structID) arrayStructIDPairs
          song <- songs
          return (fileID, (listID, propsID, song))
    case structs of
      [] -> return Nothing
      (fileID, _) : _ -> do
        structs' <- flip mapMaybeM structs $ \(_, (listID, propsID, song)) -> case song of
          QBStructItemStruct k struct -> return $ Just $ TextPakSongStruct listID propsID k struct
          item -> do
            warn $ "Unexpected item in _text.pak instead of song struct: " <> show item
            return Nothing
        return $ guard (not $ null structs') >> Just (TextPakQB fileID structs')

combineTextPakQBs :: [TextPakQB] -> [TextPakSongStruct]
combineTextPakQBs = nubOrdOn (.songDLCID) . concatMap (.textPakSongStructs)

showTextPakQBQS :: TextPakQB -> (BL.ByteString, BL.ByteString)
showTextPakQBQS contents = let
  qbLists = do
    songlistID <- nubOrd $ map (.songArrayID) contents.textPakSongStructs
    guard $ songlistID /= "gh4_2_songlist" && songlistID /= "gh4_3_songlist" -- We only want the metadata updates, not overwrite the setlists
    return $ QBSectionArray songlistID contents.textPakFileKey $ QBArrayOfQbKey $ do
      struct <- contents.textPakSongStructs
      guard $ struct.songArrayID == songlistID
      return struct.songDLCID
  qbProps = do
    propsID <- nubOrd $ map (.songStructID) contents.textPakSongStructs
    return $ QBSectionStruct propsID contents.textPakFileKey $ QBStructHeader : do
      struct <- contents.textPakSongStructs
      guard $ struct.songStructID == propsID
      return $ QBStructItemStruct struct.songDLCID struct.songData
  qb = qbLists <> qbProps
  qs = nubOrdOn fst $ do
    item <- contents.textPakSongStructs >>= (.songData)
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
  , songOriginalArtist        :: Bool
  , songSinger                :: Maybe Gender
  } deriving (Show)

parseSongInfoStruct :: [QBStructItem QSResult QBKey] -> Either String SongInfo
parseSongInfoStruct songEntries = do
  songName <- case [ s | QBStructItemString "name" s <- songEntries ] of
    s : _ -> Right s
    []    -> Left "parseSongInfo: couldn't get song internal name"
  songTitle <- case [ (w, stripBackL s) | QBStructItemQbKeyStringQs "title" (KnownQS w s) <- songEntries ] of
    p : _ -> Right p
    []    -> Left "parseSongInfo: couldn't get song title"
  songArtist <- case [ (w, stripBackL s) | QBStructItemQbKeyStringQs "artist" (KnownQS w s) <- songEntries ] of
    p : _ -> Right p
    []    -> Left "parseSongInfo: couldn't get song artist"
  songYear <- case [ n | QBStructItemInteger "year" n <- songEntries ] of
    n : _ -> Right $ fromIntegral n
    []    -> Left "parseSongInfo: couldn't get song year"
  songAlbumTitle <- case [ (w, stripBackL s) | QBStructItemQbKeyStringQs "album_title" (KnownQS w s) <- songEntries ] of
    p : _ -> Right $ Just p
    []    -> Right Nothing
  songDoubleKick <- case [ n | QBStructItemInteger "double_kick" n <- songEntries ] of
    0 : _ -> Right False
    1 : _ -> Right True
    []    -> Right False
    _     -> Left "parseSongInfo: couldn't understand double_kick field"
  songTierGuitar <- case [ n | QBStructItemInteger "guitar_difficulty_rating" n <- songEntries ] of
    n : _ -> Right $ fromIntegral n
    []    -> Left "parseSongInfo: couldn't get guitar tier"
  songTierBass <- case [ n | QBStructItemInteger "bass_difficulty_rating" n <- songEntries ] of
    n : _ -> Right $ fromIntegral n
    []    -> Left "parseSongInfo: couldn't get bass tier"
  songTierVocals <- case [ n | QBStructItemInteger "vocals_difficulty_rating" n <- songEntries ] of
    n : _ -> Right $ fromIntegral n
    []    -> Left "parseSongInfo: couldn't get vocals tier"
  songTierDrums <- case [ n | QBStructItemInteger "drums_difficulty_rating" n <- songEntries ] of
    n : _ -> Right $ fromIntegral n
    []    -> Left "parseSongInfo: couldn't get drums tier"
  let songGenre = case [ n | QBStructItemQbKey "genre" n <- songEntries ] of
        [] -> Nothing
        n : _ -> listToMaybe $ filter (\wor -> qbWoRGenre wor == n) [minBound .. maxBound]
      songVocalsPitchScoreShift = fromMaybe 0 $ listToMaybe $ mapMaybe getVocalsCents songEntries
      songOverallSongVolume = fromMaybe 0 $ listToMaybe $ mapMaybe getOverallSongVolume songEntries
      songOriginalArtist = case [ n | QBStructItemInteger "original_artist" n <- songEntries ] of
        b : _ -> b /= 0
        []    -> True
      songSinger = case [ k | QBStructItemQbKey "singer" k <- songEntries ] of
        "male"   : _ -> Just Male
        "female" : _ -> Just Female
        _            -> Nothing
  Right SongInfo{..}
