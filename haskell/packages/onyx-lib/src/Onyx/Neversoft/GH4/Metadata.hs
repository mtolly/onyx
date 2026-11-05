{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Neversoft.GH4.Metadata where

import qualified Data.ByteString                   as B
import qualified Data.ByteString.Lazy              as BL
import           Data.Maybe                        (fromMaybe, listToMaybe,
                                                    mapMaybe)
import qualified Data.Text                         as T
import           GHC.ByteOrder
import           Onyx.Harmonix.DTA.Serialize.Magma (Gender (..))
import           Onyx.Neversoft.CRC
import           Onyx.Neversoft.GH3.Metadata
import           Onyx.Neversoft.Pak
import           Onyx.Neversoft.QB

data SongInfoGH4 = SongInfoGH4
  { gh4Name                  :: B.ByteString -- this is an id like "DLC23"
  , gh4Title                 :: T.Text
  , gh4Artist                :: T.Text
  , gh4Year                  :: T.Text
  , gh4Genre                 :: Maybe QBKey -- missing in some demo/test songs
  , gh4DoubleKick            :: Bool -- key only present in GH Metallica
  , gh4OriginalArtist        :: Bool
  , gh4Singer                :: Maybe Gender
  , gh4OverallSongVolume     :: Float -- decibels I assume
  , gh4VocalsPitchScoreShift :: Int

  -- more fields:
  -- artist_text = artist_text_by or artist_text_as_made_famous_by
  -- countoff = sticks_normal, hihat01, maybe others
  -- parts_with_mic = {StructItemArray: [parts_with_mic, {ArrayOfQbKey: [guitarist, bassist]}]} (or just guitarist)
  -- thin_fretbar_8note_params_low_bpm, thin_fretbar_8note_params_high_bpm,
  -- thin_fretbar_16note_params_low_bpm, thin_fretbar_16note_params_high_bpm = 0 or 1
  -- guitarist = male (?)
  -- band = Band_Hendrix
  -- drum_kit
  -- vocals_scroll_speed
  -- these keys aren't in dbg but they are on the 2 cover WT DLC songs:
  -- 2889875529 = QS 3066258305
  -- 2269603036 = QS 651823163
  } deriving (Show)

getVocalsCents :: QBStructItem qs QBKey -> Maybe Int
getVocalsCents
  (QBStructItemStruct "vocals_pitch_score_shift" [QBStructItemInteger "cents" n])
  = Just $ fromIntegral n
getVocalsCents _ = Nothing

makeVocalsCents :: Int -> Maybe (QBStructItem qs QBKey)
makeVocalsCents 0 = Nothing
makeVocalsCents n = Just $ QBStructItemStruct "vocals_pitch_score_shift"
  [ QBStructItemInteger "cents" $ fromIntegral n
  ]

getOverallSongVolume :: QBStructItem qs QBKey -> Maybe Float
getOverallSongVolume (QBStructItemInteger "overall_song_volume" n)
  = Just $ fromIntegral n
getOverallSongVolume (QBStructItemFloat "overall_song_volume" n)
  = Just n
getOverallSongVolume _ = Nothing

parseSongInfoGH4 :: [QBStructItem QSResult QBKey] -> Either String SongInfoGH4
parseSongInfoGH4 songEntries = do
  gh4Name <- case [ s | QBStructItemString "name" s <- songEntries ] of
    s : _ -> Right s
    []    -> Left "parseSongInfoGH4: couldn't get song internal name"
  let getString crc = listToMaybe $ songEntries >>= \case
        QBStructItemQbKeyStringQs k (KnownQS _ s) | k == crc -> [s]
        _                                                    -> []
      metaError s = Left $ "parseSongInfoGH4: " <> s <> " for song " <> show gh4Name
  gh4Title  <- maybe (metaError "couldn't get song title" ) (Right . stripBackL) $ getString "title"
  gh4Artist <- maybe (metaError "couldn't get song artist") (Right . stripBackL) $ getString "artist"
  gh4Year   <- maybe (metaError "couldn't get song year"  ) (Right . stripBackL) $ getString "year"
  let gh4Genre = listToMaybe [ n | QBStructItemQbKey "genre" n <- songEntries ]
      gh4DoubleKick = case [ n | QBStructItemInteger "double_kick" n <- songEntries ] of
        n : _ -> n /= 0
        []    -> False
      gh4OriginalArtist = case [ n | QBStructItemInteger "original_artist" n <- songEntries ] of
        b : _ -> b /= 0
        []    -> True -- should always be present but we'll just assume
      gh4Singer = case [ n | QBStructItemQbKey "singer" n <- songEntries ] of
        n : _ -> case n of
          "male"   -> Just Male
          "female" -> Just Female
          _        {- should be "none" -} -> Nothing
        []    -> Nothing
      gh4OverallSongVolume = fromMaybe 0 $ listToMaybe $ mapMaybe getOverallSongVolume songEntries
      gh4VocalsPitchScoreShift = fromMaybe 0 $ listToMaybe $ mapMaybe getVocalsCents songEntries
  Right SongInfoGH4{..}

readGH4TextPakQBDisc
  :: (MonadFail m, ?endian :: ByteOrder)
  => (BL.ByteString, Maybe BL.ByteString)
  -> (BL.ByteString, Maybe BL.ByteString)
  -> m GH3TextPakQB
readGH4TextPakQBDisc (qbpak, qbpab) (qspak, qspab) = do
  qbnodes <- splitPakNodes qbpak qbpab
  qsnodes <- splitPakNodes qspak qspab
  return $ readGH3TextPakQB $ qbnodes <> qsnodes
