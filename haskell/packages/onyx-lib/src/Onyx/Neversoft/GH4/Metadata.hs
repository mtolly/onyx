{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Neversoft.GH4.Metadata where

import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import           Data.Maybe                  (listToMaybe)
import qualified Data.Text                   as T
import           Data.Word
import           GHC.ByteOrder
import           Onyx.Neversoft.CRC
import           Onyx.Neversoft.GH3.Metadata
import           Onyx.Neversoft.Pak
import           Onyx.Neversoft.QB

data SongInfoGH4 = SongInfoGH4
  { gh4Name       :: B.ByteString -- this is an id like "DLC23"
  , gh4Title      :: T.Text
  , gh4Artist     :: T.Text
  , gh4Year       :: T.Text
  , gh4Genre      :: Maybe Word32 -- missing in some demo/test songs
  , gh4DoubleKick :: Bool -- key only present in GH Metallica
  -- lots of other fields ignored
  } deriving (Show)

parseSongInfoGH4 :: [QBStructItem QSResult Word32] -> Either String SongInfoGH4
parseSongInfoGH4 songEntries = do
  gh4Name <- case [ s | QBStructItemString k s <- songEntries, k == qbKeyCRC "name" ] of
    s : _ -> Right s
    []    -> Left "parseSongInfoGH4: couldn't get song internal name"
  let getString key = let
        crc = qbKeyCRC key
        in listToMaybe $ songEntries >>= \case
          QBStructItemQbKeyStringQs k (KnownQS _ s) | k == crc -> [s]
          _                                                    -> []
      metaError s = Left $ "parseSongInfoGH4: " <> s <> " for song " <> show gh4Name
  gh4Title  <- maybe (metaError $ "couldn't get song title" ) (Right . stripBackL) $ getString "title"
  gh4Artist <- maybe (metaError $ "couldn't get song artist") (Right . stripBackL) $ getString "artist"
  gh4Year   <- maybe (metaError $ "couldn't get song year"  ) (Right . stripBackL) $ getString "year"
  let gh4Genre = listToMaybe [ n | QBStructItemQbKey k n <- songEntries, k == qbKeyCRC "genre" ]
      gh4DoubleKick = case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "double_kick" ] of
        n : _ -> n /= 0
        []    -> False
  Right SongInfoGH4{..}

readGH4TextPakQBDisc :: (MonadFail m) => BL.ByteString -> BL.ByteString -> BL.ByteString -> m GH3TextPakQB
readGH4TextPakQBDisc qbpak qbpab qspak = do
  let ?endian = BigEndian
  qbnodes <- splitPakNodes (pakFormatGH3 ?endian) qbpak $ Just qbpab
  qsnodes <- splitPakNodes (pakFormatGH3 ?endian) qspak Nothing
  readGH3TextPakQB $ qbnodes <> qsnodes
