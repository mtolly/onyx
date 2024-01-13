{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Neversoft.GH4.Metadata where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe           (fromMaybe, listToMaybe)
import qualified Data.Text            as T
import           Data.Word
import           Onyx.Neversoft.CRC
import           Onyx.Neversoft.QB

data SongInfoGH4 = SongInfoGH4
  { gh4Name   :: B.ByteString -- this is an id like "DLC23"
  , gh4Title  :: T.Text
  , gh4Artist :: T.Text
  , gh4Year   :: T.Text
  , gh4Genre  :: Word32
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
      stripText t = fromMaybe t $ T.stripPrefix "\\L" t
  gh4Title  <- maybe (Left $ "parseSongInfoGH4: couldn't get song title" ) (Right . stripText) $ getString "title"
  gh4Artist <- maybe (Left $ "parseSongInfoGH4: couldn't get song artist") (Right . stripText) $ getString "artist"
  gh4Year   <- maybe (Left $ "parseSongInfoGH4: couldn't get song year"  ) (Right . stripText) $ getString "year"
  gh4Genre  <- case [ n | QBStructItemQbKey k n <- songEntries, k == qbKeyCRC "genre" ] of
    n : _ -> Right n
    []    -> Left "parseSongInfoGH4: couldn't get genre"
  Right SongInfoGH4{..}
