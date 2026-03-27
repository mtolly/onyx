{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
module Onyx.Import.Amplitude where

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Bifunctor            (first)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as B8
import           Data.Char                 (toLower)
import           Data.Functor              ((<&>))
import qualified Data.HashMap.Strict       as HM
import qualified Data.List.NonEmpty        as NE
import qualified Data.Text.Encoding        as TE
import           Onyx.Amplitude.PS2.TxtBin (Node (..), NodeContents (..),
                                            TxtBin (..), getTxtBin)
import           Onyx.Import.Base
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Binary          (runGetM)
import           Onyx.Util.Handle

importAmpPS2Ark :: (SendMessage m, MonadIO m) => FilePath -> Folder B.ByteString Readable -> StackTraceT m [Import m]
importAmpPS2Ark src folder = do
  let folderDowncase = first (B8.map toLower) folder
      getFile path = case findFile (B8.map toLower <$> path) folderDowncase of
        Nothing -> fatal $ "File not found: " <> show path
        Just r  -> return r
  config <- do
    r <- getFile $ "gen" NE.:| ["freq2_config.txt.bin"]
    stackIO (useHandle r handleToByteString) >>= runGetM getTxtBin
  let songPairs = do
        N Node { contents = S "db" : db } <- config.node.contents
        N Node { contents = S "songs" : allSongs } <- db
        N Node { contents = S _area : songs } <- allSongs
        -- maybe: guard $ _area /= "Tutorial"
        N Node { contents = S shortName : song } <- songs
        return (shortName, song)
  metadata <- do
    r <- getFile $ "gen" NE.:| ["english_songs.txt.bin"]
    bin <- stackIO (useHandle r handleToByteString) >>= runGetM getTxtBin
    return do
      N Node { contents = [S key, S value] } <- bin.node.contents
      return (key, value)
  return $ songPairs <&> \(shortName, song) lvl -> do
    let configMap = HM.fromList do
          N Node { contents = S key : rest } <- song
          return (key, rest)
    mid <- case lvl of
      ImportQuick -> return undefined -- return emptyChart
      ImportFull  -> case HM.lookup "midi_file" configMap of
        Just [S midiFile] -> do
          r <- getFile $ "songs" NE.:| [shortName, midiFile]
          undefined r -- TODO
        _                 -> fatal "Couldn't read midi_file path"
    return SongYaml
      { metadata = def'
        { title         = TE.decodeLatin1 <$> lookup (shortName <> "_TITLE") metadata
        , artist        = TE.decodeLatin1 <$> lookup (shortName <> "_ARTIST") metadata
        , loadingPhrase = TE.decodeLatin1 <$> lookup (shortName <> "_BIO") metadata
        , fileAlbumArt  = Nothing
        }
      , global = def'
        { backgroundVideo = Nothing
        , fileBackgroundImage = Nothing
        , fileSongAnim = Nothing
        , fileMidi = SoftFile "notes.mid" $ SoftChart undefined -- TODO
        }
      , audio = HM.empty
      , jammit = HM.empty
      , plans = HM.empty -- TODO
      , targets = HM.empty
      , parts = Parts HM.empty -- TODO
      }
