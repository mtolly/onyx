{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
module Onyx.Import.Amplitude where

import           Control.Monad.Extra       (forM, mapMaybeM, unless)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Bifunctor            (first)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Char8     as B8
import           Data.Char                 (toLower)
import           Data.Foldable             (toList)
import           Data.Functor              ((<&>))
import qualified Data.HashMap.Strict       as HM
import           Data.List.Extra           (nubOrd)
import qualified Data.List.NonEmpty        as NE
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Onyx.Amplitude.PS2.Bnk
import           Onyx.Amplitude.PS2.MIDI   (AmpTrack (..), BankTrack (..),
                                            parseAmpTrack)
import           Onyx.Amplitude.PS2.TxtBin (Node (..), NodeContents (..),
                                            TxtBin (..), getTxtBin)
import           Onyx.Audio.VGS            (VGSHeader (..), makeVGSHeader)
import           Onyx.Import.Base
import           Onyx.MIDI.Track.File      (PartName (..), RawFile (..),
                                            Song (..), loadMIDIReadable)
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Binary          (runGetM)
import           Onyx.Util.Handle
import           System.FilePath           ((<.>), (</>))
import           System.IO                 (SeekMode (..), hClose, hSeek)

sampToVGS :: SAMPEntry -> Readable -> Readable
sampToVGS samp nse = makeHandle "nse -> vgs" do
  unless (samp.sampChannels == 1) $ fail "Non-mono SAMP"
  h <- rOpen nse

  hSeek h AbsoluteSeek $ fromIntegral samp.sampFilePosition
  row0 <- B.hGet h 16
  unless (B.all (== 0) row0) $ fail "SAMP data doesn't start with zero row"
  let go !blocks = do
        row <- B.hGet h 16
        case B.indexMaybe row 1 of
          Just 0 -> go $ blocks + 1
          _      -> return blocks
  blocks <- go 0

  header <- byteStringSimpleHandle $ BB.toLazyByteString $ makeVGSHeader VGSHeader
    { version = 2
    , channels = [(fromIntegral samp.sampRate, blocks)]
    }
  let startPosn = fromIntegral $ samp.sampFilePosition + 16
      dataLength = fromIntegral $ blocks * 16
  sub <- subHandleInternal startPosn (Just dataLength) h hClose

  appendSimpleHandle header sub

importAmpPS2Ark :: (SendMessage m, MonadIO m) => FilePath -> Folder B.ByteString Readable -> StackTraceT m [Import m]
importAmpPS2Ark _src folder = do
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
      ImportQuick -> return emptyChart
      ImportFull  -> case HM.lookup "midi_file" configMap of
        Just [S midiFile] -> do
          r <- getFile $ "songs" NE.:| [shortName, midiFile]
          raw <- loadMIDIReadable r
          forM raw \(RawFile trks) -> mapMaybeM parseAmpTrack trks
        _                 -> fatal "Couldn't read midi_file path"

    let bnkPaths = nubOrd do
          Bank trk <- mid.tracks
          toList trk.banks
    bnks <- forM bnkPaths \path -> inside ("Loading: " <> T.unpack path) do
      r <- getFile $ "songs" NE.:| [shortName, TE.encodeUtf8 path]
      bnk <- stackIO (useHandle r handleToByteString) >>= runGetM bnkChunks
      return (path, bnk)
    sampleAudio <- fmap (HM.fromList . concat) $ forM bnks \(path, bnk) -> do
      base <- case T.stripSuffix ".bnk" path of
        Nothing   -> fatal $ "Bank path not ending in .bnk? " <> show path
        Just base -> return base
      -- load each nse into memory only once so we don't exhaust file handles
      nse <- getFile ("songs" NE.:| [shortName, TE.encodeUtf8 base <> ".nse"])
        >>= stackIO . readableToMemory
      let samp = concat [ xs | SAMP xs <- bnk ]
          sanm = concat [ xs | SANM xs <- bnk ]
      forM (zip samp sanm) \(entry, name) -> do
        let audioName = T.replace " " "-" $ base <> "-" <> TE.decodeUtf8 name
            vgsPath = T.unpack base </> B8.unpack name <.> "vgs"
            vgs = sampToVGS entry nse
        return (audioName, AudioFile AudioInfo
          { md5      = Nothing
          , frames   = Nothing
          , filePath = Just $ SoftFile vgsPath $ SoftReadable vgs
          , commands = []
          , rate     = Just entry.sampRate
          , channels = entry.sampChannels
          })

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
        , fileMidi = SoftFile "notes.mid" $ SoftChart emptyChart
        }
      , audio = sampleAudio
      , jammit = HM.empty
      , plans = HM.empty -- TODO
      , targets = HM.empty
      , parts = Parts $ HM.fromList do
        Catch n inst name _ <- mid.tracks
        let fullName = "T" <> T.pack (show n) <> "-" <> name
        return (PartName fullName, emptyPart
          { mania = Just $ ModeMania do
            diff <- "mellow" NE.:| ["normal", "brutal", "insane"]
            return ManiaChart
              { name = diff
              , keys = 3
              , style = ManiaAmplitude inst
              , difficulty = Tier 1
              }
          })
      }
