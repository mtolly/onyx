{-
Renumber RR custom songs due to the limited song ID range
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE ViewPatterns          #-}
module Onyx.QuickConvert.RockRevolution where

import           Control.Monad.Codec       (codecIn, codecOut)
import           Control.Monad.Extra       (forM, guard, void, zipWithM)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Bifunctor            (first)
import           Data.Binary.Put           (runPut)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import           Data.Char                 (isDigit, toLower)
import           Data.Foldable             (toList)
import           Data.Maybe                (catMaybes)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Onyx.Audio.FSB.FEV
import           Onyx.Build.Common         (crawlFolderBytes)
import           Onyx.Build.RockRevolution (intToRRSongID)
import           Onyx.PlayStation.NPData   (npdContentID,
                                            rockRevolutionEdatConfig)
import           Onyx.PlayStation.PKG      (PKG (..), getDecryptedUSRDIR,
                                            loadPKGReadable, makePKG)
import           Onyx.Resources            (getResourcesPath)
import           Onyx.StackTrace           (SendMessage, StackTraceT, stackIO)
import           Onyx.Util.Binary          (runGetM)
import           Onyx.Util.Handle
import           Onyx.Util.Text.Decode     (encodeLatin1)
import           Onyx.Xbox.STFS            (makeCONReadable, readableSTFSFolder,
                                            rrSTFSOptions)
import           System.FilePath           (takeExtension)
import           System.IO                 (hFileSize)

data RRSong = RRSong
  { songID :: T.Text
  , files  :: [(T.Text, Readable)]
  , size   :: Integer
  , ps3    :: Bool
  }

loadRRXbox :: Readable -> IO [RRSong]
loadRRXbox r = do
  dir <- readableSTFSFolder r
  scanRRFolder False $ maybe [] folderFiles $ lookup "data" $ folderSubfolders dir

loadRRPS3 :: (MonadIO m, SendMessage m) => Readable -> StackTraceT m [RRSong]
loadRRPS3 r = do
  pkg <- stackIO $ loadPKGReadable r
  usrdir <- getDecryptedUSRDIR $ pkgFolder pkg
  stackIO $ scanRRFolder True $ map (first TE.decodeLatin1) $ folderFiles usrdir

scanRRFolder :: Bool -> [(T.Text, Readable)] -> IO [RRSong]
scanRRFolder ps3 files = do
  let songIDs = do
        (T.toLower -> name, _) <- files
        num <- toList $ T.stripPrefix "s" name >>= T.stripSuffix ".lua"
        guard $ T.all isDigit num
        return num
  forM songIDs $ \songID -> do
    let songFiles = do
          (name, r) <- files
          guard $ songID `T.isInfixOf` name
          return (T.toLower name, r)
    size <- fmap sum $ forM songFiles $ \(_, r) -> useHandle r hFileSize
    return RRSong
      { songID = songID
      , files = songFiles
      , size = size
      , ps3 = ps3
      }

loadRRSongs :: (MonadIO m, SendMessage m) => [FilePath] -> StackTraceT m [(FilePath, [RRSong])]
loadRRSongs fs = fmap catMaybes $ forM fs $ \f -> do
  songs <- case map toLower $ takeExtension f of
    ".pkg" -> loadRRPS3 $ fileReadable f
    _      -> stackIO $ loadRRXbox $ fileReadable f
  return $ guard (not $ null songs) >> Just (f, songs)

newSongID :: Int -> RRSong -> IO RRSong
newSongID n song = do
  let newID = intToRRSongID n
      oldIDBytes = encodeLatin1 song.songID
      newIDBytes = encodeLatin1 newID
  files <- forM song.files $ \(name, r) -> do
    let name' = T.replace song.songID newID name
    r' <- case map toLower $ takeExtension $ T.unpack name of
      -- simple text replace for .lua, hopefully doesn't break anything
      ".lua" -> generateCachedReadable $ do
        lua <- useHandle r handleToByteString
        return $ makeHandle (T.unpack name) $ byteStringSimpleHandle
          $ BL.fromStrict
          $ replaceBytes oldIDBytes newIDBytes
          $ BL.toStrict lua
      -- replace in strings inside parsed .fev
      ".fev" -> generateCachedReadable $ do
        fev <- useHandle r handleToByteString >>= runGetM (codecIn binFEV)
        return $ makeHandle (T.unpack name) $ byteStringSimpleHandle
          $ runPut $ void $ codecOut binFEV $ newFEVID oldIDBytes newIDBytes fev
      -- all other files passed through
      _ -> return r
    return (name', r')
  return RRSong
    { songID = newID
    , files = files
    , size = song.size
    , ps3 = song.ps3
    }

newFEVID :: B.ByteString -> B.ByteString -> FEV -> FEV
newFEVID old new = onFEV where
  edit = replaceBytes old new
  onFEV FEV{..} = FEV
    { projectName = edit projectName
    , waveBanks = map onWaveBank waveBanks
    , topLevelEventCategory = onEventCategory topLevelEventCategory
    , topLevelEventGroups = map onEventGroup topLevelEventGroups
    , soundDefs = map onSoundDef soundDefs
    , ..
    }
  onWaveBank WaveBank{..} = WaveBank
    { bankName = edit bankName
    , ..
    }
  onEventCategory EventCategory{..} = EventCategory
    { name = edit name
    , subcategories = map onEventCategory subcategories
    , ..
    }
  onEventGroup EventGroup{..} = EventGroup
    { events = map onEvent events
    , ..
    }
  onEvent Event{..} = Event
    { layers = map onEventLayer layers
    , parentEventCategoryNames = map edit parentEventCategoryNames
    , ..
    }
  onEventLayer EventLayer{..} = EventLayer
    { soundDefInstances = map onSoundDefInstance soundDefInstances
    , ..
    }
  onSoundDefInstance SoundDefInstance{..} = SoundDefInstance
    { nameOrIndex = case nameOrIndex of
      Left name -> Left $ edit name
      Right _   -> nameOrIndex
    , ..
    }
  onSoundDef SoundDef{..} = SoundDef
    { name = edit name
    , waveforms = map onWaveform waveforms
    , ..
    }
  onWaveform Waveform{..} = Waveform
    { name = edit name
    , bankName = edit bankName
    , ..
    }

replaceBytes :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
-- TODO better implementation
replaceBytes old new bs = encodeLatin1 $ T.replace (TE.decodeLatin1 old) (TE.decodeLatin1 new) $ TE.decodeLatin1 bs

renumberFrom :: Int -> [(FilePath, [RRSong])] -> IO [(FilePath, [RRSong])]
renumberFrom n = let
  go _      []   = return []
  go nextID ((f, songs) : rest) = do
    songs' <- zipWithM newSongID [nextID ..] songs
    let newNextID = nextID + length songs
    ((f, songs') :) <$> go newNextID rest
  in go n

saveSongsPS3, saveSongs360 :: FilePath -> [RRSong] -> IO ()
saveSongsPS3 f songs = do
  let edatConfig = rockRevolutionEdatConfig "CUSTOMSONGS"
      contentID = npdContentID edatConfig
      container name inner = Folder { folderSubfolders = [(name, inner)], folderFiles = [] }
      main = container "USRDIR" Folder
        { folderFiles = songs >>= map (first $ \name -> TE.encodeUtf8 $ T.toUpper name <> ".EDAT") . (.files)
        , folderSubfolders = []
        }
  extra <- getResourcesPath "pkg-contents/rr" >>= crawlFolderBytes
  makePKG contentID (main <> extra) f
saveSongs360 f songs = do
  opts <- rrSTFSOptions "Custom Songs" "Created by Onyx" -- TODO better title/desc
  let container name inner = Folder { folderSubfolders = [(name, inner)], folderFiles = [] }
      dir = container "data" Folder
        { folderFiles = songs >>= map (first $ \name -> T.toLower name) . (.files)
        , folderSubfolders = []
        }
  makeCONReadable opts dir f

organizePacks :: Integer -> [RRSong] -> [[RRSong]]
organizePacks maxSize = let
  go :: Integer -> [RRSong] -> [(Integer, RRSong)] -> [[RRSong]]
  go curSize curSongs incoming = case incoming of
    [] -> case curSongs of
      []    -> []
      _ : _ -> [curSongs]
    (nextSize, nextSong) : rest -> let
      newSize = curSize + nextSize
      in if newSize > maxSize && not (null curSongs) -- always add at least one song
        then curSongs : go 0 [] incoming
        else go newSize (nextSong : curSongs) rest
  in go 0 [] . map (\song -> (song.size, song))
