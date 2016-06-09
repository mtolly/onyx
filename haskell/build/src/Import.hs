{-# LANGUAGE OverloadedStrings #-}
module Import where

import           Audio
import           Config                         hiding (Difficulty)
import           Control.Monad                  (guard, when)
import           Control.Monad.Extra            (replicateM, mapMaybeM)
import           Control.Monad.Trans.StackTrace (printStackTraceIO)
import           Data.Binary.Get                (getWord32le, runGet)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Conduit.Audio             as CA
import qualified Data.Digest.Pure.MD5           as MD5
import qualified Data.DTA                       as D
import qualified Data.DTA.Serialize             as D
import qualified Data.DTA.Serialize.RB3         as D
import           Data.Foldable                  (toList)
import qualified Data.HashMap.Strict            as HM
import           Data.List                      (nub, stripPrefix)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe, listToMaybe,
                                                 mapMaybe)
import qualified Data.Text                      as T
import qualified Data.Yaml                      as Y
import qualified FretsOnFire                    as FoF
import           Genre                          (defaultSubgenre)
import qualified RockBand.Drums                 as RBDrums
import qualified RockBand.File                  as RBFile
import qualified Sound.MIDI.File                as F
import qualified Sound.MIDI.File.Load           as Load
import qualified Sound.MIDI.Util                as U
import           STFS.Extract                   (extractSTFS)
import qualified System.Directory               as Dir
import           System.FilePath                (takeDirectory, takeFileName,
                                                 (<.>), (</>))
import           System.IO                      (IOMode (..), SeekMode (..),
                                                 hSeek, withBinaryFile,
                                                 hPutStrLn, stderr)
import           System.IO.Extra                (latin1, readFileEncoding',
                                                 utf8)
import           System.IO.Temp                 (withSystemTempDirectory)

-- | Convert a CON or RBA file (or FoF directory) to Onyx format.
importAny :: FilePath -> FilePath -> IO ()
importAny src dest = do
  Dir.createDirectoryIfMissing True dest
  isDir <- Dir.doesDirectoryExist src
  if isDir
    then importFoF src dest
    else do
      magic <- withBinaryFile src ReadMode $ \h -> B.hGet h 4
      case B8.unpack magic of
        "CON " -> importSTFS src dest
        "STFS" -> importSTFS src dest
        "RBSF" -> importRBA  src dest
        _      -> error $ src ++ " is not in a supported song format"

importFoF :: FilePath -> FilePath -> IO ()
importFoF src dest = do
  song <- FoF.loadSong $ src </> "song.ini"
  F.Cons _ _ midtrks <- Load.fromFile $ src </> "notes.mid"
  let trackNames = mapMaybe U.trackName midtrks
  Dir.copyFile (src </> "notes.mid") (dest </> "notes.mid")

  hasAlbumArt <- Dir.doesFileExist $ src </> "album.png"
  when hasAlbumArt $ Dir.copyFile (src </> "album.png") (dest </> "album.png")

  audioFiles <- let
    loadAudio x = do
      b <- Dir.doesFileExist $ src </> x
      if b
        then do
          Dir.copyFile (src </> x) (dest </> x)
          return $ Just x
        else return Nothing
    in mapMaybeM loadAudio
      [ "drums.ogg", "drums_1.ogg", "drums_2.ogg", "drums_3.ogg"
      , "guitar.ogg", "keys.ogg", "rhythm.ogg", "vocal.ogg"
      , "crowd.ogg", "song.ogg"
      ]

  let songAudio = case audioFiles of
        ["guitar.ogg"] -> "guitar.ogg"
        ["song.ogg"] -> "song.ogg"
        _ -> error $ "during FoF/PS import: unsupported audio configuration " ++ show songAudio

  let toRank = fmap $ \n -> Rank $ max 1 $ min 7 $ fromIntegral n + 1
      delay = case FoF.delay song of
        Nothing -> id
        Just n -> case compare n 0 of
          EQ -> id
          GT -> Drop Start (CA.Seconds $ fromIntegral n / 1000)
          LT -> Pad  Start (CA.Seconds $ fromIntegral (abs n) / 1000)
  Y.encodeFile (dest </> "song.yml") SongYaml
    { _metadata = Metadata
      { _title        = fromMaybe "Unknown Song" $ FoF.name song
      , _artist       = fromMaybe "Unknown Artist" $ FoF.artist song
      , _album        = fromMaybe "Unknown Album" $ FoF.album song
      , _genre        = "other" -- TODO
      , _subgenre     = "other" -- TODO
      , _year         = fromMaybe 1900 $ FoF.year song
      , _fileAlbumArt = guard hasAlbumArt >> Just "album.png"
      , _trackNumber  = fromMaybe 0 $ FoF.track song
      , _comments     = []
      , _vocalGender  = Nothing
      , _difficulty   = Difficulties
        { _difficultyDrums   = toRank $ FoF.diffDrums song
        , _difficultyGuitar  = toRank $ FoF.diffGuitar song
        , _difficultyBass    = toRank $ FoF.diffBass song
        , _difficultyKeys    = toRank $ FoF.diffKeys song
        , _difficultyProKeys = toRank $ FoF.diffKeysReal song
        , _difficultyVocal   = toRank $ FoF.diffVocals song
        , _difficultyBand    = toRank $ FoF.diffBand song
        }
      , _key          = Nothing
      , _autogenTheme = AutogenDefault
      , _author       = fromMaybe "Unknown Author" $ FoF.charter song
      , _rating       = Unrated
      , _drumKit      = HardRockKit
      , _auto2xBass   = False
      , _hopoThreshold = 170
      , _previewStart = Nothing
      , _previewEnd   = Nothing
      }
    , _audio = HM.fromList $ flip map audioFiles $ \aud -> (T.pack aud, AudioFile
      { _md5 = Nothing
      , _frames = Nothing
      , _name = Nothing
      , _path = Just aud
      , _rate = Nothing
      , _channels = 2 -- TODO
      })
    , _jammit = HM.empty
    , _plans = HM.singleton "fof" Plan
      { _song         = Just PlanAudio
        { _planExpr = delay $ Input $ Named $ T.pack songAudio
        , _planPans = []
        , _planVols = []
        }
      , _countin      = Countin []
      , _guitar       = Nothing
      , _bass         = Nothing
      , _keys         = Nothing
      , _kick         = Nothing
      , _snare        = Nothing
      , _drums        = Nothing
      , _vocal        = Nothing
      , _crowd        = Nothing
      , _planComments = []
      }
    , _instruments = Instruments
      { _hasDrums   = elem "PART DRUMS" trackNames && FoF.diffDrums song /= Just (-1)
      , _hasGuitar  = elem "PART GUITAR" trackNames && FoF.diffGuitar song /= Just (-1)
      , _hasBass    = elem "PART BASS" trackNames && FoF.diffBass song /= Just (-1)
      , _hasKeys    = elem "PART KEYS" trackNames && FoF.diffKeys song /= Just (-1)
      , _hasProKeys = elem "PART REAL_KEYS_X" trackNames && FoF.diffKeysReal song /= Just (-1)
      , _hasVocal   = if elem "PART VOCALS" trackNames && FoF.diffVocals song /= Just (-1)
        then if elem "HARM2" trackNames && FoF.diffVocalsHarm song /= Just (-1)
          then if elem "HARM3" trackNames
            then Vocal3
            else Vocal2
          else Vocal1
        else Vocal0
      }
    , _published = True
    }

importSTFS :: FilePath -> FilePath -> IO ()
importSTFS file dir = withSystemTempDirectory "onyx_con" $ \temp -> do
  extractSTFS file temp
  (_, pkg, isUTF8) <- readRB3DTA $ temp </> "songs/songs.dta"
  -- C3 puts extra info in DTA comments
  dtaLines <- fmap (lines . filter (/= '\r')) $ readFileEncoding' (if isUTF8 then utf8 else latin1) $ temp </> "songs/songs.dta"
  let author = listToMaybe $ mapMaybe (stripPrefix ";Song authored by ") dtaLines
      base = D.songName $ D.song pkg
      -- Note: the base path does NOT necessarily have to be songs/foo/foo
      -- where foo is the top key of songs.dta. foo can be different!
      -- e.g. C3's "Escape from the City" has a top key 'SonicAdvCityEscape2x'
      -- and a 'name' of "songs/sonicadv2cityescape2x/sonicadv2cityescape2x"
  importRB3 pkg (fmap T.pack author)
    (temp </> base <.> "mid")
    (temp </> base <.> "mogg")
    (temp </> takeDirectory base </> "gen" </> (takeFileName base ++ "_keep.png_xbox"))
    "cover.png_xbox"
    dir

importRBA :: FilePath -> FilePath -> IO ()
importRBA file dir = withSystemTempDirectory "onyx_rba" $ \temp -> do
  withBinaryFile file ReadMode $ \h -> do
    hSeek h AbsoluteSeek 0x08
    let read7words = runGet (replicateM 7 getWord32le) <$> BL.hGet h (7 * 4)
    offsets <- read7words
    sizes <- read7words
    let getFile i = do
          hSeek h AbsoluteSeek $ fromIntegral $ offsets !! i
          BL.hGet h $ fromIntegral $ sizes !! i
    getFile 0 >>= BL.writeFile (temp </> "songs.dta")
    getFile 1 >>= BL.writeFile (temp </> "notes.mid")
    getFile 2 >>= BL.writeFile (temp </> "audio.mogg")
    getFile 4 >>= BL.writeFile (temp </> "cover.bmp")
    getFile 6 >>= BL.writeFile (temp </> "extra.dta")
    (_, pkg, isUTF8) <- readRB3DTA $ temp </> "songs.dta"
    extra <- (if isUTF8 then D.readFileDTA_utf8 else D.readFileDTA_latin1) $ temp </> "extra.dta"
    let author = case extra of
          D.DTA _ (D.Tree _ [D.Parens (D.Tree _
            ( D.String "backend"
            : D.Parens (D.Tree _ [D.Key "author", D.String s])
            : _
            ))])
            -> Just s
          _ -> Nothing
    importRB3 pkg (fmap T.pack author)
      (temp </> "notes.mid")
      (temp </> "audio.mogg")
      (temp </> "cover.bmp")
      "cover.bmp"
      dir

-- | CONs put out by C3 Magma sometimes bizarrely have the @tracks_count@ key
-- completely removed from @songs.dta@, but the list of track counts is still
-- there. So, we have to put it back before parsing @song@ as a key-value map.
fixTracksCount :: [D.Chunk String] -> [D.Chunk String]
fixTracksCount = map findSong where
  findSong = \case
    D.Parens (D.Tree w (D.Key "song" : rest)) ->
      D.Parens (D.Tree w (D.Key "song" : map findTracksCount rest))
    x -> x
  findTracksCount = \case
    D.Parens (D.Tree w [D.Parens (D.Tree w2 nums)]) ->
      D.Parens $ D.Tree w [D.Key "tracks_count", D.Parens $ D.Tree w2 nums]
    x -> x

-- | Returns @(short song name, DTA file contents, is UTF8)@
readRB3DTA :: FilePath -> IO (String, D.SongPackage, Bool)
readRB3DTA dtaPath = do
  -- Not sure what encoding it is, try both.
  let readSongWith :: (FilePath -> IO (D.DTA String)) -> IO (String, D.SongPackage)
      readSongWith rdr = do
        dta <- rdr dtaPath
        (k, chunks) <- case D.treeChunks $ D.topTree dta of
          [D.Parens (D.Tree _ (D.Key k : chunks))] -> return (k, chunks)
          _ -> error $ dtaPath ++ " is not a valid songs.dta with exactly one song"
        case D.fromChunks $ fixTracksCount chunks of
          Left e -> error $ dtaPath ++ " couldn't be unserialized: " ++ e
          Right pkg -> return (k, pkg)
  (k_l1, l1) <- readSongWith D.readFileDTA_latin1
  case D.fromKeyword <$> D.encoding l1 of
    Just "utf8" -> (\(k, pkg) -> (k, pkg, True)) <$> readSongWith D.readFileDTA_utf8
    Just "latin1" -> return (k_l1, l1, False)
    Nothing -> return (k_l1, l1, False)
    Just enc -> error $ dtaPath ++ " specifies an unrecognized encoding: " ++ enc

-- | Collects the contents of an RBA or CON file into an Onyx project.
importRB3 :: D.SongPackage -> Maybe T.Text -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
importRB3 pkg author mid mogg cover coverName dir = do
  Dir.copyFile mogg $ dir </> "audio.mogg"
  Dir.copyFile mid $ dir </> "notes.mid"
  Dir.copyFile cover $ dir </> coverName
  md5 <- show . MD5.md5 <$> BL.readFile (dir </> "audio.mogg")
  rb3mid <- Load.fromFile (dir </> "notes.mid") >>= printStackTraceIO . RBFile.readMIDIFile
  let genre = T.pack $ D.fromKeyword $ D.genre pkg
  subgenre <- case D.subGenre pkg of
    Nothing -> do
      hPutStrLn stderr "Warning: when importing a CON file, no subgenre specified"
      return $ defaultSubgenre genre
    Just subk -> case stripPrefix "subgenre_" $ D.fromKeyword subk of
      Nothing -> do
        hPutStrLn stderr $ "Warning: when importing a CON file, couldn't parse the subgenre: " ++ D.fromKeyword subk
        return $ defaultSubgenre genre
      Just sub -> return $ T.pack sub
  drumkit <- case D.drumBank pkg of
    Nothing -> return HardRockKit
    Just x -> case either id D.fromKeyword x of
      "sfx/kit01_bank.milo" -> return HardRockKit
      "sfx/kit02_bank.milo" -> return ArenaKit
      "sfx/kit03_bank.milo" -> return VintageKit
      "sfx/kit04_bank.milo" -> return TrashyKit
      "sfx/kit05_bank.milo" -> return ElectronicKit
      s -> do
        hPutStrLn stderr $ "Warning: when importing a CON file, unrecognized drum bank " ++ show s
        return HardRockKit
  Y.encodeFile (dir </> "song.yml") SongYaml
    { _metadata = Metadata
      { _title        = T.pack $ D.name pkg
      , _artist       = T.pack $ D.artist pkg
      , _album        = T.pack $ fromMaybe "" $ D.albumName pkg
      , _genre        = genre
      , _subgenre     = subgenre
      , _year         = fromIntegral $ D.yearReleased pkg
      , _fileAlbumArt = Just coverName
      , _trackNumber  = maybe 0 fromIntegral $ D.albumTrackNumber pkg
      , _comments     = []
      , _vocalGender  = Just $ D.vocalGender pkg
      , _difficulty   = let
        diffMap :: Map.Map String Integer
        diffMap = D.fromDict $ D.rank pkg
        in Difficulties
          { _difficultyDrums   = Rank <$> Map.lookup "drum" diffMap
          , _difficultyGuitar  = Rank <$> Map.lookup "guitar" diffMap
          , _difficultyBass    = Rank <$> Map.lookup "bass" diffMap
          , _difficultyKeys    = Rank <$> Map.lookup "keys" diffMap
          , _difficultyProKeys = Rank <$> Map.lookup "real_keys" diffMap
          , _difficultyVocal   = Rank <$> Map.lookup "vocals" diffMap
          , _difficultyBand    = Rank <$> Map.lookup "band" diffMap
          }
      , _key          = toEnum . fromEnum <$> D.vocalTonicNote pkg
      , _autogenTheme = AutogenDefault
      , _author       = fromMaybe (T.pack "Unknown") author
      , _rating       = toEnum $ fromIntegral $ D.rating pkg - 1
      , _drumKit      = drumkit
      , _auto2xBass   = False
      , _hopoThreshold = fromIntegral $ fromMaybe 170 $ D.hopoThreshold $ D.song pkg
      , _previewStart = Just $ fromIntegral (fst $ D.preview pkg) / 1000
      , _previewEnd   = Just $ fromIntegral (snd $ D.preview pkg) / 1000
      }
    , _audio = HM.empty
    , _jammit = HM.empty
    , _plans = HM.singleton (T.pack "mogg") $ let
      instChans :: Map.Map String [Integer]
      instChans = fmap chanList $ D.fromDict $ D.fromInParens $ D.tracks $ D.song pkg
      chanList :: Either Integer (D.InParens [Integer]) -> [Integer]
      chanList (Left n)                = [n]
      chanList (Right (D.InParens ns)) = ns
      in MoggPlan
        { _moggMD5 = T.pack md5
        , _moggGuitar = maybe [] (map fromIntegral) $ Map.lookup "guitar" instChans
        , _moggBass   = maybe [] (map fromIntegral) $ Map.lookup "bass" instChans
        , _moggKeys   = maybe [] (map fromIntegral) $ Map.lookup "keys" instChans
        , _moggDrums  = maybe [] (map fromIntegral) $ Map.lookup "drum" instChans
        , _moggVocal  = maybe [] (map fromIntegral) $ Map.lookup "vocals" instChans
        , _moggCrowd  = maybe [] (map fromIntegral) $ D.crowdChannels $ D.song pkg
        , _pans = map realToFrac $ D.fromInParens $ D.pans $ D.song pkg
        , _vols = map realToFrac $ D.fromInParens $ D.vols $ D.song pkg
        , _planComments = []
        , _drumMix = let
          drumEvents = concat [ toList t | RBFile.PartDrums t <- RBFile.s_tracks rb3mid ]
          drumMixes = [ aud | RBDrums.DiffEvent _ (RBDrums.Mix aud _) <- drumEvents ]
          in case drumMixes of
            [] -> RBDrums.D0
            aud : auds -> if all (== aud) auds
              then aud
              else error $ "When importing a CON file: inconsistent drum mixes: " ++ show (nub drumMixes)
        }
    , _instruments = let
      diffMap :: Map.Map String Integer
      diffMap = D.fromDict $ D.rank pkg
      in Instruments
        { _hasDrums   = maybe False (/= 0) $ Map.lookup "drum" diffMap
        , _hasGuitar  = maybe False (/= 0) $ Map.lookup "guitar" diffMap
        , _hasBass    = maybe False (/= 0) $ Map.lookup "bass" diffMap
        , _hasKeys    = maybe False (/= 0) $ Map.lookup "keys" diffMap
        , _hasProKeys = maybe False (/= 0) $ Map.lookup "real_keys" diffMap
        , _hasVocal   = if maybe False (/= 0) $ Map.lookup "vocals" diffMap
          then case D.vocalParts $ D.song pkg of
            0 -> Vocal0
            1 -> Vocal1
            2 -> Vocal2
            3 -> Vocal3
            n -> error $ "When importing a CON file: invalid vocal count of " ++ show n
          else Vocal0
        }
    , _published = True
    }
