module Import where

import           Config                         hiding (Difficulty)
import           Control.Monad.Extra            (replicateM)
import           Control.Monad.Trans.StackTrace (printStackTraceIO)
import           Data.Binary.Get                (getWord32le, runGet)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
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
import qualified RockBand.Drums                 as RBDrums
import qualified RockBand.File                  as RBFile
import qualified Sound.MIDI.File.Load           as Load
import           STFS.Extract                   (extractSTFS)
import qualified System.Directory               as Dir
import           System.FilePath                (takeDirectory, takeFileName,
                                                 (<.>), (</>))
import           System.IO                      (IOMode (..), SeekMode (..),
                                                 hSeek, withBinaryFile)
import           System.IO.Extra                (latin1, readFileEncoding',
                                                 utf8)
import           System.IO.Temp                 (withSystemTempDirectory)

-- | Convert a CON or RBA file to Onyx format.
importFile :: FilePath -> FilePath -> IO ()
importFile file dir = do
  magic <- withBinaryFile file ReadMode $ \h -> B.hGet h 4
  if magic `elem` [B8.pack "CON ", B8.pack "STFS"]
    then withSystemTempDirectory "onyx_con" $ \temp -> do
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
    else if magic == B8.pack "RBSF"
      then do
        withSystemTempDirectory "onyx_rba" $ \temp -> do
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
      else error $ file ++ " is not a CON or RBA file; can't import"

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
  Y.encodeFile (dir </> "song.yml") SongYaml
    { _metadata = Metadata
      { _title        = T.pack $ D.name pkg
      , _artist       = T.pack $ D.artist pkg
      , _album        = T.pack $ fromMaybe "" $ D.albumName pkg
      , _genre        = T.pack $ D.fromKeyword $ D.genre pkg
      , _subgenre     = T.pack $ case D.subGenre pkg of
        Nothing -> error $ "When importing a CON file: no subgenre specified"
        Just subk -> case stripPrefix "subgenre_" $ D.fromKeyword subk of
          Nothing -> error $ "When importing a CON file: can't read subgenre: " ++ D.fromKeyword subk
          Just sub -> sub
      , _year         = fromIntegral $ D.yearReleased pkg
      , _fileAlbumArt = Just coverName
      , _trackNumber  = maybe 0 fromIntegral $ D.albumTrackNumber pkg
      , _fileCountin  = Nothing
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
      , _drumKit      = HardRockKit -- TODO
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
