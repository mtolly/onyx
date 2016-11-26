{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Import where

import           Audio
import           Config                           hiding (Difficulty)
import           Control.Applicative              ((<|>))
import           Control.Monad                    (guard, when)
import           Control.Monad.Extra              (mapMaybeM, replicateM)
import           Control.Monad.Trans.StackTrace   (printStackTraceIO)
import           Data.Binary.Get                  (getWord32le, runGet)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.Digest.Pure.MD5             as MD5
import qualified Data.DTA                         as D
import qualified Data.DTA.Serialize.RB3           as D
import qualified Data.DTA.Serialize2              as D2
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (nub)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Yaml                        as Y
import qualified FretsOnFire                      as FoF
import           JSONData                         (JSONEither (..))
import           PrettyDTA                        (C3DTAComments (..),
                                                   DTASingle (..),
                                                   readDTASingle, readRB3DTA)
import qualified RockBand.Drums                   as RBDrums
import qualified RockBand.File                    as RBFile
import           Scripts                          (loadMIDI_IO)
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           STFS.Extract                     (extractSTFS)
import qualified System.Directory                 as Dir
import           System.FilePath                  (takeDirectory, takeFileName,
                                                   (<.>), (</>))
import           System.IO                        (IOMode (..), SeekMode (..),
                                                   hPutStrLn, hSeek, stderr,
                                                   withBinaryFile)
import           System.IO.Temp                   (withSystemTempDirectory)

-- | Convert a CON or RBA file (or FoF directory) to Onyx format.
importAny :: KeysRB2 -> FilePath -> FilePath -> IO ()
importAny krb2 src dest = do
  Dir.createDirectoryIfMissing True dest
  isDir <- Dir.doesDirectoryExist src
  if isDir
    then importFoF krb2 src dest
    else do
      magic <- withBinaryFile src ReadMode $ \h -> B.hGet h 4
      case B8.unpack magic of
        "CON " -> importSTFS krb2 src dest
        "STFS" -> importSTFS krb2 src dest
        "RBSF" -> importRBA  krb2 src dest
        _      -> error $ src ++ " is not in a supported song format"

standardTargets :: Maybe (JSONEither Integer T.Text) -> Maybe Integer -> Bool -> KeysRB2 -> HM.HashMap T.Text Target
standardTargets songID version is2x krb2 = let
  targets1x =
    [ ("rb3", RB3 TargetRB3
        { rb3_Plan = Nothing
        , rb3_2xBassPedal = False
        , rb3_SongID = songID
        , rb3_Label = Nothing
        , rb3_Version = version
        }
      )
    , ("rb2", RB2 TargetRB2
        { rb2_Plan = Nothing
        , rb2_2xBassPedal = False
        , rb2_SongID = songID
        , rb2_Label = Nothing
        , rb2_Keys = krb2
        , rb2_Version = version
        }
      )
    , ("ps", PS TargetPS
        { ps_Plan = Nothing
        , ps_Label = Nothing
        }
      )
    ]
  targets2x =
    [ ("rb3-2x", RB3 TargetRB3
        { rb3_Plan = Nothing
        , rb3_2xBassPedal = True
        , rb3_SongID = songID
        , rb3_Label = Nothing
        , rb3_Version = version
        }
      )
    , ("rb2-2x", RB2 TargetRB2
        { rb2_Plan = Nothing
        , rb2_2xBassPedal = True
        , rb2_SongID = songID
        , rb2_Label = Nothing
        , rb2_Keys = krb2
        , rb2_Version = version
        }
      )
    ]
  in HM.fromList $ targets1x ++ if is2x then targets2x else []

importFoF :: KeysRB2 -> FilePath -> FilePath -> IO ()
importFoF krb2 src dest = do
  song <- FoF.loadSong $ src </> "song.ini"
  midi@(F.Cons _ _ midtrks) <- Load.fromFile $ src </> "notes.mid"
  let trackNames = mapMaybe U.trackName midtrks

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
      [ "drums.ogg", "drums_1.ogg", "drums_2.ogg", "drums_3.ogg", "drums_4.ogg"
      , "guitar.ogg", "keys.ogg", "rhythm.ogg", "vocals.ogg", "vocals_1.ogg", "vocals_2.ogg"
      , "crowd.ogg", "song.ogg"
      ]

  let gtrAudio = case audioFiles of
        ["guitar.ogg"] -> [] -- assume sole guitar is no-stems audio
        _              -> filter (== "guitar.ogg") audioFiles
      bassAudio = filter (== "rhythm.ogg") audioFiles
      keysAudio = filter (== "keys.ogg") audioFiles
      crowdAudio = filter (== "crowd.ogg") audioFiles
      voxAudio = filter (`elem` ["vocals.ogg", "vocals_1.ogg", "vocals_2.ogg"]) audioFiles
      d0 = "drums.ogg"
      d1 = "drums_1.ogg"
      d2 = "drums_2.ogg"
      d3 = "drums_3.ogg"
      d4 = "drums_4.ogg"
      (drumsAudio, kickAudio, snareAudio)
        | all (`elem` audioFiles) [d1, d2, d3, d4] = ([d3, d4], [d1], [d2])
        | all (`elem` audioFiles) [d1, d2, d3] = ([d3], [d1], [d2])
        | d0 `elem` audioFiles = ([d0], [], [])
        | otherwise = ([], [], [])
      songAudio = case audioFiles of
        ["guitar.ogg"] -> ["guitar.ogg"]
        _              -> filter (== "song.ogg") audioFiles

  parsed <- loadMIDI_IO $ src </> "notes.mid"
  let pad = RBFile.needsPad parsed
      padDelay :: (Num a) => a
      padDelay = if pad then 3 else 0
      maybePadAudio = if pad then Pad Start $ CA.Seconds padDelay else id
      (delayAudio, delayMIDI) = case FoF.delay song of
        Nothing -> (maybePadAudio, RBFile.padMIDI padDelay)
        Just n -> case compare n 0 of
          EQ -> (maybePadAudio, RBFile.padMIDI padDelay)
          GT -> let
            secs = fromIntegral n / 1000
            midiDelay = ceiling secs
            audioDelay = fromIntegral midiDelay - secs
            padDelay' :: (Num a) => a
            padDelay' = if midiDelay >= padDelay then 0 else padDelay - fromIntegral midiDelay
            in (Pad Start $ CA.Seconds $ audioDelay + padDelay', RBFile.padMIDI $ midiDelay + padDelay')
          LT ->
            ( Pad Start $ CA.Seconds $ fromIntegral (abs n) / 1000 + padDelay
            , RBFile.padMIDI padDelay
            )
      audioExpr [] = Nothing
      audioExpr [aud] = Just PlanAudio
        { _planExpr = delayAudio $ Input $ Named $ T.pack aud
        , _planPans = []
        , _planVols = []
        }
      audioExpr auds = Just PlanAudio
        { _planExpr = delayAudio $ Concatenate $ map (Input . Named . T.pack) auds
        , _planPans = []
        , _planVols = []
        }

  let toTier = fmap $ \n -> Tier $ max 1 $ min 7 $ fromIntegral n + 1

  let drumToDrums (RBFile.RawTrack t) = RBFile.RawTrack $ if U.trackName t == Just "PART DRUM"
        then U.setTrackName "PART DRUMS" t
        else t
      drumToDrums trk                 = trk
  mid2x <- Dir.doesFileExist $ src </> "expert+.mid"
  add2x <- if mid2x
    then do
      raw2x <- RBFile.readMIDIRaw <$> Load.fromFile (src </> "expert+.mid")
      let rawTracks = flip mapMaybe (RBFile.s_tracks raw2x) $ \case
            RBFile.RawTrack t -> Just t
            _                 -> Nothing
          isDrums t = elem (U.trackName t) [Just "PART DRUMS", Just "PART DRUM"]
      return $ case filter isDrums rawTracks of
        []    -> id
        d : _ -> (RBFile.RawTrack (U.setTrackName "PART DRUMS_2X" d) :)
    else return id

  let raw = RBFile.readMIDIRaw midi
  Save.toFile (dest </> "notes.mid") $ RBFile.showMIDIFile $ delayMIDI raw
    { RBFile.s_tracks = add2x $ map drumToDrums $ flip filter (RBFile.s_tracks raw) $ \case
      RBFile.RawTrack t -> U.trackName t /= Just "BEAT"
      RBFile.Beat _ -> False
      _ -> True
    }

  Y.encodeFile (dest </> "song.yml") SongYaml
    { _metadata = Metadata
      { _title        = FoF.name song
      , _artist       = FoF.artist song
      , _album        = FoF.album song
      , _genre        = FoF.genre song
      , _subgenre     = Nothing
      , _year         = FoF.year song
      , _fileAlbumArt = guard hasAlbumArt >> Just "album.png"
      , _trackNumber  = FoF.track song
      , _comments     = []
      , _vocalGender  = Nothing
      , _difficulty   = Difficulties
        { _difficultyDrums     = toTier $ FoF.diffDrums song
        , _difficultyGuitar    = toTier $ FoF.diffGuitar song
        , _difficultyBass      = toTier $ FoF.diffBass song
        , _difficultyKeys      = toTier $ FoF.diffKeys song
        , _difficultyProKeys   = toTier $ FoF.diffKeysReal song
        , _difficultyProGuitar = toTier $ FoF.diffGuitarReal song
        , _difficultyProBass   = toTier $ FoF.diffBassReal song
        , _difficultyVocal     = toTier $ FoF.diffVocals song
        , _difficultyBand      = toTier $ FoF.diffBand song
        }
      , _key          = Nothing
      , _autogenTheme = AutogenDefault
      , _author       = FoF.charter song
      , _rating       = Unrated
      , _drumKit      = HardRockKit
      , _drumLayout   = StandardLayout
      , _previewStart = case FoF.previewStartTime song of
        Just ms | ms >= 0 -> Just $ PreviewSeconds $ fromIntegral ms / 1000
        _       -> Nothing
      , _previewEnd   = Nothing
      , _languages    = _languages def
      , _convert      = _convert def
      , _rhythmKeys   = _rhythmKeys def
      , _rhythmBass   = _rhythmBass def
      , _catEMH       = _catEMH def
      , _expertOnly   = _expertOnly def
      , _cover        = _cover def
      }
    , _options = Options
      { _auto2xBass      = False
      , _hopoThreshold   = 170
      , _proGuitarTuning = []
      , _proBassTuning   = []
      , _proDrums        = case FoF.proDrums song of
        Just b  -> b
        Nothing -> any
          (\case RBDrums.ProType _ _ -> True; _ -> False)
          (foldr RTB.merge RTB.empty [ t | RBFile.PartDrums t <- RBFile.s_tracks parsed ])
      }
    , _audio = HM.fromList $ flip map audioFiles $ \aud -> (T.pack aud, AudioFile
      { _md5 = Nothing
      , _frames = Nothing
      , _commands = []
      , _filePath = Just aud
      , _rate = Nothing
      , _channels = 2 -- TODO
      })
    , _jammit = HM.empty
    , _plans = HM.singleton "fof" Plan
      { _song         = audioExpr songAudio
      , _countin      = Countin []
      , _guitar       = audioExpr gtrAudio
      , _bass         = audioExpr bassAudio
      , _keys         = audioExpr keysAudio
      , _kick         = audioExpr kickAudio
      , _snare        = audioExpr snareAudio
      , _drums        = audioExpr drumsAudio
      , _vocal        = audioExpr voxAudio
      , _crowd        = audioExpr crowdAudio
      , _planComments = []
      }
    , _targets = standardTargets Nothing Nothing True krb2
    , _instruments = Instruments
      { _hasDrums   = elem "PART DRUMS" trackNames && FoF.diffDrums song /= Just (-1)
      , _hasGuitar  = elem "PART GUITAR" trackNames && FoF.diffGuitar song /= Just (-1)
      , _hasBass    = elem "PART BASS" trackNames && FoF.diffBass song /= Just (-1)
      , _hasKeys    = elem "PART KEYS" trackNames && FoF.diffKeys song /= Just (-1)
      , _hasProKeys = elem "PART REAL_KEYS_X" trackNames && FoF.diffKeysReal song /= Just (-1)
      , _hasProGuitar =
        (elem "PART REAL_GUITAR" trackNames && FoF.diffGuitarReal song /= Just (-1)) ||
        (elem "PART REAL_GUITAR_22" trackNames && FoF.diffGuitarReal22 song /= Just (-1))
      , _hasProBass =
        (elem "PART REAL_BASS" trackNames && FoF.diffBassReal song /= Just (-1)) ||
        (elem "PART REAL_BASS_22" trackNames && FoF.diffBassReal22 song /= Just (-1))
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

determine2xBass :: T.Text -> (T.Text, Bool)
determine2xBass s = case T.stripSuffix " (2x Bass Pedal)" s <|> T.stripSuffix " (2X Bass Pedal)" s of
  Nothing -> (s , False)
  Just s' -> (s', True )

importSTFS :: KeysRB2 -> FilePath -> FilePath -> IO ()
importSTFS krb2 file dir = withSystemTempDirectory "onyx_con" $ \temp -> do
  extractSTFS file temp
  DTASingle _ pkg comments <- readDTASingle $ temp </> "songs/songs.dta"
  let c3Title = fromMaybe (D.name pkg) $ c3dtaSong comments
      (title, is2x) = case c3dta2xBass comments of
        Just b  -> (fst $ determine2xBass c3Title, b)
        Nothing -> determine2xBass c3Title
      meta = def
        { _author = c3dtaAuthoredBy comments
        , _title = Just title
        , _convert = fromMaybe (_convert def) $ c3dtaConvert comments
        , _rhythmKeys = fromMaybe (_rhythmKeys def) $ c3dtaRhythmKeys comments
        , _rhythmBass = fromMaybe (_rhythmBass def) $ c3dtaRhythmBass comments
        , _catEMH = fromMaybe (_catEMH def) $ c3dtaCATemh comments
        , _expertOnly = fromMaybe (_expertOnly def) $ c3dtaExpertOnly comments
        , _languages = fromMaybe (_languages def) $ c3dtaLanguages comments
        }
      karaoke = fromMaybe False $ c3dtaKaraoke comments
      multitrack = fromMaybe False $ c3dtaMultitrack comments
      base = T.unpack $ D.songName $ D.song pkg
      -- Note: the base path does NOT necessarily have to be songs/foo/foo
      -- where foo is the top key of songs.dta. foo can be different!
      -- e.g. C3's "Escape from the City" has a top key 'SonicAdvCityEscape2x'
      -- and a 'name' of "songs/sonicadv2cityescape2x/sonicadv2cityescape2x"
  importRB3 krb2 pkg
    meta
    karaoke
    multitrack
    is2x
    (temp </> base <.> "mid")
    (temp </> base <.> "mogg")
    (temp </> takeDirectory base </> "gen" </> (takeFileName base ++ "_keep.png_xbox"))
    "cover.png_xbox"
    dir

getRBAFile :: Int -> FilePath -> FilePath -> IO ()
getRBAFile i rba out = withBinaryFile rba ReadMode $ \h -> do
  hSeek h AbsoluteSeek 0x08
  let read7words = runGet (replicateM 7 getWord32le) <$> BL.hGet h (7 * 4)
  offsets <- read7words
  sizes <- read7words
  hSeek h AbsoluteSeek $ fromIntegral $ offsets !! i
  BL.hGet h (fromIntegral $ sizes !! i) >>= BL.writeFile out

importRBA :: KeysRB2 -> FilePath -> FilePath -> IO ()
importRBA krb2 file dir = withSystemTempDirectory "onyx_rba" $ \temp -> do
  getRBAFile 0 file $ temp </> "songs.dta"
  getRBAFile 1 file $ temp </> "notes.mid"
  getRBAFile 2 file $ temp </> "audio.mogg"
  getRBAFile 4 file $ temp </> "cover.bmp"
  getRBAFile 6 file $ temp </> "extra.dta"
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
      (title, is2x) = determine2xBass $ D.name pkg
      -- TODO: import more stuff from the extra dta
      meta = def
        { _author = author
        , _title = Just title
        }
  importRB3 krb2 pkg
    meta
    False
    True
    is2x
    (temp </> "notes.mid")
    (temp </> "audio.mogg")
    (temp </> "cover.bmp")
    "cover.bmp"
    dir

-- | Collects the contents of an RBA or CON file into an Onyx project.
importRB3 :: KeysRB2 -> D.SongPackage -> Metadata -> Bool -> Bool -> Bool -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
importRB3 krb2 pkg meta karaoke multitrack is2x mid mogg cover coverName dir = do
  Dir.copyFile mogg $ dir </> "audio.mogg"
  Dir.copyFile mid $ dir </> "notes.mid"
  Dir.copyFile cover $ dir </> coverName
  md5 <- show . MD5.md5 <$> BL.readFile (dir </> "audio.mogg")
  rb3mid <- Load.fromFile (dir </> "notes.mid") >>= printStackTraceIO . RBFile.readMIDIFile
  drumkit <- case D.drumBank pkg of
    Nothing -> return HardRockKit
    Just x -> case x of
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
      { _title        = _title meta <|> Just (D.name pkg)
      , _artist       = Just $ D.artist pkg
      , _album        = Just $ fromMaybe "Unknown Album" $ D.albumName pkg
      , _genre        = Just $ D.genre pkg
      , _subgenre     = D.subGenre pkg >>= T.stripPrefix "subgenre_"
      , _year         = Just $ fromIntegral $ D.yearReleased pkg
      , _fileAlbumArt = Just coverName
      , _trackNumber  = fromIntegral <$> D.albumTrackNumber pkg
      , _comments     = []
      , _vocalGender  = Just $ D.vocalGender pkg
      , _difficulty   = let
        diffMap :: Map.Map T.Text Integer
        diffMap = D2.fromDict $ D.rank pkg
        in Difficulties
          { _difficultyDrums     = Rank <$> Map.lookup "drum" diffMap
          , _difficultyGuitar    = Rank <$> Map.lookup "guitar" diffMap
          , _difficultyBass      = Rank <$> Map.lookup "bass" diffMap
          , _difficultyKeys      = Rank <$> Map.lookup "keys" diffMap
          , _difficultyProKeys   = Rank <$> Map.lookup "real_keys" diffMap
          , _difficultyProGuitar = Rank <$> Map.lookup "real_guitar" diffMap
          , _difficultyProBass   = Rank <$> Map.lookup "real_bass" diffMap
          , _difficultyVocal     = Rank <$> Map.lookup "vocals" diffMap
          , _difficultyBand      = Rank <$> Map.lookup "band" diffMap
          }
      , _key          = toEnum . fromEnum <$> D.vocalTonicNote pkg
      , _autogenTheme = AutogenDefault
      , _author       = _author meta
      , _rating       = toEnum $ fromIntegral $ D.rating pkg - 1
      , _drumKit      = drumkit
      , _drumLayout   = StandardLayout -- TODO import this
      , _previewStart = Just $ PreviewSeconds $ fromIntegral (fst $ D.preview pkg) / 1000
      , _previewEnd   = Just $ PreviewSeconds $ fromIntegral (snd $ D.preview pkg) / 1000
      , _languages    = _languages meta
      , _convert      = _convert meta
      , _rhythmKeys   = _rhythmKeys meta
      , _rhythmBass   = _rhythmBass meta
      , _catEMH       = _catEMH meta
      , _expertOnly   = _expertOnly meta
      , _cover        = not $ D.master pkg
      }
    , _options = Options
      { _auto2xBass      = is2x
      , _hopoThreshold   = fromIntegral $ fromMaybe 170 $ D.hopoThreshold $ D.song pkg
      , _proGuitarTuning = fromMaybe [] $ map fromIntegral <$> D.realGuitarTuning pkg
      , _proBassTuning   = fromMaybe [] $ map fromIntegral <$> D.realBassTuning   pkg
      , _proDrums        = True
      }
    , _audio = HM.empty
    , _jammit = HM.empty
    , _plans = HM.singleton "mogg" $ let
      instChans :: Map.Map T.Text [Integer]
      instChans = D2.fromDict $ D.tracks $ D.song pkg
      in MoggPlan
        { _moggMD5 = T.pack md5
        , _moggGuitar = maybe [] (map fromIntegral) $ Map.lookup "guitar" instChans
        , _moggBass   = maybe [] (map fromIntegral) $ Map.lookup "bass" instChans
        , _moggKeys   = maybe [] (map fromIntegral) $ Map.lookup "keys" instChans
        , _moggDrums  = maybe [] (map fromIntegral) $ Map.lookup "drum" instChans
        , _moggVocal  = maybe [] (map fromIntegral) $ Map.lookup "vocals" instChans
        , _moggCrowd  = maybe [] (map fromIntegral) $ D.crowdChannels $ D.song pkg
        , _pans = map realToFrac $ D.pans $ D.song pkg
        , _vols = map realToFrac $ D.vols $ D.song pkg
        , _planComments = []
        , _drumMix = let
          drumEvents = concat [ toList t | RBFile.PartDrums t <- RBFile.s_tracks rb3mid ]
          drumMixes = [ aud | RBDrums.DiffEvent _ (RBDrums.Mix aud _) <- drumEvents ]
          in case drumMixes of
            [] -> RBDrums.D0
            aud : auds -> if all (== aud) auds
              then aud
              else error $ "When importing a CON file: inconsistent drum mixes: " ++ show (nub drumMixes)
        , _karaoke = karaoke
        , _multitrack = multitrack
        }
    , _targets = let
      songID = fmap JSONEither $ case D.songId pkg of
        Left  i -> guard (i /= 0) >> Just (Left i)
        Right k -> Just $ Right k
      in standardTargets songID (songID >> Just (D.version pkg)) is2x krb2
    , _instruments = let
      diffMap :: Map.Map T.Text Integer
      diffMap = D2.fromDict $ D.rank pkg
      in Instruments
        { _hasDrums     = maybe False (/= 0) $ Map.lookup "drum" diffMap
        , _hasGuitar    = maybe False (/= 0) $ Map.lookup "guitar" diffMap
        , _hasBass      = maybe False (/= 0) $ Map.lookup "bass" diffMap
        , _hasKeys      = maybe False (/= 0) $ Map.lookup "keys" diffMap
        , _hasProKeys   = maybe False (/= 0) $ Map.lookup "real_keys" diffMap
        , _hasProGuitar = maybe False (/= 0) $ Map.lookup "real_guitar" diffMap
        , _hasProBass   = maybe False (/= 0) $ Map.lookup "real_bass" diffMap
        , _hasVocal     = if maybe False (/= 0) $ Map.lookup "vocals" diffMap
          then case D.vocalParts $ D.song pkg of
            Nothing -> Vocal1
            Just 0 -> Vocal0
            Just 1 -> Vocal1
            Just 2 -> Vocal2
            Just 3 -> Vocal3
            n -> error $ "When importing a CON file: invalid vocal count of " ++ show n
          else Vocal0
        }
    , _published = True
    }
