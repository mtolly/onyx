{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Main where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes
import YAMLTree
import Config
import Audio
import qualified Data.Aeson as A
import Control.Applicative ((<$>), (<|>))
import Control.Monad (forM_, unless, when)
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import Data.List (stripPrefix)
import Data.Bifunctor (bimap, first)
import Scripts.Main
import Codec.Container.Ogg.Mogg (oggToMogg)
import qualified Sound.Jammit.Base as J
import qualified Sound.Jammit.Export as J
import qualified System.Environment as Env

import qualified Data.DTA as D
import qualified Data.DTA.Serialize as D
import qualified Data.DTA.Serialize.RB3 as D
import qualified Data.DTA.Serialize.Magma as Magma
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Traversable as T

jammitLib :: IO J.Library
jammitLib = do
  env <- Env.lookupEnv "JAMMIT"
  def <- J.findJammitDir
  case env <|> def of
    Nothing  -> error "jammitDir: couldn't find Jammit directory"
    Just dir -> J.loadLibrary dir

jammitTitle :: Song -> String
jammitTitle s = fromMaybe (_title s) (_jammitTitle s)

jammitArtist :: Song -> String
jammitArtist s = fromMaybe (_artist s) (_jammitArtist s)

jammitSearch :: String -> String -> Action String
jammitSearch title artist
  = show
  . J.getAudioParts
  . J.exactSearchBy J.title title
  . J.exactSearchBy J.artist artist
  <$> liftIO jammitLib

jammitRules :: Song -> Rules ()
jammitRules s = do
  let jTitle  = jammitTitle s
      jArtist = jammitArtist s
      jSearch :: Action [(J.AudioPart, FilePath)]
      jSearch = fmap read $ askOracle $ JammitResults (jTitle, jArtist)
  forM_ ["1p", "2p"] $ \feet -> do
    let dir = "gen/jammit" </> feet
    dir </> "drums_untimed.wav" *> \out -> do
      audios <- jSearch
      case lookup (J.Only J.PartDrums) audios of
        Nothing -> buildAudio (Silence 2 0) out
        Just fp -> liftIO $ J.runAudio [fp] [] out
    dir </> "bass_untimed.wav" *> \out -> do
      audios <- jSearch
      case lookup (J.Only J.PartBass) audios of
        Nothing -> buildAudio (Silence 2 0) out
        Just fp -> liftIO $ J.runAudio [fp] [] out
    dir </> "song_untimed.wav" *> \out -> do
      audios <- jSearch
      let hasDrums = isJust $ lookup (J.Only J.PartDrums) audios
          hasBass  = isJust $ lookup (J.Only J.PartBass) audios
          configHas inst = inst `elem` _config s
          getAudio aud = case lookup aud audios of
            Nothing -> error $ "Couldn't find audio part: " ++ show aud
            Just x  -> x
          runAudio' y n = liftIO $
            J.runAudio (map getAudio y) (map getAudio n) out
      case (configHas Drums, configHas Bass) of
        (True, False) -> runAudio' [J.Without J.Drums] []
        (False, True) -> runAudio' [J.Without J.Bass] []
        (True, True) -> case (hasDrums, hasBass) of
          (True , True ) -> runAudio' [J.Without J.Drums] [J.Only J.PartBass]
          (True , False) -> runAudio' [J.Without J.Drums] []
          (False, True ) -> runAudio' [J.Without J.Bass] []
          (False, False) -> fail "Couldn't find Jammit drums or bass"
        (False, False) -> runAudio' [] []
    forM_ ["drums", "bass", "song"] $ \part -> do
      dir </> (part ++ ".wav") *> \out -> do
        let untimed = dropExtension out ++ "_untimed.wav"
        case HM.lookup "jammit" $ _audio s of
          Nothing -> fail "No jammit audio configuration"
          Just (AudioSimple aud) ->
            buildAudio (bimap realToFrac (const untimed) aud) out
          Just (AudioStems _) ->
            fail "jammit audio configuration is stems (unsupported)"

simpleRules :: Song -> Rules ()
simpleRules s = do
  forM_ [ (src, aud) | (src, AudioSimple aud) <- HM.toList $ _audio s, src /= "jammit" ] $ \(src, aud) -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
      forM_ ["drums.wav", "bass.wav"] $ \inst -> do
        dir </> inst *> buildAudio (Silence 2 0)
      dir </> "song.wav" *> \out -> do
        let pat = "audio-" ++ src ++ ".*"
        ls <- getDirectoryFiles "" [pat]
        case ls of
          []    -> fail $ "No file found matching pattern " ++ pat
          f : _ -> buildAudio (bimap realToFrac (const f) aud) out

stemsRules :: Song -> Rules ()
stemsRules s = do
  forM_ [ (src, amap) | (src, AudioStems amap) <- HM.toList $ _audio s ] $ \(src, amap) -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
      forM_ (HM.toList amap) $ \(inst, aud) -> do
        dir </> (inst <.> "wav") *> \out -> do
          aud' <- T.forM aud $ \f -> do
            let pat = f -<.> "*"
            ls <- getDirectoryFiles "" [pat]
            case ls of
              []     -> fail $ "No file found matching pattern " ++ pat
              f' : _ -> return f'
          buildAudio (first realToFrac aud') out

eachAudio :: (Monad m) => Song -> (String -> m ()) -> m ()
eachAudio = forM_ . HM.keys . _audio

eachVersionDir :: (Monad m) => Song -> (FilePath -> m ()) -> m ()
eachVersionDir s f = eachAudio s $ \src ->
  forM_ ["1p", "2p"] $ \feet ->
    f $ "gen" </> src </> feet

countinRules :: Song -> Rules ()
countinRules s = eachVersionDir s $ \dir -> do
  dir </> "countin.wav" *> \out -> do
    let mid = dir </> "notes.mid"
        hit = "../../sound/hihat-foot.wav"
    makeCountin mid hit out
  dir </> "song-countin.wav" *> \out -> do
    let song = File $ dir </> "song.wav"
        countin = File $ dir </> "countin.wav"
    buildAudio (Combine Mix [song, countin]) out

oggRules :: Song -> Rules ()
oggRules s = eachVersionDir s $ \dir -> do
  dir </> "audio.ogg" *> \out -> do
    let drums = File $ dir </> "drums.wav"
        bass  = File $ dir </> "bass.wav"
        song  = File $ dir </> "song-countin.wav"
        audio = Combine Merge $ let
          parts = concat
            [ [drums | Drums `elem` _config s]
            , [bass | Bass `elem` _config s]
            , [song]
            ]
          in if length parts == 3
            then parts ++ [Silence 1 0]
            -- the Silence is to work around oggenc bug:
            -- it assumes 6 channels is 5.1 surround where the last channel
            -- is LFE, so instead we add a silent 7th channel
            else parts
    buildAudio audio out
  dir </> "audio.mogg" *> \mogg -> do
    let ogg = mogg -<.> "ogg"
    need [ogg]
    liftIO $ oggToMogg ogg mogg

midRules :: Song -> Rules ()
midRules s = eachAudio s $ \src -> do
  let mid1p = "gen" </> src </> "1p/notes.mid"
      mid2p = "gen" </> src </> "2p/notes.mid"
  mid1p *> \out -> do
    need ["notes.mid"]
    let tempos = "tempo-" ++ src ++ ".mid"
    b <- doesFileExist tempos
    if b
      then replaceTempos "notes.mid" tempos out
      else fixResolution "notes.mid" out
  mid2p *> \out -> make2xBassPedal mid1p out

newtype JammitResults = JammitResults (String, String)
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

main :: IO ()
main = do
  yaml <- readYAMLTree "song.yml"
  case A.fromJSON yaml of
    A.Error s -> fail s
    A.Success song -> shakeArgs shakeOptions $ do
      _ <- addOracle $ \(JammitResults (title, artist)) ->
        jammitSearch title artist
      phony "clean" $ cmd "rm -rf gen"
      midRules song
      jammitRules song
      simpleRules song
      stemsRules song
      countinRules song
      oggRules song
      coverRules song
      rb3Rules song
      magmaRules song
      fofRules song

rb3Rules :: Song -> Rules ()
rb3Rules s = eachVersionDir s $ \dir -> do
  let pkg = _package s
      pathDta = dir </> "rb3/songs/songs.dta"
      pathMid = dir </> "rb3/songs" </> pkg </> (pkg <.> "mid")
      pathMogg = dir </> "rb3/songs" </> pkg </> (pkg <.> "mogg")
      pathPng = dir </> "rb3/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
      pathCon = dir </> "rb3.con"
  pathDta *> \out -> do
    songPkg <- makeDTA (dir </> "notes.mid") s
    let dta = D.DTA 0 $ D.Tree 0 $ (:[]) $ D.Parens $ D.Tree 0 $
          D.Key (B8.pack pkg) : D.toChunks songPkg
    writeFile' out $ D.sToDTA dta
  pathMid *> copyFile' (dir </> "notes.mid")
  pathMogg *> copyFile' (dir </> "audio.mogg")
  pathPng *> copyFile' "gen/cover.png_xbox"
  pathCon *> \out -> do
    need [pathDta, pathMid, pathMogg, pathPng]
    cmd "rb3pkg -p" [_artist s ++ ": " ++ _title s] "-d"
      ["Version: " ++ drop 4 dir] "-f" [dir </> "rb3"] out

makeDTA :: FilePath -> Song -> Action D.SongPackage
makeDTA mid s = do
  (pstart, pend) <- previewBounds mid
  len <- songLength mid
  let numChannels = length (_config s) * 2 + 2
  return D.SongPackage
    { D.name = B8.pack $ _title s
    , D.artist = B8.pack $ _artist s
    , D.master = True
    , D.songId = Right $ D.Keyword $ B8.pack $ _package s
    , D.song = D.Song
      { D.songName = B8.pack $ "songs/" ++ _package s ++ "/" ++ _package s
      , D.tracksCount = Just $ D.InParens
        [ if Drums `elem` _config s then 2 else 0
        , if Bass `elem` _config s then 2 else 0
        , 0
        , 0
        , 0
        , 2
        ]
      , D.tracks = D.InParens $ D.Dict $ Map.fromList $ let
        channelNums = zip [0..] $ concatMap (\x -> [x, x]) $ _config s
        channelNumsFor inst = [ i | (i, inst') <- channelNums, inst == inst' ]
        trackDrum = (B8.pack "drum", Right $ D.InParens $ channelNumsFor Drums)
        trackBass = (B8.pack "bass", Right $ D.InParens $ channelNumsFor Bass)
        in concat
          [ [trackDrum | Drums `elem` _config s]
          , [trackBass | Bass `elem` _config s]
          ]
      , D.vocalParts = 0
      , D.pans = D.InParens $ take numChannels $ cycle [-1, 1]
      , D.vols = D.InParens $ replicate numChannels 0
      , D.cores = D.InParens $ replicate numChannels (-1)
      , D.drumSolo = D.DrumSounds $ D.InParens $ map (D.Keyword . B8.pack) $ words
        "kick.cue snare.cue tom1.cue tom2.cue crash.cue"
      , D.drumFreestyle = D.DrumSounds $ D.InParens $ map (D.Keyword . B8.pack) $ words
        "kick.cue snare.cue hat.cue ride.cue crash.cue"
      }
    , D.bank = Just $ Left $ B8.pack "sfx/tambourine_bank.milo"
    , D.drumBank = Nothing
    , D.animTempo = Left D.KTempoMedium
    , D.bandFailCue = Nothing
    , D.songScrollSpeed = 2300
    , D.preview = (fromIntegral pstart, fromIntegral pend)
    , D.songLength = fromIntegral len
    , D.rank = D.Dict $ Map.fromList
      [ (B8.pack "drum", if Drums `elem` _config s then 1 else 0)
      , (B8.pack "guitar", 0)
      , (B8.pack "bass", if Bass `elem` _config s then 1 else 0)
      , (B8.pack "vocals", 0)
      , (B8.pack "keys", 0)
      , (B8.pack "real_keys", 0)
      , (B8.pack "band", 1)
      ]
    , D.solo = Nothing
    , D.format = 10
    , D.version = 30
    , D.gameOrigin = D.Keyword $ B8.pack "ugc_plus"
    , D.rating = 4
    , D.genre = D.Keyword $ B8.pack $ _genre s
    , D.subGenre = Just $ D.Keyword $ B8.pack $ "subgenre_" ++ _subgenre s
    , D.vocalGender = case _vocalGender s of
      Male -> Magma.Male
      Female -> Magma.Female
    , D.shortVersion = Nothing
    , D.yearReleased = fromIntegral $ _year s
    , D.albumArt = Just True
    , D.albumName = Just $ B8.pack $ _album s
    , D.albumTrackNumber = Just $ fromIntegral $ _trackNumber s
    , D.vocalTonicNote = Nothing
    , D.songTonality = Nothing
    , D.tuningOffsetCents = Just 0
    , D.realGuitarTuning = Nothing
    , D.realBassTuning = Nothing
    , D.guidePitchVolume = Just (-3)
    , D.encoding = Just $ D.Keyword $ B8.pack "latin1"
    }

coverRules :: Song -> Rules ()
coverRules s = do
  let img = _fileAlbumArt s
  forM_ ["bmp", "dds", "png"] $ \ext -> do
    "gen/cover" <.> ext *> \out -> do
      need [img]
      cmd "convert" [img] "-resize 256x256!" [out]
  "gen/cover.png_xbox" *> \out -> do
    let dds = out -<.> "dds"
    need [dds]
    b <- liftIO $ B.readFile dds
    let header =
          [ 0x01, 0x04, 0x08, 0x00, 0x00, 0x00, 0x04, 0x00
          , 0x01, 0x00, 0x01, 0x80, 0x00, 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
          , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
          ]
        bytes = B.unpack $ B.drop 0x80 b
        flipPairs (x : y : xs) = y : x : flipPairs xs
        flipPairs _ = []
        b' = B.pack $ header ++ flipPairs bytes
    liftIO $ B.writeFile out b'

checkPrograms :: Action ()
checkPrograms = do
  Stdout soxHelp <- cmd "sox -h"
  case mapMaybe (stripPrefix "AUDIO FILE FORMATS:") $ lines soxHelp of
    [fmtLine] -> let
      formats = words fmtLine
      in unless (all (`elem` formats) ["ogg", "vorbis"]) $
        fail $ unlines
          [ "Your SoX doesn't support OGG Vorbis."
          , "You must recompile SoX with libsndfile installed, see:"
          , "http://sourceforge.net/p/sox/mailman/message/30383689/"
          ]
    _ -> fail "Couldn't read SoX supported file formats."

magmaRules :: Song -> Rules ()
magmaRules s = eachVersionDir s $ \dir -> do
  let drums = dir </> "magma/drums.wav"
      bass = dir </> "magma/bass.wav"
      song = dir </> "magma/song-countin.wav"
      cover = dir </> "magma/cover.bmp"
      mid = dir </> "magma/notes.mid"
      proj = dir </> "magma/magma.rbproj"
      rba = dir </> "magma.rba"
  drums *> copyFile' (dir </> "drums.wav")
  bass *> copyFile' (dir </> "bass.wav")
  song *> copyFile' (dir </> "song-countin.wav")
  cover *> copyFile' "gen/cover.bmp"
  mid *> magmaClean (dir </> "notes.mid")
  proj *> \out -> do
    p <- makeMagmaProj (dir </> "notes.mid") s
    let dta = D.DTA 0 $ D.Tree 0 $ D.toChunks p
    writeFile' out $ D.sToDTA dta
  rba *> \_ -> do
    when (Drums `elem` _config s) $ need [drums]
    when (Bass `elem` _config s) $ need [bass]
    need [song, cover, mid, proj]
    cmd "magmyx -c3" [proj, rba]

makeMagmaProj :: FilePath -> Song -> Action Magma.RBProj
makeMagmaProj mid s = do
  (pstart, _) <- previewBounds mid
  let emptyDryVox = Magma.DryVoxPart
        { Magma.dryVoxFile = B8.pack ""
        , Magma.dryVoxEnabled = True
        }
      emptyAudioFile = Magma.AudioFile
        { Magma.audioEnabled = False
        , Magma.channels = 0
        , Magma.pan = []
        , Magma.vol = []
        , Magma.audioFile = B8.pack ""
        }
      stereoFile f = Magma.AudioFile
        { Magma.audioEnabled = True
        , Magma.channels = 2
        , Magma.pan = [-1, 1]
        , Magma.vol = [0, 0]
        , Magma.audioFile = B8.pack f
        }
  return Magma.RBProj
    { Magma.project = Magma.Project
      { Magma.toolVersion = B8.pack "110411_A"
      , Magma.projectVersion = 24
      , Magma.metadata = Magma.Metadata
        { Magma.songName = B8.pack $ _title s
        , Magma.artistName = B8.pack $ _artist s
        , Magma.genre = D.Keyword $ B8.pack $ _genre s
        , Magma.subGenre = D.Keyword $ B8.pack $ "subgenre_" ++ _subgenre s
        , Magma.yearReleased = fromIntegral $ _year s
        , Magma.albumName = B8.pack $ _album s
        , Magma.author = B8.pack "Onyxite"
        , Magma.releaseLabel = B8.pack "Onyxite Customs"
        , Magma.country = D.Keyword $ B8.pack "ugc_country_us"
        , Magma.price = 160
        , Magma.trackNumber = fromIntegral $ _trackNumber s
        , Magma.hasAlbum = True
        }
      , Magma.gamedata = Magma.Gamedata
        { Magma.previewStartMs = fromIntegral pstart
        , Magma.rankGuitar = 1
        , Magma.rankBass = 1
        , Magma.rankDrum = 1
        , Magma.rankVocals = 1
        , Magma.rankKeys = 1
        , Magma.rankProKeys = 1
        , Magma.rankBand = 1
        , Magma.vocalScrollSpeed = 2300
        , Magma.animTempo = 32
        , Magma.vocalGender = case _vocalGender s of
          Male -> Magma.Male
          Female -> Magma.Female
        , Magma.vocalPercussion = Magma.Tambourine
        , Magma.vocalParts = 0
        , Magma.guidePitchVolume = -3
        }
      , Magma.languages = Magma.Languages
        { Magma.english = True
        , Magma.french = False
        , Magma.italian = False
        , Magma.spanish = False
        , Magma.german = False
        , Magma.japanese = False
        }
      , Magma.destinationFile = B8.pack $ _package s <.> "rba"
      , Magma.midi = Magma.Midi
        { Magma.midiFile = B8.pack "notes.mid"
        , Magma.autogenTheme = Right $ B8.pack ""
        }
      , Magma.dryVox = Magma.DryVox
        { Magma.part0 = emptyDryVox
        , Magma.part1 = emptyDryVox
        , Magma.part2 = emptyDryVox
        , Magma.tuningOffsetCents = 0
        }
      , Magma.albumArt = Magma.AlbumArt $ B8.pack "cover.bmp"
      , Magma.tracks = Magma.Tracks
        { Magma.drumLayout = Magma.Kit
        , Magma.drumKit = if Drums `elem` _config s
          then stereoFile "drums.wav"
          else emptyAudioFile
        , Magma.drumKick = emptyAudioFile
        , Magma.drumSnare = emptyAudioFile
        , Magma.bass = if Bass `elem` _config s
          then stereoFile "bass.wav"
          else emptyAudioFile
        , Magma.guitar = emptyAudioFile
        , Magma.vocals = emptyAudioFile
        , Magma.keys = emptyAudioFile
        , Magma.backing = stereoFile "song-countin.wav"
        }
      }
    }

fofRules :: Song -> Rules ()
fofRules s = eachVersionDir s $ \dir -> do
  let mid = dir </> "fof/notes.mid"
      png = dir </> "fof/album.png"
      drums = dir </> "fof/drums.ogg"
      bass = dir </> "fof/rhythm.ogg"
      song = dir </> "fof/song.ogg"
      ini = dir </> "fof/song.ini"
  mid *> copyFile' (dir </> "notes.mid")
  png *> copyFile' "gen/cover.png"
  drums *> buildAudio (File $ dir </> "drums.wav")
  bass *> buildAudio (File $ dir </> "bass.wav")
  song *> buildAudio (File $ dir </> "song-countin.wav")
  ini *> \out -> makeIni mid s >>= writeFile' out
  phony (dir </> "fof-all") $ do
    need [mid, png, song, ini]
    when (Drums `elem` _config s) $ need [drums]
    when (Bass `elem` _config s) $ need [bass]

makeIni :: FilePath -> Song -> Action String
makeIni mid s = do
  len <- songLength mid
  let iniLines =
        [ ("name", _title s)
        , ("artist", _artist s)
        , ("album", _album s)
        , ("genre", _genre s)
        , ("year", show $ _year s)
        , ("song_length", show len)
        , ("charter", "Onyxite")
        , ("diff_band", "0")
        , ("diff_drums", if Drums `elem` _config s then "0" else "-1")
        , ("diff_guitar", "-1")
        , ("diff_bass", if Bass `elem` _config s then "0" else "-1")
        , ("diff_vocals", "-1")
        , ("diff_keys", "-1")
        ]
      makeLine (x, y) = x ++ " = " ++ y
  return $ unlines $ "[song]" : map makeLine iniLines
