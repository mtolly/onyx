{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Development.Shake hiding ((%>))
import qualified Development.Shake as Shake
import Development.Shake.FilePath
import Development.Shake.Classes
import YAMLTree
import Config
import Audio
import OneFoot
import Magma
import X360
import Image
import qualified Data.Aeson as A
import Control.Applicative ((<$>), (<|>))
import Control.Monad (forM_, when, guard)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (sort)
import Scripts
import qualified Sound.Jammit.Base as J
import qualified Sound.Jammit.Export as J
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified Data.Conduit.Audio as CA

import qualified Data.DTA as D
import qualified Data.DTA.Serialize as D
import qualified Data.DTA.Serialize.RB3 as D
import qualified Data.DTA.Serialize.Magma as Magma
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Traversable as T

import qualified RockBand.File as RBFile

import Codec.Picture

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
      jSearchInstrument :: J.Instrument -> Action [FilePath]
      jSearchInstrument inst = do
        audios <- jSearch
        let parts = [ J.Only p | p <- [minBound .. maxBound], J.partToInstrument p == inst ]
        return $ mapMaybe (`lookup` audios) parts
  forM_ ["1p", "2p"] $ \feet -> do
    let dir = "gen/jammit" </> feet
    dir </> "drums_untimed.wav" %> \out -> do
      audios <- jSearchInstrument J.Drums
      case audios of
        [] -> buildAudio (Silence 2 $ CA.Seconds 0) out
        _  -> liftIO $ J.runAudio audios [] out
    dir </> "bass_untimed.wav" %> \out -> do
      audios <- jSearchInstrument J.Bass
      case audios of
        [] -> buildAudio (Silence 2 $ CA.Seconds 0) out
        _  -> liftIO $ J.runAudio audios [] out
    dir </> "guitar_untimed.wav" %> \out -> do
      audios <- jSearchInstrument J.Guitar
      case audios of
        [] -> buildAudio (Silence 2 $ CA.Seconds 0) out
        _  -> liftIO $ J.runAudio audios [] out
    dir </> "song_untimed.wav" %> \out -> do
      audios <- jSearch
      let backs = do
            (jpart, rbpart) <-
              -- listed in order of backing preference
              [ (J.Drums , Drums  )
              , (J.Bass  , Bass   )
              , (J.Guitar, Guitar )
              ]
            guard $ rbpart `elem` _config s
            case lookup (J.Without jpart) audios of
              Nothing    -> []
              Just audio -> [(rbpart, audio)]
      case backs of
        [] -> fail "No Jammit instrument package used in this song was found."
        (rbpart, back) : _ -> flip buildAudio out $ Mix $ concat
          [ [ Input $ JammitAIFC back ]
          , [ Gain (-1) $ Input $ sndable $ dir </> "drums_untimed.wav"
            | rbpart /= Drums && elem Drums (_config s)
            ]
          , [ Gain (-1) $ Input $ sndable $ dir </> "bass_untimed.wav"
            | rbpart /= Bass && elem Bass (_config s)
            ]
          , [ Gain (-1) $ Input $ sndable $ dir </> "guitar_untimed.wav"
            | rbpart /= Guitar && elem Guitar (_config s)
            ]
          ]
    forM_ ["drums", "bass", "guitar", "song"] $ \part -> do
      dir </> (part ++ ".wav") %> \out -> do
        let untimed = sndable $ dropExtension out ++ "_untimed.wav"
        case HM.lookup "jammit" $ _audio s of
          Nothing -> fail "No jammit audio configuration"
          Just (AudioSimple aud) ->
            buildAudio (fmap (const untimed) aud) out
          Just (AudioStems _) ->
            fail "jammit audio configuration is stems (unsupported)"

-- | Rules to build non-stems audio, where the single audio file goes into
-- the \"song\", and the individual instrument tracks are silent.
simpleRules :: Song -> Rules ()
simpleRules s = do
  forM_ [ (src, aud) | (src, AudioSimple aud) <- HM.toList $ _audio s, src /= "jammit" ] $ \(src, aud) -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
      forM_ ["drums.wav", "bass.wav", "guitar.wav"] $ \inst -> do
        dir </> inst %> buildAudio (Silence 2 $ CA.Seconds 0)
      dir </> "song.wav" %> \out -> do
        let pat = "audio-" ++ src ++ ".*"
        ls <- getDirectoryFiles "" [pat]
        case ls of
          []    -> fail $ "No file found matching pattern " ++ pat
          f : _ -> buildAudio (fmap (const $ sndable f) aud) out

-- | Rules to build (non-Jammit) stems audio, where each instrument plus the
-- backing track can have its own audio expression.
stemsRules :: Song -> Rules ()
stemsRules s = do
  forM_ [ (src, amap) | (src, AudioStems amap) <- HM.toList $ _audio s ] $ \(src, amap) -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
      forM_ (HM.toList amap) $ \(inst, aud) -> do
        dir </> (inst <.> "wav") %> \out -> do
          aud' <- T.forM aud $ \f -> let
            findMatch fp = do
              let pat = fp -<.> "*"
              ls <- getDirectoryFiles "" [pat]
              case ls of
                []     -> fail $ "No file found matching pattern " ++ pat
                f' : _ -> return f'
            in case f of
              Sndable    fp pos -> (\g -> Sndable g pos) <$> findMatch fp
              Rate r     fp pos -> (\g -> Rate  r g pos) <$> findMatch fp
              JammitAIFC fp     -> JammitAIFC <$> findMatch fp -- shouldn't happen
          buildAudio aud' out

eachAudio :: (Monad m) => Song -> (String -> m ()) -> m ()
eachAudio = forM_ . HM.keys . _audio

-- | The given function should accept the version title and directory.
eachVersion :: (Monad m) => Song -> (String -> FilePath -> m ()) -> m ()
eachVersion s f = eachAudio s $ \src ->
  forM_ [("1p", ""), ("2p", " (2x Bass Pedal)")] $ \(feet, titleSuffix) ->
    f (_title s ++ titleSuffix) ("gen" </> src </> feet)

-- | Rules to generate the countin audio from a track in the MIDI file, and
-- then combine it with the backing track.
countinRules :: Song -> Rules ()
countinRules s = eachVersion s $ \_ dir -> do
  dir </> "countin.wav" %> \out -> do
    let mid = dir </> "notes.mid"
        hit = _fileCountin s
    makeCountin mid hit out
  dir </> "song-countin.wav" %> \out -> do
    let song = Input $ sndable $ dir </> "song.wav"
        countin = Input $ sndable $ dir </> "countin.wav"
    buildAudio (Mix [song, countin]) out
  dir </> "song-countin.ogg" %> \out -> do
    buildAudio (Input $ sndable $ out -<.> "wav") out

-- | Rules to generate the multi-track OGG Vorbis file (.ogg), and then
-- stick the Rock Band seeking header onto the front (making a .mogg).
oggRules :: Song -> Rules ()
oggRules s = eachVersion s $ \_ dir -> do
  dir </> "audio.ogg" %> \out -> do
    let drums = Input $ sndable $ dir </> "drums.wav"
        bass  = Input $ sndable $ dir </> "bass.wav"
        guitar = Input $ sndable $ dir </> "guitar.wav"
        song  = Input $ sndable $ dir </> "song-countin.wav"
        audio = Merge $ let
          parts = concat
            [ [drums | Drums `elem` _config s]
            , [bass | Bass `elem` _config s]
            , [guitar | Guitar `elem` _config s]
            , [song]
            ]
          in if length parts == 3
            then parts ++ [Silence 1 $ CA.Seconds 0]
            -- the Silence is to work around oggenc bug:
            -- it assumes 6 channels is 5.1 surround where the last channel
            -- is LFE, so instead we add a silent 7th channel
            else parts
    buildAudio audio out
  dir </> "audio.mogg" %> \mogg -> do
    let ogg = mogg -<.> "ogg"
    need [ogg]
    liftIO $ oggToMogg ogg mogg

-- | Makes the (low-quality) audio files for the online preview app.
crapRules :: Rules ()
crapRules = do
  let src = "gen/album/2p/song-countin.wav"
      preview ext = "gen/album/2p/preview-audio" <.> ext
  preview "wav" %> \out -> do
    need [src]
    cmd "sox" [src, out] "remix 1,2"
  preview "mp3" %> \out -> do
    need [preview "wav"]
    cmd "lame" [preview "wav", out] "-b 16"
  preview "ogg" %> \out -> do
    need [preview "wav"]
    cmd "oggenc -b 16 --resample 16000 -o" [out, preview "wav"]

midRules :: Song -> Rules ()
midRules s = eachAudio s $ \src -> do
  let mid1p = "gen" </> src </> "1p/notes.mid"
      mid2p = "gen" </> src </> "2p/notes.mid"
  mid2p %> \out -> do
    input <- loadMIDI "notes.mid"
    let ftempos = "tempo-" ++ src ++ ".mid"
    tempos <- doesFileExist ftempos >>= \b -> if b
      then loadMIDI ftempos
      else return input
    let withTempos = input { RBFile.s_tempos = RBFile.s_tempos tempos }
        fixed = RBFile.eachTrack (RBFile.copyExpert . RBFile.autoHandPosition) $
          fixRolls $ autoBeat $ drumMix 0 withTempos
    saveMIDI out fixed
  mid1p %> \out -> loadMIDI mid2p >>= saveMIDI out . oneFoot 0.18 0.11

guitarRules :: Song -> Rules ()
guitarRules s = eachVersion s $ \_ dir -> do
  let mid = dir </> "notes.mid"
      gtr = dir </> "guitar.mid"
  gtr %> \out -> do
    input <- loadMIDI mid
    saveMIDI out $ RBFile.playGuitarFile (_guitarTuning s) (_bassTuning s) input

newtype JammitResults = JammitResults (String, String)
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Wraps the Shake operator to also install a dependency on song.yml.
(%>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
pat %> f = pat Shake.%> \out -> do
  need ["song.yml"]
  f out
infix 1 %>

main :: IO ()
main = Env.getArgs >>= \argv -> case filter ((/= "-") . take 1) argv of
  [] -> interactive
  [f] | takeFileName f == "song.yml" -> do
    Dir.setCurrentDirectory $ takeDirectory f
    interactive
  _ -> batch
  where interactive = do
          song <- theSong
          -- interactive mode not implemented yet,
          -- for now we just list the files you can build
          prog <- Env.getProgName
          putStrLn $ "Usage: " ++ prog ++ " <files to build>"
          putStrLn "This song can produce the following files:"
          eachVersion song $ \_ dir -> do
            putStrLn $ dir </> "rb3.con"
            putStrLn $ dir </> "magma.rba"
          shake shakeOptions $ do
            want []
            theRules song
          cleanup
        batch = do
          song <- theSong
          shakeArgs shakeOptions $ theRules song
          cleanup
        theSong = readYAMLTree "song.yml" >>= \yaml -> case A.fromJSON yaml of
          A.Error   s    -> fail s
          A.Success song -> return song
        theRules song = do
          _ <- addOracle $ \(JammitResults (title, artist)) ->
            jammitSearch title artist
          phony "clean" $ cmd "rm -rf gen"
          midRules song
          guitarRules song
          jammitRules song
          simpleRules song
          stemsRules song
          countinRules song
          oggRules song
          coverRules song
          rb3Rules song
          magmaRules song
          fofRules song
          crapRules
        cleanup = do
          e <-     Dir.doesDirectoryExist       "gen/temp"
          when e $ Dir.removeDirectoryRecursive "gen/temp"

packageID :: FilePath -> Song -> String
packageID dir s = let
  buildID = hash (_title s, _artist s, dir) `mod` 1000000000
  in "onyx" ++ show buildID

rb3Rules :: Song -> Rules ()
rb3Rules s = eachVersion s $ \title dir -> do
  let pkg = packageID dir s
      pathDta = dir </> "rb3/songs/songs.dta"
      pathMid = dir </> "rb3/songs" </> pkg </> (pkg <.> "mid")
      pathMogg = dir </> "rb3/songs" </> pkg </> (pkg <.> "mogg")
      pathPng = dir </> "rb3/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
      pathCon = dir </> "rb3.con"
  pathDta %> \out -> do
    songPkg <- makeDTA pkg title (dir </> "notes.mid") s
    let dta = D.DTA 0 $ D.Tree 0 $ (:[]) $ D.Parens $ D.Tree 0 $
          D.Key (B8.pack pkg) : D.toChunks songPkg
    writeFile' out $ D.sToDTA dta
  pathMid %> copyFile' (dir </> "notes-magma-export.mid")
  pathMogg %> copyFile' (dir </> "audio.mogg")
  pathPng %> copyFile' "gen/cover.png_xbox"
  pathCon %> \out -> do
    need [pathDta, pathMid, pathMogg, pathPng]
    liftIO $ rb3pkg
      (_artist s ++ ": " ++ _title s)
      ("Version: " ++ drop 4 dir)
      (dir </> "rb3")
      out

makeDTA :: String -> String -> FilePath -> Song -> Action D.SongPackage
makeDTA pkg title mid s = do
  (pstart, pend) <- previewBounds mid
  len <- songLength mid
  let numChannels = length (_config s) * 2 + 2
  return D.SongPackage
    { D.name = B8.pack title
    , D.artist = B8.pack $ _artist s
    , D.master = True
    , D.songId = Right $ D.Keyword $ B8.pack pkg
    , D.song = D.Song
      { D.songName = B8.pack $ "songs/" ++ pkg ++ "/" ++ pkg
      , D.tracksCount = Just $ D.InParens
        [ if Drums `elem` _config s then 2 else 0
        , if Bass `elem` _config s then 2 else 0
        , if Guitar `elem` _config s then 2 else 0
        , 0
        , 0
        , 2
        ]
      , D.tracks = D.InParens $ D.Dict $ Map.fromList $ let
        channelNums = zip [0..] $ concatMap (\x -> [x, x]) $ sort $ _config s
        -- ^ the sort is important
        channelNumsFor inst = [ i | (i, inst') <- channelNums, inst == inst' ]
        trackDrum = (B8.pack "drum", Right $ D.InParens $ channelNumsFor Drums)
        trackBass = (B8.pack "bass", Right $ D.InParens $ channelNumsFor Bass)
        trackGuitar = (B8.pack "guitar", Right $ D.InParens $ channelNumsFor Guitar)
        in concat
          [ [trackDrum | Drums `elem` _config s]
          , [trackBass | Bass `elem` _config s]
          , [trackGuitar | Guitar `elem` _config s]
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
      , (B8.pack "bass", if Bass `elem` _config s then 1 else 0)
      , (B8.pack "guitar", if Guitar `elem` _config s then 1 else 0)
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
      Just Male   -> Magma.Male
      Just Female -> Magma.Female
      Nothing     -> Magma.Female
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
  let loadRGB8 = do
        let img = _fileAlbumArt s
        need [img]
        res <- liftIO $ readImage img
        case res of
          Left  err -> fail $ "Failed to load cover art (" ++ img ++ "): " ++ err
          Right dyn -> return $ anyToRGB8 dyn
  "gen/cover.bmp" %> \out -> loadRGB8 >>= liftIO . writeBitmap out . scaleBilinear 256 256
  "gen/cover.png" %> \out -> loadRGB8 >>= liftIO . writePng    out . scaleBilinear 256 256
  "gen/cover.dds" %> \out -> loadRGB8 >>= liftIO . writeDDS    out . scaleBilinear 256 256
  "gen/cover.png_xbox" %> \out -> do
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

magmaRules :: Song -> Rules ()
magmaRules s = eachVersion s $ \title dir -> do
  let drums  = dir </> "magma/drums.wav"
      bass   = dir </> "magma/bass.wav"
      guitar = dir </> "magma/guitar.wav"
      song   = dir </> "magma/song-countin.wav"
      cover  = dir </> "magma/cover.bmp"
      mid    = dir </> "magma/notes.mid"
      proj   = dir </> "magma/magma.rbproj"
      rba    = dir </> "magma.rba"
      export = dir </> "notes-magma-export.mid"
  drums %> copyFile' (dir </> "drums.wav")
  bass %> copyFile' (dir </> "bass.wav")
  guitar %> copyFile' (dir </> "guitar.wav")
  song %> copyFile' (dir </> "song-countin.wav")
  cover %> copyFile' "gen/cover.bmp"
  mid %> \out -> do
    base <- loadMIDI $ dir </> "notes.mid"
    let cleaned = base { RBFile.s_tracks = filter magmaSafe $ RBFile.s_tracks base }
        magmaSafe (RBFile.Countin          _) = False
        magmaSafe (RBFile.PartRealGuitar   _) = False
        magmaSafe (RBFile.PartRealGuitar22 _) = False
        magmaSafe (RBFile.PartRealBass     _) = False
        magmaSafe (RBFile.PartRealBass22   _) = False
        magmaSafe _                             = True
    saveMIDI out cleaned
  proj %> \out -> do
    let pkg = packageID dir s
    p <- makeMagmaProj pkg title (dir </> "notes.mid") s
    let dta = D.DTA 0 $ D.Tree 0 $ D.toChunks p
    writeFile' out $ D.sToDTA dta
  rba %> \out -> do
    when (Drums `elem` _config s) $ need [drums]
    when (Bass `elem` _config s) $ need [bass]
    when (Guitar `elem` _config s) $ need [guitar]
    need [song, cover, mid, proj]
    liftIO $ runMagma proj out
  export %> \out -> do
    need [mid, proj]
    liftIO $ runMagmaMIDI proj out

makeMagmaProj :: String -> String -> FilePath -> Song -> Action Magma.RBProj
makeMagmaProj pkg title mid s = do
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
        { Magma.songName = B8.pack title
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
        , Magma.rankDrum = 1
        , Magma.rankBass = 1
        , Magma.rankGuitar = 1
        , Magma.rankVocals = 1
        , Magma.rankKeys = 1
        , Magma.rankProKeys = 1
        , Magma.rankBand = 1
        , Magma.vocalScrollSpeed = 2300
        , Magma.animTempo = 32
        , Magma.vocalGender = case _vocalGender s of
          Just Male   -> Magma.Male
          Just Female -> Magma.Female
          Nothing     -> Magma.Female
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
      , Magma.destinationFile = B8.pack $ pkg <.> "rba"
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
        , Magma.guitar = if Guitar `elem` _config s
          then stereoFile "guitar.wav"
          else emptyAudioFile
        , Magma.vocals = emptyAudioFile
        , Magma.keys = emptyAudioFile
        , Magma.backing = stereoFile "song-countin.wav"
        }
      }
    }

fofRules :: Song -> Rules ()
fofRules s = eachVersion s $ \title dir -> do
  let mid = dir </> "fof/notes.mid"
      png = dir </> "fof/album.png"
      drums = dir </> "fof/drums.ogg"
      bass = dir </> "fof/rhythm.ogg"
      guitar = dir </> "fof/guitar.ogg"
      song = dir </> "fof/song.ogg"
      ini = dir </> "fof/song.ini"
  mid %> copyFile' (dir </> "notes.mid")
  png %> copyFile' "gen/cover.png"
  drums %> buildAudio (Input $ sndable $ dir </> "drums.wav")
  bass %> buildAudio (Input $ sndable $ dir </> "bass.wav")
  guitar %> buildAudio (Input $ sndable $ dir </> "guitar.wav")
  song %> buildAudio (Input $ sndable $ dir </> "song-countin.wav")
  ini %> \out -> makeIni title mid s >>= writeFile' out
  phony (dir </> "fof-all") $ do
    need [mid, png, song, ini]
    when (Drums `elem` _config s) $ need [drums]
    when (Bass `elem` _config s) $ need [bass]
    when (Guitar `elem` _config s) $ need [guitar]

makeIni :: String -> FilePath -> Song -> Action String
makeIni title mid s = do
  len <- songLength mid
  let iniLines =
        [ ("name", title)
        , ("artist", _artist s)
        , ("album", _album s)
        , ("genre", _genre s)
        , ("year", show $ _year s)
        , ("song_length", show len)
        , ("charter", "Onyxite")
        , ("diff_band", "0")
        , ("diff_drums", if Drums `elem` _config s then "0" else "-1")
        , ("diff_bass", if Bass `elem` _config s then "0" else "-1")
        , ("diff_guitar", if Guitar `elem` _config s then "0" else "-1")
        , ("diff_vocals", "-1")
        , ("diff_keys", "-1")
        ]
      makeLine (x, y) = x ++ " = " ++ y
  return $ unlines $ "[song]" : map makeLine iniLines
