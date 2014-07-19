{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Main where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes
import YAMLTree
import Config
import Audio
import qualified Data.Aeson as A
import Control.Applicative ((<$>))
import Control.Monad (forM_, unless)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (stripPrefix)
import Data.Bifunctor (bimap)
import Scripts.Main

import qualified Data.DTA as D
import qualified Data.DTA.Serialize as D
import qualified Data.DTA.Serialize.RB3 as D
import qualified Data.DTA.Serialize.Magma as Magma
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM

jammitTitle :: Song -> String
jammitTitle s = fromMaybe (_title s) (_jammitTitle s)

jammitArtist :: Song -> String
jammitArtist s = fromMaybe (_artist s) (_jammitArtist s)

jammitSearch :: String -> String -> Action String
jammitSearch title artist = do
  Stdout out <- cmd "jammittools -d -T" [title] "-R" [artist]
  return $ case reverse $ words out of
    []          -> ""
    "Parts" : _ -> ""
    parts   : _ -> parts

jammitRules :: Song -> Rules ()
jammitRules s = do
  let jCmd = ["jammittools", "-T", jTitle, "-R", jArtist]
      jTitle  = jammitTitle s
      jArtist = jammitArtist s
      jSearch = askOracle $ JammitResults (jTitle, jArtist)
  forM_ ["1p", "2p"] $ \feet -> do
    let dir = "gen/jammit" </> feet
    dir </> "drums_untimed.wav" *> \out -> do
      hasDrums <- ('d' `elem`) <$> jSearch
      if hasDrums
        then cmd jCmd "-y d -a" out
        else buildAudio (Silence 2 0) out
    dir </> "bass_untimed.wav" *> \out -> do
      hasBass <- ('b' `elem`) <$> jSearch
      if hasBass
        then cmd jCmd "-y b -a" out
        else buildAudio (Silence 2 0) out
    dir </> "song_untimed.wav" *> \out -> do
      has <- jSearch
      let hasDrums = 'd' `elem` has
          hasBass  = 'b' `elem` has
          configHas inst = inst `elem` _config s
      case (configHas Drums, configHas Bass) of
        (True, False) -> cmd jCmd "-y D -a" [out]
        (False, True) -> cmd jCmd "-y B -a" [out]
        (True, True) -> case (hasDrums, hasBass) of
          (True , True ) -> cmd jCmd "-y D -n b -a" [out]
          (True , False) -> cmd jCmd "-y D -a" [out]
          (False, True ) -> cmd jCmd "-y B -a" [out]
          (False, False) -> fail "Couldn't find Jammit drums or bass"
        (False, False) -> cmd jCmd "-y" [""] "-a" [out]
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
  forM_ [ (src, aud) | (src, AudioSimple aud) <- HM.toList $ _audio s, src /= "jammit"] $ \(src, aud) -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
      forM_ ["drums.wav", "bass.wav"] $ \inst -> do
        dir </> inst *> buildAudio (Silence 2 0)
        dir </> "song.wav" *> \out -> do
          userAudio <- getDirectoryFiles "" ["audio-" ++ src ++ ".*"]
          case userAudio of
            [] -> fail $ "no audio-" ++ src ++ ".xxx found"
            ua : _ -> do
              need [ua]
              buildAudio (bimap realToFrac (const ua) aud) out

eachAudio :: (Monad m) => Song -> (String -> m ()) -> m ()
eachAudio = forM_ . HM.keys . _audio

countinRules :: Song -> Rules ()
countinRules s = do
  eachAudio s $ \src -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
      dir </> "countin.wav" *> \out -> do
        let mid = dir </> "notes.mid"
            hit = "../../sound/hihat-foot.wav"
        makeCountin mid hit out
      dir </> "song-countin.wav" *> \out -> do
        let song = File $ dir </> "song.wav"
            countin = File $ dir </> "countin.wav"
        buildAudio (Combine Mix [song, countin]) out

oggRules :: Song -> Rules ()
oggRules s =
  eachAudio s $ \src -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
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
        cmd "ogg2mogg" [ogg, mogg]

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
  yaml <- readYAMLTree "erotomania.yml"
  case A.fromJSON yaml of
    A.Error s -> fail s
    A.Success song -> shakeArgs shakeOptions $ do
      _ <- addOracle $ \(JammitResults (title, artist)) ->
        jammitSearch title artist
      phony "clean" $ cmd "rm -rf gen"
      midRules song
      jammitRules song
      simpleRules song
      -- TODO: stemsRules song
      countinRules song
      oggRules song
      coverRules song
      rb3Rules song

rb3Rules :: Song -> Rules ()
rb3Rules s = do
  eachAudio s $ \src -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
          pkg = _package s
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
      pathPng *> copyFile' ("gen/cover.png_xbox")
      pathCon *> \out -> do
        need [pathDta, pathMid, pathMogg, pathPng]
        cmd "rb3pkg -p" [_artist s ++ ": " ++ _title s] "-d"
          ["Version: " ++ src ++ "/" ++ feet] "-f" [dir </> "rb3"] out

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
      , D.vols = D.InParens $ take numChannels $ repeat 0
      , D.cores = D.InParens $ take numChannels $ repeat (-1)
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
  forM_ ["bmp", "dds"] $ \ext -> do
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
